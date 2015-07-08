-module(ewebapi_router).

-export([
         execute/3
        ]).

-include("resource.hrl").
-include("router.hrl").

-record(state, {
          resources=[],
          hops=[],
          handler=undefined,
          cta=undefined,
          ctp=undefined,
          env=[],
          api
         }).

%% =============================================================================
%%% Api
%% =============================================================================

execute(Req, Env,
        #ewebapi_router{
           prefix=Prefix,
           resources=ResourcesProplist
          } = Api) ->
    {Path, Req2} = cowboy_req:path(Req),
    {ok, Path2} = ewebapi_http_utils:split_path(Path),
    Len = length(Prefix),
    State = #state{api = Api},
    try
        case catch lists:split(Len, Path2) of
            {Prefix, RestPath} ->
                match(Req2, RestPath, ResourcesProplist, State);
            _ ->
                {ok, Req2, Env}
        end
    catch E:R ->
            reply(500, Req),
            io:format("~p,~p,~p\n", [E, R, erlang:get_stacktrace()]),
            erlang:raise(E, R, erlang:get_stacktrace())
    end.


%% =============================================================================
%%% Flow control
%% =============================================================================

match(Req, RestPath, ResourcesProplist, State) ->
    case match_(ResourcesProplist, RestPath) of
        {ok, {Methods, Resources}} ->
            State2 = State#state{resources=Resources},
            choose_handler(Req, Methods, State2);
        {error, nomatch} ->
            default_error_handler(Req, 404, wrong_path, State)
    end.

choose_handler(Req, Methods, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    case maps:find(Method, Methods) of
        {ok, Handler} ->
            State2 = State#state{handler=Handler},
            content_types_provided(Req2, State2, fun content_types_accepted/2);
        error ->
            AllowedMethods = maps:keys(Methods),
            Req3 = cowboy_req:set_resp_header(<<"allow">>, AllowedMethods, Req2),
            default_error_handler(Req3, 405, method_not_avaible, State)
    end.

content_types_provided(Req, #state{resources=[#resource{ctp=Ctps}|_]}=State,
                       Next) ->
    content_types_provided(Req, Ctps, State, Next);
content_types_provided(Req, #state{api=#ewebapi_router{ctp=Ctps}}=State,
                       Next) ->
    content_types_provided(Req, Ctps, State, Next).

content_types_provided(Req, Ctps, State, Next) ->
    case cowboy_req:parse_header(<<"accept">>, Req) of
        {error, badarg} ->
            State2 = State#state{ctp=unreachable},
            default_error_handler(Req, 400, bad_accept_header, State2);
        {ok, undefined, Req2} ->
            Ctp = hd(Ctps),
            State2 = State#state{ctp=Ctp},
            Next(Req2, State2);
        {ok, Accept, Req2} ->
            Accept2 = ewebapi_http_utils:prioritize_accept(Accept),
            choose_media_type(Req2, Accept2, Ctps, State, Next)
    end.

choose_media_type(Req, Accept, Ctps, State, Next) ->
    case ewebapi_http_utils:choose_media_type(Ctps, Accept) of
        {ok, Ctp} ->
            State2 = State#state{ctp=Ctp},
            Next(Req, State2);
        {error, Reason} ->
            State2 = State#state{ctp=unreachable},
            default_error_handler(Req, 406, Reason, State2)
    end.

content_types_accepted(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    DataRequires =
        case Method of
            <<"PUT">> -> true;
            <<"POST">> -> true;
            _ -> false
        end,
    case DataRequires of
        true ->
            content_types_accepted_(Req2, State);
        false ->
            init(Req2, State, fun apply_handler/2)
    end.

content_types_accepted_(Req, #state{resources=[#resource{cta=Ctas}|_]}=State) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, ContentType, Req2} ->
            io:format("~p of ~p\n", [ContentType, Ctas]),
            case ewebapi_http_utils:choose_content_type(ContentType, Ctas) of
                {ok, Cta} ->
                    State2 = State#state{cta=Cta},
                    init(Req2, State2, fun decode/2);
                {error, Reason} ->
                    default_error_handler(Req2, 415, Reason, State)
            end;
        {error, badarg} ->
            default_error_handler(Req, 400, bad_content_type_header, State)
    end.

decode(Req, #state{cta={_Cta, Decode}, resources=[Resource|_]}=State) ->
    case Decode(Req) of
        {ok, Result, Req2} ->
            apply_handler(Req2, Result, State);
        {halt, _Req2} = Halt ->
            Halt;
        {error, Reason, Req2} ->
            resource_error_handler(Req2, 415, Reason, Resource, State)
    end.

init(Req, #state{api=#ewebapi_router{init_handler=InitHandler}}=State, Next) ->
    case InitHandler(Req) of
        {ok, Req2, Env} ->
            State2 = State#state{env=Env},
            fold_hops(Req2, State2, Next);
        {halt, _Req2} = Halt ->
            Halt;
        {error, Reason, Req2} ->
            default_error_handler(Req2, undefined, Reason, State)
    end.

fold_hops(Req, #state{resources=[_|Resources], env=Env}=State, Next) ->
    fold_hops_(Resources, Req, Env, Next, State).
fold_hops_([], Req, Env, Next, State) ->
    State2 = State#state{env=Env},
    Next(Req, State2);
fold_hops_([#resource{hop=undefined}|Rest], Req, Env, Next, State) ->
    fold_hops_(Rest, Req, Env, Next, State);
fold_hops_([Resource|Rest], Req, Env, Next, State) ->
    Result =
        case Resource of
            #resource{is_id=true, id=Id, hop=Hop} -> Hop(Req, Id, Env);
            #resource{is_id=false, hop=Hop} -> Hop(Req, Env)
        end,
    case Result of
        {ok, Req2, Env2} -> fold_hops_(Rest, Req2, Env2, Next, State);
        {halt, _Req2} = Halt -> Halt;
        {error, Reason, Req2} ->
            State2 = State#state{env=Env},
            resource_error_handler(Req2, undefined, Reason, Resource, State2)
    end.

apply_handler(Req, Data,
              #state{
                 resources=[Resource|_],
                 handler=Handle,
                 env=Env
                } = State) ->
    Result =
        case Resource of
            #resource{id=Id, is_id=true} ->
                Handle(Req, Id, Data, Env);
            _ ->
                Handle(Req, Data, Env)
        end,
    case Result of
        {ok, Req2} ->
            reply(204, Req2);
        {ok, Reply, Req2} ->
            encode_reply(Req2, 200, Reply, State);
        {ok, Code, Reply, Req2} ->
            encode_reply(Req2, Code, Reply, State);
        {halt, _Req2} = Halt ->
            Halt;
        {error, Reason, Req2} ->
            resource_error_handler(Req2, undefined, Reason, Resource, State)
    end.

apply_handler(Req,
              #state{
                 handler=Handle,
                 resources=[Resource|_],
                 env=Env
                } = State) ->
    Result =
        case Resource of
            #resource{id=Id, is_id=true} ->
                Handle(Req, Id, Env);
            _ ->
                Handle(Req, Env)
        end,
    case Result of
        {ok, Req2} ->
            reply(204, Req2);
        {ok, Data, Req2} ->
            encode_reply(Req2, 200, Data, State);
        {ok, Code, Reply, Req2} ->
            encode_reply(Req2, Code, Reply, State);
        {halt, _Req2} = Halt ->
            Halt;
        {error, Reason, Req2} ->
            resource_error_handler(Req2, undefined, Reason, Resource, State)
    end.

encode_reply(Req, Code, Data, #state{ctp={Ctp, Encode}}=State) ->
    BinCtp = ewebapi_http_utils:content_type_to_binary(Ctp),
    Req2 = cowboy_req:set_resp_header(<<"content-type">>, BinCtp, Req),
    case Encode(Req2, Data) of
        {ok, EncodedData, Req3} ->
            reply(Req3, Code, EncodedData, State);
        {halt, _Req2} = Halt ->
            Halt
    end.

resource_error_handler(Req, Code, Reason,
                       #resource{error_handler=ErrorHandler},
                       State) when ErrorHandler =/= undefined ->
    case ErrorHandler(Req, Code, Reason) of
        {handled, Code2, Reason2, Req2} ->
            terminate_error(Req2, Code2, Reason2, State);
        {halt, _Req2} = Halt ->
            Halt;
        {unhandled, Req2} ->
            default_error_handler(Req2, Code, Reason, State);
        {unhandled, Code2, Reason2, Req2} ->
            default_error_handler(Req2, Code2, Reason2, State)
    end;
resource_error_handler(Req, Code, Reason, _Resources, State) ->
    default_error_handler(Req, Code, Reason, State).

default_error_handler(
  Req, Code, Reason,
  #state{api=#ewebapi_router{error_handler=ErrorHandler}}=State) ->
    case ErrorHandler(Req, Code, Reason) of
        {handled, Code2, Reason2, Req2} ->
            terminate_error(Req2, Code2, Reason2, State);
        {halt, _Req2} = Halt ->
            Halt;
        {unhandled, Req2} ->
            reply(Code, Req2)
    end.

terminate_error(Req, Code, _Reason, #state{ctp=unreachable}) ->
    reply(Code, Req);
terminate_error(Req, Code, Reason, #state{ctp=undefined}=State) ->
    Next =
        fun(Req2, State2) ->
                encode_reply(Req2, Code, Reason, State2)
        end,
    content_types_provided(Req, State, Next);
terminate_error(Req, Code, Reason, State) ->
    encode_reply(Req, Code, Reason, State).

reply(Req, Code, EncodedData, _State) ->
    Req2 =
        case EncodedData of
            {stream, StreamFun} ->
                cowboy_req:set_resp_body_fun(StreamFun, Req);
            {stream, Len, StreamFun} ->
                cowboy_req:set_resp_body_fun(Len, StreamFun, Req);
            {chunked, StreamFun} ->
                cowboy_req:set_resp_body_fun(chunked, StreamFun, Req);
            _Contents ->
                cowboy_req:set_resp_body(EncodedData, Req)
        end,
    reply(Code, Req2).

reply(undefined, Req) ->
    reply(500, Req);
reply(Code, Req) ->
    {ok, Req2} = cowboy_req:reply(Code, Req),
    {halt, Req2}.

%% =============================================================================
%%% Internal functions
%% =============================================================================

-spec match_([{binary(), ewebapi:resource()}], [binary()]) ->
                    {ok, {ewebapi:methods(), [ewebapi:resource()]}} |
                    {error, nomatch}.
match_(_ResourcesProplist, []) ->
    {error, nomatch};
match_(ResourcesProplist, [Path|RestPath]) ->
    case lists:keyfind(Path, 1, ResourcesProplist) of
        {_, Resource} ->
            match_(Resource, RestPath, []);
        false ->
            {error, nomatch}
    end.

match_(#resource{methods=Methods}=Resource, [], Acc) ->
    {ok, {Methods, [Resource|Acc]}};
match_(#resource{
          sub_resources=SubResourcesProplist,
          verbs=Verbs,
          id=IdResource,
          is_id=IsId
         } = Resource, [Path|Rest], Acc) ->
    %% first of all search in sub_resources
    case lists:keyfind(Path, 1, SubResourcesProplist) of
        {_, SubResource} ->
            match_(SubResource, Rest, [Resource|Acc]);
        false ->
            %% Case path is last search in verbs
            SearchInId =
                fun() ->
                    %% Try id resource
                    case IsId =:= false andalso
                        IdResource =/= undefined of
                        true ->
                            IdResource2 = IdResource#resource{id=Path},
                            match_(IdResource2, Rest,
                                   [Resource|Acc]);
                        false ->
                            {error, nomatch}
                    end
                end,
            case Rest of
                [] ->
                    case maps:find(Path, Verbs) of
                        {ok, Methods} ->
                            {ok, {Methods, [Resource|Acc]}};
                        error -> SearchInId()
                    end;
                _ -> SearchInId()
            end
    end.
