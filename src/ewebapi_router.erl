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
          env=[]
         }).

%% =============================================================================
%%% Api
%% =============================================================================

execute(Req, Env,
        #ewebapi_router{
           prefix=Prefix,
           cta=Cta,
           ctp=Ctp,
           resources=ResourcesProplist
          }) ->
    {Path, Req2} = cowboy_req:path(Req),
    {ok, Path2} = ewebapi_http_utils:split_path(Path),
    Len = length(Prefix),
    State = #state{
               cta=Cta,
               ctp=Ctp
              },
    case catch lists:split(Len, Path2) of
        {Prefix, RestPath} ->
            match(Req2, RestPath, ResourcesProplist, State);
        _ ->
            {ok, Req2, Env}
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
            terminate_error(Req, 404, wrong_path, State)
    end.

choose_handler(Req, Methods, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    case get_method_handler(Method, Methods) of
        {ok, Handler} ->
            State2 = State#state{handler=Handler},
            content_types_provided(Req2, State2);
        {error, undefined} ->
            AllowedMethods = methods_avaible(Methods),
            Req3 = cowboy_req:set_resp_header(<<"allow">>, AllowedMethods, Req2),
            terminate_error(Req3, 405, method_not_avaible, State)
    end.

content_types_provided(Req, #state{resources=[#resource{ctp=Ctps}|_]}=State) ->
    Next = fun content_types_accepted/2,
    case cowboy_req:parse_header(<<"accept">>, Req) of
        {error, badarg} ->
            terminate_error(Req, 400, bad_accept_header, State);
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
            terminate_error(Req, 406, Reason, State)
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
            fold_hops(Req2, State, fun apply_handler/2)
    end.

content_types_accepted_(Req, #state{resources=[#resource{cta=Ctas}|_]}=State) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, ContentType, Req2} ->
            case ewebapi_http_utils:choose_content_type(ContentType, Ctas) of
                {ok, Cta} ->
                    State2 = State#state{cta=Cta},
                    fold_hops(Req2, State2, fun decode/2);
                {error, Reason} ->
                    io:format("ewebapi_http_utils:choose_content_type(~p, ~p).\n", [ContentType, Ctas]),
                    terminate_error(Req2, 415, Reason, State)
            end;
        {error, badarg} ->
            terminate_error(Req, 400, bad_content_type_header, State)
    end.

decode(Req, #state{cta={_Cta, Decode}}=State) ->
    case Decode(Req) of
        {ok, Result, Req2} ->
            apply_handler(Req2, Result, State);
        {error, Reason, Req2} ->
            terminate_error(Req2, 415, Reason, State)
    end.

fold_hops(Req, #state{resources=[_|Resources], env=Env}=State, Next) ->
    Result =
        ewebapi_utils:success_foldl(
          fun(#resource{hop=undefined}, D) -> D;
             (#resource{is_id=true, id=Id, hop=Hop}, {R, E}) ->
                  handle_hop_result(Hop(R, Id, E));
             (#resource{is_id=false, hop=Hop}, {R, E}) ->
                  handle_hop_result(Hop(R, E))
          end, {Req, Env}, Resources),
    case Result of
        {ok, {Req2, Env2}} ->
            State2 = State#state{env=Env2},
            Next(Req2, State2);
        {error, {Reason, Req2}} ->
            terminate_error(Req2, 400, Reason, State)
    end.

handle_hop_result({ok, Req, Env}) -> {ok, {Req, Env}};
handle_hop_result({error, Reason, Req}) -> {error, {Reason, Req}}.

apply_handler(Req, Data,
              #state{
                 resources=[#resource{id=Id, is_id=true}|_],
                 handler=Handle,
                 env=Env
                } = State) ->
    Result = Handle(Req, Id, Data, Env),
    handle_result(Result, State);
apply_handler(Req, Data, #state{handler=Handle, env=Env}=State) ->
    Result = Handle(Req, Data, Env),
    handle_result(Result, State).

apply_handler(Req,
              #state{
                 resources=[#resource{id=Id, is_id=true}|_],
                 handler=Handle,
                 env=Env
                } = State) ->
    Result = Handle(Req, Id, Env),
    handle_result(Result, State);
apply_handler(Req, #state{handler=Handle, env=Env}=State) ->
    Result = Handle(Req, Env),
    handle_result(Result, State).

handle_result({ok, Result, Req2}, State) ->
    encode(Req2, Result, State);
handle_result({error, Reason, Req2}, State) ->
    terminate_error(Req2, 400, Reason, State).

encode(Req, Data, #state{ctp={_Ctp, Encode}}=State) ->
    case Encode(Req, Data) of
        {ok, Req2} ->
            finish(Req2, State);
        {error, Reason, Req2} ->
            terminate_error(Req2, 416, Reason, State)
    end.

finish(Req, _State) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {halt, Req2}.

terminate_error(Req, Code, Reason, _State) ->
    %% TODO: implement error logic
    Str = io_lib:format("~p\n", [Reason]),
    {ok, Req2} = cowboy_req:reply(Code, [], Str, Req),
    {error, Reason, Req2}.

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
          verbs=VerbsProplist,
          id=IdResource,
          is_id=IsId
         } = Resource, [Path|Rest], Acc) ->
    %% first of all search in sub_resources
    case lists:keyfind(Path, 1, SubResourcesProplist) of
        {_, SubResource} ->
            match_(SubResource, Rest, [Resource|Acc]);
        false ->
            %% Case path is last search in verbs
            case Rest of
                [] ->
                    case lists:keyfind(Path, 1, VerbsProplist) of
                        {_, Methods} ->
                            {ok, {Methods, [Resource|Acc]}};
                        false ->
                            %% Try id resource
                            case IsId =:= false andalso
                                IdResource =/= undefined of
                                true ->
                                    Resource2 = Resource#resource{id=Path},
                                    match_(IdResource, Rest,
                                           [Resource2|Acc]);
                                false ->
                                    {error, nomatch}
                            end
                    end;
                _ ->
                    {error, nomatch}
            end
    end.

-spec methods_avaible(ewebapi:methods()) -> [binary()].
methods_avaible(Methods) ->
    [M ||
        {M, P} <-
            [
             {<<"GET">>, #methods.get},
             {<<"PUT">>, #methods.put},
             {<<"POST">>, #methods.post},
             {<<"DELETE">>, #methods.delete}
            ],
        element(P, Methods) =/= undefined
    ].

-spec get_method_handler(binary(), ewebapi:methods()) ->
                                {ok, function()} | {error, undefined}.
get_method_handler(Method, Methods) ->
    Result =
        case Method of
            <<"GET">> -> Methods#methods.get;
            <<"PUT">> -> Methods#methods.put;
            <<"POST">> -> Methods#methods.post;
            <<"DELETE">> -> Methods#methods.delete;
            _ -> undefined
        end,
    case Result of
        undefined ->
            {error, undefined};
        Fun ->
            {ok, Fun}
    end.
