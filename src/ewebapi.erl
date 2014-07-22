-module(ewebapi).

%% @doc
%% Api path represented as a tree with array of resources:
%%   {resource, Name, Opts}.
%% Where options are
%%   {id, Opts}.
%%   {resource, Opts, Verbs}.
%%   {hop, Fun}
%%   {method, Method, Fun}.
%%   {verb, Name, Methods, Fun/3}.

-export([
         init/3
        ]).

-include("resource.hrl").
-include("router.hrl").

-type resource() :: #resource{}.
-type methods() :: #methods{}.

-export_type([
              resource/0,
              methods/0
             ]).

-record(state, {
          resources,
          cta,
          ctp,
          init_handler,
          error_handler
         }).

%% =============================================================================
%%% Api
%% =============================================================================

-spec init(Prefix, Version, Opts) -> #ewebapi_router{} when
      Prefix :: binary(),
      Version :: binary(),
      Opts :: [Opt],
      Opt :: {content_types_provided, [{binary(), any()}]} |
             {content_types_accepted, [{binary(), any()}]} |
             {resource, Resources},
      Resources :: [#resource{}].
init(Prefix, Version, Opts) ->
    Size = byte_size(Prefix),
    Prefix0 =
        case Prefix of
            <<P:Size/binary, $/>> -> P;
            _ -> Prefix
        end,
    Prefix2 = <<Prefix0/binary, $/, Version/binary>>,
    case ewebapi_http_utils:split_path(Prefix2) of
        {ok, PrefixList} ->
            case compile(Opts) of
                {ok, Api} ->
                    Api2 = Api#ewebapi_router{prefix=PrefixList},
                    {ok, Api2};
                {error, _Reason} = Err ->
                    Err
            end;
        {error, Reason} ->
            {error, {prefix, Reason}}
    end.


%% =============================================================================
%%% Internal logic
%% =============================================================================

compile(Opts) ->
    ewebapi_utils:success_apply(
      [
       fun(S) -> fold_opts(Opts, S) end,
       fun valid_opts/1,
       fun default_ct/1,
       fun(#state{
              cta=Cta,
              ctp=Ctp,
              resources=Resources,
              init_handler=InitHandler,
              error_handler=ErrorHandler
             }) ->
               Api =
                   #ewebapi_router{
                      cta=Cta,
                      ctp=Ctp,
                      resources=Resources,
                      init_handler=InitHandler,
                      error_handler=ErrorHandler
                     },
               {ok, Api}
       end
      ], #state{}).

fold_opts(Opts, State) ->
    ewebapi_utils:error_writer_foldl(fun opts/2, State, Opts).

default_ct(#state{cta=DCta, ctp=DCtp, resources=Resources}=State) ->
    Resources2 =
        traverse_resources_nodes(
          Resources,
          fun(#resource{cta=RCta, ctp=RCtp, is_id=IsId} = R, Parent) ->
                  {PCta, PCtp} =
                      case IsId =:= false orelse Parent =:= [] of
                          true -> {undefined, undefined};
                          false ->
                              P = hd(Parent),
                              {P#resource.cta, P#resource.ctp}
                      end,
                  R#resource{
                    cta=ewebapi_utils:take_first_defined([RCta, PCta, DCta]),
                    ctp=ewebapi_utils:take_first_defined([RCtp, PCtp, DCtp])
                   }
          end),
    State2 = State#state{resources=Resources2},
    {ok, State2}.

opts({resources, Resources}, State) ->
    case compile_resources(Resources) of
        {ok, CompiledResources} ->
            State2 = State#state{resources=CompiledResources},
            {ok, State2};
        {error, _Reason} = Err ->
            Err
    end;
opts({content_types_accepted, Ctas}, State) ->
    Ctas2 = [ewebapi_http_utils:normalize_content_type(Cta) || Cta <- Ctas],
    State2 = State#state{cta=Ctas2},
    {ok, State2};
opts({content_types_provided, Ctps}, State) ->
    Ctps2 = [ewebapi_http_utils:normalize_content_type(Ctp) || Ctp <- Ctps],
    State2 = State#state{ctp=Ctps2},
    {ok, State2};
opts({init_handler, Fun}, State) ->
    case is_function(Fun, 1) of
        true ->
            State2 = State#state{init_handler=Fun},
            {ok, State2};
        false ->
            {error, {init_handler, invalid_function}}
    end;
opts({error_handler, Fun}, State) ->
    case is_function(Fun, 3) of
        true ->
            State2 = State#state{error_handler=Fun},
            {ok, State2};
        false ->
            {error, {error_handler, invalid_function}}
    end;
opts({Opt, _Data}, _Api) ->
    {error, {Opt, unknown}}.

%% = Opts validators ===========================================================

valid_opts(State) ->
    Validators =
        [
         fun init_required/1
        ],
    case ewebapi_utils:error_writer_test(Validators, State) of
        ok ->
            {ok, State};
        {error, _Reasons} = Err ->
            Err
    end.

init_required(#state{init_handler=undefined}) -> {error, {init_handler, required}};
init_required(_) -> ok.

%% = Resource ==================================================================

compile_resources(Routes) ->
    ewebapi_utils:error_writer_map(fun compile_resource/1, Routes).

compile_resource({resource, Name, Opts}) ->
    case fold_resource_opts(#resource{is_id=false}, Opts) of
        {ok, Resource} ->
            {ok, {Name, Resource}};
        {error, Reason} ->
            {error, {{resource, Name}, Reason}}
    end;
compile_resource(Opt) ->
    {error, {Opt, unknown}}.

%% = Resource Opts =============================================================

fold_resource_opts(Resource, Opts)->
    ewebapi_utils:error_writer_foldl(fun resource_opt/2, Resource, Opts).

resource_opt({resource, Name, Opts},
             #resource{sub_resources=SubResources}=Resource) ->
    case lists:keymember(Name, 1, SubResources) of
        false ->
            case fold_resource_opts(#resource{is_id=false}, Opts) of
                {ok, SubResource} ->
                    SubResources2 = [{Name, SubResource}|SubResources],
                    Resource2 = Resource#resource{sub_resources=SubResources2},
                    {ok, Resource2};
                {error, Reason} ->
                    {error, {{resource, Name}, Reason}}
            end;
        true ->
            {error, {{resource, Name}, already_defineded}}
    end;
resource_opt({hop, Fun}, Resource)  ->
    case is_function(Fun, idinc(2, Resource)) of
        true ->
            case Resource#resource.hop of
                undefined ->
                    Resource2 = Resource#resource{hop=Fun},
                    {ok, Resource2};
                _ ->
                    {error, {hop, already_set}}
            end;
        false ->
            {error, {hop, invalid_function}}
    end;
resource_opt({id, Opts}, #resource{is_id=false} = Resource) ->
    case fold_resource_opts(#resource{is_id=true}, Opts) of
        {ok, IdResource} ->
            Resource2 = Resource#resource{id=IdResource},
            {ok, Resource2};
        {error, Reason} ->
            {error, {id, Reason}}
    end;
resource_opt({method, Method, Fun}, #resource{methods=Methods} = Resource) ->
    Clauses =
        [
         {<<"GET">>, idinc(2, Resource), #methods.get},
         {<<"PUT">>, idinc(3, Resource), #methods.put},
         {<<"POST">>, idinc(3, Resource), #methods.post},
         {<<"DELETE">>, idinc(2, Resource), #methods.delete}
        ],
    case apply_method(Method, Fun, Clauses, Methods) of
        {ok, Methods2} ->
            Resource2 = Resource#resource{methods=Methods2},
            {ok, Resource2};
        {error, Reason} ->
            {error, {{method, Method}, Reason}}
    end;
resource_opt({verb, Name, Method, Fun}, #resource{verbs=Verbs} = Resource) ->
    {VerbMethods, Verbs2} =
        case lists:keytake(Name, 1, Verbs) of
            {value, {_, M}, Vs} -> {M, Vs};
            false -> {#methods{}, Verbs}
        end,
    Clauses =
        [
         {<<"GET">>, idinc(2, Resource), #methods.get},
         {<<"PUT">>, idinc(3, Resource), #methods.put},
         {<<"POST">>, idinc(3, Resource), #methods.post},
         {<<"DELETE">>, idinc(2, Resource), #methods.delete}
        ],
    case apply_method(Method, Fun, Clauses, VerbMethods) of
        {ok, VerbMethods2} ->
            Resource2 = Resource#resource{verbs=[{Name, VerbMethods2}|Verbs2]},
            {ok, Resource2};
        {error, Reason} ->
            {error, {{method, Method}, Reason}}
    end;
resource_opt({content_types_accepted, Ctas}, Resource) ->
    Ctas2 = [ewebapi_http_utils:normalize_content_type(Cta) || Cta <- Ctas],
    Resource2 = Resource#resource{cta=Ctas2},
    {ok, Resource2};
resource_opt({content_types_provided, Ctps}, Resource) ->
    Ctps2 = [ewebapi_http_utils:normalize_content_type(Ctp) || Ctp <- Ctps],
    Resource2 = Resource#resource{ctp=Ctps2},
    {ok, Resource2};
resource_opt({init_handler, Fun}, Resource) ->
    case is_function(Fun, idinc(1, Resource)) of
        true ->
            Resource2 = Resource#resource{init_handler=Fun},
            {ok, Resource2};
        false ->
            {error, {init_handler, invalid_function}}
    end;
resource_opt({error_handler, Fun}, Resource) ->
    case is_function(Fun, 3) of
        true ->
            Resource2 = Resource#resource{error_handler=Fun},
            {ok, Resource2};
        false ->
            {error, {error_handler, invalid_function}}
    end;
resource_opt(Opt, _Resource) ->
    {error, {{option, Opt}, unknown}}.

%% =============================================================================
%%% Internal fucntions
%% =============================================================================

apply_method(_M, _F, [], _Methods) ->
    {error, unknown};
apply_method(M, F, [Clause|Rest], Resource) ->
    case apply_method_(M, F, Clause, Resource) of
        {ok, _Resource2} = Ok -> Ok;
        {error, _Reason} = Err -> Err;
        next -> apply_method(M, F, Rest, Resource)
    end.

apply_method_(M, Fun, {M, Arity, Pos}, Methods) ->
    case is_function(Fun, Arity) of
        true ->
            case element(Pos, Methods) of
                undefined ->
                    Methods2 = setelement(Pos, Methods, Fun),
                    {ok, Methods2};
                _ ->
                    {error, already_defineded}
            end;
        false ->
            {error, wrong_function}
    end;
apply_method_(_M, _Fun, _Clause, _Resource) ->
    next.

%% increment if resource is id resource
idinc(A, #resource{is_id=true}) -> A+1;
idinc(A, _Resource) -> A.

traverse_resources_nodes(Rs, Fun) ->
    traverse_resources_nodes(Rs, [], Fun).

traverse_resources_nodes(Rs, Parent, Fun) ->
    traverse_resources_nodes(Rs, Parent, Fun, []).

traverse_resources_nodes([], _Parent, _Fun, Acc) ->
    lists:reverse(Acc);
traverse_resources_nodes(
  [{Name, #resource{sub_resources=SubRs, id=IdResource}=R}|Rest],
  Parent, Fun, Acc) ->
    R1 =
        case IdResource of
            undefined -> R;
            #resource{sub_resources=IdSubRs} ->
                Parent2 = [R|Parent],
                IdResource2 = Fun(IdResource, Parent2),
                IdResource3 =
                    IdResource2#resource{
                      sub_resources=traverse_resources_nodes(IdSubRs, Parent2, Fun)
                     },
                R#resource{id=IdResource3}
        end,
    R2 = R1#resource{sub_resources=traverse_resources_nodes(SubRs, [R1|Parent], Fun)},
    R3 = Fun(R2, Parent),
    traverse_resources_nodes(Rest, Parent, Fun, [{Name, R3}|Acc]).
