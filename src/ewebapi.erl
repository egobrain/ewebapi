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
         compile/1,
         init/3
        ]).

-include("resource.hrl").
-include("router.hrl").

-type resource() :: #resource{}.

-export_type([resource/0]).

%% =============================================================================
%%% Api
%% =============================================================================

compile(Routes) ->
    ewebapi_utils:error_writer_map(fun compile_resource/1, Routes).

-spec init(Prefix, Version, Resources) -> #ewebapi_router{} when
      Prefix :: binary(),
      Version :: binary(),
      Resources :: [#resource{}].
init(Prefix, Version, Resources) ->
    Size = byte_size(Prefix),
    Prefix0 =
        case Prefix of
            <<P:Size/binary, $/>> -> P;
            _ -> Prefix
        end,
    Prefix2 = <<Prefix0/binary, $/, Version/binary>>,
    PrefixList = ewebapi_utils:split_path(Prefix2),
    #ewebapi_router{
        prefix = PrefixList,
        resources = Resources
       }.

%% =============================================================================
%%% Internal logic
%% =============================================================================

compile_resource({resource, Name, Opts}) ->
    case fold_opts(#resource{is_id=false}, Opts) of
        {ok, Resource} ->
            {ok, {Name, Resource}};
        {error, Reason} ->
            {error, {{resource, Name}, Reason}}
    end;
compile_resource(Opt) ->
    {error, {Opt, unknown}}.

%% = Resource Opts =============================================================

fold_opts(Resource, Opts)->
    ewebapi_utils:error_writer_foldl(fun opt/2, Resource, Opts).

opt({resource, Name, Opts}, #resource{sub_resources=SubResources} = Resource) ->
    case lists:keymeber(Name, 1, SubResources) of
        false ->
            case fold_opts(#resource{is_id=false}, Opts) of
                {ok, Resource} ->
                    SubResources2 = [{Name, Resource}|SubResources],
                    Resource2 = Resource#resource{sub_resources=SubResources2},
                    {ok, Resource2};
                {error, Reason} ->
                    {error, {{resource, Name}, Reason}}
            end;
        true ->
            {error, {{resource, Name}, already_defineded}}
    end;
opt({hop, Fun}, Resource)  ->
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
opt({id, Opts}, #resource{is_id=false} = Resource) ->
    case fold_opts(#resource{is_id=true}, Opts) of
        {ok, IdResource} ->
            Resource2 = Resource#resource{id=IdResource},
            {ok, Resource2};
        {error, Reason} ->
            {error, {id, Reason}}
    end;
opt({method, Method, Fun}, #resource{methods=Methods} = Resource) ->
    Clauses =
        [
         {<<"GET">>, idinc(2, Resource), #methods.get},
         {<<"PUT">>, idinc(3, Resource), #methods.put},
         {<<"POST">>, idinc(3, Resource), #methods.post},
         {<<"DELETE">>, idinc(3, Resource), #methods.delete}
        ],
    case apply_method(Method, Fun, Clauses, Methods) of
        {ok, Methods2} ->
            Resource2 = Resource#resource{methods=Methods2},
            {ok, Resource2};
        {error, Reason} ->
            {error, {{method, Method}, Reason}}
    end;
opt({verb, Name, Method, Fun}, #resource{verbs=Verbs} = Resource) ->
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
         {<<"DELETE">>, idinc(3, Resource), #methods.delete}
        ],
    case apply_method(Method, Fun, Clauses, VerbMethods) of
        {ok, VerbMethods2} ->
            Resource2 = Resource#resource{verbs=[{Name, VerbMethods2}|Verbs2]},
            {ok, Resource2};
        {error, Reason} ->
            {error, {{method, Method}, Reason}}
    end;
opt(Opt, _Resource) ->
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
