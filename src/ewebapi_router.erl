-module(ewebapi_router).

-export([
         execute/3
        ]).

-include("resource.hrl").
-include("router.hrl").

%% =============================================================================
%%% Api
%% =============================================================================

execute(Req, Env,
        #ewebapi_router{
           prefix=Prefix,
           resources=ResourcesProplist
          }) ->
    {Path, Req2} = cowboy_req:path(Req),
    {ok, Path2} = ewebapi_utils:split_path(Path),
    Len = length(Prefix),
    case catch lists:split(Len, Path2) of
        {Prefix, RestPath} ->
            match(RestPath, Req2, ResourcesProplist);
        _ ->
            {ok, Req2, Env}
    end.

%% =============================================================================
%%% Flow control
%% =============================================================================


match(RestPath, Req, ResourcesProplist) ->
    %% [_Host, Path] = cowboy_req:get([host, path], Req),
    case match_(ResourcesProplist, RestPath) of
        {ok, {Methods, Resources}} ->
            choose_operation(Req, State2);
        {error, nomatch} ->
            terminate_error(404, Reason, Req, State)
    end.


%% =============================================================================
%%% Internal functions
%% =============================================================================

match_(_ResourcesProplist, []) ->
    {error, nomatch};
match_(ResourcesProplist, [Path|RestPath]) ->
    case lists:keyfind(Path, 1, ResourcesProplist) of
        {_, Resource} ->
            match_(Resource, RestPath, [])
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
                            case IsId =:= false andalso IdResource =/= undefined of
                                true ->
                                    match_(IdResource, Rest, [Resource|Acc]);
                                false ->
                                    {error, nomatch}
                            end
                    end;
                _ ->
                    {error, nomatch}
            end
    end.
