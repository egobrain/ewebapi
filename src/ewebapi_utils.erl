-module(ewebapi_utils).

-export([
         error_writer_foldl/3,
         error_writer_map/2,
         error_writer_test/2,
         success_apply/2,
         success_foldl/3,
         take_first_defined/1
        ]).

-spec error_writer_foldl(Fun, State, List) -> {ok, NewState} | {error, Reasons} when
      List :: [Elem],
      Fun :: fun((Elem, State) -> {ok, NewState} | {error, Reason}),
      Reasons :: [Reason].
error_writer_foldl(Fun, InitState, Opts) ->
    {ResultState, ResultErrors} =
        lists:foldl(fun(Val, {State, Errors}) ->
                            case Fun(Val, State) of
                                {ok, State2} ->
                                    {State2, Errors};
                                {error, Reason} ->
                                    {State, [Reason | Errors]}
                            end
                    end,
                    {InitState, []},
                    Opts),
    case ResultErrors of
        [] -> {ok, ResultState};
        _ -> {error, lists:reverse(ResultErrors)}
    end.

-spec error_writer_map(Fun, ArgsList) -> {ok, ResultList} | {error, Errors} when
      Fun :: fun((Arg) -> {ok, Result} | {error, Error}),
      ArgsList :: [Arg],
      ResultList :: [Result],
      Errors :: [Error].
error_writer_map(Fun, List) when is_list(List) ->
    MapFun = fun(Item, Acc) ->
                     case Fun(Item) of
                         ok ->
                             {ok, Acc};
                         {ok, Res} ->
                             {ok, [Res | Acc]};
                         {error, Reason} ->
                             {error, Reason}
                     end
             end,
    case error_writer_foldl(MapFun, [], List) of
        {ok, Result} ->
            {ok, lists:reverse(Result)};
        {error, _} = Err -> Err
    end.

-spec success_apply([Fun], State) -> {ok, State} | {error, Reason} when
      Fun :: fun((State) -> {ok, State} | {error, Reason}).
success_apply([], State) ->
    {ok, State};
success_apply([Fun|Rest], State) ->
    case Fun(State) of
        {ok, State2} ->
            success_apply(Rest, State2);
        {error, _Reason} = Err ->
            Err
    end.

-spec take_first_defined([undefined | Value]) -> Value | undefined.
take_first_defined([]) ->
    undefined;
take_first_defined([Value|Rest]) ->
    case Value of
        undefined ->
            take_first_defined(Rest);
        _ ->
            Value
    end.

-spec success_foldl(Fun, State, [E]) -> {ok, State} | {error, Reason} when
      Fun :: fun((E, State) -> {ok, State} | {error, Reason}).
success_foldl(_Fun, State, []) ->
    {ok, State};
success_foldl(Fun, State, [H|T]) ->
    case Fun(H, State) of
        {ok, State2} ->
            success_foldl(Fun, State2, T);
        {error, Reason} ->
            {error, Reason}
    end.


-spec error_writer_test([Fun], Data) -> ok | {error, [Reason]} when
      Fun :: fun((Data) -> ok | {error, Reason}).
error_writer_test(Funs, Data) ->
    error_writer_test(Funs, Data, []).

error_writer_test([], _Data, []) -> ok;
error_writer_test([], _Data, Errors) -> {error, Errors};
error_writer_test([Fun|Rest], Data, Errors) ->
    case Fun(Data) of
        ok -> error_writer_test(Rest, Data);
        {error, Reason} -> error_writer_test(Rest, Data, [Reason|Errors])
    end.
