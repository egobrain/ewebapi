-module(ewebapi_utils).

-export([
         error_writer_foldl/3,
         error_writer_map/2,
         split_path/1
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

-spec split_path(binary()) -> {ok, [binary()]} | {error, badrequest}.
split_path(<< $/, Path/bits >>) ->
    split_path(Path, []);
split_path(_) ->
    {error, badrequest}.

split_path(Path, Acc) ->
    try
        case binary:match(Path, <<"/">>) of
            nomatch when Path =:= <<>> ->
                {ok, lists:reverse([cowboy_http:urldecode(S) || S <- Acc])};
            nomatch ->
                {ok, lists:reverse([cowboy_http:urldecode(S) || S <- [Path|Acc]])};
            {Pos, _} ->
                << Segment:Pos/binary, _:8, Rest/bits >> = Path,
                split_path(Rest, [Segment|Acc])
        end
    catch
        error:badarg ->
            {error, badrequest}
    end.
