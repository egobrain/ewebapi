-module(ewebapi_http_utils).

-export([
         split_path/1,
         normalize_content_type/1,
         prioritize_accept/1,
         choose_media_type/2,
         choose_content_type/2,
         content_type_to_binary/1
        ]).

-spec split_path(binary()) -> {ok, [binary()]} | {error, badrequest}.
split_path(<< $/, Path/bits >>) ->
    split_path(Path, []);
split_path(_) ->
    {error, badrequest}.

split_path(Path, Acc) ->
    try binary:match(Path, <<"/">>) of
        nomatch when Path =:= <<>> ->
            {ok, lists:reverse([cow_qs:urldecode(S) || S <- Acc])};
        nomatch ->
            {ok, lists:reverse([cow_qs:urldecode(S) || S <- [Path|Acc]])};
        {Pos, _} ->
            << Segment:Pos/binary, _:8, Rest/bits >> = Path,
            split_path(Rest, [Segment|Acc])
    catch
        error:badarg ->
            {error, badrequest}
    end.

normalize_content_type({ContentType, Callback})
  when is_binary(ContentType) ->
    {cowboy_http:content_type(ContentType), Callback};
normalize_content_type(Normalized) ->
    Normalized.

content_type_to_binary({Type, SubType, Params}) ->
    ParamsBin = content_type_params_to_iolist(Params, []),
    iolist_to_binary([Type, <<"/">>, SubType, ParamsBin]).

content_type_params_to_iolist('*', []) ->
    <<>>;
content_type_params_to_iolist([], []) ->
    <<>>;
content_type_params_to_iolist([], Acc) ->
    lists:reverse(Acc);
content_type_params_to_iolist([{Attr, Value}|Tail], Acc) ->
    content_type_params_to_iolist(Tail, [[Attr, <<"=">>, Value], <<";">>|Acc]).

prioritize_accept(Accept) ->
    lists:sort(
      fun ({MediaTypeA, Quality, _AcceptParamsA},
           {MediaTypeB, Quality, _AcceptParamsB}) ->
              %% Same quality, check precedence in more details.
              prioritize_mediatype(MediaTypeA, MediaTypeB);
          ({_MediaTypeA, QualityA, _AcceptParamsA},
           {_MediaTypeB, QualityB, _AcceptParamsB}) ->
              %% Just compare the quality.
              QualityA > QualityB
      end, Accept).

prioritize_mediatype({TypeA, SubTypeA, ParamsA}, {TypeB, SubTypeB, ParamsB}) ->
    case TypeB of
        TypeA ->
            case SubTypeB of
                SubTypeA -> length(ParamsA) > length(ParamsB);
                <<"*">> -> true;
                _Any -> false
            end;
        <<"*">> -> true;
        _Any -> false
    end.

choose_media_type(_Ctps, []) ->
    {error, not_acceptable};
choose_media_type(Ctps, [MediaType|Tail]) ->
    match_media_type(Ctps, Tail, Ctps, MediaType).

match_media_type(Ctps, Accept, [], _MediaType) ->
    choose_media_type(Ctps, Accept);
match_media_type(Ctps, Accept, CTP,
                 MediaType = {{<<"*">>, <<"*">>, _Params_A}, _QA, _APA}) ->
    match_media_type_params(Ctps, Accept, CTP, MediaType);
match_media_type(Ctps, Accept,
                 CTP = [{{Type, SubType_P, _PP}, _Fun}|_Tail],
                 MediaType = {{Type, SubType_A, _PA}, _QA, _APA})
  when SubType_P =:= SubType_A; SubType_A =:= <<"*">> ->
    match_media_type_params(Ctps, Accept, CTP, MediaType);
match_media_type(Ctps, Accept, [_Any|Tail], MediaType) ->
    match_media_type(Ctps, Accept, Tail, MediaType).

match_media_type_params(_Ctps, _Accept,
                        [{{TP, STP, '*'}, Fun}|_Tail],
                        {{_TA, _STA, Params_A}, _QA, _APA}) ->
    PMT = {TP, STP, Params_A},
    {ok, {PMT, Fun}};
match_media_type_params(Ctps, Accept,
                        [{{_TP, _STP, Params_P}, _Fun}=Ctp|Tail],
                        MediaType = {{_TA, _STA, Params_A}, _QA, _APA}) ->
    case lists:sort(Params_P) =:= lists:sort(Params_A) of
        true ->
            {ok, Ctp};
        false ->
            match_media_type(Ctps, Accept, Tail, MediaType)
    end.

choose_content_type(_ContentType, []) ->
    {error, content_type};
choose_content_type(ContentType, [{Accepted, _Fun}=Cta|_Tail])
  when Accepted =:= '*'; Accepted =:= ContentType ->
    {ok, Cta};
choose_content_type({Type, SubType, Param},
                    [{{Type, SubType, AcceptedParam}, _Fun}=Cta|_Tail])
  when AcceptedParam =:= '*'; AcceptedParam =:= Param ->
    {ok, Cta};
choose_content_type(ContentType, [_Any|Tail]) ->
    choose_content_type(ContentType, Tail).
