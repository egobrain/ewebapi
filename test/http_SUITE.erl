-module(http_SUITE).
-compile(export_all).

-define(METHODS, [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]).
-define(RAND_STRING, rand_string(256)).

all() ->
    [
     {group, http}
    ].

groups() ->
    [
     {http, [],
      [
       test_paths
      ]}
    ].

resources() ->
    R1 = resource(<<"r1">>),
    R2 = resource(<<"r2">>),
    SR1 = resource(<<"sr1">>),
    SR2 = resource(<<"sr2">>),
    [
     set(R1, SR1),
     set(R2, SR2)
    ].

init_per_suite(Config) ->
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowlib),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(cowboy),
    ok = application:start(gun),
    Config.

end_per_suite(_) ->
    ok = application:stop(gun),
    ok = application:stop(cowboy),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(asn1),
    ok = application:stop(cowlib),
    ok = application:stop(crypto),
    ok = application:stop(ranch),
    ok.

init_per_group(http, Config) ->
    Resources = resources(),
    ContentTypesAccepted =
        [
         {'*', fun cowboy_req:body/1}
        ],
    ContentTypesProvided =
        [
         {<<"*">>, fun to_bert/2}
        ],
    {ok, ApiHandler} =
        ewebapi:init(
          <<"/api">>, <<"v1.0">>,
          [
           {content_types_accepted, ContentTypesAccepted},
           {content_types_provided, ContentTypesProvided},
           {resources, Resources}
           %% {init_state, []}
          ]),
    {ok, _} =
        cowboy:start_http(
          http, 100, [{port, 0}],
          [
           {middlewares, [ApiHandler]}
          ]),
    Port = ranch:get_port(http),
    [
     {port, Port},
     {type, tcp}
     | Config
    ];
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

test_paths(Config) ->
    [
     Test(Tag, Path, Env)
     || Test <-
            [
             fun(Tag, P, Env) ->
                     ct:pal("~p = ~p", [{200, {Tag, <<"GET">>, Env}}, get(Config, P)]),
                     {200, {Tag, <<"GET">>, Env}} = get(Config, P)
             end,
             fun(Tag, P, Env) ->
                     Data = ?RAND_STRING,
                     {200, {Tag, <<"PUT">>, Data, Env}} = put(Config, P, Data)
             end,
             fun(Tag, P, Env) ->
                     Data = ?RAND_STRING,
                     {200, {Tag, <<"POST">>, Data, Env}} = post(Config, P, Data)
             end,
             fun(Tag, P, Env) ->
                     {200, {Tag, <<"DELETE">>, Env}} = delete(Config, P)
             end
            ],
        {Tag, Path, Env} <-
            [
             {lists:last(P), path_from_list(P), tl(lists:reverse(P))}
             || P <-
                    [
                     [<<"r1">>],
                     [<<"r1">>, <<"sr1">>],
                     [<<"r2">>],
                     [<<"r2">>, <<"sr2">>]
                    ]
            ]
    ].

%% =============================================================================
%%% Internal fucntions
%% =============================================================================

%% = Resource helpers ==========================================================

tag(T, Fun3) when is_function(Fun3, 3) ->
    fun(A, B) -> Fun3(T, A, B) end;
tag(T, Fun4) when is_function(Fun4, 4) ->
    fun(A, B, C) -> Fun4(T, A, B, C) end;
tag(T, Fun5) when is_function(Fun5, 5) ->
    fun(A, B, C, D) -> Fun5(T, A, B, C, D) end.

handle(Tag, Req, Env) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, {Tag, Method, Env}, Req2}.

handle(Tag, Req, Data, Env) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, {Tag, Method, Data, Env}, Req2}.

handle(Tag, Req, Id, Data, Env) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, {Tag, Method, Id, Data, Env}, Req2}.


resource(Name) ->
    {resource, Name,
     [
      {hop, fun(Req, Env) -> {ok, Req, [Name|Env]} end},
      {method, <<"GET">>, tag(Name, fun handle/3)},
      {method, <<"PUT">>, tag(Name, fun handle/4)},
      {method, <<"POST">>, tag(Name, fun handle/4)},
      {method, <<"DELETE">>, tag(Name, fun handle/3)}
     ]}.

to_bert(Req, Data) ->
    Result = term_to_binary(Data),
    Req2 = cowboy_req:set_resp_body(Result, Req),
    {ok, Req2}.

%% from_bert(Req) ->
%%     {ok, Body, Req2} = cowboy_req:body(Req),
%%     Result = binary_to_term(Body),
%%     {ok, Result, Req2}.

path_from_list(Path) ->
    Bin = << <<$/, P/binary>> || P <- Path >>,
    <<"/api/v1.0", Bin/binary>>.

%% = Utils =====================================================================

set({resource, Name, Opts}, Value) ->
    {resource, Name, [Value|Opts]}.

rand_string(Bytes) ->
    base64:encode(crypto:strong_rand_bytes(Bytes)).

%% = Req =======================================================================

gun_open(Config) ->
    gun_open(Config, []).

gun_open(Config, Opts) ->
    {ok, ConnPid} =
        gun:open("localhost",
                 config(port, Config),
                 [
                  {retry, 0},
                  {type, config(type, Config)}
                  |Opts
                 ]),
    ConnPid.

config(Key, Config) ->
    {_, Value} = lists:keyfind(Key, 1, Config),
    Value.

get(Config, Path) ->
    ConnPid = gun_open(Config),
    Ref = gun:get(ConnPid, Path),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.

post(Config, Path, Data) ->
    ConnPid = gun_open(Config),
    Ref = gun:post(ConnPid, Path, [], Data),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.

put(Config, Path, Data) ->
    ConnPid = gun_open(Config),
    Ref = gun:put(ConnPid, Path, [], Data),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.

delete(Config, Path) ->
    ConnPid = gun_open(Config),
    Ref = gun:delete(ConnPid, Path),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.
