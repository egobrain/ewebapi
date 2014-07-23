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
       test_paths,
       test_verbs,
       error_handler,
       id_handler
      ]}
    ].

resources() ->
    R1 = resource(<<"r1">>),
    R2 = resource(<<"r2">>),
    SR1 = resource(<<"sr1">>),
    SR2 = resource(<<"sr2">>),
    VerbR =
        {resource, <<"test_verbs">>,
         [
          {verb, M, M, tag(M, get_handler(M))}
          || M <- ?METHODS
         ]},
    ErrorR = error_resource(<<"error">>),
    IdR = {resource, <<"id_resource">>,
           [
            {method, <<"GET">>, tag(<<"id_resource">>, fun handle/3)},
            {id, [
                  {method, <<"GET">>, tag(<<"id_resource">>, fun id_handle/4)}
                 ]}
           ]},
    [
     set(R1, SR1),
     set(R2, SR2),
     VerbR,
     ErrorR,
     IdR
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
         {<<"application/bert">>, fun from_bert/1}
        ],
    ContentTypesProvided =
        [
         {<<"application/bert">>, fun to_bert/2}
        ],
    {ok, ApiHandler} =
        ewebapi:init(
          <<"/api">>, <<"v1.0">>,
          [
           {content_types_accepted, ContentTypesAccepted},
           {content_types_provided, ContentTypesProvided},
           {resources, Resources},
           {init_handler, fun init/1},
           {error_handler, fun default_error_handler/3}
          ]),
    {ok, _} =
        cowboy:start_http(
          http, 100, [],
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

test_verbs(Config) ->
    Path = fun(M) -> path_from_list([<<"test_verbs">>, M]) end,
    [
     Test()
     || Test <-
            [
             fun() ->
                     M = <<"GET">>,
                     {200, {M, M, []}} = get(Config, Path(M))
             end,
             fun() ->
                     M = <<"PUT">>,
                     Data = ?RAND_STRING,
                     {200, {M, M, Data, Env}} = put(Config, Path(M), Data)
             end,
             fun() ->
                     M = <<"POST">>,
                     Data = ?RAND_STRING,
                     {200, {M, M, Data, Env}} = post(Config, Path(M), Data)
             end,
             fun() ->
                     M = <<"DELETE">>,
                     {200, {M, M, Env}} = delete(Config, Path(M))
             end
            ]
    ].

error_handler(Config) ->
    WrongPath = path_from_list([<<"wrong_path">>]),

    %% WrongPath
    {404, {default_handled, wrong_path}} = get(Config, WrongPath),

    ErrorPath = path_from_list([<<"error">>]),

    %% WrongContentType Accepted
    {response, fin, 405, _} =
        simple_get(
          Config,
          ErrorPath,
          [{<<"Accept">>, <<"application/json">>}]),

    %% WrongContentType Provided
    {response, fin, 406, _} =
        simple_post(
          Config,
          ErrorPath,
          [{<<"Accept">>, <<"application/json">>}],
          <<"{\"data\":123}">>),

    %% Custom handler
    {403, {custom_handled, forbidden}} =
        post(Config, ErrorPath,
             {true, true, 403, forbidden}),

    %% Default handler
    {403, {default_handled, forbidden}} =
        post(Config, ErrorPath,
             {false, true, 403, forbidden}),

    %% Unhandled
    500 =
        post(Config, ErrorPath,
             {false, false, 403, forbidden}).

id_handler(Config) ->
    Path = path_from_list([<<"id_resource">>]),
    {200, {<<"id_resource">>, <<"GET">>, []}} = get(Config, Path),

    IdPath = path_from_list([<<"id_resource">>, <<"test_id">>]),
    {200, {<<"id_resource">>, <<"GET">>, <<"test_id">>, []}} =
        get(Config, IdPath).

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

id_handle(Tag, Req, Id, Env) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, {Tag, Method, Id, Env}, Req2}.

handle(Tag, Req, Data, Env) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, {Tag, Method, Data, Env}, Req2}.

id_handle(Tag, Req, Id, Data, Env) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, {Tag, Method, Id, Data, Env}, Req2}.

get_handler(<<"GET">>) -> fun handle/3;
get_handler(<<"PUT">>) -> fun handle/4;
get_handler(<<"POST">>) -> fun handle/4;
get_handler(<<"DELETE">>) -> fun handle/3.

resource(Name) ->
    {resource, Name,
     [
      {hop, fun(Req, Env) -> {ok, Req, [Name|Env]} end} |
      [
       {method, M, tag(Name, get_handler(M))}
       || M <- ?METHODS
      ]
     ]}.

error_resource(Name) ->
    {resource, Name,
     [
      {method, <<"POST">>,
       fun(Req, Reason, Env) ->
               {error, Reason, Req}
       end},
       {error_handler,
        fun (Req, _Code, {true, _DefaultHandle, Code, Reason}) ->
                {handled, Code, {custom_handled, Reason}, Req};
            (Req, _Code, _Reason) ->
                {unhandled, Req}
        end}
      ]}.

to_bert(Req, Data) ->
    Result = term_to_binary(Data),
    {ok, Result, Req}.

from_bert(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Result = binary_to_term(Body),
    {ok, Result, Req2}.

default_error_handler(Req, 404 = Code, wrong_path) ->
    {handled, Code, {default_handled, wrong_path}, Req};
default_error_handler(Req, _, {_CustomHandle, true, Code, Reason}) ->
    {handled, Code, {default_handled, Reason}, Req};
default_error_handler(Req, Code, Reason) ->
    {unhandled, Req}.

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

init(Req) ->
    {ok, Req, []}.

-define(HEADERS, [{<<"Content-Type">>, <<"application/bert">>}]).

get(Config, Path) ->
    ConnPid = gun_open(Config),
    Ref = gun:get(ConnPid, Path, ?HEADERS),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.

post(Config, Path, Data) ->
    DataEncoded = term_to_binary(Data),
    ConnPid = gun_open(Config),
    Ref = gun:post(ConnPid, Path, ?HEADERS, DataEncoded),
    {response, Fin, Code, _Headers} = gun:await(ConnPid, Ref),
    case Fin of
        nofin ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            {Code, binary_to_term(Body)};
        fin ->
            Code
    end.

put(Config, Path, Data) ->
    DataEncoded = term_to_binary(Data),
    ConnPid = gun_open(Config),
    Ref = gun:put(ConnPid, Path, ?HEADERS, DataEncoded),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.

delete(Config, Path) ->
    ConnPid = gun_open(Config),
    Ref = gun:delete(ConnPid, Path, ?HEADERS),
    {response, nofin, Code, _Headers} = gun:await(ConnPid, Ref),
    {ok, Body} = gun:await_body(ConnPid, Ref),
    {Code, binary_to_term(Body)}.

%% === Simple Req helpers ======================================================

simple_get(Config, Path, Headers) ->
    ConnPid = gun_open(Config),
    Ref = gun:get(ConnPid, Path, [Headers]),
    gun:await(ConnPid, Ref).

simple_post(Config, Path, Headers, Data) ->
    ConnPid = gun_open(Config),
    Ref = gun:post(ConnPid, Path, Headers, Data),
    gun:await(ConnPid, Ref).
