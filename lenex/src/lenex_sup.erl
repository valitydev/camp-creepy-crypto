-module(lenex_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% API functions

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link(?MODULE, []).

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    Flags = #{strategy => one_for_all},
    {ok, {Flags, [get_cowboy_child_spec({0, 0, 0, 0}, 8001)]}}.

get_cowboy_child_spec(IP, Port) ->
    Key = read_key(),
    DB = ets:new(?MODULE, [public, set]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/status", lenex_handler, #{
                key => Key,
                db => DB
            }}
        ]}
    ]),
    ranch:child_spec(
        ?SERVER,
        ranch_tcp,
        [
            {ip, IP},
            {port, Port},
            {num_acceptors, 4}
        ],
        cowboy_clear,
        #{
            env => #{dispatch => Dispatch}
        }
    ).

read_key() ->
    {ok, Path} = application:get_env(lenex, keyfile),
    {ok, Key} = file:read_file(Path),
    Key.
