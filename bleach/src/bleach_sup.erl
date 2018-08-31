-module(bleach_sup).

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
    {ok, {Flags, [get_cowboy_child_spec({0, 0, 0, 0}, 8008)]}}.

get_cowboy_child_spec(IP, Port) ->
    Sk = read_secret_key(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", bleach_handler, #{sk => Sk}}
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

%%

-include_lib("public_key/include/public_key.hrl").

read_secret_key() ->
    {ok, Path} = application:get_env(bleach, keyfile),
    {ok, Bytes} = file:read_file(Path),
    [Entry] = public_key:pem_decode(Bytes),
    Sk = #'RSAPrivateKey'{} = public_key:pem_entry_decode(Entry),
    Sk.
