-module(nossh_sup).

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
    {ok, {Flags, [get_cowboy_child_spec({0, 0, 0, 0}, 2222)]}}.

get_cowboy_child_spec(IP, Port) ->
    Secret = read_ssh_secret(),
    SID = read_ssh_sid(),
    ranch:child_spec(
        ?SERVER,
        ranch_tcp,
        #{
            socket_opts   => [{ip, IP}, {port, Port}],
            num_acceptors => 4
        },
        nossh_handler,
        #{
            ssh_shared_secret => Secret,
            ssh_session_id    => SID
        }
    ).

read_ssh_secret() ->
    {ok, Path} = application:get_env(nossh, ssh_secret_file),
    {ok, Secret} = file:read_file(Path),
    binary_to_integer(Secret).

read_ssh_sid() ->
    {ok, Path} = application:get_env(nossh, ssh_sid_file),
    {ok, SID} = file:read_file(Path),
    SID.
