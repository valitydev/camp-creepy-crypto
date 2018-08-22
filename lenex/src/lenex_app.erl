-module(lenex_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API

-spec start(_StartType, _StartArgs) ->
    {ok, pid()}.

start(_StartType, _StartArgs) ->
    lenex_sup:start_link().

-spec stop(_) ->
    ok.

stop(_State) ->
    ok.
