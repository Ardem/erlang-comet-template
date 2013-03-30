%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc server.

-module(server).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the server server.
start() ->
    server_deps:ensure(),
    ensure_started(crypto),
    application:start(server).


%% @spec stop() -> ok
%% @doc Stop the server server.
stop() ->
    application:stop(server).
