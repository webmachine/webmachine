%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc mock_resource startup code

-module(mock_resource).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    mock_resource_sup:start_link().

%% @spec start() -> ok
%% @doc Start the mock_resource server.
start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(mock_resource).

%% @spec stop() -> ok
%% @doc Stop the mock_resource server.
stop() ->
    Res = application:stop(mock_resource),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.
