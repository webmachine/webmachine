-module(webmachine_demo).
-author('Andy Gross <andy@basho.com>').
-author('Justin Sheehy <justin@@basho.com>').
-export([start/0, start_link/0, stop/0]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    LogHandlers = [{webmachine_access_log_handler, ["priv/log"]},
                   {webmachine_error_log_handler, ["priv/log"]}],
    application:set_env(webmachine, log_handlers, LogHandlers),
    webmachine_util:ensure_all_started(webmachine),
    webmachine_demo_sup:start_link().

%% @spec start() -> ok
%% @doc Start the webmachine_demo server.
start() ->
    LogHandlers = [{webmachine_access_log_handler, ["priv/log"]},
                   {webmachine_error_log_handler, ["priv/log"]}],
    application:set_env(webmachine, log_handlers, LogHandlers),
    webmachine_util:ensure_all_started(webmachine),
    application:start(webmachine_demo).

%% @spec stop() -> ok
%% @doc Stop the webmachine_demo server.
stop() ->
    Res = application:stop(webmachine_demo),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
