%% Webmachine uses the default lager sink to log most errors and info
%% logs.

%% Webmachine will also set up a `wm_access` sink for the access and
%% perf logs.

%% Rather than try to reverse engineer old style configurations. We'll
%% start with the successful new style configuration which will just
%% assume a primary lager sink for the regular logs. That means that
%% if you don't have lager configured for your application, you won't
%% see any webmachine logs, but if you do, they'll be in the place you
%% expect them.

%% Here's how you can configure the `access` and `perf` logs in the
%% webmachine application env.

%% {webmachine,
%%   {log_handlers,
%%     [
%%       {access, Filename},
%%       {perf, Filename}
%%     ]}
%% }

%% Filename can be relative, or absolute. Relative will use
%% `lager.log_root`, whereas absolute will use the full path. Lager
%% will then use its rotation magic to make more than one file based
%% on the name.

-module(webmachine_lager).

-export([
         start/0
        ,put_metadata/1
        ]).

-type log_type() :: access | perf.
-type wm_log_handler() :: {log_type(), file:filename()}.

start() ->
    ExtraSinks = application:get_env(lager, extra_sinks, []),
    WMLogHandlers = application:get_env(
                      webmachine,
                      log_handlers,
                      []),

    NewExtraSinks = build_extra_sinks(WMLogHandlers) ++ ExtraSinks,
    application:set_env(lager, extra_sinks, NewExtraSinks),
    lager:start().

-spec build_extra_sinks([wm_log_handler()]) -> LagerSinkConfig :: list().
build_extra_sinks([]) -> [];
build_extra_sinks(WMLogHandlers) ->
    Handlers = build_wm_access_handlers(WMLogHandlers, []),

    [{wm_access_lager_event,
      [
       {handlers, Handlers}
      ]
     }].

-spec build_wm_access_handlers([wm_log_handler()], [tuple()]) -> [tuple()].
build_wm_access_handlers([], Acc) ->
    Acc;
build_wm_access_handlers([LogHandler|Rest], Acc) ->
    build_wm_access_handlers(
      Rest,
      [build_handler(LogHandler)|Acc]).

-spec build_handler(wm_log_handler()) -> LagerFileBackend :: tuple().
build_handler({access, Filename}) ->
    {lager_file_backend, [
                          {file, Filename}
                         ,{level, info}
                         ,{date, "$D0"}
                         ,{count, 5}
                         ,{size, 10485760}
                         ,{formatter, webmachine_httpd_formatter}
                         ,{formatter_config,
                           "%h %l %u %t \"%r\" %>s %b %{Referer}i %{User-agent}i"
                          }
                         ]};
build_handler({perf, Filename}) ->
    {lager_file_backend, [
                          {file, Filename}
                         ,{level, info}
                         ,{date, "$D0"}
                         ,{count, 5}
                         ,{size, 10485760}
                         ,{formatter, webmachine_httpd_formatter}
                         ,{formatter_config,
                           "%h %l %t \"%r\" %>s %b %R %{msec}T %wm-ppt"
                          }
                         ]}.

put_metadata(PList) ->
    MD = lager:md(),
    NewMD = put_metadata(PList, MD),
    lager:md(NewMD).

put_metadata([], MD) ->
    MD;
put_metadata([{Key, Value}|Rest], MD) ->
    NewMD = lists:keystore(Key, 1, MD, {Key, Value}),
    put_metadata(Rest, NewMD).
