%% @copyright 2007-2013 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

-module(decision_core_test).

-ifdef(TEST).

-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(PORT, 12001).
-define(URL, "http://localhost:12001/decisioncore").
-define(HTML_CONTENT, "<html><body>Foo</body></html>").

%% B3 - There is one path to state B3
-define(PATH_TO_B3, [v3b13, v3b13b, v3b12, v3b11, v3b10, v3b9, v3b9b, v3b8,
                      v3b7, v3b6, v3b5, v3b4, v3b3]).

%% C3 - There is one path to state C3
-define(PATH_TO_C3, ?PATH_TO_B3 ++ [v3c3]).

%% C4 - There is one path to state C4
-define(PATH_TO_C4, ?PATH_TO_C3 ++ [v3c4]).

%% D4 - There are two paths to D4:
%%  via C3 or
%%  via C4
-define(PATH_TO_D4_VIA_C3, ?PATH_TO_C3 ++ [v3d4]).
-define(PATH_TO_D4_VIA_C4, ?PATH_TO_C4 ++ [v3d4]).

%% D5 - There are two paths to D5:
%%  via C3 or
%%  via C4
-define(PATH_TO_D5_VIA_C3, ?PATH_TO_D4_VIA_C3 ++ [v3d5]).
-define(PATH_TO_D5_VIA_C4, ?PATH_TO_D4_VIA_C4 ++ [v3d5]).

%% E5 - There are four paths to E5:
%%  via D5 (via C3 or
%%          via C4) or
%%  via D4 (via C3 or
%%          via C4)
-define(PATH_TO_E5_VIA_D5_VIA_C3, ?PATH_TO_D5_VIA_C3 ++ [v3e5]).
-define(PATH_TO_E5_VIA_D5_VIA_C4, ?PATH_TO_D5_VIA_C4 ++ [v3e5]).
-define(PATH_TO_E5_VIA_D4_VIA_C3, ?PATH_TO_D4_VIA_C3 ++ [v3e5]).
-define(PATH_TO_E5_VIA_D4_VIA_C4, ?PATH_TO_D4_VIA_C4 ++ [v3e5]).

%% E6 - There are four paths to E6:
%%  via D5 (via C3 or
%%          via C4) or
%%  via D4 (via C3 or
%%          via C4)
-define(PATH_TO_E6_VIA_D5_VIA_C3, ?PATH_TO_E5_VIA_D5_VIA_C3 ++ [v3e6]).
-define(PATH_TO_E6_VIA_D5_VIA_C4, ?PATH_TO_E5_VIA_D5_VIA_C4 ++ [v3e6]).
-define(PATH_TO_E6_VIA_D4_VIA_C3, ?PATH_TO_E5_VIA_D4_VIA_C3 ++ [v3e6]).
-define(PATH_TO_E6_VIA_D4_VIA_C4, ?PATH_TO_E5_VIA_D4_VIA_C4 ++ [v3e6]).

decision_core_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {<<"503 it's not you, it's me">>, fun service_unavailable/0},
      {<<"503 ping doesn't return pong">>, fun ping_invalid/0},
      {<<"200 head method allowed">>, fun head_method_allowed/0},
      {<<"405 head method not allowed">>, fun head_method_not_allowed/0},
      {<<"200 get method">>, fun simple_get/0},
      {<<"406 via c4">>, fun not_acceptable_c4/0},
      {<<"406 via d5 via c4">>, fun not_acceptable_d5_c4/0},
      {<<"406 via d5 via c3">>, fun not_acceptable_d5_c3/0},
      {<<"406 via e6 via d5 via c3">>, fun not_acceptable_e6_d5_c3/0}
     ]}.

setup() ->
    error_logger:tty(false),
    initialize_resource_settings(),
    application:start(inets),
    WebConfig = [{ip, "0.0.0.0"},
                 {port, ?PORT},
                 {dispatch, [{["decisioncore", '*'],
                              ?MODULE, []}]}
                ],
    {ok, Pid0} = webmachine_sup:start_link(),
    {ok, Pid} = webmachine_mochiweb:start(WebConfig),
    link(Pid),
    meck:new(webmachine_resource, [passthrough]),
    {Pid0, Pid}.

cleanup({Pid0, Pid1}) ->
    meck:unload(webmachine_resource),
    %% clean up
    unlink(Pid0),
    exit(Pid0, kill),
    unlink(Pid1),
    exit(Pid1, kill),
    application:stop(inets),
    clear_resource_settings().

get_decision_ids() ->
    [DecisionID || {_, {webmachine_resource, log_d, [DecisionID|_]}, _}
                       <- meck:history(webmachine_resource)].

%%
%% TEST CASES
%%
%% The decision trace is the path through the HTTP/1.1 activity diagram, which
%% has nodes labeled from A-P on the x-axis and 1-26 on the y-axis.
%%
%% Note: The expected decision traces in these tests is simply the output from
%% the test itself. These have not yet been hand-verified!

service_unavailable() ->
    put_setting(service_available, false),
    {ok, Result} = httpc:request(head, {?URL, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 503, "Service Unavailable"}, _, _}, Result),
    ExpectedDecisionTrace =
        [v3b13, v3b13b],
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

ping_invalid() ->
    % "breakout" for "anything other than pong"
    put_setting(ping, breakout),
    {ok, Result} = httpc:request(head, {?URL, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 503, "Service Unavailable"}, _, _}, Result),
    ExpectedDecisionTrace =
        [v3b13],
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

head_method_allowed() ->
    put_setting(allowed_methods, ['GET', 'HEAD']),
    {ok, Result} = httpc:request(head, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace =
        [v3b13, v3b13b, v3b12, v3b11, v3b10, v3b9, v3b9b, v3b8, v3b7, v3b6,
         v3b5, v3b4, v3b3, v3c3, v3d4, v3e5, v3f6, v3g7, v3g8, v3h10, v3i12,
         v3l13, v3m16, v3n16, v3o16, v3o18, v3o18b],
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

head_method_not_allowed() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    {ok, Result} = httpc:request(head, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 405, "Method Not Allowed"}, _, _}, Result),
    ExpectedDecisionTrace =
        [v3b13, v3b13b, v3b12, v3b11, v3b10],
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 via C4
not_acceptable_c4() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_accepted, [{"text/html", accept_html_on_put}]),
    Headers = [{"Accept", "video/mp4"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_C4,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 via D5 via C4
not_acceptable_d5_c4() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(content_types_accepted, [{"text/plain", accept_html_on_put}]),
    put_setting(language_available, false),
    Headers = [{"Accept", "text/plain"},
               {"Accept-Language", "x-pig-latin"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_D5_VIA_C4,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 result via D5 via C3
not_acceptable_d5_c3() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(content_types_accepted, [{"text/plain", accept_html_on_put}]),
    put_setting(language_available, false),
    Headers = [{"Accept-Language", "x-pig-latin"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_D5_VIA_C3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 result via E6 via D5 via C3
not_acceptable_e6_d5_c3() ->
%    no charset available
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(content_types_accepted, [{"text/plain", accept_html_on_put}]),
    put_setting(charsets_provided, [{"utf-8", make_utf8}]),
    Headers = [{"Accept-Language", "en-US"},
               {"Accept-Charset", "ISO-8859-1"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_E6_VIA_D5_VIA_C3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

accept_html_on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

simple_get() ->
    put_setting(allowed_methods, ['GET']),
    {ok, Result} = httpc:request(get, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, ?HTML_CONTENT}, Result),
    ExpectedDecisionTrace =
        [v3b13, v3b13b, v3b12, v3b11, v3b10, v3b9, v3b9b, v3b8, v3b7, v3b6,
         v3b5, v3b4, v3b3, v3c3, v3d4, v3e5, v3f6, v3g7, v3g8, v3h10, v3i12,
         v3l13, v3m16, v3n16, v3o16, v3o18, v3o18b],
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

simple_put() ->
    Body = {?URL ++ "/bar", [], "binary/octet-stream", <<1,1,2,3,5>>},
    {ok, Result} = httpc:request(put, Body, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    io:format(user, "~p~n", [get_decision_ids()]),
    ok.

simple_post() ->
    Body = {?URL ++ "/bar", [], "binary/octet-stream", <<1,1,2,3,5>>},
    {ok, Result} = httpc:request(post, Body, [], []),
    io:format(user, "\n<<~p>>\n", [Result]).

%%
%% WEBMACHINE RESOURCE FUNCTIONS AND CONFIGURATION
%%

initialize_resource_settings() ->
    %% Configure ETS table to hold resource settings for each test
    ets:new(?MODULE, [named_table, public]),
    
    %% Defaults
    put_setting(service_available, true),
    put_setting(ping, pong),
    put_setting(content_types_provided, [{"text/html", to_html}]),
    put_setting(language_available, true),
    put_setting(charsets_provided, no_charset),
    ok.

clear_resource_settings() ->
    ets:delete(?MODULE).

put_setting(SettingName, SettingValue) ->
    ets:insert(?MODULE, {SettingName, SettingValue}).

lookup_setting(Setting) ->
    [{Setting, Value}] = ets:lookup(?MODULE, Setting),
    Value.

init([]) ->
    {ok, undefined}.

ping(ReqData, State) ->
    Setting = lookup_setting(ping),
    case Setting of
        ping_raise_error -> error(foobar);
        _ -> {Setting, ReqData, State}
    end.

service_available(ReqData, Context) ->
    Setting = lookup_setting(service_available),
    {Setting, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    Setting = lookup_setting(allowed_methods),
    {Setting, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    Setting = lookup_setting(content_types_provided),
    {Setting, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    Setting = lookup_setting(content_types_accepted),
    {Setting, ReqData, Context}.

language_available(ReqData, Context) ->
    Setting = lookup_setting(language_available),
    {Setting, ReqData, Context}.

charsets_provided(ReqData, Context) ->
    Setting = lookup_setting(charsets_provided),
    {Setting, ReqData, Context}.

to_html(ReqData, Context) ->
    {?HTML_CONTENT, ReqData, Context}.

-endif.
