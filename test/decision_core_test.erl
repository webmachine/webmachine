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

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(PORT, 12001).
-define(URL, "http://localhost:12001/decisioncore").
-define(HTML_CONTENT, "<html><body>Foo</body></html>").

decision_core_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {<<"200 head method allowed">>, fun head_method_allowed/0},
      {<<"405 head method not allowed">>, fun head_method_not_allowed/0},
      {<<"200 get method">>, fun simple_get/0}
%      {<<"204 from a put">>,
%       fun simple_put/0
%      },
%      {<<"a post">>,
%       fun simple_post/0
%      }
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
%% Test cases
%%
%% Note: The expected decision traces in these tests is simply the output
%% from the test itself. These have not yet been hand-verified!
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
%% Webmachine resource functions and configuration
%%

initialize_resource_settings() ->
    %% Configure ETS table to hold resource settings for each test
    ets:new(?MODULE, [named_table, public]),
    
    %% Defaults
    put_setting(service_available, true).

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
    {pong, ReqData, State}.

service_available() ->
%    Setting = lookup_setting(service_available),
    true.

allowed_methods(ReqData, Context) ->
    Setting = lookup_setting(allowed_methods),
    {Setting, ReqData, Context}.

to_html(ReqData, Context) ->
    {?HTML_CONTENT, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"binary/octet-stream", on_put}], ReqData, Context}.

on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

-endif.
