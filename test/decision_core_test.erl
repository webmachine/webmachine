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

%% DECISION TRACE PATHS
%%
%% Not all possible paths will be tested at this point. The current testing
%% goal is decision/condition coverage: "It requires sufficient test cases that
%% each condition in a decision takes on all possible outcomes at least once,
%% and each point of entry is invoked at least once" (The Art of Software
%% Testing, 2nd, p 49). (Exercising all possible paths is called
%% multiple-condition coverage.)

%% B9 - There is one path to state B9
-define(PATH_TO_B9, [v3b13, v3b13b, v3b12, v3b11, v3b10, v3b9]).

%% B9A - There is one path to the sub-state B9A
-define(PATH_TO_B9A, ?PATH_TO_B9 ++ [v3b9a]).

%% B8 - There is one path to state B8 (skips sub-state B9A?)
-define(PATH_TO_B8, ?PATH_TO_B9 ++ [v3b9b, v3b8]).

%% B3 - There is one path to state B3
-define(PATH_TO_B3, ?PATH_TO_B8 ++ [v3b7, v3b6, v3b5, v3b4, v3b3]).

%% C3 - There is one path to state C3
-define(PATH_TO_C3, ?PATH_TO_B3 ++ [v3c3]).

%% C4 - There is one path to state C4
-define(PATH_TO_C4, ?PATH_TO_C3 ++ [v3c4]).

%% D4 - There are two paths to D4: via C3 or via C4
-define(PATH_TO_D4_VIA_C3, ?PATH_TO_C3 ++ [v3d4]).
-define(PATH_TO_D4_VIA_C4, ?PATH_TO_C4 ++ [v3d4]).

%% D5 - There are two paths to D5: via C3 or via C4
-define(PATH_TO_D5_VIA_C3, ?PATH_TO_D4_VIA_C3 ++ [v3d5]).
-define(PATH_TO_D5_VIA_C4, ?PATH_TO_D4_VIA_C4 ++ [v3d5]).

%% E5 - There are four paths to E5:
%%  via D5 (via C3 or via C4) or
%%  via D4 (via C3 or via C4)
%% Only some of these paths are tested.
-define(PATH_TO_E5_VIA_D5_C3, ?PATH_TO_D5_VIA_C3 ++ [v3e5]).
-define(PATH_TO_E5_VIA_D5_C4, ?PATH_TO_D5_VIA_C4 ++ [v3e5]).
-define(PATH_TO_E5_VIA_D4_C3, ?PATH_TO_D4_VIA_C3 ++ [v3e5]).

%% E6 - There are four paths to E6:
%%  via D5 (via C3 or via C4) or
%%  via D4 (via C3 or via C4)
%% Only two of these paths to E6 are tested
-define(PATH_TO_E6_VIA_D5_C3, ?PATH_TO_E5_VIA_D5_C3 ++ [v3e6]).
-define(PATH_TO_E6_VIA_D5_C4, ?PATH_TO_E5_VIA_D5_C4 ++ [v3e6]).

%% F6 - Selection of the paths to F6
-define(PATH_TO_F6_VIA_E6_D5_C4, ?PATH_TO_E6_VIA_D5_C4 ++ [v3f6]).
-define(PATH_TO_F6_VIA_E5_D4_C3, ?PATH_TO_E5_VIA_D4_C3 ++ [v3f6]).

%% F7 - A path to F7
-define(PATH_TO_F7_VIA_E6_D5_C4, ?PATH_TO_F6_VIA_E6_D5_C4 ++ [v3f7]).

%% G7 - The path to G7, without accept headers in the request
-define(PATH_TO_G7_VIA_F6_E6_D5_C4, ?PATH_TO_F6_VIA_E5_D4_C3 ++ [v3g7]).
-define(PATH_TO_G7_NO_ACCEPT_HEADERS, ?PATH_TO_G7_VIA_F6_E6_D5_C4).

%% G9 - The path to G9, without accept headers in the request
-define(PATH_TO_G9_VIA_F6_E6_D5_C4,
        ?PATH_TO_G7_VIA_F6_E6_D5_C4 ++ [v3g8, v3g9]).

%% G11 - The path to G11, without accept headers in the request
-define(PATH_TO_G11_VIA_F6_E6_D5_C4,
        ?PATH_TO_G7_VIA_F6_E6_D5_C4 ++ [v3g8, v3g9, v3g11]).
-define(PATH_TO_G11_NO_ACCEPT_HEADERS, ?PATH_TO_G11_VIA_F6_E6_D5_C4).

%% H7 - The path to H7 without accept headers
-define(PATH_TO_H7_NO_ACCEPT_HEADERS, ?PATH_TO_G7_NO_ACCEPT_HEADERS ++ [v3h7]).

%% H10 - The path to H10 without accept headers
-define(PATH_TO_H10_VIA_G8_F6_E6_D5_C4,
        ?PATH_TO_G7_VIA_F6_E6_D5_C4 ++ [v3g8, v3h10]).

%% H11 - The path to H11 without accept headers, via G11
-define(PATH_TO_H11_VIA_G11_F6_E6_D5_C4,
        ?PATH_TO_G11_NO_ACCEPT_HEADERS ++ [v3h10, v3h11]).

%% H12 - Two paths to H12 without accept headers
-define(PATH_TO_H12_VIA_G8_F6_E6_D5_C4,
        ?PATH_TO_H10_VIA_G8_F6_E6_D5_C4 ++ [v3h11, v3h12]).
-define(PATH_TO_H12_VIA_G9_F6_E6_D5_C4,
        ?PATH_TO_G9_VIA_F6_E6_D5_C4 ++ [v3h10, v3h11, v3h12]).
-define(PATH_TO_H12_NO_ACCEPT_HEADERS, ?PATH_TO_H12_VIA_G8_F6_E6_D5_C4).
-define(PATH_TO_H12_NO_ACCEPT_HEADERS_2, ?PATH_TO_H12_VIA_G9_F6_E6_D5_C4).

%% I12 - Two paths to I12 without accept headers
-define(PATH_TO_I12_VIA_H10_G8_F6_E6_D5_C4,
        ?PATH_TO_H10_VIA_G8_F6_E6_D5_C4 ++ [v3i12]).
-define(PATH_TO_I12_VIA_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_H11_VIA_G11_F6_E6_D5_C4 ++ [v3i12]).

%% I13 - Two paths to I13 without accept headers
-define(PATH_TO_I13_VIA_H10_G8_F6_E6_D5_C4,
        ?PATH_TO_I12_VIA_H10_G8_F6_E6_D5_C4 ++ [v3i13]).
-define(PATH_TO_I13_VIA_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_I12_VIA_H11_G11_F6_E6_D5_C4 ++ [v3i13]).

%% K13 - The path to K13 without accept headers, via I13, I12, H11, G11
-define(PATH_TO_K13_VIA_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_I13_VIA_H11_G11_F6_E6_D5_C4 ++ [v3k13]).

%% J18 - Three paths to J18 without accept headers (one via H10; one via H11
%% and K13; one via H12)
-define(PATH_TO_J18_VIA_I13_H10_G8_F6_E6_D5_C4,
        ?PATH_TO_I13_VIA_H10_G8_F6_E6_D5_C4 ++ [v3j18]).
-define(PATH_TO_J18_VIA_K13_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_K13_VIA_H11_G11_F6_E6_D5_C4 ++ [v3j18]).
-define(PATH_TO_J18_NO_ACCEPT_HEADERS, ?PATH_TO_J18_VIA_I13_H10_G8_F6_E6_D5_C4).
-define(PATH_TO_J18_NO_ACCEPT_HEADERS_2,
        ?PATH_TO_J18_VIA_K13_H11_G11_F6_E6_D5_C4).
-define(PATH_TO_J18_NO_ACCEPT_HEADERS_3,
       ?PATH_TO_H12_NO_ACCEPT_HEADERS_2 ++ [v3i12, v3i13, v3j18]).

%% A path to a 200 with most defaults used
-define(PATH_TO_REGULAR_200,
        ?PATH_TO_I12_VIA_H10_G8_F6_E6_D5_C4
        ++ [v3l13, v3m16, v3n16, v3o16, v3o18, v3o18b]).

%% A path to a 204 with most defaults used, md5 checksum substate [Clever way
%% to write this removed until a better solution for decision paths and
%% substates is devised]
-define(PATH_TO_204_WITH_MD5_CHECKSUM,
        [v3b13, v3b13b, v3b12, v3b11, v3b10, v3b9, v3b9a, v3b9b, v3b8, v3b7,
         v3b6, v3b5, v3b4, v3b3, v3c3, v3d4, v3e5, v3f6, v3g7, v3g8, v3h10,
         v3i12, v3l13, v3m16, v3n16, v3o16, v3o14, v3p11, v3o20]).

%%
%% TEST SETUP AND CLEANUP
%%
decision_core_test_() ->
    Tests =
        [
         {"503 it's not you, it's me", fun service_unavailable/0},
         {"503 ping doesn't return pong", fun ping_invalid/0},
         {"200 head method allowed", fun head_method_allowed/0},
         {"405 head method not allowed", fun head_method_not_allowed/0},
         {"200 get method", fun simple_get/0},
         {"406 via c4", fun not_acceptable_c4/0},
         {"406 via d5<-c4", fun not_acceptable_d5_c4/0},
         {"406 via d5<-c3", fun not_acceptable_d5_c3/0},
         {"406 via e6<-d5<-c3", fun not_acceptable_e6_d5_c3/0},
         {"406 via f7<-e6<-d5<-c4", fun not_acceptable_f7_e6_d5_c4/0},
         {"412 no headers, no resource", fun precond_fail_no_resource/0},
         {"412 via g11 if-match, no etag", fun precond_fail_g11/0},
         {"412 via h12, greater last modified", fun precond_fail_h12/0},
         {"412 via j18<-i13<-i12<-h10", fun precond_fail_j18/0},
         {"412 via j18<-k13<-h11<-g11", fun precond_fail_j18_via_k13/0},
         {"412 via j18<-i13<-i12<-h12", fun precond_fail_j18_via_h12/0},
         {"204 md5 header matches", fun content_md5_valid_b9a/0},
         {"400 md5 header doesn't match", fun content_md5_invalid_b9a/0},
         {"401 result, unauthorized", fun authorized_b8/0},
         {"200 result, via options", fun options_b3/0},
         {"200 result with vary", fun variances_g7/0}
        ],
    {foreach, fun setup/0, fun cleanup/1, Tests}.

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
    put_setting(language_available, false),
    Headers = [{"Accept-Language", "x-pig-latin"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_D5_VIA_C3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 result via E6 via D5 via C3
not_acceptable_e6_d5_c3() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(charsets_provided, [{"utf-8", make_utf8}]),
    Headers = [{"Accept-Language", "en-US"},
               {"Accept-Charset", "ISO-8859-1"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_E6_VIA_D5_C3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 result via F7 via E6 via D5 via C4
not_acceptable_f7_e6_d5_c4() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(language_available, true),
    put_setting(charsets_provided, [{"utf-8", fun(X) -> X end}]),
    put_setting(encodings_provided, none),
    Headers = [{"Accept", "text/plain"},
               {"Accept-Language", "en-US"},
               {"Accept-Charset", "utf-8"},
               {"Accept-Encoding", "gzip"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_F7_VIA_E6_D5_C4,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via H7, no accept headers and no resource
precond_fail_no_resource() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(resource_exists, false),
    Headers = [{"If-Match", "*"}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_H7_NO_ACCEPT_HEADERS,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via G11, no accept headers
precond_fail_g11() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(generate_etag, "v2"),
    Headers = [{"If-Match", "\"v0\", \"v1\""}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_G11_NO_ACCEPT_HEADERS,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via H12, greater last modified
precond_fail_h12() ->
    put_setting(allowed_methods, ['GET']),
    TenAM = "Wed, 20 Feb 2013 10:00:00 GMT",
    FivePM = "Wed, 20 Feb 2013 17:00:00 GMT",
    ResErlDate = httpd_util:convert_request_date(FivePM),
    put_setting(last_modified, ResErlDate),
    Headers = [{"If-Unmodified-Since", TenAM}],
    {ok, Result} = httpc:request(get, {?URL, Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_H12_NO_ACCEPT_HEADERS,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via J18 via I13 via I12 via H10
precond_fail_j18() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT']),
    Headers = [{"If-None-Match", "*"}],
    PutRequest = {?URL, Headers, "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACCEPT_HEADERS,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via J18 via K13 via H11 via G11
precond_fail_j18_via_k13() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT']),
    put_setting(generate_etag, "v1"),
    Headers = [{"If-Match", "\"v1\""},
               {"If-None-Match", "\"v1\""},
               {"If-Unmodified-Since", "{{INVALID DATE}}"}],
    PutRequest = {?URL, Headers, "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACCEPT_HEADERS_2,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via J18 via I13 via I12 via H12
precond_fail_j18_via_h12() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT']),
    TenAM = "Wed, 20 Feb 2013 10:00:00 GMT",
    FivePM = "Wed, 20 Feb 2013 17:00:00 GMT",
    ResErlDate = httpd_util:convert_request_date(TenAM),
    put_setting(last_modified, ResErlDate),
    Headers = [{"If-Match", "*"},
               {"If-None-Match", "*"},
               {"If-Unmodified-Since", FivePM}],
    PutRequest = {?URL, Headers, "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACCEPT_HEADERS_3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 204 result, content-md5 header matches
content_md5_valid_b9a() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT']),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    Body = "foo",
    MD5Sum = base64:encode_to_string(crypto:md5(Body)),
    Headers = [{"Content-MD5", MD5Sum}],
    PutRequest = {?URL ++ "/new", Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_204_WITH_MD5_CHECKSUM,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 400 result, content-md5 header does not match
content_md5_invalid_b9a() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT']),
    ContentType = "text/plain",
    Body = "foo",
    InvalidMD5Sum = base64:encode_to_string("this is invalid for foo"),
    Headers = [{"Content-MD5", InvalidMD5Sum}],
    PutRequest = {?URL, Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 400, "Bad Request"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B9A,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 401 result, unauthorized
authorized_b8() ->
    put_setting(is_authorized, "Basic"),
    put_setting(allowed_methods, ['GET']),
    {ok, Result} = httpc:request(get, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 401, "Unauthorized"},
                  [_, _, {"www-authenticate", "Basic"}, _], _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B8,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result, via OPTIONS
options_b3() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT', 'OPTIONS']),
    {ok, Result} = httpc:request(options, {?URL, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result with Vary
variances_g7() ->
    put_setting(allowed_methods, ['GET']),
    Id = fun(X) -> X end,
    Charsets = [{"utf-8", Id},
                {"iso-8859-5", Id},
                {"unicode-1-1", Id}],
    put_setting(charsets_provided, Charsets),
    {ok, Result} = httpc:request(get, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_REGULAR_200,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

accept_html_on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

simple_get() ->
    put_setting(allowed_methods, ['GET']),
    {ok, Result} = httpc:request(get, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, ?HTML_CONTENT}, Result),
    ExpectedDecisionTrace = ?PATH_TO_REGULAR_200,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%%
%% WEBMACHINE RESOURCE FUNCTIONS AND CONFIGURATION
%%

initialize_resource_settings() ->
    %% Configure ETS table to hold resource settings for each test
    ets:new(?MODULE, [named_table, public]),
    
    %% Defaults
    put_setting(service_available, true),
    put_setting(ping, pong),
    put_setting(validate_content_checksum, not_validated),
    put_setting(is_authorized, true),
    put_setting(content_types_provided, [{"text/html", to_html}]),
    put_setting(language_available, true),
    put_setting(charsets_provided, no_charset),
    put_setting(encodings_provided, use_identity),
    put_setting(resource_exists, true),
    put_setting(generate_etag, undefined),
    put_setting(last_modified, undefined),
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

validate_content_checksum(ReqData, Context) ->
    Setting = lookup_setting(validate_content_checksum),
    {Setting, ReqData, Context}.

is_authorized(ReqData, Context) ->
    Setting = lookup_setting(is_authorized),
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

encodings_provided(ReqData, Context) ->
    Setting = lookup_setting(encodings_provided),
    %% Since we can't store Erlang funs in term storage, we use a case to
    %% match atoms to intended sets of funs
    Value =
        case Setting of
            use_identity ->
                [{"identity", fun(X) -> X end}];
            none ->
                [];
            Setting -> Setting
        end,
    {Value, ReqData, Context}.

resource_exists(ReqData, Context) ->
    Setting = lookup_setting(resource_exists),
    {Setting, ReqData, Context}.

generate_etag(ReqData, Context) ->
    Setting = lookup_setting(generate_etag),
    {Setting, ReqData, Context}.

last_modified(ReqData, Context) ->
    Setting = lookup_setting(last_modified),
    {Setting, ReqData, Context}.

to_html(ReqData, Context) ->
    {?HTML_CONTENT, ReqData, Context}.

-endif.
