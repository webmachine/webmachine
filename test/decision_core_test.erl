%% @author Macneil Shonle <mshonle@basho.com>
%% @copyright 2007-2014 Basho Technologies
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

-include("wm_reqdata.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(RESOURCE, atom_to_list(?MODULE)).
-define(RESOURCE_PATH, "/" ++ ?RESOURCE).
-define(HTML_CONTENT, "<html><body>Foo</body></html>").
-define(TEXT_CONTENT, ?HTML_CONTENT).

-ifndef(old_hash).
md5(Bin) ->
    crypto:hash(md5,Bin).
-else.
md5(Bin) ->
    crypto:md5(Bin).
-endif.

-define(HTTP_1_0_METHODS, ['GET', 'POST', 'HEAD']).
-define(HTTP_1_1_METHODS, ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE',
                           'CONNECT', 'OPTIONS']).
-define(DEFAULT_ALLOWED_METHODS, ['GET', 'HEAD', 'PUT']).

%% It doesn't matter if the date is current, it just needs to be valid
-define(PRESENT_YEAR, 2013).
-define(FIRST_DAY_OF_PRESENT_YEAR, {{?PRESENT_YEAR, 1, 1}, {12, 0, 0}}).
-define(FIRST_DAY_OF_LAST_YEAR, {{?PRESENT_YEAR - 1, 1, 1}, {12, 0, 0}}).
-define(FIRST_DAY_OF_NEXT_YEAR, {{?PRESENT_YEAR + 1, 1, 1}, {12, 0, 0}}).

%% DECISION TRACE PATHS
%%
%% Not all possible paths will be tested at this point. The current testing
%% goal is decision/condition coverage: "It requires sufficient test cases that
%% each condition in a decision takes on all possible outcomes at least once,
%% and each point of entry is invoked at least once" (The Art of Software
%% Testing, 2nd, p 49). (Exercising all possible paths is called
%% multiple-condition coverage.)

-define(DECISION_ID_PATTERN, "v3[[:alpha:]]+\\d+[[:alpha:]]*").
-define(DECISION_ID_SUBSTATE_PATTERN, "v3[[:alpha:]]+\\d+[[:alpha:]]+").

%% B13-B3 -- All decision-trace-paths start at B13. There is a linear path
%% crossing the B nodes if you ignore substates.
-define(PATH_TO_B13, [v3b13]).
-define(PATH_TO_B12, ?PATH_TO_B13++[v3b12]).
-define(PATH_TO_B11, ?PATH_TO_B12++[v3b11]).
-define(PATH_TO_B10, ?PATH_TO_B11++[v3b10]).
-define(PATH_TO_B9, ?PATH_TO_B10++[v3b9]).
-define(PATH_TO_B8, ?PATH_TO_B9++[v3b8]).
-define(PATH_TO_B7, ?PATH_TO_B8++[v3b7]).
-define(PATH_TO_B6, ?PATH_TO_B7++[v3b6]).
-define(PATH_TO_B5, ?PATH_TO_B6++[v3b5]).
-define(PATH_TO_B4, ?PATH_TO_B5++[v3b4]).
-define(PATH_TO_B3, ?PATH_TO_B4++[v3b3]).

%% C3 - There is one path to state C3
-define(PATH_TO_C3, ?PATH_TO_B3++[v3c3]).

%% C4 - There is one path to state C4
-define(PATH_TO_C4, ?PATH_TO_C3++[v3c4]).

%% D4 - There are two paths to D4: via C3 or via C4
-define(PATH_TO_D4_VIA_C3, ?PATH_TO_C3++[v3d4]).
-define(PATH_TO_D4_VIA_C4, ?PATH_TO_C4++[v3d4]).

%% D5 - There are two paths to D5: via C3 or via C4
-define(PATH_TO_D5_VIA_C3, ?PATH_TO_D4_VIA_C3++[v3d5]).
-define(PATH_TO_D5_VIA_C4, ?PATH_TO_D4_VIA_C4++[v3d5]).

%% E5 - There are four paths to E5: via D5 (via C3 or via C4) or via D4 (via C3
%% or via C4). Only some of these paths are tested.
-define(PATH_TO_E5_VIA_D5_C3, ?PATH_TO_D5_VIA_C3++[v3e5]).
-define(PATH_TO_E5_VIA_D5_C4, ?PATH_TO_D5_VIA_C4++[v3e5]).
-define(PATH_TO_E5_VIA_D4_C3, ?PATH_TO_D4_VIA_C3++[v3e5]).

%% E6 - There are four paths to E6: via D5 (via C3 or via C4) or via D4 (via C3
%%  or via C4). Only two of these paths to E6 are tested
-define(PATH_TO_E6_VIA_D5_C3, ?PATH_TO_E5_VIA_D5_C3++[v3e6]).
-define(PATH_TO_E6_VIA_D5_C4, ?PATH_TO_E5_VIA_D5_C4++[v3e6]).

%% F6 - Selection of the paths to F6
-define(PATH_TO_F6_VIA_E6_D5_C4, ?PATH_TO_E6_VIA_D5_C4++[v3f6]).
-define(PATH_TO_F6_VIA_E5_D4_C3, ?PATH_TO_E5_VIA_D4_C3++[v3f6]).

%% F7 - A path to F7
-define(PATH_TO_F7_VIA_E6_D5_C4, ?PATH_TO_F6_VIA_E6_D5_C4++[v3f7]).

%% G7 - The path to G7, without accept headers in the request
-define(PATH_TO_G7_VIA_F6_E6_D5_C4, ?PATH_TO_F6_VIA_E5_D4_C3++[v3g7]).
-define(PATH_TO_G7_NO_ACPTHEAD, ?PATH_TO_G7_VIA_F6_E6_D5_C4).

%% G9 - The path to G9, without accept headers in the request
-define(PATH_TO_G9_VIA_F6_E6_D5_C4,
        ?PATH_TO_G7_VIA_F6_E6_D5_C4++[v3g8,v3g9]).

%% G11 - The path to G11, without accept headers in the request
-define(PATH_TO_G11_VIA_F6_E6_D5_C4,
        ?PATH_TO_G7_VIA_F6_E6_D5_C4++[v3g8,v3g9,v3g11]).
-define(PATH_TO_G11_NO_ACPTHEAD, ?PATH_TO_G11_VIA_F6_E6_D5_C4).

%% H7 - The path to H7 without accept headers
-define(PATH_TO_H7_NO_ACPTHEAD, ?PATH_TO_G7_NO_ACPTHEAD++[v3h7]).

%% I7 - The path to I7 without accept headers
-define(PATH_TO_I7_NO_ACPTHEAD, ?PATH_TO_H7_NO_ACPTHEAD++[v3i7]).

%% I4 - The path to I4 without accept headers
-define(PATH_TO_I4_NO_ACPTHEAD, ?PATH_TO_I7_NO_ACPTHEAD++[v3i4]).

%% K7 - The path to K7 without accept headers
-define(PATH_TO_K7_NO_ACPTHEAD, ?PATH_TO_I7_NO_ACPTHEAD++[v3k7]).

%% L7 - The path to L7 without accept headers
-define(PATH_TO_L7_NO_ACPTHEAD, ?PATH_TO_K7_NO_ACPTHEAD++[v3l7]).

%% M7 - The path to M7 without accept headers
-define(PATH_TO_M7_NO_ACPTHEAD, ?PATH_TO_L7_NO_ACPTHEAD++[v3m7]).

%% N11 - Two paths to N11 without accept headers
-define(PATH_TO_N11_VIA_M7_NO_ACPTHEAD, ?PATH_TO_M7_NO_ACPTHEAD++[v3n11]).
-define(PATH_TO_N11_VIA_N5_NO_ACPTHEAD, ?PATH_TO_N5_NO_ACPTHEAD++[v3n11]).

%% P3 - The path to P3 without accept headers
-define(PATH_TO_P3_NO_ACPTHEAD, ?PATH_TO_I4_NO_ACPTHEAD++[v3p3]).

%% K5 - The path to K5 without accept headers
-define(PATH_TO_K5_NO_ACPTHEAD, ?PATH_TO_K7_NO_ACPTHEAD++[v3k5]).

%% L5 - The path to L5 without accept headers
-define(PATH_TO_L5_NO_ACPTHEAD, ?PATH_TO_K5_NO_ACPTHEAD++[v3l5]).

%% M5 - The path to M5 without accept headers
-define(PATH_TO_M5_NO_ACPTHEAD, ?PATH_TO_L5_NO_ACPTHEAD++[v3m5]).

%% N5 - The path to N5 without accept headers
-define(PATH_TO_N5_NO_ACPTHEAD, ?PATH_TO_M5_NO_ACPTHEAD++[v3n5]).

%% H10 - The path to H10 without accept headers
-define(PATH_TO_H10_VIA_G8_F6_E6_D5_C4,
        ?PATH_TO_G7_VIA_F6_E6_D5_C4++[v3g8,v3h10]).

%% H11 - The path to H11 without accept headers, via G11
-define(PATH_TO_H11_VIA_G11_F6_E6_D5_C4,
        ?PATH_TO_G11_NO_ACPTHEAD++[v3h10,v3h11]).

%% H12 - Two paths to H12 without accept headers
-define(PATH_TO_H12_VIA_G8_F6_E6_D5_C4,
        ?PATH_TO_H10_VIA_G8_F6_E6_D5_C4++[v3h11,v3h12]).
-define(PATH_TO_H12_VIA_G9_F6_E6_D5_C4,
        ?PATH_TO_G9_VIA_F6_E6_D5_C4++[v3h10,v3h11,v3h12]).
-define(PATH_TO_H12_NO_ACPTHEAD, ?PATH_TO_H12_VIA_G8_F6_E6_D5_C4).
-define(PATH_TO_H12_NO_ACPTHEAD_2, ?PATH_TO_H12_VIA_G9_F6_E6_D5_C4).

%% I12 - Two paths to I12 without accept headers
-define(PATH_TO_I12_VIA_H10_G8_F6_E6_D5_C4,
        ?PATH_TO_H10_VIA_G8_F6_E6_D5_C4++[v3i12]).
-define(PATH_TO_I12_VIA_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_H11_VIA_G11_F6_E6_D5_C4++[v3i12]).

%% L13 - A path to L13 without accept headers
-define(PATH_TO_L13_NO_ACPTHEAD, ?PATH_TO_I12_VIA_H10_G8_F6_E6_D5_C4++[v3l13]).

%% M16 - A path to M16 without accept headers
-define(PATH_TO_M16_NO_ACPTHEAD, ?PATH_TO_L13_NO_ACPTHEAD++[v3m16]).

%% M20 - A path to M20 without accept headers
-define(PATH_TO_M20_NO_ACPTHEAD, ?PATH_TO_M16_NO_ACPTHEAD++[v3m20]).

%% N16 - A path to N16 without accept headers
-define(PATH_TO_N16_NO_ACPTHEAD, ?PATH_TO_M16_NO_ACPTHEAD++[v3n16]).

%% O16 - A path to O16 without accept headers
-define(PATH_TO_O16_NO_ACPTHEAD, ?PATH_TO_N16_NO_ACPTHEAD++[v3o16]).

%% O14 - A path to O14 without accept headers
-define(PATH_TO_O14_NO_ACPTHEAD, ?PATH_TO_O16_NO_ACPTHEAD++[v3o14]).

%% O18 - A path to O18 without accept headers
-define(PATH_TO_O18_NO_ACPTHEAD, ?PATH_TO_O16_NO_ACPTHEAD++[v3o18]).

%% O20 - A path to O20 without accept headers
-define(PATH_TO_O20_NO_ACPTHEAD, PATH_TO_P11).

%% L17 - A path to L17 without accept headers
-define(PATH_TO_L17_NO_ACPTHEAD,
        ?PATH_TO_L13_NO_ACPTHEAD++[v3l14,v3l15,v3l17]).

%% I13 - Two paths to I13 without accept headers
-define(PATH_TO_I13_VIA_H10_G8_F6_E6_D5_C4,
        ?PATH_TO_I12_VIA_H10_G8_F6_E6_D5_C4++[v3i13]).
-define(PATH_TO_I13_VIA_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_I12_VIA_H11_G11_F6_E6_D5_C4++[v3i13]).

%% K13 - The path to K13 without accept headers, via I13, I12, H11, G11
-define(PATH_TO_K13_VIA_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_I13_VIA_H11_G11_F6_E6_D5_C4++[v3k13]).

%% J18 - Three paths to J18 without accept headers (one via H10; one via H11
%% and K13; one via H12)
-define(PATH_TO_J18_VIA_I13_H10_G8_F6_E6_D5_C4,
        ?PATH_TO_I13_VIA_H10_G8_F6_E6_D5_C4++[v3j18]).
-define(PATH_TO_J18_VIA_K13_H11_G11_F6_E6_D5_C4,
        ?PATH_TO_K13_VIA_H11_G11_F6_E6_D5_C4++[v3j18]).
-define(PATH_TO_J18_NO_ACPTHEAD, ?PATH_TO_J18_VIA_I13_H10_G8_F6_E6_D5_C4).
-define(PATH_TO_J18_NO_ACPTHEAD_2, ?PATH_TO_J18_VIA_K13_H11_G11_F6_E6_D5_C4).
-define(PATH_TO_J18_NO_ACPTHEAD_3,
        ?PATH_TO_H12_NO_ACPTHEAD_2++[v3i12,v3i13,v3j18]).

%% P11 - Three paths to P11 without accept headers, via N11, P3, or O14
-define(PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
        ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD++[v3p11]).
-define(PATH_TO_P11_VIA_P3_NO_ACPTHEAD, ?PATH_TO_P3_NO_ACPTHEAD++[v3p11]).
-define(PATH_TO_P11_VIA_O14_NO_ACPTHEAD, ?PATH_TO_O14_NO_ACPTHEAD++[v3p11]).

%% O20 - The path to O20 via P11 via O14
-define(PATH_TO_O20_VIA_P11_VIA_O14_NO_ACPTHEAD,
        ?PATH_TO_P11_VIA_O14_NO_ACPTHEAD++[v3o20]).

%%
%% TEST SETUP AND CLEANUP
%%
core_tests() ->
    [fun service_unavailable/0,
     fun ping_invalid/0,
     fun ping_error/0,
     fun internal_server_error_o18/0,
     fun not_implemented_b12/0,
     fun not_implemented_b6/0,
     fun uri_too_long_b11/0,
     fun unsupported_media_type_b5/0,
     fun request_entity_too_large_b4/0,
     fun head_method_allowed/0,
     fun head_method_not_allowed/0,
     fun non_standard_method_501/0,
     fun non_standard_method_200/0,
     fun bad_request_b9/0,
     fun simple_get/0,
     fun not_acceptable_c4/0,
     fun not_acceptable_d5_c4/0,
     fun not_acceptable_d5_c3/0,
     fun not_acceptable_e6_d5_c3/0,
     fun not_acceptable_f7_e6_d5_c4/0,
     fun precond_fail_no_resource/0,
     fun precond_fail_g11/0,
     fun precond_fail_h12/0,
     fun precond_fail_j18/0,
     fun precond_fail_j18_via_k13/0,
     fun precond_fail_j18_via_h12/0,
     fun content_md5_valid_b9a/0,
     fun content_md5_valid_b9a_validated/0,
     fun content_md5_invalid_b9a/0,
     fun content_md5_custom_inval_b9a/0,
     fun authorized_b8/0,
     fun forbidden_b7/0,
     fun options_b3/0,
     fun variances_o18/0,
     fun variances_o18_2/0,
     fun ok_o18b/0,
     fun multiple_choices_o18/0,
     fun moved_permanently_i4/0,
     fun moved_permanently_k5/0,
     fun moved_temporarily_l5/0,
     fun not_modified_j18/0,
     fun not_modified_j18_via_k13/0,
     fun not_modified_j18_via_h12/0,
     fun not_modified_l17/0,
     fun see_other_n11/0,
     fun internal_server_error_n11/0,
     fun see_other_n11_resource_calls/0,
     fun see_other_n11_custom_base_uri/0,
     fun see_other_n11_wrq_base_uri/0,
     fun see_other_n5/0,
     fun not_found_l7/0,
     fun not_found_m7/0,
     fun created_p11_post/0,
     fun created_p11_put/0,
     fun conflict_p3/0,
     fun conflict_o14/0,
     fun gone_m5/0,
     fun gone_n5/0,
     fun accepted_m20/0,
     fun unsupported_media_type_accept_helper/0,
     fun created_p11_streamed/0,
     fun created_p11_accept_helper/0,
     fun writer_callback/0,
     fun head_length_access_for_cs/0,
     fun get_known_length_for_cs/0,
     fun get_for_range_capable_stream/0
     %% known_failure -- fun stream_content_md5/0
    ].

decision_core_test_() ->
    {foreach, local, fun setup/0, fun cleanup/1,
     [{spawn, Test} || Test <- core_tests()]}.

setup() ->
    try
        initialize_resource_settings(),
        DispatchList = [{[?RESOURCE, '*'], ?MODULE, []}],
        Ctx = wm_integration_test_util:start(?MODULE, "0.0.0.0", DispatchList),
        set_context(Ctx),
        MeckOpts = [passthrough, no_link, no_passthrough_cover],
        meck:new(webmachine_resource, MeckOpts),
        Ctx
    catch
        T:E ->
            io:format(user, "~n~p : ~p : ~p", [T, E, erlang:get_stacktrace()]),
            error(setup_failed)
    end.

cleanup(Ctx) ->
    meck:unload(webmachine_resource),
    wm_integration_test_util:stop(Ctx),
    clear_resource_settings().

get_decision_ids() ->
    History = meck:history(webmachine_resource),
    Result = [DecisionID || {_, {webmachine_resource, log_d, [DecisionID|_]}, _}
                                <- History, not is_substate(DecisionID)],
    Result.

%% Is the decision ID a sub-state? Sub-states are not on the HTTP/1.1 activity
%% diagram and exist as an implementation detail for control flow. The decision
%% traces can be simplified by ignoring these sub-states.
is_substate(DecisionID) ->
    ?assert(is_decision_id(DecisionID)),
    atom_matches(DecisionID, ?DECISION_ID_SUBSTATE_PATTERN).

is_decision_id(Atom) ->
    atom_matches(Atom, ?DECISION_ID_PATTERN).

%% Does the given atom match the regular expression?
atom_matches(Atom, RE) when is_atom(Atom) ->
    case re:run(atom_to_list(Atom), RE, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

%%
%% TEST CASES
%%
%% The decision trace is the path through the HTTP/1.1 activity diagram, which
%% has nodes labeled from A-P on the x-axis and 1-26 on the y-axis.
%%
%% The expected decision traces in these tests are hand verified.

%% 503 result via B13B (predicate: service_available)
service_unavailable() ->
    put_setting(service_available, false),
    {ok, Result} = httpc:request(head, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 503, "Service Unavailable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B13,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 503 result via B13 (at ping)
ping_invalid() ->
                                                % "breakout" for "anything other than pong"
    put_setting(ping, breakout),
    {ok, Result} = httpc:request(head, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 503, "Service Unavailable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B13,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 500 error response result via B13 (ping raises error)
ping_error() ->
    put_setting(ping, ping_raise_error),
    {ok, Result} = httpc:request(head, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 500, "Internal Server Error"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B13,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 500 error response via O18 from a callback raising an error
internal_server_error_o18() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain",
                                          size_stream_raises_error}]),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 500, "Internal Server Error"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% It seems that a callback that returns a callback is what's necessary to get
%% the top-level `catch` in webmachine_decision_core:handle_request to be
%% covered, hence the use of the range form of stream (other callbacks can also
%% cover the catch clause, but not without cluttering the test output with
%% badmatch errors from other modules)
size_stream_raises_error(ReqData, Context) ->
    Error = fun(_Start, _End) ->
                    error(foobar)
            end,
    {{stream, 1, Error}, ReqData, Context}.

%% 501 result via B12
not_implemented_b12() ->
    put_setting(allowed_methods, ?HTTP_1_0_METHODS),
    put_setting(known_methods, ?HTTP_1_0_METHODS),
    {ok, Result} = httpc:request(delete, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 501, "Not Implemented"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B12,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 501 result via B6
not_implemented_b6() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(valid_content_headers, false),
    {ok, Result} = httpc:request(get, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 501, "Not Implemented"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B6,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 414 result via B11
uri_too_long_b11() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(uri_too_long, true),
    TooLong = url("notreallythatlongactually"),
    {ok, Result} = httpc:request(get, {TooLong, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 414, "Request-URI Too Large"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B11,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 415 result via B5
unsupported_media_type_b5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(known_content_type, false),
    {ok, Result} = httpc:request(get, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 415, "Unsupported Media Type"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B5,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 413 result via B4
request_entity_too_large_b4() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(valid_entity_length, false),
    {ok, Result} = httpc:request(get, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 413, "Request Entity Too Large"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B4,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 from head method allowed
head_method_allowed() ->
    put_setting(allowed_methods, ['GET', 'HEAD']),
    {ok, Result} = httpc:request(head, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 405 from head method not allowed
head_method_not_allowed() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    {ok, Result} = httpc:request(head, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 405, "Method Not Allowed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B10,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 501 from non-standard method
non_standard_method_501() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    Ctx = get_context(),
    Port = wm_integration_test_util:get_port(Ctx),
    Url = wm_integration_test_util:url(Ctx, "foo"),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active,false}]),
    ok = gen_tcp:send(Sock, ["FOO ", Url, " HTTP/1.1\r\nConnection: close\r\n\r\n"]),
    ?assertMatch({ok, <<"HTTP/1.1 501 Not Implemented", _/binary>>},
                 gen_tcp:recv(Sock, 0, 2000)),
    ok = gen_tcp:close(Sock),
    ok.

%% 200 from non-standard method
non_standard_method_200() ->
    Method = "FOO",
    put_setting(known_methods, [Method|?HTTP_1_1_METHODS]),
    put_setting(allowed_methods, ['GET', 'POST', 'PUT', Method]),
    Ctx = get_context(),
    Port = wm_integration_test_util:get_port(Ctx),
    Url = wm_integration_test_util:url(Ctx, "foo"),
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active,false}]),
    ok = gen_tcp:send(Sock, [Method, " ", Url, " HTTP/1.1\r\nConnection: close\r\n\r\n"]),
    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n", _/binary>>},
                 gen_tcp:recv(Sock, 0, 2000)),
    ok = gen_tcp:close(Sock),
    ok.

%% 400 result via B9
bad_request_b9() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(malformed_request, true),
    {ok, Result} = httpc:request(get, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 400, "Bad Request"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B9,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 from a get
simple_get() ->
    put_setting(allowed_methods, ['GET']),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, ?HTML_CONTENT}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 via C4
not_acceptable_c4() ->
    put_setting(allowed_methods, ['GET']),
    Headers = [{"Accept", "video/mp4"}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
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
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
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
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_D5_VIA_C3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 result via E6 via D5 via C3
not_acceptable_e6_d5_c3() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(charsets_provided, [{"utf-8", fun identity/1}]),
    Headers = [{"Accept-Language", "en-US"},
               {"Accept-Charset", "ISO-8859-1"}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_E6_VIA_D5_C3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 406 result via F7 via E6 via D5 via C4
not_acceptable_f7_e6_d5_c4() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(content_types_provided, [{"text/plain", to_html}]),
    put_setting(language_available, true),
    put_setting(charsets_provided, [{"utf-8", fun identity/1}]),
    put_setting(encodings_provided, none),
    Headers = [{"Accept", "text/plain"},
               {"Accept-Language", "en-US"},
               {"Accept-Charset", "utf-8"},
               {"Accept-Encoding", "gzip"}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 406, "Not Acceptable"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_F7_VIA_E6_D5_C4,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via H7, no accept headers and no resource
precond_fail_no_resource() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(resource_exists, false),
    Headers = [{"If-Match", "*"}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_H7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via G11, no accept headers
precond_fail_g11() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(generate_etag, "v2"),
    Headers = [{"If-Match", "\"v0\", \"v1\""}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_G11_NO_ACPTHEAD,
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
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_H12_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via J18 via I13 via I12 via H10
precond_fail_j18() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    Headers = [{"If-None-Match", "*"}],
    PutRequest = {url(), Headers, "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via J18 via K13 via H11 via G11
precond_fail_j18_via_k13() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(generate_etag, "v1"),
    Headers = [{"If-Match", "\"v1\""},
               {"If-None-Match", "\"v1\""},
               {"If-Unmodified-Since", "{{INVALID DATE}}"}],
    PutRequest = {url(), Headers, "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACPTHEAD_2,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 412 result via J18 via I13 via I12 via H12
precond_fail_j18_via_h12() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    TenAM = "Wed, 20 Feb 2013 10:00:00 GMT",
    FivePM = "Wed, 20 Feb 2013 17:00:00 GMT",
    ResErlDate = httpd_util:convert_request_date(TenAM),
    put_setting(last_modified, ResErlDate),
    Headers = [{"If-Match", "*"},
               {"If-None-Match", "*"},
               {"If-Unmodified-Since", FivePM}],
    PutRequest = {url(), Headers, "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 412, "Precondition Failed"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACPTHEAD_3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 204 result, content-md5 header matches
content_md5_valid_b9a() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    Body = "foo",
    MD5Sum = base64:encode_to_string(md5(Body)),
    Headers = [{"Content-MD5", MD5Sum}],
    PutRequest = {url("new"), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O20_VIA_P11_VIA_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 204 result, content-md5 header matches, but checked by
%% validate_content_checksum instead of webmachine_decision_core itself.
content_md5_valid_b9a_validated() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(validate_content_checksum, true),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    Body = "foo",
    MD5Sum = base64:encode_to_string(md5(Body)),
    Headers = [{"Content-MD5", MD5Sum}],
    PutRequest = {url("new"), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O20_VIA_P11_VIA_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 400 result, content-md5 header does not match
content_md5_invalid_b9a() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentType = "text/plain",
    Body = "foo",
    InvalidMD5Sum = base64:encode_to_string("this is invalid for foo"),
    Headers = [{"Content-MD5", InvalidMD5Sum}],
    PutRequest = {url(), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 400, "Bad Request"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B9,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 400 result, resource's custom validate_content_checksum function rejects it
content_md5_custom_inval_b9a() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(validate_content_checksum, false),
    ContentType = "text/plain",
    Body = "foo",
    InvalidMD5Sum = base64:encode_to_string("this is invalid for foo"),
    Headers = [{"Content-MD5", InvalidMD5Sum}],
    PutRequest = {url(), Headers, ContentType, Body},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 400, "Bad Request"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B9,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 401 result via B8
authorized_b8() ->
    put_setting(is_authorized, "Basic"),
    put_setting(allowed_methods, ['GET']),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    {{Protocol, Code, Status}, Headers, _} = Result,
    ?assertEqual("HTTP/1.1", Protocol),
    ?assertEqual(401, Code),
    ?assertEqual("Unauthorized", Status),
    ?assertEqual({"www-authenticate", "Basic"}, lists:keyfind("www-authenticate", 1, Headers)),
    ExpectedDecisionTrace = ?PATH_TO_B8,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 403 result via B7
forbidden_b7() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(forbidden, true),
    {ok, Result} = httpc:request(get, {url("forbiddenfoo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 403, "Forbidden"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B7,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result, via OPTIONS
options_b3() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'PUT', 'OPTIONS']),
    {ok, Result} = httpc:request(options, {url(), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_B3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result with Vary
variances_o18() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    Charsets = [{"utf-8", fun identity/1},
                {"iso-8859-5", fun identity/1},
                {"unicode-1-1", fun identity/1}],
    put_setting(charsets_provided, Charsets),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result with other Vary
variances_o18_2() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentTypes = [{"text/html", to_html},
                    {"text/plain", to_html}],
    put_setting(content_types_provided, ContentTypes),
    Charsets = [{"utf-8", fun identity/1}],
    put_setting(charsets_provided, Charsets),
    put_setting(encodings_provided, use_identity_or_gzip),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result with body generation
ok_o18b() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(generate_etag, "v1"),
    put_setting(last_modified, ?FIRST_DAY_OF_LAST_YEAR),
    put_setting(expires, ?FIRST_DAY_OF_NEXT_YEAR),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 300 result via O18
multiple_choices_o18() ->
    put_setting(allowed_methods, ['GET']),
    put_setting(multiple_choices, true),
    Charsets = [{"utf-8", fun identity/1},
                {"iso-8859-5", fun identity/1},
                {"unicode-1-1", fun identity/1}],
    put_setting(charsets_provided, Charsets),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 300, "Multiple Choices"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 301 result via I4
moved_permanently_i4() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(moved_permanently, {true, url("new")}),
    PutRequest = {url("old"), [], "text/plain", "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 301, "Moved Permanently"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_I4_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 301 result via K5
moved_permanently_k5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    put_setting(moved_permanently, {true, url("new")}),
    %% We just want to get the 301 from httpc, we don't want it to actually
    %% try redirecting, so we turn off autoredirect
    HTTPOptions = [{autoredirect, false}],
    {ok, Result} = httpc:request(get, {url("old"), []}, HTTPOptions, []),
    ?assertMatch({{"HTTP/1.1", 301, "Moved Permanently"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_K5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 307 result via L5
moved_temporarily_l5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    put_setting(moved_temporarily, {true, url("new")}),
    %% We just want to get the 307 from httpc - similar to note about 301 above
    HTTPOptions = [{autoredirect, false}],
    {ok, Result}= httpc:request(get, {url("old"), []}, HTTPOptions, []),
    ?assertMatch({{"HTTP/1.1", 307, "Temporary Redirect"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_L5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 304 result via J18 via K13 via H11 via G11
not_modified_j18() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    Headers = [{"If-None-Match", "*"}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 304, "Not Modified"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 304 result via J18 via K13 via H11 via G11
not_modified_j18_via_k13() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(generate_etag, "v1"),
    Headers = [{"If-Match", "\"v1\""},
               {"If-None-Match", "\"v1\""},
               {"If-Unmodified-Since", "{{INVALID DATE}}"}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 304, "Not Modified"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACPTHEAD_2,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 304 result via J18 via I13 via I12 via H12
not_modified_j18_via_h12() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    TenAM = "Wed, 20 Feb 2013 10:00:00 GMT",
    FivePM = "Wed, 20 Feb 2013 17:00:00 GMT",
    ResErlDate = httpd_util:convert_request_date(TenAM),
    put_setting(last_modified, ResErlDate),
    Headers = [{"If-Match", "*"},
               {"If-None-Match", "*"},
               {"If-Unmodified-Since", FivePM}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 304, "Not Modified"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_J18_NO_ACPTHEAD_3,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 304 result via L17
not_modified_l17() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(last_modified, ?FIRST_DAY_OF_LAST_YEAR),
    put_setting(expires, ?FIRST_DAY_OF_NEXT_YEAR),
    RFC1123LastYear = httpd_util:rfc1123_date(?FIRST_DAY_OF_LAST_YEAR),
    Headers = [{"If-Modified-Since", RFC1123LastYear}],
    {ok, Result} = httpc:request(get, {url(), Headers}, [], []),
    ?assertMatch({{"HTTP/1.1", 304, "Not Modified"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_L17_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 303 result via N11 using request data rewriting
see_other_n11() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(process_post, {set_resp_redirect, ?RESOURCE_PATH ++ "/new1"}),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [{autoredirect,false}], []),
    ?assertMatch({{"HTTP/1.1", 303, "See Other"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 500 result via N11 - Setting do_redirect without a Location
internal_server_error_n11() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(process_post, {set_resp_redirect_but_not_location}),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 500, "Internal Server Error"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 303 result via N11 using the result of resource calls
see_other_n11_resource_calls() ->
    see_other_n11_resource_calls_base_uri(undefined).

%% 303 result via N11 using the result of resource calls and a custom base_uri
see_other_n11_custom_base_uri() ->
    BaseURIFun = {decision_core_test, base_uri_add_slash},
    see_other_n11_resource_calls_base_uri(BaseURIFun).

%% 303 result via N11 using the result of resource calls and a passthrough
%% base_uri
see_other_n11_wrq_base_uri() ->
    BaseURIFun = {wrq, base_uri},
    see_other_n11_resource_calls_base_uri(BaseURIFun).

%% helper function to remove common code
see_other_n11_resource_calls_base_uri(Value) ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    put_setting(post_is_create, true),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(create_path, {set_resp_redirect, ?RESOURCE_PATH ++ "/new1"}),
    put_setting(base_uri, Value),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [{autoredirect,false}], []),
    ?assertMatch({{"HTTP/1.1", 303, "See Other"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 303 result via N5
see_other_n5() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    put_setting(allow_missing_post, true),
    put_setting(process_post, {set_resp_redirect, ?RESOURCE_PATH ++ "/new1"}),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [{autoredirect,false}], []),
    ?assertMatch({{"HTTP/1.1", 303, "See Other"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N11_VIA_N5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 404 result via L7
not_found_l7() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    {ok, Result} = httpc:request(get, {url("nothere"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 404, "Object Not Found"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_L7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 404 result via M7
not_found_m7() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 404, "Object Not Found"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_M7_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 201 result via P11 from POST
created_p11_post() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    put_setting(process_post, {new_resource, ?RESOURCE_PATH ++ "/new1"}),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 201 result via P11 from PUT
created_p11_put() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(is_conflict, {new_location, url("new")}),
    PutRequest = {url("put"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_P3_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 409 result via P3 (must be a PUT)
conflict_p3() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(is_conflict, true),
    PutRequest = {url("put"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 409, "Conflict"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P3_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 409 result via O14
conflict_o14() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(is_conflict, true),
    PutRequest = {url("put"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 409, "Conflict"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 410 result via M5
gone_m5() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    {ok, Result} = httpc:request(get, {url("gone"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 410, "Gone"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_M5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 410 result via N5
gone_n5() ->
    put_setting(allowed_methods, ['GET', 'POST', 'PUT']),
    ContentType = "text/html",
    put_setting(content_types_accepted, [{ContentType, to_html}]),
    put_setting(resource_exists, false),
    put_setting(previously_existed, true),
    PostRequest = {url("post"), [], ContentType, "foo"},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 410, "Gone"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_N5_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 202 result via M20 - The delete has been "accepted" but it didn't actually
%% happen (or, rather, may or may not happen in the future)
accepted_m20() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS ++ ['DELETE']),
    put_setting(delete_resource, true),
    put_setting(delete_completed, false),
    DeleteRequest = {url("doomed"), []},
    {ok, Result} = httpc:request(delete, DeleteRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 202, "Accepted"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_M20_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 415 via accept_helper - This path is not explicit in the state diagram in
%% http-headers-status-v3.png, but is a path in the
%% webmachine_decision_core.erl logic. The accept_helper function itself can be
%% reached from P3 (as a PUT), O14 (as a PUT), or N11 (as a POST).
unsupported_media_type_accept_helper() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    HTMLContent = "text/html",
    PlainTextContent = "text/plain",
    put_setting(content_types_accepted, [{HTMLContent, to_html}]),
    put_setting(is_conflict, false),
    PutRequest = {url("put"), [], PlainTextContent, "foo"},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 415, "Unsupported Media Type"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O14_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 201 result via P11 with body streaming
created_p11_streamed() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'POST', 'PUT']),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    NewLocation = ?RESOURCE_PATH ++ "/posted",
    put_setting(process_post,
                {mfa, ?MODULE, process_post_for_created_p11, NewLocation}),
    ContentType = "text/plain",
    FooPrime = string:copies("foo", 128),
    PostRequest = {url("post"), [], ContentType, FooPrime},
    {ok, Result} = httpc:request(post, PostRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

process_post_for_created_p11(ReqData, Context, NewLocation) ->
    StreamBody = wrq:stream_req_body(ReqData, 3),
    Body = get_streamed_body(StreamBody, []),
    StreamedResponse = send_streamed_body(Body, 4),
    RDWithBody = wrq:set_resp_body({stream, StreamedResponse}, ReqData),
    Headers = [{"Location", NewLocation}],
    RDWithBodyAndLocation = wrq:set_resp_headers(Headers, RDWithBody),
    {true, RDWithBodyAndLocation, Context}.

%% The get_streamed_body and send_streamed_body functions here are derived from
%% the example in the Webmachine docs
get_streamed_body({Hunk, done}, Acc) ->
    List = lists:reverse([Hunk | Acc]),
    iolist_to_binary(List);
get_streamed_body({Hunk, Next}, Acc) ->
    get_streamed_body(Next(), [Hunk | Acc]).

send_streamed_body(Body, Max) ->
    HunkLen = 8 * Max,
    case Body of
        <<Hunk:HunkLen/bitstring, Rest/binary>> ->
            {Hunk, fun() -> send_streamed_body(Rest, Max) end};
        _ ->
            {Body, done}
    end.

%% 201 result via P11, exercising webmachine_decision_core:accept_helper code
created_p11_accept_helper() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(resource_exists, false),
    FourtyTwoMugs = string:copies("mug", 42),
    ContentType = "text/plain",
    put_setting(content_types_accepted, [{ContentType, accept_text}]),
    put_setting(is_conflict, {new_location, url("new")}),
    PutRequest = {url("put"), [], ContentType, FourtyTwoMugs},
    {ok, Result} = httpc:request(put, PutRequest, [], []),
    ?assertMatch({{"HTTP/1.1", 201, "Created"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_P3_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

accept_text(ReqData, Context) ->
    ReqBody = wrq:req_body(ReqData),
    Text = binary:bin_to_list(ReqBody),
    Reply = binary:list_to_bin("Recieved: " ++ Text ++ "."),
    RDWithBody = wrq:set_resp_body(Reply, ReqData),
    {true, RDWithBody, Context}.

%% 200 result from a GET using the "Write callable response method," which is
%% commented on this commit:
%% github.com/basho/webmachine/commit/96f5c5a679595e3554fc3e6af565faf5c6e37bbd
writer_callback() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", writer_response}]),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

writer_response(ReqData, Context) ->
    Body =
        fun(Write) ->
                Content = string:copies("mug", 42),
                Write(Content)
        end,
    {{writer, Body}, ReqData, Context}.

%% 200 result from a HEAD when the length is known, a special case for Riak CS
head_length_access_for_cs() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", known_length_body}]),
    {ok, Result} = httpc:request(head, {url("knownlength"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% 200 result from a GET when the length is known, a special case for Riak CS
get_known_length_for_cs() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", known_length_body}]),
    {ok, Result} = httpc:request(get, {url("knownlength"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

known_length_body(ReqData, Context) ->
    Content = "You have requested " ++ wrq:raw_path(ReqData) ++ ".",
    Size = string:len(Content),
    StreamBody = send_streamed_body(Content, 4),
    {{known_length_stream, Size, StreamBody}, ReqData, Context}.

%% 200 result from a GET exercising the range response form of returning bodies
get_for_range_capable_stream() ->
    put_setting(allowed_methods, ?DEFAULT_ALLOWED_METHODS),
    put_setting(content_types_provided, [{"text/plain", range_response}]),
    {ok, Result} = httpc:request(get, {url("foo"), []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, _}, Result),
    ExpectedDecisionTrace = ?PATH_TO_O18_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

%% Return a function that, when given a range, returns a StreamBody for the
%% content in that range
range_response(ReqData, Context) ->
    Content = string:copies("mug", 42),
    Size = string:len(Content),
    Fun = fun(Start, End) ->
                  Length = (End - Start) + 1,
                  Result = string:substr(Content, Start + 1, Length),
                  {Result, done}
          end,
    {{stream, Size, Fun}, ReqData, Context}.

%% 201 result via P11 from a POST with a streaming/chunked body and an MD5-sum
%%
%% This test exposes a bug with the code that checks a stream's MD5 sum
%% Known Failure
stream_content_md5() ->
    put_setting(allowed_methods, ['GET', 'HEAD', 'POST', 'PUT']),
    put_setting(validate_content_checksum,
                {mfa, ?MODULE, validate_checksum_for_md5stream, not_validated}),
    NewLocation = ?RESOURCE_PATH ++ "/posted",
    put_setting(process_post,
                {mfa, ?MODULE, process_post_for_md5_stream, NewLocation}),
    put_setting(resource_exists, false),
    put_setting(allow_missing_post, true),
    ContentType = "text/plain",
    Content = "foo",
    ValidMD5Sum = base64:encode_to_string(md5(Content)),
    ibrowse:start(),
    Url = url("post"),
    Headers = [{"Content-Type", ContentType},
               {"Content-MD5", ValidMD5Sum},
               {"Expect", "100-continue"}],
    BodyGenerator = fun(Step) ->
                            case Step of
                                0 -> {ok, Content, Step + 1};
                                _ -> eof
                            end
                    end,
    Body = {BodyGenerator, 0},
    Options = [{transfer_encoding, {chunked, 3}}],
    Result = ibrowse:send_req(Url, Headers, post, Body, Options),
    {ok, Status, _RespHeaders, _RespBody} = Result,
    ?assertEqual("201", Status),
    ExpectedDecisionTrace = ?PATH_TO_P11_VIA_N11_NO_ACPTHEAD,
    ?assertEqual(ExpectedDecisionTrace, get_decision_ids()),
    ok.

validate_checksum_for_md5stream(ReqData, Context, Result) ->
    _StreamBody = wrq:stream_req_body(ReqData, 5),
    {Result, ReqData, Context}.

process_post_for_md5_stream(ReqData, Context, NewLocation) ->
    Headers = [{"Location", NewLocation}],
    RDWithLocation = wrq:set_resp_headers(Headers, ReqData),
    %% ReqBody = wrq:stream_req_body(ReqData, 1024),
    %% Text = get_streamed_body(ReqBody, []),
    %% Text = wrq:req_body(ReqData),
    {true, RDWithLocation, Context}.

%%
%% WEBMACHINE RESOURCE FUNCTIONS AND CONFIGURATION
%%

initialize_resource_settings() ->
    %% Configure ETS table to hold resource settings for each test
    ets:new(?MODULE, [named_table, public]),

    %% Defaults
    put_setting(service_available, true),
    put_setting(ping, pong),
    put_setting(known_methods, ?HTTP_1_1_METHODS),
    put_setting(uri_too_long, false),
    put_setting(known_content_type, true),
    put_setting(valid_entity_length, true),
    put_setting(malformed_request, false),
    put_setting(forbidden, false),
    put_setting(valid_content_headers, true),
    put_setting(validate_content_checksum, not_validated),
    put_setting(is_authorized, true),
    put_setting(content_types_provided, [{"text/html", to_html}]),
    put_setting(language_available, true),
    put_setting(charsets_provided, no_charset),
    put_setting(encodings_provided, use_identity),
    put_setting(resource_exists, true),
    put_setting(generate_etag, undefined),
    put_setting(last_modified, undefined),
    put_setting(moved_permanently, false),
    put_setting(moved_temporarily, false),
    put_setting(previously_existed, false),
    put_setting(allow_missing_post, false),
    put_setting(post_is_create, false),
    put_setting(process_post, false),
    put_setting(create_path, undefined),
    put_setting(is_conflict, false),
    put_setting(multiple_choices, false),
    put_setting(base_uri, undefined),
    put_setting(expires, undefined),
    put_setting(delete_resource, false),
    put_setting(delete_completed, true),
    ok.

clear_resource_settings() ->
    ets:delete(?MODULE).

put_setting(SettingName, SettingValue) ->
    ets:insert(?MODULE, {SettingName, SettingValue}).

lookup_setting(Setting) ->
    [{Setting, Value}] = ets:lookup(?MODULE, Setting),
    Value.

set_context(Context) ->
    put_setting(context, Context).

get_context() ->
    lookup_setting(context).

url() ->
    Ctx = get_context(),
    wm_integration_test_util:url(Ctx).

url(Path) ->
    Ctx = get_context(),
    wm_integration_test_util:url(Ctx, Path).

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
    case Setting of
        %% general callback
        {mfa, Mod, Fun, Arg} ->
            erlang:apply(Mod, Fun, [ReqData, Context, Arg]);
        _ ->
            {Setting, ReqData, Context}
    end.

is_authorized(ReqData, Context) ->
    Setting = lookup_setting(is_authorized),
    {Setting, ReqData, Context}.

allowed_methods(ReqData, Context) ->
    Setting = lookup_setting(allowed_methods),
    {Setting, ReqData, Context}.

known_methods(ReqData, Context) ->
    Setting = lookup_setting(known_methods),
    {Setting, ReqData, Context}.

uri_too_long(ReqData, Context) ->
    Setting = lookup_setting(uri_too_long),
    {Setting, ReqData, Context}.

known_content_type(ReqData, Context) ->
    Setting = lookup_setting(known_content_type),
    {Setting, ReqData, Context}.

valid_entity_length(ReqData, Context) ->
    Setting = lookup_setting(valid_entity_length),
    {Setting, ReqData, Context}.

malformed_request(ReqData, Context) ->
    Setting = lookup_setting(malformed_request),
    {Setting, ReqData, Context}.

forbidden(ReqData, Context) ->
    Setting = lookup_setting(forbidden),
    {Setting, ReqData, Context}.

valid_content_headers(ReqData, Context) ->
    Setting = lookup_setting(valid_content_headers),
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
                [{"identity", fun identity/1}];
            use_identity_or_gzip ->
                [{"identity", fun identity/1},
                 {"gzip", fun(X) -> zlib:gzip(X) end}];
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

moved_permanently(ReqData, Context) ->
    Setting = lookup_setting(moved_permanently),
    {Setting, ReqData, Context}.

moved_temporarily(ReqData, Context) ->
    Setting = lookup_setting(moved_temporarily),
    {Setting, ReqData, Context}.

previously_existed(ReqData, Context) ->
    Setting = lookup_setting(previously_existed),
    {Setting, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    Setting = lookup_setting(allow_missing_post),
    {Setting, ReqData, Context}.

post_is_create(ReqData, Context) ->
    Setting = lookup_setting(post_is_create),
    {Setting, ReqData, Context}.

process_post(ReqData, Context) ->
    Setting = lookup_setting(process_post),
    case Setting of
        %% general callback
        {mfa, Mod, Fun, Arg} ->
            erlang:apply(Mod, Fun, [ReqData, Context, Arg]);
        %% new resource, with a redirect
        {set_resp_redirect, Location} ->
            RDRedirect = wrq:do_redirect(true, ReqData),
            Headers = [{"Location", Location}],
            RDWithLocation = wrq:set_resp_headers(Headers, RDRedirect),
            {true, RDWithLocation, Context};
        %% new resource with a redirect, but error case where the Location
        %% isn't set
        {set_resp_redirect_but_not_location} ->
            RDRedirect = wrq:do_redirect(true, ReqData),
            {true, RDRedirect, Context};
        %% new resource, no redirect (create instead)
        {new_resource, Location} ->
            Headers = [{"Location", Location}],
            RDWithLocation = wrq:set_resp_headers(Headers, ReqData),
            {true, RDWithLocation, Context};
        _ ->
            {Setting, ReqData, Context}
    end.

create_path(ReqData, Context) ->
    Setting = lookup_setting(create_path),
    case Setting of
        {set_resp_redirect, Location} ->
            %% Note, in this test we return the Location instead of setting the
            %% location in the ReqData's header
            RDRedirect = wrq:do_redirect(true, ReqData),
            {Location, RDRedirect, Context};
        _ ->
            {Setting, ReqData, Context}
    end.

is_conflict(ReqData, Context) ->
    Setting = lookup_setting(is_conflict),
    case Setting of
        {new_location, Location} ->
            Headers = [{"Location", Location}],
            RDWithLocation = wrq:set_resp_headers(Headers, ReqData),
            {false, RDWithLocation, Context};
        _ ->
            {Setting, ReqData, Context}
    end.

multiple_choices(ReqData, Context) ->
    Setting = lookup_setting(multiple_choices),
    {Setting, ReqData, Context}.

base_uri(ReqData, Context) ->
    Setting = lookup_setting(base_uri),
    case Setting of
        {Mod, Fun} ->
            {erlang:apply(Mod, Fun, [ReqData]), ReqData, Context};
        _ ->
            {Setting, ReqData, Context}
    end.

base_uri_add_slash(RD) ->
    wrq:base_uri(RD) ++ "/".

expires(ReqData, Context) ->
    Setting = lookup_setting(expires),
    {Setting, ReqData, Context}.

delete_resource(ReqData, Context) ->
    Setting = lookup_setting(delete_resource),
    {Setting, ReqData, Context}.

delete_completed(ReqData, Context) ->
    Setting = lookup_setting(delete_completed),
    {Setting, ReqData, Context}.

identity(X) ->
    X.

to_html(ReqData, Context) ->
    {?HTML_CONTENT, ReqData, Context}.

-endif.
