%% A batch of tests to verify that errors encountered during stream
%% production are handled correctly.
%%
%% These cases used to cause the stream to be re-evaluated and sent
%% twice, with an error appended after that. See
%% https://github.com/webmachine/webmachine/issues/60.
-module(stream_error_test).
-include("webmachine_logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([
         init/1,
         content_types_provided/2,
         provide_chunked_text/2,
         provide_unchunked_text/2,
         provide_writer_text/2
        ]).

%%% TESTS

stream_error_tests() ->
    [
     fun chunked_stream_stops/1,
     fun unchunked_stream_stops/1,
     fun writer_stops/1
    ].

%% General test for the "_stops" tests. This contacts a resource that
%% purposely hits an error in its stream-producing function.
stream_stops(Ctx, Type, ExpectBody, ExpectLength) ->
    WaitRef = test_log_handler:clear_logs(),
    Socket = open_stream_stop_request(Ctx, Type),
    Logs = test_log_handler:wait_for_logs(WaitRef, #{log_access => 1}),
    %% Logging happens after response sending, so if we
    %% wait until logging happens, we can probably just
    %% read without delay.
    Response = read_until_close(Socket),

    %% We expect a success response code
    ?assertMatch("HTTP/1.1 200 OK"++_, Response),

    %% We should only see the partial stream once
    ?assertEqual(string:find(Response, ExpectBody, leading),
                 string:find(Response, ExpectBody, trailing)),

    %% The body should have only one response (checking for one status
    %% line here), NOT the start of the stream plus an error
    ?assertEqual(string:find(Response, "HTTP/1.1", leading),
                 string:find(Response, "HTTP/1.1", trailing)),

    {log_access, #wm_log_data{response_code=Code,
                              response_length=ResponseLength,
                              notes=Notes}} =
        lists:keyfind(log_access, 1, Logs),

    %% the stream error should show up in an error note
    ?assertMatch({error, {stream_error, _}},
                 lists:keyfind(error, 1, Notes)),

    %% the response code should still be OK
    ?assertEqual(200, webmachine_status_code:status_code(Code)),

    %% number of bytes sent before error should be correct
    ?assertMatch(ExpectLength, ResponseLength).

chunked_stream_stops(Ctx) ->
    %% Chunked encoding only counts bytes of data, not length and line
    %% endings.
    Expectlength = 5,
    stream_stops(Ctx, "text/chunked",
                 string:copies("\r\n1\r\nx", 5), Expectlength).

unchunked_stream_stops(Ctx) ->
    %% Unchunked streaming reports all bytes sent, even if stopped in
    %% the middle.
    ExpectLength = 10,
    stream_stops(Ctx, "text/unchunked",
                 string:copies("x", 5), ExpectLength).

writer_stops(Ctx) ->
    %% Writer reports only data bytes, not chunk length and line
    %% endings.
    ExpectLength = 5,
    stream_stops(Ctx, "text/writer", "\r\n5\r\nxxxxx", ExpectLength).

%%% SUPPORT/UTIL

open_stream_stop_request(Ctx, ContentType) ->
    Request = ["GET ", "/", atom_to_list(?MODULE), " HTTP/1.1\r\n",
               "Accept: ", ContentType, "\r\n\r\n"],
    {ok, Sock} = gen_tcp:connect("localhost",
                                 wm_integration_test_util:get_port(Ctx),
                                 [list, {active, false}]),
    ok = gen_tcp:send(Sock, iolist_to_binary(Request)),
    Sock.

read_until_close(Sock) ->
    read_until_close(Sock, gen_tcp:recv(Sock, 0, 100), []).

read_until_close(Sock, {ok, Data}, Acc) ->
    read_until_close(Sock, gen_tcp:recv(Sock, 0, 100), [Data|Acc]);
read_until_close(Sock, {error, _}, Acc) ->
    gen_tcp:close(Sock),
    lists:flatten(lists:reverse(Acc)).

%%% REQUEST MODULE

init([]) ->
    {ok, undefined}.

%% Using content type to pick the kind of stream the resource will use.
content_types_provided(RD, Ctx) ->
    {[{"text/chunked", provide_chunked_text},
      {"text/unchunked", provide_unchunked_text},
      {"text/writer", provide_writer_text}],
     RD, Ctx}.

provide_chunked_text(RD, Ctx) ->
    {{stream, {<<"x">>, fun() -> stream(4) end}}, RD, Ctx}.

provide_unchunked_text(RD, Ctx) ->
    %% the 10 is a lie
    {{known_length_stream, 10, {<<"x">>, fun() -> stream(4) end}}, RD, Ctx}.

stream(0) ->
    this_call:is_not_defined(should_cause_error);
stream(N) ->
    {<<"x">>, fun() -> stream(N-1) end}.

provide_writer_text(RD, Ctx) ->
    {{writer, fun writer/1}, RD, Ctx}.

writer(Sender) ->
    Sender(<<"xxxxx">>),
    this_call:is_not_defined(should_cause_error).

%%% TEST SETUP

stream_error_test_() ->
    {foreach,
     %% Setup
     fun() ->
             DL = [{[atom_to_list(?MODULE), '*'], ?MODULE, []}],
             Ctx = wm_integration_test_util:start(?MODULE, "0.0.0.0", DL),
             webmachine_log:add_handler(test_log_handler, []),
             Ctx
     end,
     %% Cleanup
     fun(Ctx) ->
             wm_integration_test_util:stop(Ctx)
     end,
     %% Test functions provided with context from setup
     [fun(Ctx) ->
              {spawn, {with, Ctx, stream_error_tests()}}
      end]}.

-endif.
