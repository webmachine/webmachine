%% Regression test for maybe_flush_req_body/1's cross-process fallback.
%%
%% Some downstream apps dispatch the eager body read
%% (wrq:req_body/1) to a worker process. wrq:req_body/1 stashes an
%% updated ReqState via `put(tmp_reqstate, NewReqState)` in the
%% CALLER's process dictionary, and the underlying
%% recv_stream_body/2 records
%% `put(webmachine_stream_progress, done)` there too. When the caller
%% is a worker (not the mochiweb loop process), the loop's pdict is
%% left with neither flag, so maybe_flush_req_body/1's
%% `get(webmachine_stream_progress)` returns `undefined` and — on the
%% pre-fix code path — falls into a drain that recv's on an already-
%% empty socket with ?IDLE_TIMEOUT = infinity, wedging the request.
%%
%% multi_request_connection_test masks this by pipelining a second
%% request on the same socket: the drain finds the follow-up bytes
%% already on the wire, tracks them against the first request's
%% content-length, and returns quickly. Real clients (curl, most HTTP
%% libraries) send one request and wait, hitting the hang.
%%
%% The fix adds a cross-process-safe guard: even when the loop's
%% pdict is empty, `Req#wm_reqstate.bodyfetch` travels with the
%% ReqState by value across process boundaries, and matching it
%% against `standard`/`stream` short-circuits the drain.
%%
%% This test simulates the downstream pattern: worker spawns, reads
%% the body, and returns its `tmp_reqstate` to the resource callback,
%% which stores it in the loop's pdict — same as fe_api_base_resource
%% does. Then a single-request-per-socket POST with a bounded recv
%% timeout distinguishes "responded fast" (post-fix) from "hanging"
%% (pre-fix).
-module(flush_req_body_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-export([
         init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         to_text/2,
         process_post/2
        ]).

%% Bounded per-response wait. Without the fix the drain calls
%% recv(..., ?IDLE_TIMEOUT = infinity) on an empty socket, so 3
%% seconds is plenty to distinguish "responded fast" from "hanging".
-define(RECV_TIMEOUT_MS, 3000).

%%% TESTS

flush_req_body_tests() ->
    [
     fun cross_process_eager_read_test/1
    ].

cross_process_eager_read_test(Ctx) ->
    Resp = send_single_request(
             Ctx,
             "POST", "worker",
             [{"content-type", "text/plain"}],
             "flush me from a worker process"),
    ?assertMatch({ok, "HTTP/1.1 204" ++ _, _, _}, Resp),
    ok.

%%% RESOURCE

init([]) ->
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.

%% Required by the decision core even for POST -> 204: the decision
%% graph inspects the provided types on the way to picking a response
%% renderer. Omitting this makes the resource crash to 500.
content_types_provided(RD, Ctx) ->
    {[{"text/plain", to_text}], RD, Ctx}.

to_text(RD, Ctx) ->
    {"", RD, Ctx}.

%% For POST, webmachine calls process_post/2, so content_types_accepted
%% just needs a callable to satisfy negotiation.
content_types_accepted(RD, Ctx) ->
    {[{"text/plain", process_post}], RD, Ctx}.

%% Dispatch the eager read to a worker, then propagate the worker's
%% tmp_reqstate back into the loop process's pdict so decision_core
%% threads the updated ReqState (with bodyfetch = standard) into Req
%% before maybe_flush_req_body/1 runs.
process_post(RD, Ctx) ->
    Parent = self(),
    {WorkerPid, WorkerMon} = spawn_monitor(
        fun() ->
            _Body = wrq:req_body(RD),
            %% wrq:req_body/1 sets tmp_reqstate in the caller's pdict.
            %% Send it back to the parent so the parent can put it into
            %% its own pdict; that's how the updated ReqState makes the
            %% cross-process trip.
            Parent ! {self(), tmp_reqstate, get(tmp_reqstate)},
            ok
        end),
    receive
        {WorkerPid, tmp_reqstate, WorkerReqState} ->
            erlang:demonitor(WorkerMon, [flush]),
            put(tmp_reqstate, WorkerReqState);
        {'DOWN', WorkerMon, process, WorkerPid, Reason} ->
            erlang:error({worker_died, Reason})
    after 5000 ->
        erlang:error(worker_timeout)
    end,
    {true, RD, Ctx}.

%%% TEST SETUP

flush_req_body_test_() ->
    {foreach,
     fun() ->
             DL = [{[atom_to_list(?MODULE), '*'], ?MODULE, []}],
             wm_integration_test_util:start(?MODULE, "0.0.0.0", DL)
     end,
     fun(Ctx) ->
             wm_integration_test_util:stop(Ctx)
     end,
     [fun(Ctx) ->
              {spawn, {with, Ctx, flush_req_body_tests()}}
      end]}.

%%% HTTP HELPERS (single-request-per-socket)

%% NOTE: no `Connection: close`. webmachine short-circuits the
%% flush-and-idle path on close-marked connections, so a close header
%% would let a broken build "pass" this test.
send_single_request(Ctx, Method, Path, Headers, Body) ->
    Request = build_request(Method, Path, Headers, Body),
    {ok, Sock} = gen_tcp:connect(
                   "localhost",
                   wm_integration_test_util:get_port(Ctx),
                   [list, {active, false}]),
    try
        ok = gen_tcp:send(Sock, iolist_to_binary(Request)),
        receive_response([], Sock)
    after
        gen_tcp:close(Sock)
    end.

build_request(Method, Path, Headers, Body) ->
    ContentLength = integer_to_list(length(Body)),
    ExtraHeaders = case Body of
                       [] -> Headers;
                       _  -> [{"content-length", ContentLength} | Headers]
                   end,
    [Method, " /", atom_to_list(?MODULE), "/", Path, " HTTP/1.1\r\n",
     "Host: localhost\r\n",
     [ [K, ": ", V, "\r\n"] || {K, V} <- ExtraHeaders ],
     "\r\n",
     Body].

receive_response(Buffer, Sock) ->
    case string:split(Buffer, "\r\n\r\n") of
        [Head, RestAfterHead] ->
            [Code | RawHeaders] = string:tokens(Head, "\r\n"),
            Hdrs = [list_to_tuple(string:tokens(H, ": ")) || H <- RawHeaders],
            BodyLen = case lists:keyfind("Content-Length", 1, Hdrs) of
                          {_, LStr} -> list_to_integer(LStr);
                          false     -> 0
                      end,
            BodyBytes = lists:flatten(RestAfterHead),
            Body = case length(BodyBytes) >= BodyLen of
                       true  -> lists:sublist(BodyBytes, BodyLen);
                       false -> BodyBytes ++ recv_body(Sock,
                                                       BodyLen -
                                                       length(BodyBytes))
                   end,
            {ok, Code, Hdrs, Body};
        _IncompleteHead ->
            case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT_MS) of
                {ok, Data} ->
                    receive_response(Buffer ++ Data, Sock);
                {error, _} = Error ->
                    Error
            end
    end.

recv_body(_Sock, 0) ->
    [];
recv_body(Sock, Remaining) when Remaining > 0 ->
    case gen_tcp:recv(Sock, Remaining, ?RECV_TIMEOUT_MS) of
        {ok, Data} -> Data ++ recv_body(Sock, Remaining - length(Data));
        {error, _} -> []
    end.

-endif.
