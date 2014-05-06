%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2010 Basho Technologies
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

-module(etag_test).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("webmachine.hrl").

-compile(export_all).

-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

unique(L) ->
    lists:reverse(lists:foldl(fun(Elem, Acc) ->
                                      case lists:member(Elem, Acc) of
                                          true ->
                                              Acc;
                                          false ->
                                              [Elem | Acc]
                                      end
                              end, [], L)).

etag(Bin) ->
    integer_to_list(erlang:crc32(Bin)).

etag_list([]) ->
    "*";
etag_list(Bins) ->
    string:join([[$", etag(B), $"] || B <- Bins], ",").

http_request(_Match, _IfVals, _NewVal, 0) ->
    error;
http_request(Match, IfVals, NewVal, Count) ->
    case httpc:request(put, {"http://localhost:12000/etagtest/foo",
                             [{Match, etag_list(IfVals)}],
                             "binary/octet-stream",
                             NewVal},
                       [], []) of
        {ok, Result} ->
            {ok, Result};
        {error, socket_closed_remotely} ->
            io:format(user, "Retry!\n", []),
            http_request(Match, IfVals, NewVal, Count-1)
    end.

etag_prop() ->
    ?LET({AllVals, Match}, {non_empty(list(binary())), oneof(["If-Match", "If-None-Match"])},
         ?FORALL({IfVals0, CurVal, NewVal},
                 {list(oneof(AllVals)), oneof(AllVals), oneof(AllVals)},
                 begin
                     ets:insert(?MODULE, [{etag, etag(CurVal)}]),
                     IfVals = unique(IfVals0),
                     {ok, Result} = http_request(Match, IfVals, NewVal, 3),
                     Code = element(2, element(1, Result)),
                     ExpectedCode =
                         expected_response_code(Match,
                                                IfVals,
                                                lists:member(CurVal, IfVals)),
                     equals(ExpectedCode, Code)
                 end)).

expected_response_code("If-Match", _, true) ->
    204;
expected_response_code("If-Match", [], false) ->
    204;
expected_response_code("If-Match", _, false) ->
    412;
expected_response_code("If-None-Match", _, true) ->
    412;
expected_response_code("If-None-Match", [], false) ->
    412;
expected_response_code("If-None-Match", _, false) ->
    204.

etag_test_() ->
    Time = 10,
    {spawn,
     [{setup,
       fun setup/0,
       fun cleanup/1,
       [
        {timeout, Time*3,
         ?_assert(eqc:quickcheck(eqc:testing_time(Time, ?QC_OUT(etag_prop()))))}
       ]}]}.

%% The EQC tests can periodically fail, however the counter examples it
%% produces are just coincidental. One reduction in particlar (the tuple of the
%% empty list and two empty binaries) is enough of a red herring that it's
%% included here as a sanity check.
etag_regressions_test_() ->
    CounterExample1 = [{[], <<>>, <<>>}],
    CounterExample2 = [{[<<25,113,71,254>>, <<25,113,71,254>>, <<"?">>],
                        <<"?">>, <<"r?}">>}],
    CounterExample3 = [{[<<19>>, <<70,6,56,181,38,128>>,
                         <<70,6,56,181,38,128>>, <<19>>, <<19>>, <<19>>,
                         <<70,6,56,181,38,128>>, <<19>>, <<19>>, <<19>>,
                         <<19>>, <<70,6,56,181,38,128>>], <<19>>,
                         <<70,6,56,181,38,128>>}],
    {spawn,
     [{setup, fun setup/0, fun cleanup/1,
       [{"counter example 1",
         ?_assert(eqc:check(?QC_OUT(etag_prop()), CounterExample1))},
        {"counter example 2",
         ?_assert(eqc:check(?QC_OUT(etag_prop()), CounterExample2))},
        {"counter example 3",
         ?_assert(eqc:check(?QC_OUT(etag_prop()), CounterExample3))}]}]}.

setup() ->
    error_logger:tty(false),
    %% Setup ETS table to hold current etag value
    ets:new(?MODULE, [named_table, public]),

    %% Spin up webmachine
    application:start(inets),
    WebConfig = [{ip, "0.0.0.0"}, {port, 12000},
                 {dispatch, [{["etagtest", '*'], ?MODULE, []}]}],
    {ok, Pid0} = webmachine_sup:start_link(),
    {ok, Pid1} = webmachine_mochiweb:start(WebConfig),
    link(Pid1),
    {Pid0, Pid1}.

cleanup({Pid0, Pid1}) ->
    %% clean up
    unlink(Pid0),
    exit(Pid0, normal),
    unlink(Pid1),
    exit(Pid1, kill),
    application:stop(inets).

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"binary/octet-stream", on_put}], ReqData, Context}.

on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

generate_etag(ReqData, Context) ->
    case ets:lookup(?MODULE, etag) of
        [] ->
            {undefined, ReqData, Context};
        [{etag, ETag}] ->
            {ETag, ReqData, Context}
    end.

-endif.
