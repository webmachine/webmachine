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

-include("wm_reqdata.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

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
                  case {Match, lists:member(CurVal, IfVals)} of
                      {"If-Match", true} ->
                          ?assertEqual(204, Code);
                      {"If-Match", false} ->
                          ?assertEqual(412, Code);
                      {"If-None-Match", true} ->
                          ?assertEqual(412, Code);
                      {"If-None-Match", false} ->
                          ?assertEqual(204, Code)
                  end,
                  true
                 end)).


etag_test() ->
    %% Setup ETS table to hold current etag value
    ets:new(?MODULE, [named_table, public]),

    %% Spin up webmachine
    WebConfig = [{ip, "0.0.0.0"}, {port, 12000},
                 {dispatch, [{["etagtest", '*'], ?MODULE, []}]}],
    {ok, Pid} = webmachine_mochiweb:start(WebConfig),
    link(Pid),

    ?assert(eqc:quickcheck(eqc:numtests(250, ?QC_OUT(etag_prop())))).


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

ping(ReqData, State) ->
    {pong, ReqData, State}.

-endif.
