%% Copyright (c) 2011-2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Default performance log handler for webmachine
-module(webmachine_perf_lager_formatter).

%% Lager Formatter
-export([format/2, format/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec format(lager_msg:lager_msg(),list()) -> any().
format(Msg, Config) ->
    format(Msg, Config, []).

-spec format(lager_msg:lager_msg(),list(),list()) -> any().
format(Msg,_, _) ->
    Metadata = lager_msg:metadata(Msg),
    format_req(Metadata).

%% ===================================================================
%% Internal functions
%% ===================================================================

format_req(Metadata) ->
    {_, Mod} = lists:keyfind(wm_resource_module, 1, Metadata),
    {_, StartTime} = lists:keyfind(wm_start_time, 1, Metadata),
    {_, Method} = lists:keyfind(wm_method, 1, Metadata),
    {_, Peer} = lists:keyfind(wm_peer, 1, Metadata),
    {_, Path} = lists:keyfind(wm_path, 1, Metadata),
    {_, Version} = lists:keyfind(wm_version, 1, Metadata),
    {_, ResponseCode} = lists:keyfind(wm_response_code, 1, Metadata),
    {_, ResponseLength} = lists:keyfind(wm_response_length, 1, Metadata),
    {_, EndTime} = lists:keyfind(wm_end_time, 1, Metadata),
    {_, FinishTime} = lists:keyfind(wm_finish_time, 1, Metadata),

    Time = webmachine_log:fmtnow(),
    Status = case ResponseCode of
                 {Code, _ReasonPhrase} when is_integer(Code)  ->
                     integer_to_list(Code);
                 _ when is_integer(ResponseCode) ->
                     integer_to_list(ResponseCode);
                 _ ->
                     ResponseCode
             end,
    Length = integer_to_list(ResponseLength),
    TTPD = webmachine_util:now_diff_milliseconds(EndTime, StartTime),
    TTPS = webmachine_util:now_diff_milliseconds(FinishTime, EndTime),
    fmt_plog(Time, Peer, Method, Path, Version,
             Status, Length, atom_to_list(Mod), integer_to_list(TTPD),
             integer_to_list(TTPS)).

fmt_plog(Time, Ip,  Method0, Path, {VM,Vm}, Status, Length, Mod, TTPD, TTPS)
  when is_atom(Method0) ->
    Method = atom_to_list(Method0),
    fmt_plog(Time, Ip,  Method, Path, {VM,Vm}, Status, Length, Mod, TTPD, TTPS);
fmt_plog(Time, Ip,  Method, Path, {VM,Vm}, Status, Length, Mod, TTPD, TTPS) ->
    [webmachine_log:fmt_ip(Ip), " - ", [$\s], Time, [$\s, $"], Method, " ", Path,
     " HTTP/", integer_to_list(VM), ".", integer_to_list(Vm), [$",$\s],
     Status, [$\s], Length, " " , Mod, " ", TTPD, " ", TTPS, $\n].


-ifdef(TEST).

non_standard_method_test() ->
    LogData = [
               {wm_resource_module, foo},
               {wm_start_time, os:timestamp()},
               {wm_method, "FOO"},
               {wm_peer, {127,0,0,1}},
               {wm_path, "/"},
               {wm_version, {1,1}},
               {wm_response_code, 501},
               {wm_response_length, 1234},
               {wm_end_time, os:timestamp()},
               {wm_finish_time, os:timestamp()}
        ],
    LogEntry = format_req(LogData),
    ?assert(is_list(LogEntry)),
    ok.

-endif.
