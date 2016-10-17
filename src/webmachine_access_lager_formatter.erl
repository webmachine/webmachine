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

%% @doc Default access log handler for webmachine
-module(webmachine_access_lager_formatter).

%% Lager Formatter
-export([format/2, format/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec format(lager_msg:lager_msg(),list()) -> any().
format(Msg, Config) ->
    format(Msg, Config, []).

-spec format(lager_msg:lager_msg(),list(),list()) -> any().
format(Msg, Config, _) ->
    ReqTiming = lists:member(req_timing, Config),
    Metadata = lager_msg:metadata(Msg),
    format_req(Metadata, ReqTiming).

%% ===================================================================
%% Internal functions
%% ===================================================================

format_req(Metadata, RequestTiming) ->
    {_, Method} = lists:keyfind(wm_method, 1, Metadata),
    {_, Peer} = lists:keyfind(wm_peer, 1, Metadata),
    {_, Path} = lists:keyfind(wm_path, 1, Metadata),
    {_, Version} = lists:keyfind(wm_version, 1, Metadata),
    {_, ResponseCode} = lists:keyfind(wm_response_code, 1, Metadata),
    {_, ResponseLength} = lists:keyfind(wm_response_length, 1, Metadata),
    {_, Headers} = lists:keyfind(wm_headers, 1, Metadata),

    User = "-",
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
    Referer =
        case mochiweb_headers:get_value("Referer", Headers) of
            undefined -> "";
            R -> R
        end,
    UserAgent =
        case mochiweb_headers:get_value("User-Agent", Headers) of
            undefined -> "";
            U -> U
        end,
    case RequestTiming of
        true ->
            {_, StartTime} = lists:keyfind(wm_start_time, 1, Metadata),
            {_, EndTime} = lists:keyfind(wm_end_time, 1, Metadata),
            fmt_alog(Time, Peer, User, Method, Path, Version,
                     Status, Length, Referer, UserAgent, timer:now_diff(EndTime, StartTime));
        false ->
            fmt_alog(Time, Peer, User, Method, Path, Version,
                     Status, Length, Referer, UserAgent)
    end.

fmt_alog(Time, Ip, User, Method, Path, Version,
         Status,  Length, Referer, UserAgent) when is_atom(Method) ->
    fmt_alog(Time, Ip, User, atom_to_list(Method), Path, Version,
             Status, Length, Referer, UserAgent);
fmt_alog(Time, Ip, User, Method, Path, {VM,Vm},
         Status,  Length, Referer, UserAgent) ->
    [webmachine_log:fmt_ip(Ip), " - ", User, [$\s], Time, [$\s, $"], Method, " ", Path,
     " HTTP/", integer_to_list(VM), ".", integer_to_list(Vm), [$",$\s],
     Status, [$\s], Length, [$\s,$"], Referer,
     [$",$\s,$"], UserAgent, [$",$\n]].

fmt_alog(Time, Ip, User, Method, Path, Version,
         Status,  Length, Referer, UserAgent, ReqDuration) when is_atom(Method) ->
    fmt_alog(Time, Ip, User, atom_to_list(Method), Path, Version,
             Status, Length, Referer, UserAgent, ReqDuration);
fmt_alog(Time, Ip, User, Method, Path, {VM,Vm},
         Status,  Length, Referer, UserAgent, ReqDuration) ->
    [webmachine_log:fmt_ip(Ip), " - ", User, [$\s], Time, [$\s, $"], Method, " ", Path,
     " HTTP/", integer_to_list(VM), ".", integer_to_list(Vm), [$",$\s],
     Status, [$\s], Length, [$\s,$"], Referer,
     [$",$\s,$"], UserAgent, [$",$\s], integer_to_list(ReqDuration), [$\n]].


-ifdef(TEST).

non_standard_method_test() ->
    LogData = [
               {wm_method, "FOO"},
               {wm_headers, mochiweb_headers:make([])},
               {wm_peer, {127,0,0,1}},
               {wm_path, "/"},
               {wm_version, {1,1}},
               {wm_response_code, 501},
               {wm_response_length, 1234}
        ],
    LogEntry = format_req(LogData, false),
    ?assert(is_list(LogEntry)),
    ok.

-endif.
