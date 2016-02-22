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

-module(webmachine_access_log_handler).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("webmachine_logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {hourstamp,
                filename,
                handle,
                request_timing=false :: boolean()}).

-define(FILENAME, "access.log").

%% ===================================================================
%% gen_event callbacks
%% ===================================================================

%% @private
init([BaseDir]) ->
    {ok,_} = webmachine_log:defer_refresh(?MODULE),
    FileName = filename:join(BaseDir, ?FILENAME),
    {Handle, DateHour} = webmachine_log:log_open(FileName),
    {ok, #state{filename=FileName, handle=Handle, hourstamp=DateHour}};
init([BaseDir, true]) ->
    {ok,_} = webmachine_log:defer_refresh(?MODULE),
    FileName = filename:join(BaseDir, ?FILENAME),
    {Handle, DateHour} = webmachine_log:log_open(FileName),
    {ok, #state{filename=FileName,
                handle=Handle,
                hourstamp=DateHour,
                request_timing=true}};
init([BaseDir, _]) ->
    init([BaseDir]).

%% @private
handle_call({_Label, MRef, get_modules}, State) ->
    {ok, {MRef, [?MODULE]}, State};
handle_call({refresh, Time}, State) ->
    {NewHour, NewHandle} = webmachine_log:maybe_rotate(?MODULE,
                                                       State#state.filename,
                                                       State#state.handle,
                                                       Time,
                                                       State#state.hourstamp),
    {ok, ok, State#state{hourstamp=NewHour, handle=NewHandle}};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log_access, LogData}, State) ->
    {NewHour, NewHandle} = webmachine_log:maybe_rotate(?MODULE,
                                                       State#state.filename,
                                                       State#state.handle,
                                                       os:timestamp(),
                                                       State#state.hourstamp),
    NewState = State#state{hourstamp=NewHour, handle=NewHandle},
    Msg = format_req(LogData, NewState#state.request_timing),
    _ = webmachine_log:log_write(NewState#state.handle, Msg),
    {ok, NewState};
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

format_req(#wm_log_data{method=Method,
                        headers=Headers,
                        peer=Peer,
                        path=Path,
                        version=Version,
                        response_code=ResponseCode,
                        response_length=ResponseLength,
                        start_time=StartTime,
                        end_time=EndTime},
          RequestTiming) ->
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
    XForwardedFor =
        case mochiweb_headers:get_value("X-Forwarded-For", Headers) of
            undefined -> "";
            X -> X
        end,    
case RequestTiming of
        true ->
            fmt_alog(Time, Peer, XForwardedFor, User, Method, Path, Version,
                     Status, Length, Referer, UserAgent, timer:now_diff(EndTime, StartTime));
        false ->
            fmt_alog(Time, Peer, XForwardedFor, User, Method, Path, Version,
                     Status, Length, Referer, UserAgent)
    end.

fmt_alog(Time, Ip, XForwardedFor, User, Method, Path, Version,
         Status,  Length, Referer, UserAgent) when is_atom(Method) ->
    fmt_alog(Time, Ip, XForwardedFor, User, atom_to_list(Method), Path, Version,
             Status, Length, Referer, UserAgent);
fmt_alog(Time, Ip, XForwardedFor, User, Method, Path, {VM,Vm},
         Status,  Length, Referer, UserAgent) ->
    [webmachine_log:fmt_ip(Ip), " - ", webmachine_log:fmt_ip(XForwardedFor), " - ", User, [$\s], Time, [$\s, $"], Method, " ", Path,
     " HTTP/", integer_to_list(VM), ".", integer_to_list(Vm), [$",$\s],
     Status, [$\s], Length, [$\s,$"], Referer,
     [$",$\s,$"], UserAgent, [$",$\n]].

fmt_alog(Time, Ip, XForwardedFor, User, Method, Path, Version,
         Status,  Length, Referer, UserAgent, ReqDuration) when is_atom(Method) ->
    fmt_alog(Time, Ip, XForwardedFor, User, atom_to_list(Method), Path, Version,
             Status, Length, Referer, UserAgent, ReqDuration);
fmt_alog(Time, Ip, XForwardedFor, User, Method, Path, {VM,Vm},
         Status,  Length, Referer, UserAgent, ReqDuration) ->
    [webmachine_log:fmt_ip(Ip), " - ", webmachine_log:fmt_ip(XForwardedFor), " - ", User, [$\s], Time, [$\s, $"], Method, " ", Path,
     " HTTP/", integer_to_list(VM), ".", integer_to_list(Vm), [$",$\s],
     Status, [$\s], Length, [$\s,$"], Referer,
     [$",$\s,$"], UserAgent, [$",$\s], integer_to_list(ReqDuration), [$\n]].


-ifdef(TEST).

non_standard_method_test() ->
    LogData = #wm_log_data{method="FOO",
                           headers=mochiweb_headers:make([]),
                           peer={127,0,0,1},
                           path="/",
                           version={1,1},
                           response_code=501,
                           response_length=1234},
    LogEntry = format_req(LogData, false),
    ?assert(is_list(LogEntry)),
    ok.

-endif.
