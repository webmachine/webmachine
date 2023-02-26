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

%% @doc Default log handler for webmachine

-module(webmachine_error_log_handler).

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

-record(state, {hourstamp, filename, handle}).

-define(FILENAME, "wm_error.log").

%% ===================================================================
%% gen_event callbacks
%% ===================================================================

%% @private
init([BaseDir]) ->
    {ok,_} = webmachine_log:defer_refresh(?MODULE),
    FileName = filename:join(BaseDir, ?FILENAME),
    {Handle, DateHour} = webmachine_log:log_open(FileName),
    {ok, #state{filename=FileName, handle=Handle, hourstamp=DateHour}}.

%% @private
handle_call({_Label, MRef, get_modules}, State) ->
    {ok, {MRef, [?MODULE]}, State};
handle_call({refresh, Time}, State) ->
    {ok, ok, maybe_rotate(State, Time)};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log_error, Msg}, State) ->
    {ok, write_log(format_log(error, Msg), State)};
handle_event({log_error, Code, _Req, _Reason}, State) when Code < 500 ->
    {ok, State};
handle_event({log_error, Code, Req, Reason}, State) ->
    {ok, write_log(format_log(error, format_req(Code, Req, Reason)), State)};
handle_event({log_info, Msg}, State) ->
    {ok, write_log(format_log(info, Msg), State)};
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

maybe_rotate(State) ->
    maybe_rotate(State, os:timestamp()).

maybe_rotate(State, Time) ->
    {NewHour, NewHandle} = webmachine_log:maybe_rotate(?MODULE,
                                                       State#state.filename,
                                                       State#state.handle,
                                                       Time,
                                                       State#state.hourstamp),
    State#state{hourstamp=NewHour, handle=NewHandle}.

write_log(FormattedMsg, State) ->
    NewState = maybe_rotate(State),
    _ = webmachine_log:log_write(NewState#state.handle, FormattedMsg),
    NewState.

format_log(info, Msg) ->
    ["[info] ", Msg];
format_log(error, Msg) ->
    ["[error] ", Msg].

format_req(501, Req, _) ->
    {Path, _} = webmachine_request:path(Req),
    {Method, _} = webmachine_request:method(Req),
    Reason = "Webmachine does not support method ",
    [Reason, Method, ": path=", Path, $\n];
format_req(503, Req, _) ->
    {Path, _} = webmachine_request:path(Req),
    Reason = "Webmachine cannot fulfill the request at this time",
    [Reason, ": path=", Path, $\n];
format_req(500, Req, {stream_error, Reason}) ->
    {Path, _} = webmachine_request:path(Req),
    Str = io_lib:format("~p", [Reason]),
    ["Webmachine encountered an error while streaming the response."
     " path=", Path, $\n,
     "        ", Str, $\n];
format_req(_Code, Req, Reason) ->
    {Path, _} = webmachine_request:path(Req),
    Str = io_lib:format("~p", [Reason]),
    ["path=", Path, $\x20, Str, $\n].

-ifdef(TEST).

-include("wm_reqstate.hrl").

format_req_test() ->
    Headers = webmachine_headers:empty(),
    Wrq = wrq:create('GET', {1,1}, "/test/path", Headers),
    Req = #wm_reqstate{reqdata=Wrq},
    ?assertMatch("[error] path=/test/path {error,test}\n",
                 lists:flatten(
                   format_log(error, format_req(500, Req, {error, test})))).
-endif.
