%% Copyright (c) 2011-2012 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc A process that does a gen_event:add_sup_handler and attempts to re-add
%% event handlers when they exit.

%% @private

-module(webmachine_logger_watcher).

-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/3,
         start/3]).

-record(state, {module, config, event}).

start_link(Event, Module, Config) ->
    gen_server:start_link(?MODULE, [Event, Module, Config], []).

start(Event, Module, Config) ->
    gen_server:start(?MODULE, [Event, Module, Config], []).

init([Event, Module, Config]) ->
    install_handler(Event, Module, Config),
    {ok, #state{event=Event, module=Module, config=Config}}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({gen_event_EXIT, Module, normal}, #state{module=Module} = State) ->
    {stop, normal, State};
handle_info({gen_event_EXIT, Module, shutdown}, #state{module=Module} = State) ->
    {stop, normal, State};
handle_info({gen_event_EXIT, Module, _Reason}, #state{module=Module,
        config=Config, event=Event} = State) ->
    install_handler(Event, Module, Config),
    {noreply, State};
handle_info(reinstall_handler, #state{module=Module, config=Config, event=Event} = State) ->
    install_handler(Event, Module, Config),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal

install_handler(Event, Module, Config) ->
    case gen_event:add_sup_handler(Event, Module, Config) of
        ok ->
            ok;
        _Error ->
            erlang:send_after(5000, self(), reinstall_handler),
            ok
    end.
