%%   This is a template of a basic logger that could be used to log
%%   webmachine responses.
%%   The webmachine logger used is set as an environmental variable: 
%%   application:set_env(webmachine, webmachine_logger_module, logger)
%%
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

-module(logger).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([log_access/1).
-include("webmachine_logger.hrl").
-record(state, {}).

start_link(BaseDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([BaseDir]) ->
    {ok, #state{}}.

log_access(#wm_log_data{}=D) ->
    gen_server:cast(?MODULE, {log_access, D}).

handle_call(_Msg,_From,State) ->
    {noreply,State}.

handle_cast({log_access, _LogData}, State) ->
    %% Peform your logging here.
    {noreply, State}.

%% Don't remove! Clause need for Release Handler to work.
handle_info({_Label, {From, MRef}, get_modules}, State) ->
    From ! {MRef, [?MODULE]},
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
