%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
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

%% @doc Callbacks for the webmachine application.

-module(webmachine_app).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-behaviour(application).

-export([start/2,
         stop/1]).

-include("webmachine_logger.hrl").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webmachine.
start(_Type, _StartArgs) ->
    webmachine_deps:ensure(),
    {ok, _Pid} = SupLinkRes = webmachine_sup:start_link(),
    Handlers = case application:get_env(webmachine, log_handlers) of
        undefined ->
            [];
        {ok, Val} ->
            Val
    end,
    %% handlers failing to start are handled in the handler_watcher
    _ = [supervisor:start_child(webmachine_logger_watcher_sup,
                                [?EVENT_LOGGER, Module, Config]) ||
            {Module, Config} <- Handlers],
    SupLinkRes.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for webmachine.
stop(_State) ->
    ok.
