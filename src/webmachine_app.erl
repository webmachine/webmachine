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

-define(QUIP, "cafe not found").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webmachine.
start(_Type, _StartArgs) ->
    webmachine_deps:ensure(),

    %% Populate dynamic defaults on load:
    load_default_app_config(),

    {ok, _Pid} = SupLinkRes = webmachine_sup:start_link(),
    Handlers = application:get_env(webmachine, log_handlers, []),

    %% handlers failing to start are handled in the handler_watcher
    _ = [supervisor:start_child(webmachine_logger_watcher_sup,
                                [?EVENT_LOGGER, Module, Config]) ||
            {Module, Config} <- Handlers],
    SupLinkRes.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for webmachine.
stop(_State) ->
    ok.

-spec load_default_app_config() -> ok.
load_default_app_config() ->

    case application:get_env(webmachine, server_name, undefined) of
        Name when is_list(Name) ->
            ok;
        _ ->
            set_default_server_name()
    end,
    ok.


set_default_server_name() ->
    {mochiweb, _, MochiVersion} =
        lists:keyfind(mochiweb, 1, application:loaded_applications()),

    {webmachine, _, WMVersion} =
        lists:keyfind(webmachine, 1, application:loaded_applications()),
    ServerName =
        lists:flatten(
          io_lib:format(
            "MochiWeb/~s WebMachine/~s (~s)",
            [MochiVersion, WMVersion, ?QUIP])),
    application:set_env(
      webmachine, server_name, ServerName),
    ok.
