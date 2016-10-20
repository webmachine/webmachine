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

-define(QUIP, "cafe not found").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for webmachine.
start(_Type, _StartArgs) ->
    webmachine_lager:start(),
    webmachine_deps:ensure(),

    %% Populate dynamic defaults on load:
    load_default_app_config(),

    {ok, _Pid} = SupLinkRes = webmachine_sup:start_link(),
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
    set_timezone(),
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

set_timezone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(Time),
    Zone = lists:flatten(zone((DiffSecs/3600)*100)),
    application:set_env(webmachine, timezone, Zone).

%% Ugly reformatting code to get times like +0000 and -1300
-spec zone(float()) -> string().
zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).
