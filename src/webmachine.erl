%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2014 Basho Technologies
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

-module(webmachine).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([start/0, stop/0, new_request/2]).

-type headers() :: webmachine_headers:t().
-type response_body() :: iodata()
                       | {stream, StreamBody::any()}
                       | {known_length_stream, non_neg_integer(), StreamBody::any()}
                       | {stream, non_neg_integer(), fun()} %% TODO: type for fun()
                       | {writer, WrtieBody::any()}
                       | {file, IoDevice::any()}.


-export_type([headers/0, response_body/0]).

%% @spec start() -> ok
%% @doc Start the webmachine server.
start() ->
    webmachine_deps:ensure(),
    ok = ensure_started(crypto),
    ok = ensure_started(webmachine).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        {error, _} = E ->
            E
    end.

%% @spec stop() -> ok
%% @doc Stop the webmachine server.
stop() ->
    application:stop(webmachine).

new_request(mochiweb, MochiReq) ->
    webmachine_mochiweb:new_webmachine_req(MochiReq).

%%
%% TEST
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

start_mochiweb() ->
    webmachine_util:ensure_all_started(mochiweb).

start_stop_test() ->
    {Res, Apps} = start_mochiweb(),
    ?assertEqual(ok, Res),
    ?assertEqual(ok, webmachine:start()),
    ?assertEqual(ok, webmachine:stop()),
    [application:stop(App) || App <- Apps],
    ok.

-endif.
