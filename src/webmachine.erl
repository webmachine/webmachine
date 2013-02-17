%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2009 Basho Technologies
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
-export([start/0, stop/0]).
-export([new_request/2]).

-include("webmachine_logger.hrl").
-include("wm_reqstate.hrl").
-include("wm_reqdata.hrl").

%% @spec start() -> ok
%% @doc Start the webmachine server.
start() ->
    webmachine_deps:ensure(),
    application:start(crypto),
    application:start(webmachine).

%% @spec stop() -> ok
%% @doc Stop the webmachine server.
stop() ->
    application:stop(webmachine).

new_request(mochiweb, Request) ->
    Method = Request:get(method),
    Scheme = Request:get(scheme),
    Version = Request:get(version),
    {Headers, RawPath} = case application:get_env(webmachine, rewrite_module) of
        {ok, RewriteMod} ->
            do_rewrite(RewriteMod,
                       Method,
                       Scheme,
                       Version,
                       Request:get(headers),
                       Request:get(raw_path));
        undefined ->
            {Request:get(headers), Request:get(raw_path)}
    end,
    Socket = Request:get(socket),
    InitState = #wm_reqstate{socket=Socket,
                          reqdata=wrq:create(Method,Scheme,Version,RawPath,Headers)},

    InitReq = {webmachine_request,InitState},
    {Peer, ReqState} = InitReq:get_peer(),
    PeerState = ReqState#wm_reqstate{reqdata=wrq:set_peer(Peer,
                                              ReqState#wm_reqstate.reqdata)},
    LogData = #wm_log_data{start_time=now(),
                           method=Method,
                           headers=Headers,
                           peer=PeerState#wm_reqstate.peer,
                           path=RawPath,
                           version=Version,
                           response_code=404,
                           response_length=0},
    webmachine_request:new(PeerState#wm_reqstate{log_data=LogData}).

do_rewrite(RewriteMod, Method, Scheme, Version, Headers, RawPath) ->
    case RewriteMod:rewrite(Method, Scheme, Version, Headers, RawPath) of
        %% only raw path has been rewritten (older style rewriting)
        NewPath when is_list(NewPath) -> {Headers, NewPath};

        %% headers and raw path rewritten (new style rewriting)
        {NewHeaders, NewPath} -> {NewHeaders,NewPath}
    end.
