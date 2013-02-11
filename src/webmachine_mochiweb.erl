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

%% @doc Mochiweb interface for webmachine.
-module(webmachine_mochiweb).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Steve Vinoski <vinoski@ieee.org>').
-export([start/1, stop/0, loop/1]).
-export([get_header_value/2,
         new_headers/0,
         make_headers/1,
         add_header/3,
         merge_header/3,
         headers_to_list/1,
         headers_from_list/1,
         socket_send/2,
         socket_recv/3,
         socket_setopts/2,
         urlsplit_path/1,
         parse_qs/1,
         parse_cookie/1,
         make_reqdata/1
        ]).

start(Options0) ->
    {PName, Options} = webmachine_ws:start(Options0, ?MODULE),
    Res = mochiweb_http:start([{name, PName}, {loop, fun loop/1} | Options]),
    {mochiweb, _, Version} = lists:keyfind(mochiweb, 1,
        proplists:get_value(loaded, application_controller:info())),
    application:set_env(webmachine, server_version, "MochiWeb/" ++ Version),
    Res.

stop() ->
    {registered_name, PName} = process_info(self(), registered_name),
    mochiweb_http:stop(PName).

loop(MochiReq) ->
    Req = webmachine:new_request(mochiweb, MochiReq),
    webmachine_ws:dispatch_request(Req).

get_header_value(HeaderName, Headers) ->
    mochiweb_headers:get_value(HeaderName, Headers).

new_headers() ->
    mochiweb_headers:empty().

make_headers(Headers) ->
    mochiweb_headers:make(Headers).

add_header(Header, Value, Headers) ->
    mochiweb_headers:enter(Header, Value, Headers).

merge_header(Header, Value, Headers) ->
    mochiweb_headers:insert(Header, Value, Headers).

headers_to_list(Headers) ->
    mochiweb_headers:to_list(Headers).

headers_from_list(Headers) ->
    mochiweb_headers:from_list(Headers).

socket_send(Socket, Data) ->
    mochiweb_socket:send(Socket, iolist_to_binary(Data)).

socket_recv(Socket, Length, Timeout) ->
    mochiweb_socket:recv(Socket, Length, Timeout).

socket_setopts(Socket, Options) ->
    mochiweb_socket:setopts(Socket, Options).

urlsplit_path(Path) ->
    mochiweb_util:urlsplit_path(Path).

parse_qs(QueryString) ->
    mochiweb_util:parse_qs(QueryString).

parse_cookie(Value) ->
    mochiweb_cookies:parse_cookie(Value).

make_reqdata(Path) ->
    %% Helper function to construct a request and return the ReqData
    %% object. Used only for testing.
    MochiReq = mochiweb_request:new(testing, 'GET', Path, {1, 1},
                                    mochiweb_headers:make([])),
    Req = webmachine:new_request(mochiweb, MochiReq),
    {RD, _} = Req:get_reqdata(),
    RD.
