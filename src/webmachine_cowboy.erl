%% @copyright 2012 Basho Technologies
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

%% @doc Cowboy interface for webmachine.
-module(webmachine_cowboy).

-ifdef(WEBMACHINE_COWBOY).
-export([start/1, stop/0]).
-export([init/3, handle/2, terminate/2]).
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

-include_lib("cowboy/include/http.hrl").

start(Options0) ->
    {_PName, Options} = webmachine_ws:start(Options0, ?MODULE),
    application:start(cowboy),
    Conf = convert_options(Options, []),
    Dispatch = [
            %% {Host, list({Path, Handler, Opts})}
            {'_', [{'_', ?MODULE, []}]}
        ],
    cowboy:start_listener('webmachine-cowboy', 100,
        cowboy_tcp_transport, Conf,
        cowboy_http_protocol, [{dispatch, Dispatch}]).

stop() ->
    cowboy:stop_listener('webmachine-cowboy').

init({tcp, http}, Req, _Opts) ->
    {ok, Req, {}}.

handle(CowboyReq, State) ->
    Req = webmachine:new_request(cowboy, CowboyReq),
    webmachine_ws:dispatch_request(Req),
    {ok, CowboyReq, State}.

terminate(_Req, _State) ->
    ok.

get_header_value(HeaderName, Headers) ->
    %io:format("get header ~p ~p~n", [HeaderName, Headers]),
    Res = lists:foldl(fun({H, V}, undefined) ->
                H1 = if
                    is_atom(H) -> string:to_lower(atom_to_list(H));
                    is_binary(H) -> string:to_lower(binary_to_list(H));
                    is_list(H) -> string:to_lower(H)
                end,
                case H1 == HeaderName of
                    true ->
                        case is_binary(V) of
                            true ->
                                binary_to_list(V);
                            false ->
                                V
                        end;
                    false ->
                        undefined
                end;
            (_, Acc) ->
                Acc
        end, undefined, Headers),
    %io:format("result is ~p~n", [Res]),
    Res.

new_headers() ->
    [].

make_headers(Headers) ->
    Headers.

add_header(Header, Value, Headers) ->
    [{Header, Value} | Headers].

merge_header(Header, Value, Headers) ->
    case lists:keytake(Header, 1, Headers) of
        {value, {Header, OldValue}, Rest} ->
            [{Header, OldValue ++ ", " ++ Value} | Rest];
        false ->
            [{Header, Value} | Headers]
    end.

headers_to_list(Headers) ->
    Headers.

headers_from_list(Headers) ->
    Headers.

socket_send(S, Data) ->
    {Transport, Socket} = get_transport(S),
    Transport:send(Socket, Data).

socket_recv(S, Length, Timeout) ->
    {Transport, Socket} = get_transport(S),
    Transport:recv(Socket, Length, Timeout).

socket_setopts(S, Options) ->
    {Transport, Socket} = get_transport(S),
    Transport:setopts(Socket, Options).

urlsplit_path(Path) ->
    URLDecode = fun(Bin) -> cowboy_http:urldecode(Bin, crash) end,
    {_, P, QueryString} = cowboy_dispatcher:split_path(list_to_binary(Path), URLDecode),
    case binary:split(QueryString, <<"#">>) of
        [QS, Fragment] ->
            {binary_to_list(P), binary_to_list(QS), binary_to_list(Fragment)};
        [QS] ->
            {binary_to_list(P), binary_to_list(QS), []}
    end.

parse_qs(QueryString) ->
    %% is there any point doing this ourselves?
    mochiweb_util:parse_qs(QueryString).

parse_cookie(Value) ->
    %% is there any point doing this ourselves?
    mochiweb_cookies:parse_cookie(Value).

make_reqdata(Path) ->
    %% Helper function to construct a request and return the ReqData
    %% object. Used only for testing.
    CowboyReq = #http_req{socket=testing, transport=cowboy_tcp_transport, method='GET', raw_path=list_to_binary(Path), version={1,1}, headers=[]},
    Req = webmachine:new_request(cowboy, CowboyReq),
    {RD, _} = Req:get_reqdata(),
    RD.

%% internal functions
convert_options([], Conf) ->
    Conf;
convert_options([{ip, IpAddr0}|Opts], Conf) ->
    {ok, IpAddr} = inet_parse:address(IpAddr0),
    convert_options(Opts, [{ip, IpAddr}|Conf]);
convert_options([{port, Port}|Opts], Conf) ->
    convert_options(Opts, [{port, Port}|Conf]);
convert_options([_|Opts], Conf) ->
    convert_options(Opts, Conf).

get_transport({ssl, SSLSocket}) ->
    {cowboy_ssl_transport, SSLSocket};
get_transport(Socket) ->
    {cowboy_tcp_transport, Socket}.

-endif. %% WEBMACHINE_COWBOY
