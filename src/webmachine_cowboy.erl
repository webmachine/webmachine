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
         make_reqdata/1
        ]).

-include_lib("cowboy/include/http.hrl").

start(Options0) ->
    {_PName, Options} = webmachine_ws:start(Options0, ?MODULE),
    application:start(cowboy),
    {cowboy, _, Version} = lists:keyfind(cowboy, 1,
        proplists:get_value(loaded, application_controller:info())),
    application:set_env(webmachine, server_version, "Cowboy/" ++ Version),
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
    put(request, Req),
    {ok, Req, {}}.

handle(CowboyReq, State) ->
    Req = webmachine:new_request(cowboy, CowboyReq),
    webmachine_ws:dispatch_request(Req),
    {ok, CowboyReq, State}.

terminate(_Req, _State) ->
    ok.

get_header_value(RawHeaderName, Headers) ->
    HeaderName = string:to_lower(RawHeaderName),
    Res = lists:foldl(fun({H, V}, undefined) ->
                case normalize(H) == HeaderName of
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
            case normalize(Header) of
                "set-cookie" ->
                    %% set-cookie headers are space delineated
                    [{Header, OldValue}, {Header, Value} | Rest];
                _ ->
                    [{Header, OldValue ++ ", " ++ Value} | Rest]
            end;
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
    Req = get(request),
    %% cowboy automagically reads the body into the request, so we have to
    %% buffer it by hand
    case cowboy_http_req:stream_body(Req) of
        {ok, Data0, Req2} ->
            put(request, Req2),
            Data = case get(body_buffer) of
                undefined ->
                    Data0;
                Buff ->
                    iolist_to_binary([Buff, Data0])
            end,
            case Length =< byte_size(Data) of
                true ->
                    Res = binary:part(Data, {0, Length}),
                    Rem = binary:part(Data, {Length, byte_size(Data) - Length}),
                    put(body_buffer, Rem),
                    {ok, Res};
                false ->
                    put(body_buffer, Data),
                    socket_recv(S, Length, Timeout)
            end;
        {done, Req} ->
            Data = case get(body_buffer) of
                undefined ->
                    <<>>;
                Buff ->
                    Buff
            end,
            case Length =< byte_size(Data) of
                true ->
                    Res = binary:part(Data, {0, Length}),
                    Rem = binary:part(Data, {Length, byte_size(Data) - Length}),
                    put(body_buffer, Rem),
                    {ok, Res};
                false ->
                    {error, no_more_bytes}
            end
    end.

socket_setopts(S, Options) ->
    {Transport, Socket} = get_transport(S),
    Transport:setopts(Socket, Options).

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

normalize(H) when is_atom(H) ->
    string:to_lower(atom_to_list(H));
normalize(H) when is_binary(H) ->
    string:to_lower(binary_to_list(H));
normalize(H) when is_list(H) ->
    string:to_lower(H).

-endif. %% WEBMACHINE_COWBOY
