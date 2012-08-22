%% @author Steve Vinoski <vinoski@ieee.org>
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

%% @doc Yaws interface for webmachine.
-module(webmachine_yaws).
-author('Steve Vinoski <vinoski@ieee.org>').
-export([start/1, stop/0, out/1, get_req_info/2]).
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

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("yaws/include/yaws.hrl").


start(Options0) ->
    {_PName, Options} = webmachine_ws:start(Options0, ?MODULE),
    SConf0 = [{appmods, [{"/", ?MODULE}]}],
    {SConf, GConf} = convert_options(Options, SConf0, []),
    Docroot = "/tmp",
    ok = yaws:start_embedded(Docroot, SConf, GConf, "webmachine-yaws"),
    {ok, whereis(yaws_server)}.

stop() ->
    yaws:stop().

out(Arg) ->
    Req = webmachine:new_request(yaws, {?MODULE, Arg}),
    webmachine_ws:dispatch_request(Req),
    ok.

get_req_info(Want, Arg) ->
    get_req_info(Want, Arg, []).
get_req_info([socket|T], #arg{clisock=Socket}=Arg, Acc) ->
    get_req_info(T, Arg, [{socket, Socket}|Acc]);
get_req_info([method|T], #arg{req=Req}=Arg, Acc) ->
    Method = Req#http_request.method,
    get_req_info(T, Arg, [{method, Method}|Acc]);
get_req_info([scheme|T], Arg, Acc) ->
    Uri = yaws_api:request_url(Arg),
    get_req_info(T, Arg, [{scheme, Uri#url.scheme}|Acc]);
get_req_info([path|T], #arg{req=Req}=Arg, Acc) ->
    {abs_path, Path} = Req#http_request.path,
    get_req_info(T, Arg, [{path, Path}|Acc]);
get_req_info([version|T], #arg{req=Req}=Arg, Acc) ->
    Version = Req#http_request.version,
    get_req_info(T, Arg, [{version, Version}|Acc]);
get_req_info([headers|T], #arg{headers = Headers}=Arg, Acc) ->
    get_req_info(T, Arg, [{headers, yaws_api:reformat_header(Headers)}|Acc]);
get_req_info([], _Arg, Acc) ->
    lists:reverse(Acc).

convert_options([], SConf, GConf) ->
    {SConf, GConf};
convert_options([{ip, IpAddr0}|Opts], SConf, GConf) ->
    {ok, IpAddr} = inet_parse:address(IpAddr0),
    convert_options(Opts, [{listen, IpAddr}|SConf], GConf);
convert_options([{port, Port}|Opts], SConf, GConf) ->
    convert_options(Opts, [{port, Port}|SConf], GConf);
convert_options([_|Opts], SConf, GConf) ->
    convert_options(Opts, SConf, GConf).

get_header_value(HeaderName, Headers) when is_list(Headers) ->
    get_header_value(HeaderName, headers_from_list(Headers));
get_header_value(HeaderName, Headers) ->
    yaws_api:get_header(Headers, HeaderName).

new_headers() ->
    #headers{}.

make_headers(Headers) when is_list(Headers) ->
    headers_from_list(Headers);
make_headers(Headers) ->
    Headers.

add_header(Header, Value, Headers) ->
    yaws_api:set_header(Headers, Header, Value).

merge_header(Header, Value, Headers) ->
    NewVal = case yaws_api:get_header(Headers, Header) of
                 undefined ->
                     Value;
                 OldVal ->
                     OldVal ++ ", " ++ Value
             end,
    yaws_api:set_header(Headers, Header, NewVal).

headers_to_list(Headers) ->
    Formatter = fun(H,V) when is_atom(H) ->
                        {atom_to_list(H), V};
                   (H,V) ->
                        {H,V}
                end,
    yaws_api:reformat_header(Headers, Formatter).

headers_from_list(Headers) ->
    lists:foldl(fun({Hdr,Val}, Hdrs) ->
                        yaws_api:set_header(Hdrs, Hdr, Val);
                   (HdrStr, Hdrs) ->
                        {ok, {http_header, _, Hdr, _, Val}, _} =
                            erlang:decode_packet(
                              httph_bin,
                              list_to_binary([HdrStr, <<"\r\n\r\n">>]),
                              []),
                        yaws_api:set_header(Hdrs, Hdr, Val)
                end, #headers{}, Headers).

socket_send(Socket, Data) ->
    yaws:gen_tcp_send(Socket, Data).

socket_recv(Socket, Length, Timeout) ->
    SC = get(sc),
    yaws:do_recv(Socket, Length, yaws:is_ssl(SC), Timeout).

socket_setopts(Socket, Options) ->
    SC = get(sc),
    yaws:setopts(Socket, Options, yaws:is_ssl(SC)).

urlsplit_path(Path) ->
    Url = yaws_api:parse_url(Path),
    {Url#url.path, Url#url.querypart, ""}.

parse_qs(QueryString) ->
    yaws_api:parse_query(#arg{querydata=QueryString}).

parse_cookie(Value) ->
    lists:foldl(fun(#cookie{key=Key, value=Val}, Acc) ->
                        [{Key,Val}|Acc]
                end, [], yaws_api:parse_cookie(Value)).

make_reqdata(Path) ->
    %% Helper function to construct a request and return the ReqData
    %% object. Used only for testing.
    Arg = #arg{clisock=testing,
               headers=new_headers(),
               req=#http_request{method='GET', path={abs_path,Path}, version={1,1}},
               docroot="/tmp"},
    put(sc, #sconf{}),
    Req = webmachine:new_request(yaws, {?MODULE, Arg}),
    {RD, _} = Req:get_reqdata(),
    RD.
