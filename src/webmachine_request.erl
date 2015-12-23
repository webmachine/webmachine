%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2014 Basho Technologies
%% Based on mochiweb_request.erl, which is Copyright 2007 Mochi Media, Inc.
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

%% @doc Webmachine HTTP Request Abstraction. The functions in this module
%% can be invoked using either parameterized module syntax or regular
%% invocation syntax. Since the Ericsson OTP team is removing the
%% parameterized module syntax in version R16, we encourage you to write
%% your applications using regular function syntax.
%%
%% To use parameterized module syntax, you create an instance and then
%% invoke functions on that instance, like this:
%%
%% <pre><code>
%%   Req = webmachine_request:new(ReqState),
%%   Result = Req:some_fun(Args),
%% </code></pre>
%%
%% where `ReqState' is an instance of a `#wm_reqstate' record. The runtime
%% then ensures the `ReqState' variable is implicitly passed to each
%% function invoked through the `Req' instance.
%%
%% To call functions using regular syntax, simply explicitly pass the
%% `ReqState' variable yourself; note there's no need to call
%% `webmachine_request:new/1' to perform regular invocations.

-module(webmachine_request).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([get_peer/1, get_sock/1]). % used in initialization
-export([call/2]). % internal switching interface, used by wrcall

% actual interface for resource functions
-export([
         new/1,
         trim_state/1,
         get_reqdata/1,
         set_reqdata/2,
         socket/1,
         method/1,
         version/1,
         disp_path/1,
         path/1,
         raw_path/1,
         get_req_header/2,
         req_headers/1,
         req_body/2,
         stream_req_body/2,
         headers/1,
         resp_headers/1,
         out_headers/1,
         get_out_header/2,
         has_out_header/2,
         peer/1,
         get_header_value/2,
         add_response_header/3,
         add_response_headers/2,
         remove_response_header/2,
         merge_response_headers/2,
         append_to_response_body/2,
         send_response/2,
         response_code/1,
         set_response_code/2,
         set_resp_body/2,
         response_body/1,
         has_response_body/1,
         do_redirect/1,
         resp_redirect/1,
         set_metadata/3,
         get_metadata/2,
         get_path_info/1,
         get_path_info/2,
         load_dispatch_data/7,
         get_path_tokens/1,
         get_app_root/1,
         parse_cookie/1,
         get_cookie_value/2,
         parse_qs/1,
         get_qs_value/2,
         get_qs_value/3,
         range/1,
         log_data/1
        ]).

-include("webmachine_logger.hrl").
-include("wm_reqstate.hrl").
-include("wm_reqdata.hrl").

-define(WMVSN, "1.10.9").
-define(QUIP, "cafe not found").
-define(IDLE_TIMEOUT, infinity).

new(#wm_reqstate{}=ReqState) ->
    {?MODULE, ReqState}.

trim_state({?MODULE, ReqState}) ->
    TrimData = (ReqState#wm_reqstate.reqdata)#wm_reqdata{wm_state='WMSTATE'},
    webmachine_request:new(ReqState#wm_reqstate{reqdata=TrimData});
trim_state(ReqState) ->
    trim_state({?MODULE, ReqState}).

get_peer({?MODULE, ReqState}=Req) ->
    case ReqState#wm_reqstate.peer of
    undefined ->
        PeerName = case ReqState#wm_reqstate.socket of
            testing -> {ok, {{127,0,0,1}, 80}};
            {ssl,SslSocket} -> ssl:peername(SslSocket);
            _ -> inet:peername(ReqState#wm_reqstate.socket)
        end,
        Peer = peer_from_peername(PeerName, Req),
        NewReqState = ReqState#wm_reqstate{peer=Peer},
        {Peer, NewReqState};
    _ ->
        {ReqState#wm_reqstate.peer, ReqState}
    end;
get_peer(ReqState) ->
    get_peer({?MODULE, ReqState}).

get_sock({?MODULE, ReqState} = Req) ->
    case ReqState#wm_reqstate.sock of
        undefined ->
            Sockname = case ReqState#wm_reqstate.socket of
                testing -> {ok, {{127,0,0,1}, 80}};
                {ssl,SslSocket} -> ssl:sockname(SslSocket);
                _ -> inet:sockname(ReqState#wm_reqstate.socket)
            end,
            Sock = peer_from_peername(Sockname, Req),
            NewReqState = ReqState#wm_reqstate{sock=Sock},
            {Sock, NewReqState};
        _ ->
            {ReqState#wm_reqstate.peer, ReqState}
    end;
get_sock(ReqState) ->
    get_sock({?MODULE, ReqState}).

peer_from_peername({error, Error}, _Req) ->
    {error, Error};
peer_from_peername({ok, {Addr={10, _, _, _}, _Port}}, Req) ->
    x_peername(inet_parse:ntoa(Addr), Req);
peer_from_peername({ok, {Addr={172, Second, _, _}, _Port}}, Req)
  when (Second > 15) andalso (Second < 32) ->
    x_peername(inet_parse:ntoa(Addr), Req);
peer_from_peername({ok, {Addr={192, 168, _, _}, _Port}}, Req) ->
    x_peername(inet_parse:ntoa(Addr), Req);
peer_from_peername({ok, {{127, 0, 0, 1}, _Port}}, Req) ->
    x_peername("127.0.0.1", Req);
peer_from_peername({ok, {Addr, _Port}}, _Req) ->
    inet_parse:ntoa(Addr).

x_peername(Default, Req) ->
    case get_header_value("x-forwarded-for", Req) of
    {undefined, _} ->
        Default;
    {Hosts, _} ->
        string:strip(lists:last(string:tokens(Hosts, ",")))
    end.

call(base_uri, {?MODULE, ReqState}) ->
    {wrq:base_uri(ReqState#wm_reqstate.reqdata), ReqState};
call(socket, {?MODULE, ReqState}) -> {ReqState#wm_reqstate.socket,ReqState};
call(get_reqdata, {?MODULE, ReqState}) -> {ReqState#wm_reqstate.reqdata, ReqState};
call({set_reqdata, RD}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=RD}};
call(method, {?MODULE, ReqState}) ->
    {wrq:method(ReqState#wm_reqstate.reqdata), ReqState};
call(version, {?MODULE, ReqState}) ->
    {wrq:version(ReqState#wm_reqstate.reqdata), ReqState};
call(raw_path, {?MODULE, ReqState}) ->
    {wrq:raw_path(ReqState#wm_reqstate.reqdata), ReqState};
call(req_headers, {?MODULE, ReqState}) ->
    {wrq:req_headers(ReqState#wm_reqstate.reqdata), ReqState};
call({req_body, MaxRecvBody}, {?MODULE, ReqState}) ->
    case ReqState#wm_reqstate.bodyfetch of
        stream ->
            {stream_conflict, ReqState};
        standard ->
            {ReqState#wm_reqstate.reqbody, ReqState};
        undefined ->
            RD=(ReqState#wm_reqstate.reqdata)#wm_reqdata{
                 max_recv_body=MaxRecvBody},
            NewReqState=ReqState#wm_reqstate{reqdata=RD},
            NewBody = case get(req_body) of
                undefined ->
                    NewB = do_recv_body(NewReqState),
                    put(req_body, NewB),
                    NewB;
                B -> B
            end,
            NewRD = RD#wm_reqdata{req_body=NewBody},
            {NewBody, NewReqState#wm_reqstate{
                        bodyfetch=standard,reqdata=NewRD,reqbody=NewBody}}
    end;
call({stream_req_body, MaxHunk}, {?MODULE, ReqState}) ->
    case ReqState#wm_reqstate.bodyfetch of
        standard ->
            {stream_conflict, ReqState};
        _ ->
            {recv_stream_body(ReqState, MaxHunk),
             ReqState#wm_reqstate{bodyfetch=stream}}
    end;
call(resp_headers, {?MODULE, ReqState}) ->
    {wrq:resp_headers(ReqState#wm_reqstate.reqdata), ReqState};
call(resp_redirect, {?MODULE, ReqState}) ->
    {wrq:resp_redirect(ReqState#wm_reqstate.reqdata), ReqState};
call({get_resp_header, HdrName}, {?MODULE, ReqState}) ->
    Reply = mochiweb_headers:get_value(HdrName,
                wrq:resp_headers(ReqState#wm_reqstate.reqdata)),
    {Reply, ReqState};
call(get_path_info, {?MODULE, ReqState}) ->
    PropList = orddict:to_list(wrq:path_info(ReqState#wm_reqstate.reqdata)),
    {PropList, ReqState};
call({get_path_info, Key}, {?MODULE, ReqState}) ->
    {wrq:path_info(Key, ReqState#wm_reqstate.reqdata), ReqState};
call(peer, Req) -> get_peer(Req);
call(sock, Req) -> get_sock(Req);
call(range, Req) -> get_range(Req);
call(response_code, {?MODULE, ReqState}) ->
    {wrq:response_code(ReqState#wm_reqstate.reqdata), ReqState};
call(app_root, {?MODULE, ReqState}) ->
    {wrq:app_root(ReqState#wm_reqstate.reqdata), ReqState};
call(disp_path, {?MODULE, ReqState}) ->
    {wrq:disp_path(ReqState#wm_reqstate.reqdata), ReqState};
call(path, {?MODULE, ReqState}) ->
    {wrq:path(ReqState#wm_reqstate.reqdata), ReqState};
call({get_req_header, K}, {?MODULE, ReqState}) ->
    {wrq:get_req_header(K, ReqState#wm_reqstate.reqdata), ReqState};
call({set_response_code, Code}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_response_code(
                                     Code, ReqState#wm_reqstate.reqdata)}};
call({set_resp_header, K, V}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_resp_header(
                                     K, V, ReqState#wm_reqstate.reqdata)}};
call({set_resp_headers, Hdrs}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_resp_headers(
                                     Hdrs, ReqState#wm_reqstate.reqdata)}};
call({remove_resp_header, K}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:remove_resp_header(
                                     K, ReqState#wm_reqstate.reqdata)}};
call({merge_resp_headers, Hdrs}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:merge_resp_headers(
                                     Hdrs, ReqState#wm_reqstate.reqdata)}};
call({append_to_response_body, Data}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:append_to_response_body(
                                     Data, ReqState#wm_reqstate.reqdata)}};
call({set_disp_path, P}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_disp_path(
                                     P, ReqState#wm_reqstate.reqdata)}};
call(do_redirect, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{
           reqdata=wrq:do_redirect(true, ReqState#wm_reqstate.reqdata)}};
call({send_response, Code}, Req) when is_integer(Code) ->
    call({send_response, {Code, undefined}}, Req);
call({send_response, {Code, ReasonPhrase}=CodeAndReason}, Req) when is_integer(Code) ->
    {Reply, NewState} =
        case Code of
            200 ->
                send_ok_response(ReasonPhrase, Req);
            _ ->
                send_response(CodeAndReason, Req)
        end,
    LogData = NewState#wm_reqstate.log_data,
    NewLogData = LogData#wm_log_data{finish_time=os:timestamp()},
    {Reply, NewState#wm_reqstate{log_data=NewLogData}};
call(resp_body, {?MODULE, ReqState}) ->
    {wrq:resp_body(ReqState#wm_reqstate.reqdata), ReqState};
call({set_resp_body, Body}, {?MODULE, ReqState}) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_resp_body(Body,
                                       ReqState#wm_reqstate.reqdata)}};
call(has_resp_body, {?MODULE, ReqState}) ->
    Reply = case wrq:resp_body(ReqState#wm_reqstate.reqdata) of
                undefined -> false;
                <<>> -> false;
                _ -> true
            end,
    {Reply, ReqState};
call({get_metadata, Key}, {?MODULE, ReqState}) ->
    Reply = case orddict:find(Key, ReqState#wm_reqstate.metadata) of
                {ok, Value} -> Value;
                error -> undefined
            end,
    {Reply, ReqState};
call({set_metadata, Key, Value}, {?MODULE, ReqState}) ->
    NewDict = orddict:store(Key, Value, ReqState#wm_reqstate.metadata),
    {ok, ReqState#wm_reqstate{metadata=NewDict}};
call(path_tokens, {?MODULE, ReqState}) ->
    {wrq:path_tokens(ReqState#wm_reqstate.reqdata), ReqState};
call(req_cookie, {?MODULE, ReqState}) ->
    {wrq:req_cookie(ReqState#wm_reqstate.reqdata), ReqState};
call(req_qs, {?MODULE, ReqState}) ->
    {wrq:req_qs(ReqState#wm_reqstate.reqdata), ReqState};
call({load_dispatch_data, PathProps, HostTokens, Port,
      PathTokens, AppRoot, DispPath}, {?MODULE, ReqState}) ->
    PathInfo = orddict:from_list(PathProps),
    NewState = ReqState#wm_reqstate{reqdata=wrq:load_dispatch_data(
                        PathInfo,HostTokens,Port,PathTokens,AppRoot,
                        DispPath,ReqState#wm_reqstate.reqdata)},
    {ok, NewState};
call(log_data, {?MODULE, ReqState}) -> {ReqState#wm_reqstate.log_data, ReqState};
call(notes, {?MODULE, ReqState}) -> {wrq:get_notes(ReqState#wm_reqstate.reqdata), ReqState};
call(Arg, #wm_reqstate{}=ReqState) -> call(Arg, {?MODULE, ReqState}).

get_header_value(K, {?MODULE, ReqState}) ->
    {wrq:get_req_header(K, ReqState#wm_reqstate.reqdata), ReqState};
get_header_value(K, ReqState) ->
    get_header_value(K, {?MODULE, ReqState}).

get_outheader_value(K, {?MODULE, ReqState}) ->
    {mochiweb_headers:get_value(K,
                                wrq:resp_headers(ReqState#wm_reqstate.reqdata)), ReqState}.

send(Socket, Data) ->
    case mochiweb_socket:send(Socket, iolist_to_binary(Data)) of
        ok -> ok;
        {error,closed} -> ok;
        _ -> exit(normal)
    end.

send_stream_body(Socket, X) -> send_stream_body(Socket, X, 0).
send_stream_body(Socket, {<<>>, done}, SoFar) ->
    send_chunk(Socket, <<>>),
    SoFar;
send_stream_body(Socket, {Data, done}, SoFar) ->
    Size = send_chunk(Socket, Data),
    send_chunk(Socket, <<>>),
    Size + SoFar;
send_stream_body(Socket, {<<>>, Next}, SoFar) ->
    send_stream_body(Socket, Next(), SoFar);
send_stream_body(Socket, {[], Next}, SoFar) ->
    send_stream_body(Socket, Next(), SoFar);
send_stream_body(Socket, {Data, Next}, SoFar) ->
    Size = send_chunk(Socket, Data),
    send_stream_body(Socket, Next(), Size + SoFar).

send_stream_body_no_chunk(Socket, {Data, done}) ->
    send(Socket, Data);
send_stream_body_no_chunk(Socket, {Data, Next}) ->
    send(Socket, Data),
    send_stream_body_no_chunk(Socket, Next()).

send_writer_body(Socket, {Encoder, Charsetter, BodyFun}) ->
    put(bytes_written, 0),
    Writer = fun(Data) ->
        Size = send_chunk(Socket, Encoder(Charsetter(Data))),
        put(bytes_written, get(bytes_written) + Size),
        Size
    end,
    BodyFun(Writer),
    send_chunk(Socket, <<>>),
    get(bytes_written).

send_chunk(Socket, Data) ->
    Size = iolist_size(Data),
    send(Socket, [mochihex:to_hex(Size), <<"\r\n">>, Data, <<"\r\n">>]),
    Size.

send_ok_response(ReasonPhrase, {?MODULE, ReqState}=Req) ->
    RD0 = ReqState#wm_reqstate.reqdata,
    {Range, State} = get_range(Req),
    case Range of
        X when X =:= undefined; X =:= fail; X =:= ignore ->
            send_response({200, ReasonPhrase}, Req);
        Ranges ->
            {PartList, Size} = range_parts(RD0, Ranges),
            case PartList of
                [] -> %% no valid ranges
                    %% could be 416, for now we'll just return 200
                    send_response({200, ReasonPhrase}, Req);
                PartList ->
                    {RangeHeaders, RangeBody} =
                        parts_to_body(PartList, Size, Req),
                    RespHdrsRD = wrq:set_resp_headers(
                             [{"Accept-Ranges", "bytes"} | RangeHeaders], RD0),
                    RespBodyRD = wrq:set_resp_body(
                                   RangeBody, RespHdrsRD),
                    NewState = State#wm_reqstate{reqdata=RespBodyRD},
                    send_response({206, ReasonPhrase}, NewState, Req)
            end
    end.

send_response(Code, #wm_reqstate{}=ReqState) -> send_response(Code,ReqState,{?MODULE,ReqState});
send_response(Code, {?MODULE, ReqState}=Req) -> send_response(Code,ReqState,Req).
send_response(Code, PassedState=#wm_reqstate{reqdata=RD}, _Req) ->
    Body0 = wrq:resp_body(RD),
    {Body,Length} = case Body0 of
        {stream, StreamBody} -> {{stream, StreamBody}, chunked};
        {known_length_stream, Size, StreamBody} -> {{known_length_stream, StreamBody}, Size};
        {stream, Size, Fun} -> {{stream, Fun(0, Size-1)}, chunked};
        {writer, WriteBody} -> {{writer, WriteBody}, chunked};
        _ -> {Body0, iolist_size([Body0])}
    end,
    send(PassedState#wm_reqstate.socket,
         [make_version(wrq:version(RD)),
          make_code(Code), <<"\r\n">> |
         make_headers(Code, Length, RD)]),
    FinalLength = case wrq:method(RD) of
         'HEAD' -> Length;
         _ ->
            case Body of
                {stream, Body2} ->
                    send_stream_body(PassedState#wm_reqstate.socket, Body2);
                {known_length_stream, Body2} ->
                    send_stream_body_no_chunk(PassedState#wm_reqstate.socket, Body2),
                    Length;
                {writer, Body2} ->
                    send_writer_body(PassedState#wm_reqstate.socket, Body2);
                _ ->
                    send(PassedState#wm_reqstate.socket, Body),
                    Length
            end
    end,
    InitLogData = PassedState#wm_reqstate.log_data,
    FinalLogData = InitLogData#wm_log_data{response_code=Code,
                                           response_length=FinalLength},
    {ok, PassedState#wm_reqstate{reqdata=wrq:set_response_code(Code, RD),
                     log_data=FinalLogData}}.

%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length(Req) ->
    case get_header_value("transfer-encoding", Req) of
        {undefined, _} ->
            case get_header_value("content-length", Req) of
                {undefined, _} -> undefined;
                {Length, _} -> list_to_integer(Length)
            end;
        {"chunked", _} -> chunked;
        Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length
do_recv_body(PassedState=#wm_reqstate{reqdata=RD}) ->
    MRH = RD#wm_reqdata.max_recv_hunk,
    MRB = RD#wm_reqdata.max_recv_body,
    read_whole_stream(recv_stream_body(PassedState, MRH), [], MRB, 0).

read_whole_stream({Hunk,_}, _, MaxRecvBody, SizeAcc)
  when SizeAcc + byte_size(Hunk) > MaxRecvBody ->
    {error, req_body_too_large};
read_whole_stream({Hunk,Next}, Acc0, MaxRecvBody, SizeAcc) ->
    HunkSize = byte_size(Hunk),
    if SizeAcc + HunkSize > MaxRecvBody ->
            {error, req_body_too_large};
       true ->
            Acc = [Hunk|Acc0],
            case Next of
                done -> iolist_to_binary(lists:reverse(Acc));
                _ -> read_whole_stream(Next(), Acc,
                                       MaxRecvBody, SizeAcc + HunkSize)
            end
    end.

recv_stream_body(PassedState=#wm_reqstate{reqdata=RD}, MaxHunkSize) ->
    put(mochiweb_request_recv, true),
    case get_header_value("expect", PassedState) of
        {"100-continue", _} ->
            send(PassedState#wm_reqstate.socket,
                 [make_version(wrq:version(RD)),
                  make_code(100), <<"\r\n\r\n">>]);
        _Else ->
            ok
    end,
    case body_length(PassedState) of
        {unknown_transfer_encoding, X} -> exit({unknown_transfer_encoding, X});
        undefined -> {<<>>, done};
        0 -> {<<>>, done};
        chunked -> recv_chunked_body(PassedState#wm_reqstate.socket,
                                     MaxHunkSize);
        Length -> recv_unchunked_body(PassedState#wm_reqstate.socket,
                                      MaxHunkSize, Length)
    end.

recv_unchunked_body(Socket, MaxHunk, DataLeft) ->
    case MaxHunk >= DataLeft of
        true ->
            {ok,Data1} = mochiweb_socket:recv(Socket,DataLeft,?IDLE_TIMEOUT),
            {Data1, done};
        false ->
            {ok,Data2} = mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_unchunked_body(Socket, MaxHunk, DataLeft-MaxHunk)
             end}
    end.

recv_chunked_body(Socket, MaxHunk) ->
    case read_chunk_length(Socket, false) of
        0 -> {<<>>, done};
        ChunkLength -> recv_chunked_body(Socket,MaxHunk,ChunkLength)
    end.
recv_chunked_body(Socket, MaxHunk, LeftInChunk) ->
    case MaxHunk >= LeftInChunk of
        true ->
            {ok,Data1} = mochiweb_socket:recv(Socket,LeftInChunk,?IDLE_TIMEOUT),
            {Data1,
             fun() -> recv_chunked_body(Socket, MaxHunk)
             end};
        false ->
            {ok,Data2} = mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT),
            {Data2,
             fun() -> recv_chunked_body(Socket, MaxHunk, LeftInChunk-MaxHunk)
             end}
    end.

read_chunk_length(Socket, MaybeLastChunk) ->
    mochiweb_socket:setopts(Socket, [{packet, line}]),
    case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            mochiweb_socket:setopts(Socket, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                                   andalso C =/= 59 % semicolon
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            case Hex of
                [] ->
                    %% skip the \r\n at the end of a chunk, or
                    %% allow [badly formed] last chunk header to be
                    %% empty instead of '0' explicitly
                    if MaybeLastChunk -> 0;
                       true -> read_chunk_length(Socket, true)
                    end;
                _ ->
                    erlang:list_to_integer(Hex, 16)
            end;
        _ ->
            exit(normal)
    end.

get_range({?MODULE, #wm_reqstate{reqdata = RD}=ReqState}=Req) ->
    case RD#wm_reqdata.resp_range of
        ignore_request ->
            {ignore, ReqState#wm_reqstate{range=undefined}};
        follow_request ->
            case get_header_value("range", Req) of
                {undefined, _} ->
                    {undefined, ReqState#wm_reqstate{range=undefined}};
                {RawRange, _} ->
                    Range = mochiweb_http:parse_range_request(RawRange),
                    {Range, ReqState#wm_reqstate{range=Range}}
            end
    end.

range_parts(_RD=#wm_reqdata{resp_body={file, IoDevice}}, Ranges) ->
    Size = mochiweb_io:iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
                case mochiweb_http:range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    V ->
                        [V | Acc]
                end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};

range_parts(RD=#wm_reqdata{resp_body={stream, {Hunk,Next}}}, Ranges) ->
    % for now, streamed bodies are read in full for range requests
    MRB = RD#wm_reqdata.max_recv_body,
    range_parts(read_whole_stream({Hunk,Next}, [], MRB, 0), Ranges);

range_parts(_RD=#wm_reqdata{resp_body={known_length_stream, Size, StreamBody}},
            Ranges) ->
    SkipLengths = [ mochiweb_http:range_skip_length(R, Size) || R <- Ranges],
    {[ {Skip, Skip+Length-1, {known_length_stream, Length, StreamBody}} ||
         {Skip, Length} <- SkipLengths ],
     Size};

range_parts(_RD=#wm_reqdata{resp_body={stream, Size, StreamFun}}, Ranges) ->
    SkipLengths = [ mochiweb_http:range_skip_length(R, Size) || R <- Ranges],
    {[ {Skip, Skip+Length-1, StreamFun} || {Skip, Length} <- SkipLengths ],
     Size};

range_parts(#wm_reqdata{resp_body=Body}, Ranges) when is_binary(Body); is_list(Body) ->
    range_parts(Body, Ranges);

range_parts(Body0, Ranges) when is_binary(Body0); is_list(Body0) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
                case mochiweb_http:range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    {Skip, Length} ->
                        <<_:Skip/binary,
                         PartialBody:Length/binary,
                         _/binary>> = Body,
                        [{Skip, Skip + Length - 1, PartialBody} | Acc]
                end
        end,
    {lists:foldr(F, [], Ranges), Size}.

parts_to_body([{Start, End, Body0}], Size, Req) ->
    %% return body for a range reponse with a single body
    ContentType =
        case get_outheader_value("content-type", Req) of
            {undefined, _} ->
                "text/html";
            {CT, _} ->
                CT
        end,
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    mochiweb_util:make_io(Start), "-",
                    mochiweb_util:make_io(End),
                    "/", mochiweb_util:make_io(Size)]}],
    Body = case Body0 of
              _ when is_function(Body0) ->
                   {known_length_stream, End - Start + 1, Body0(Start, End)};
              {known_length_stream, ContentSize, StreamBody} ->
                   {known_length_stream, ContentSize, StreamBody};
              _ ->
                   Body0
           end,
    {HeaderList, Body};
parts_to_body(BodyList, Size, Req) when is_list(BodyList) ->
    %% return
    %% header Content-Type: multipart/byteranges; boundary=441934886133bdee4
    %% and multipart body
    ContentType =
        case get_outheader_value("content-type", Req) of
            {undefined, _} ->
                "text/html";
            {CT, _} ->
                CT
        end,
    Boundary = mochihex:to_hex(crypto:rand_bytes(8)),
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = case hd(BodyList) of
                        {_, _, Fun} when is_function(Fun) ->
                            stream_multipart_body(BodyList, ContentType,
                                                  Boundary, Size);
                        _ ->
                            multipart_body(BodyList, ContentType,
                                           Boundary, Size)
                    end,
    {HeaderList, MultiPartBody}.

multipart_body([], _ContentType, Boundary, _Size) ->
    end_boundary(Boundary);
multipart_body([{Start, End, Body} | BodyList],
               ContentType, Boundary, Size) ->
    [part_preamble(Boundary, ContentType, Start, End, Size),
     Body, <<"\r\n">>
     | multipart_body(BodyList, ContentType, Boundary, Size)].

boundary(B)     -> [<<"--">>, B, <<"\r\n">>].
end_boundary(B) -> [<<"--">>, B, <<"--\r\n">>].

part_preamble(Boundary, CType, Start, End, Size) ->
    [boundary(Boundary),
     <<"Content-Type: ">>, CType, <<"\r\n">>,
     <<"Content-Range: bytes ">>,
     mochiweb_util:make_io(Start), <<"-">>, mochiweb_util:make_io(End),
     <<"/">>, mochiweb_util:make_io(Size),
     <<"\r\n\r\n">>].

stream_multipart_body(BodyList, ContentType, Boundary, Size) ->
    Helper = stream_multipart_body_helper(
               BodyList, ContentType, Boundary, Size),
    %% executing Helper() here is an optimization;
    %% it's just as valid to say {<<>>, Helper}
    {stream, Helper()}.

stream_multipart_body_helper([], _CType, Boundary, _Size) ->
    fun() -> {end_boundary(Boundary), done} end;
stream_multipart_body_helper([{Start, End, Fun}|Rest],
                             CType, Boundary, Size) ->
    fun() ->
            {part_preamble(Boundary, CType, Start, End, Size),
             stream_multipart_part_helper(
               fun() -> Fun(Start, End) end,
               Rest, CType, Boundary, Size)}
    end.

stream_multipart_part_helper(Fun, Rest, CType, Boundary, Size) ->
    fun() ->
            case Fun() of
                {Data, done} ->
                    %% when this part is done, start the next part
                    {[Data, <<"\r\n">>],
                     stream_multipart_body_helper(
                       Rest, CType, Boundary, Size)};
                {Data, Next} ->
                    %% this subpart has more data coming
                    {Data, stream_multipart_part_helper(
                             Next, Rest, CType, Boundary, Size)}
            end
    end.

make_code({Code, undefined}) when is_integer(Code) ->
    make_code({Code, httpd_util:reason_phrase(Code)});
make_code({Code, ReasonPhrase}) when is_integer(Code) ->
    [integer_to_list(Code), [" ", ReasonPhrase]];
make_code(Code) when is_integer(Code) ->
    make_code({Code, httpd_util:reason_phrase(Code)});
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

make_headers({Code, _ReasonPhrase}, Length, RD) ->
    make_headers(Code, Length, RD);
make_headers(Code, Length, RD) when is_integer(Code) ->
    Hdrs0 = case Code of
        304 ->
            mochiweb_headers:make(wrq:resp_headers(RD));
        _ ->
            case Length of
                chunked ->
                    mochiweb_headers:enter(
                      "Transfer-Encoding","chunked",
                      mochiweb_headers:make(wrq:resp_headers(RD)));
                _ ->
                    mochiweb_headers:enter(
                      "Content-Length",integer_to_list(Length),
                      mochiweb_headers:make(wrq:resp_headers(RD)))
            end
    end,
    case application:get_env(webmachine, server_name) of
      undefined -> ServerHeader = "MochiWeb/1.1 WebMachine/" ++ ?WMVSN ++ " (" ++ ?QUIP ++ ")";
      {ok, ServerHeader} when is_list(ServerHeader) -> ok
    end,
    WithSrv = mochiweb_headers:enter("Server", ServerHeader, Hdrs0),
    Hdrs = case mochiweb_headers:get_value("date", WithSrv) of
        undefined ->
            mochiweb_headers:enter("Date", httpd_util:rfc1123_date(), WithSrv);
        _ ->
            WithSrv
    end,
    F = fun({K, V}, Acc) ->
                [mochiweb_util:make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    lists:foldl(F, [<<"\r\n">>], mochiweb_headers:to_list(Hdrs)).

get_reqdata(#wm_reqstate{}=ReqState) -> call(get_reqdata, {?MODULE, ReqState});
get_reqdata(Req) -> call(get_reqdata, Req).

set_reqdata(RD, #wm_reqstate{}=ReqState) -> call({set_reqdata, RD}, {?MODULE, ReqState});
set_reqdata(RD, Req) -> call({set_reqdata, RD}, Req).

socket(#wm_reqstate{}=ReqState) -> call(socket, {?MODULE, ReqState});
socket(Req) -> call(socket, Req).

method(#wm_reqstate{}=ReqState) -> call(method, {?MODULE, ReqState});
method(Req) -> call(method, Req).

version(#wm_reqstate{}=ReqState) -> call(version, {?MODULE, ReqState});
version(Req) -> call(version, Req).

disp_path(#wm_reqstate{}=ReqState) -> call(disp_path, {?MODULE, ReqState});
disp_path(Req) -> call(disp_path, Req).

path(#wm_reqstate{}=ReqState) -> call(path, {?MODULE, ReqState});
path(Req) -> call(path, Req).

raw_path(#wm_reqstate{}=ReqState) -> call(raw_path, {?MODULE, ReqState});
raw_path(Req) -> call(raw_path, Req).

req_headers(#wm_reqstate{}=ReqState) -> call(req_headers, {?MODULE, ReqState});
req_headers(Req) -> call(req_headers, Req).
headers(Req) -> req_headers(Req).

req_body(MaxRevBody, #wm_reqstate{}=ReqState) -> call({req_body,MaxRevBody}, {?MODULE, ReqState});
req_body(MaxRevBody, Req) -> call({req_body,MaxRevBody}, Req).
stream_req_body(MaxHunk, #wm_reqstate{}=ReqState) ->
    call({stream_req_body, MaxHunk}, {?MODULE, ReqState});
stream_req_body(MaxHunk, Req) -> call({stream_req_body, MaxHunk}, Req).

resp_headers(#wm_reqstate{}=ReqState) -> call(resp_headers, {?MODULE, ReqState});
resp_headers(Req) -> call(resp_headers, Req).
out_headers(Req) -> resp_headers(Req).

get_resp_header(HeaderName, Req) ->
    call({get_resp_header, HeaderName}, Req).
get_out_header(HeaderName, #wm_reqstate{}=ReqState) ->
    get_resp_header(HeaderName, {?MODULE, ReqState});
get_out_header(HeaderName, Req) -> get_resp_header(HeaderName, Req).

has_resp_header(HeaderName, Req) ->
    case get_out_header(HeaderName, Req) of
        {undefined, _} -> false;
        {_, _}         -> true
    end.
has_out_header(HeaderName, #wm_reqstate{}=ReqState) ->
    has_resp_header(HeaderName, {?MODULE, ReqState});
has_out_header(HeaderName, Req) -> has_resp_header(HeaderName, Req).

has_resp_body(#wm_reqstate{}=ReqState) -> call(has_resp_body, {?MODULE, ReqState});
has_resp_body(Req) -> call(has_resp_body, Req).
has_response_body(Req) -> has_resp_body(Req).

response_code(#wm_reqstate{}=ReqState) -> call(response_code, {?MODULE, ReqState});
response_code(Req) -> call(response_code, Req).
set_response_code(Code, {?MODULE, ReqState}=Req) ->
    call({ReqState, set_response_code, Code}, Req);
set_response_code(Code, ReqState) ->
    set_response_code(Code, {?MODULE, ReqState}).

peer(#wm_reqstate{}=ReqState) -> call(peer, {?MODULE, ReqState});
peer(Req) -> call(peer, Req).

range(#wm_reqstate{}=ReqState) -> call(range, {?MODULE, ReqState});
range(Req) -> call(range, Req).

req_cookie(#wm_reqstate{}=ReqState) -> call(req_cookie, {?MODULE, ReqState});
req_cookie(Req) -> call(req_cookie, Req).
parse_cookie(Req) -> req_cookie(Req).
get_cookie_value(Key, #wm_reqstate{}=ReqState) -> get_cookie_value(Key, {?MODULE, ReqState});
get_cookie_value(Key, Req) ->
    {ReqCookie, NewReqState} = req_cookie(Req),
    case lists:keyfind(Key, 1, ReqCookie) of
        false -> {undefined, NewReqState};
        {Key, Value} -> {Value, NewReqState}
    end.

req_qs(#wm_reqstate{}=ReqState) -> call(req_qs, {?MODULE, ReqState});
req_qs(Req) -> call(req_qs, Req).
parse_qs(Req) -> req_qs(Req).
get_qs_value(Key, #wm_reqstate{}=ReqState) -> get_qs_value(Key, {?MODULE, ReqState});
get_qs_value(Key, Req) ->
    {ReqQS, NewReqState} = req_qs(Req),
    case lists:keyfind(Key, 1, ReqQS) of
        false -> {undefined, NewReqState};
        {Key, Value} -> {Value, NewReqState}
    end.
get_qs_value(Key, Default, #wm_reqstate{}=ReqState) ->
    get_qs_value(Key, Default, {?MODULE, ReqState});
get_qs_value(Key, Default, Req) ->
    {ReqQS, NewReqState} = req_qs(Req),
    case lists:keyfind(Key, 1, ReqQS) of
        false -> {Default, NewReqState};
        {Key, Value} -> {Value, NewReqState}
    end.
set_resp_body(Body, #wm_reqstate{}=ReqState) -> call({set_resp_body, Body}, {?MODULE, ReqState});
set_resp_body(Body, Req) -> call({set_resp_body, Body}, Req).
resp_body(#wm_reqstate{}=ReqState) -> call(resp_body, {?MODULE, ReqState});
resp_body(Req) -> call(resp_body, Req).
response_body(Req) -> resp_body(Req).

get_req_header(K, #wm_reqstate{}=ReqState) -> call({get_req_header, K}, {?MODULE, ReqState});
get_req_header(K, Req) -> call({get_req_header, K}, Req).

set_resp_header(K, V, Req) -> call({set_resp_header, K, V}, Req).
add_response_header(K, V, #wm_reqstate{}=ReqState) -> set_resp_header(K, V, {?MODULE, ReqState});
add_response_header(K, V, Req) -> set_resp_header(K, V, Req).

set_resp_headers(Hdrs, Req) -> call({set_resp_headers, Hdrs}, Req).
add_response_headers(Hdrs, #wm_reqstate{}=ReqState) -> set_resp_headers(Hdrs, {?MODULE, ReqState});
add_response_headers(Hdrs, Req) -> set_resp_headers(Hdrs, Req).

remove_resp_header(K, Req) -> call({remove_resp_header, K}, Req).
remove_response_header(K, #wm_reqstate{}=ReqState) -> remove_resp_header(K, {?MODULE, ReqState});
remove_response_header(K, Req) -> remove_resp_header(K, Req).

merge_resp_headers(Hdrs, Req) -> call({merge_resp_headers, Hdrs}, Req).
merge_response_headers(Hdrs, #wm_reqstate{}=ReqState) ->
    merge_resp_headers(Hdrs, {?MODULE, ReqState});
merge_response_headers(Hdrs, Req) -> merge_resp_headers(Hdrs, Req).

append_to_response_body(Data, #wm_reqstate{}=ReqState) ->
    call({append_to_response_body, Data}, {?MODULE, ReqState});
append_to_response_body(Data, Req) ->
    call({append_to_response_body, Data}, Req).

do_redirect(#wm_reqstate{}=ReqState) -> call(do_redirect, {?MODULE, ReqState});
do_redirect(Req) -> call(do_redirect, Req).

resp_redirect(#wm_reqstate{}=ReqState) -> call(resp_redirect, {?MODULE, ReqState});
resp_redirect(Req) -> call(resp_redirect, Req).

get_metadata(Key, #wm_reqstate{}=ReqState) -> call({get_metadata, Key}, {?MODULE, ReqState});
get_metadata(Key, Req) -> call({get_metadata, Key}, Req).

set_metadata(Key, Value, #wm_reqstate{}=ReqState) ->
    call({set_metadata, Key, Value}, {?MODULE, ReqState});
set_metadata(Key, Value, Req) -> call({set_metadata, Key, Value}, Req).

get_path_info(#wm_reqstate{}=ReqState) -> call(get_path_info, {?MODULE, ReqState});
get_path_info(Req) -> call(get_path_info, Req).

get_path_info(Key, #wm_reqstate{}=ReqState) -> call({get_path_info, Key}, {?MODULE, ReqState});
get_path_info(Key, Req) -> call({get_path_info, Key}, Req).

path_tokens(#wm_reqstate{}=ReqState) -> call(path_tokens, {?MODULE, ReqState});
path_tokens(Req) -> call(path_tokens, Req).
get_path_tokens(Req) -> path_tokens(Req).

app_root(#wm_reqstate{}=ReqState) -> call(app_root, {?MODULE, ReqState});
app_root(Req) -> call(app_root, Req).
get_app_root(Req) -> app_root(Req).

load_dispatch_data(Bindings, HostTokens, Port, PathTokens,
                   AppRoot, DispPath, #wm_reqstate{}=ReqState) ->
    call({load_dispatch_data, Bindings, HostTokens, Port,
          PathTokens, AppRoot, DispPath}, {?MODULE, ReqState});
load_dispatch_data(Bindings, HostTokens, Port, PathTokens,
                   AppRoot, DispPath, Req) ->
    call({load_dispatch_data, Bindings, HostTokens, Port,
          PathTokens, AppRoot, DispPath}, Req).

log_data(#wm_reqstate{}=ReqState) -> call(log_data, {?MODULE, ReqState});
log_data(Req) -> call(log_data, Req).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

reqdata_test() ->
    ReqData = #wm_reqdata{req_headers = mochiweb_headers:make([])},
    {ok, ReqState} = set_reqdata(ReqData, #wm_reqstate{}),
    ?assertEqual(ReqData, element(1, get_reqdata(ReqState))).

header_test() ->
    HdrName = "Accept",
    HdrValue = "application/json",
    ReqData = #wm_reqdata{req_headers = mochiweb_headers:make([{HdrName, HdrValue}])},
    {ok, ReqState} = set_reqdata(ReqData, #wm_reqstate{}),
    ?assertEqual({HdrValue, ReqState}, get_header_value(HdrName, ReqState)),
    ?assertEqual({HdrValue, ReqState}, get_req_header(HdrName, ReqState)).

metadata_test() ->
    Key = "webmachine",
    Value = "eunit",
    {ok, ReqState} = set_metadata(Key, Value, #wm_reqstate{metadata=orddict:new()}),
    ?assertEqual({Value, ReqState}, get_metadata(Key, ReqState)).

peer_test() ->
    Self = self(),
    Pid = spawn_link(fun() ->
                             {ok, LS} = gen_tcp:listen(0, [binary, {active, false}]),
                             {ok, {_, Port}} = inet:sockname(LS),
                             Self ! {port, Port},
                             {ok, S} = gen_tcp:accept(LS),
                             receive
                                 stop ->
                                     ok
                             after 2000 ->
                                     ok
                             end,
                             gen_tcp:close(S),
                             gen_tcp:close(LS)
                     end),
    receive
        {port, Port} ->
            {ok, S} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}]),
            ReqData = #wm_reqdata{req_headers = mochiweb_headers:make([])},
            ReqState = #wm_reqstate{socket=S, reqdata=ReqData},
            ?assertEqual({S, ReqState}, socket(ReqState)),
            {"127.0.0.1", NReqState} = get_peer(ReqState),
            ?assertEqual("127.0.0.1", NReqState#wm_reqstate.peer),
            Pid ! stop,
            gen_tcp:close(S)
    after 2000 ->
            exit({error, listener_fail})
    end.

sock_test() ->
    Self = self(),
    Pid = spawn_link(fun() ->
                             {ok, LS} = gen_tcp:listen(0, [binary, {active, false}]),
                             {ok, {_, Port}} = inet:sockname(LS),
                             Self ! {port, Port},
                             {ok, S} = gen_tcp:accept(LS),
                             receive
                                 stop ->
                                     ok
                             after 2000 ->
                                     ok
                             end,
                             gen_tcp:close(S),
                             gen_tcp:close(LS)
                     end),
    receive
        {port, Port} ->
            {ok, S} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active, false}]),
            ReqData = #wm_reqdata{req_headers = mochiweb_headers:make([])},
            ReqState = #wm_reqstate{socket=S, reqdata=ReqData},
            ?assertEqual({S, ReqState}, socket(ReqState)),
            {"127.0.0.1", NReqState} = get_sock(ReqState),
            ?assertEqual("127.0.0.1", NReqState#wm_reqstate.sock),
            Pid ! stop,
            gen_tcp:close(S)
    after 2000 ->
            exit({error, listener_fail})
    end.


-endif.
