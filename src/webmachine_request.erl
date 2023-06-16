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

%% @doc Webmachine HTTP Request Abstraction.
%%
%% Note: this module used to support parametrized module syntax. If
%% you have code like, `Req = webmachine_request:new(ReqState),
%% Req:path()`, please update it to call
%% `webmachine_request:path(ReqState)`.

-module(webmachine_request).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').

-export([get_peer/1, get_sock/1]). % used in initialization
-export([call/2]). % internal switching interface, used by wrcall

% actual interface for resource functions
-export([
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
-include("wm_compat.hrl").

-define(IDLE_TIMEOUT, infinity).

-type t() :: #wm_reqstate{}.
-export_type([t/0]).

trim_state(ReqState) ->
    TrimData = (ReqState#wm_reqstate.reqdata)#wm_reqdata{wm_state='WMSTATE'},
    ReqState#wm_reqstate{reqdata=TrimData}.

get_peer(ReqState) ->
    case ReqState#wm_reqstate.peer of
    undefined ->
        PeerName = case ReqState#wm_reqstate.socket of
            testing -> {ok, {{127,0,0,1}, 80}};
            {ssl,SslSocket} -> ssl:peername(SslSocket);
            _ -> inet:peername(ReqState#wm_reqstate.socket)
        end,
        Peer = peer_from_peername(PeerName, ReqState),
        NewReqState = ReqState#wm_reqstate{peer=Peer},
        {Peer, NewReqState};
    _ ->
        {ReqState#wm_reqstate.peer, ReqState}
    end.

get_sock(ReqState) ->
    case ReqState#wm_reqstate.sock of
        undefined ->
            Sockname = case ReqState#wm_reqstate.socket of
                testing -> {ok, {{127,0,0,1}, 80}};
                {ssl,SslSocket} -> ssl:sockname(SslSocket);
                _ -> inet:sockname(ReqState#wm_reqstate.socket)
            end,
            Sock = peer_from_peername(Sockname, ReqState),
            NewReqState = ReqState#wm_reqstate{sock=Sock},
            {Sock, NewReqState};
        _ ->
            {ReqState#wm_reqstate.peer, ReqState}
    end.

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
        case string:tokens(Hosts, ",") of
            [] ->
                %% both Hosts="" and Hosts=",,," cause this, so check
                %% here instead of before tokenization
                Default;
            HostList ->
                string:strip(lists:last(HostList))
        end
    end.

call(base_uri, ReqState) ->
    {wrq:base_uri(ReqState#wm_reqstate.reqdata), ReqState};
call(socket, ReqState) -> {ReqState#wm_reqstate.socket,ReqState};
call(get_reqdata, ReqState) -> {ReqState#wm_reqstate.reqdata, ReqState};
call({set_reqdata, RD}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=RD}};
call(method, ReqState) ->
    {wrq:method(ReqState#wm_reqstate.reqdata), ReqState};
call(version, ReqState) ->
    {wrq:version(ReqState#wm_reqstate.reqdata), ReqState};
call(raw_path, ReqState) ->
    {wrq:raw_path(ReqState#wm_reqstate.reqdata), ReqState};
call(req_headers, ReqState) ->
    {wrq:req_headers(ReqState#wm_reqstate.reqdata), ReqState};
call({req_body, MaxRecvBody}, ReqState) ->
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
call({stream_req_body, MaxHunk}, ReqState) ->
    case ReqState#wm_reqstate.bodyfetch of
        standard ->
            {stream_conflict, ReqState};
        _ ->
            {recv_stream_body(ReqState, MaxHunk),
             ReqState#wm_reqstate{bodyfetch=stream}}
    end;
call(maybe_flush_req_body, ReqState) ->
    {maybe_flush_req_body(ReqState), ReqState};
call(resp_headers, ReqState) ->
    {wrq:resp_headers(ReqState#wm_reqstate.reqdata), ReqState};
call(resp_redirect, ReqState) ->
    {wrq:resp_redirect(ReqState#wm_reqstate.reqdata), ReqState};
call({get_resp_header, HdrName}, ReqState) ->
    Reply = mochiweb_headers:get_value(HdrName,
                wrq:resp_headers(ReqState#wm_reqstate.reqdata)),
    {Reply, ReqState};
call(get_path_info, ReqState) ->
    PropList = orddict:to_list(wrq:path_info(ReqState#wm_reqstate.reqdata)),
    {PropList, ReqState};
call({get_path_info, Key}, ReqState) ->
    {wrq:path_info(Key, ReqState#wm_reqstate.reqdata), ReqState};
call(peer, Req) -> get_peer(Req);
call(sock, Req) -> get_sock(Req);
call(range, Req) -> get_range(Req);
call(response_code, ReqState) ->
    {wrq:response_code(ReqState#wm_reqstate.reqdata), ReqState};
call(app_root, ReqState) ->
    {wrq:app_root(ReqState#wm_reqstate.reqdata), ReqState};
call(disp_path, ReqState) ->
    {wrq:disp_path(ReqState#wm_reqstate.reqdata), ReqState};
call(path, ReqState) ->
    {wrq:path(ReqState#wm_reqstate.reqdata), ReqState};
call({get_req_header, K}, ReqState) ->
    {wrq:get_req_header(K, ReqState#wm_reqstate.reqdata), ReqState};
call({set_response_code, Code}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_response_code(
                                     Code, ReqState#wm_reqstate.reqdata)}};
call({set_resp_header, K, V}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_resp_header(
                                     K, V, ReqState#wm_reqstate.reqdata)}};
call({set_resp_headers, Hdrs}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_resp_headers(
                                     Hdrs, ReqState#wm_reqstate.reqdata)}};
call({remove_resp_header, K}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:remove_resp_header(
                                     K, ReqState#wm_reqstate.reqdata)}};
call({merge_resp_headers, Hdrs}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:merge_resp_headers(
                                     Hdrs, ReqState#wm_reqstate.reqdata)}};
call({append_to_response_body, Data}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:append_to_response_body(
                                     Data, ReqState#wm_reqstate.reqdata)}};
call({set_disp_path, P}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_disp_path(
                                     P, ReqState#wm_reqstate.reqdata)}};
call(do_redirect, ReqState) ->
    {ok, ReqState#wm_reqstate{
           reqdata=wrq:do_redirect(true, ReqState#wm_reqstate.reqdata)}};
call({send_response, CodeAndPhrase}, Req) ->
    {Reply, NewState} =
        case webmachine_status_code:status_code(CodeAndPhrase) of
            200 ->
                send_ok_response(
                  webmachine_status_code:reason_phrase(CodeAndPhrase), Req);
            _ ->
                send_response(CodeAndPhrase, Req)
        end,
    LogData = NewState#wm_reqstate.log_data,
    NewLogData = LogData#wm_log_data{finish_time=erlang:monotonic_time()},
    {Reply, NewState#wm_reqstate{log_data=NewLogData}};
call(resp_body, ReqState) ->
    {wrq:resp_body(ReqState#wm_reqstate.reqdata), ReqState};
call({set_resp_body, Body}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:set_resp_body(Body,
                                       ReqState#wm_reqstate.reqdata)}};
call(has_resp_body, ReqState) ->
    Reply =
        case wrq:resp_body(ReqState#wm_reqstate.reqdata) of
            <<>> -> false;
            _ -> true
        end,
    {Reply, ReqState};
call({get_metadata, Key}, ReqState) ->
    Reply = case orddict:find(Key, ReqState#wm_reqstate.metadata) of
                {ok, Value} -> Value;
                error -> undefined
            end,
    {Reply, ReqState};
call({set_metadata, Key, Value}, ReqState) ->
    NewDict = orddict:store(Key, Value, ReqState#wm_reqstate.metadata),
    {ok, ReqState#wm_reqstate{metadata=NewDict}};
call(path_tokens, ReqState) ->
    {wrq:path_tokens(ReqState#wm_reqstate.reqdata), ReqState};
call(req_cookie, ReqState) ->
    {wrq:req_cookie(ReqState#wm_reqstate.reqdata), ReqState};
call(req_qs, ReqState) ->
    {wrq:req_qs(ReqState#wm_reqstate.reqdata), ReqState};
call({load_dispatch_data, PathProps, HostTokens, Port,
      PathTokens, AppRoot, DispPath}, ReqState) ->
    PathInfo = orddict:from_list(PathProps),
    NewState = ReqState#wm_reqstate{reqdata=wrq:load_dispatch_data(
                        PathInfo,HostTokens,Port,PathTokens,AppRoot,
                        DispPath,ReqState#wm_reqstate.reqdata)},
    {ok, NewState};
call(log_data, ReqState) -> {ReqState#wm_reqstate.log_data, ReqState};
call(notes, ReqState) -> {wrq:get_notes(ReqState#wm_reqstate.reqdata), ReqState};
call({add_note, Key, Value}, ReqState) ->
    {ok, ReqState#wm_reqstate{reqdata=wrq:add_note(Key, Value, ReqState#wm_reqstate.reqdata)}};
call(Arg, #wm_reqstate{}=ReqState) -> call(Arg, ReqState).

get_header_value(K, ReqState) ->
    {wrq:get_req_header(K, ReqState#wm_reqstate.reqdata), ReqState}.

get_outheader_value(K, ReqState) ->
    {mochiweb_headers:get_value(K,
                                wrq:resp_headers(ReqState#wm_reqstate.reqdata)), ReqState}.

-type mochiweb_socket() :: gen_tcp:socket() | {ssl, gen_tcp:socket()}.
-spec send(mochiweb_socket(), iolist() | binary()) -> ok | {error, any()}.
send(Socket, Data) ->
    mochiweb_socket:send(Socket, iolist_to_binary(Data)).

%% For use with the stream_body functions.
-spec catch_next(fun()) ->
          {iolist() | binary(), fun() | done} | {stream_error, any()}.
catch_next(NextFun) ->
    try NextFun()
    catch ?STPATTERN(Class:Reason) ->
        {error, {Class, Reason, ?STACKTRACE}}
    end.

-spec send_stream_body(mochiweb_socket(),
                       {iolist() | binary(), fun() | done}) ->
          {ok | {error, any()}, integer()}.
send_stream_body(Socket, X) -> send_stream_body(Socket, X, 0).

send_stream_body(Socket, {<<>>, done}, SoFar) ->
    {Result, _} = send_chunk(Socket, <<>>),
    {Result, SoFar};
send_stream_body(Socket, {Data, done}, SoFar) when is_list(Data);
                                                   is_binary(Data) ->
    {Result, Size} = send_chunk(Socket, Data),
    send_chunk(Socket, <<>>),
    {Result, Size + SoFar};
send_stream_body(Socket, {<<>>, Next}, SoFar) ->
    send_stream_body(Socket, catch_next(Next), SoFar);
send_stream_body(Socket, {[], Next}, SoFar) ->
    send_stream_body(Socket, catch_next(Next), SoFar);
send_stream_body(Socket, {Data, Next}, SoFar) when is_list(Data);
                                                   is_binary(Data) ->
    case send_chunk(Socket, Data) of
        {ok, Size} ->
            send_stream_body(Socket, catch_next(Next), Size + SoFar);
        {Error, Size} ->
            {Error, Size + SoFar}
    end;
send_stream_body(_Socket, {error, _}=Error, SoFar) ->
    {Error, SoFar};
send_stream_body(_Socket, Other, SoFar) ->
    {{error, {bad_chunk, Other}}, SoFar}.


send_stream_body_no_chunk(Socket, {Data, done}) when is_list(Data);
                                                     is_binary(Data) ->
    send(Socket, Data);
send_stream_body_no_chunk(Socket, {Data, Next}) when is_list(Data);
                                                     is_binary(Data) ->
    case send(Socket, Data) of
        ok ->
            send_stream_body_no_chunk(Socket, catch_next(Next));
        {error, _}=Error ->
            Error
    end;
send_stream_body_no_chunk(_Socket, {error, _}=Error) ->
    Error;
send_stream_body_no_chunk(_Socket, Other) ->
    {error, {bad_chunk, Other}}.

send_writer_body(Socket, {Encoder, Charsetter, BodyFun}) ->
    put(bytes_written, 0),
    Writer = fun(Data) ->
        {_Result, Size} = send_chunk(Socket, Encoder(Charsetter(Data))),
        put(bytes_written, get(bytes_written) + Size),
        Size
    end,
    Result = try
                 BodyFun(Writer),
                 {SendResult, _} = send_chunk(Socket, <<>>),
                 SendResult
             catch ?STPATTERN(Class:Reason) ->
                 {error, {Class, Reason, ?STACKTRACE}}
             end,
    {Result, get(bytes_written)}.

send_chunk(Socket, Data) ->
    Size = iolist_size(Data),
    {send(Socket, [mochihex:to_hex(Size), <<"\r\n">>, Data, <<"\r\n">>]),
     Size}.

send_ok_response(ReasonPhrase, ReqState) ->
    RD0 = ReqState#wm_reqstate.reqdata,
    {Range, State} = get_range(ReqState),
    case Range of
        X when X =:= undefined; X =:= fail; X =:= ignore ->
            send_response({200, ReasonPhrase}, ReqState);
        Ranges ->
            {PartList, Size} = range_parts(RD0, Ranges),
            case PartList of
                [] -> %% no valid ranges
                    %% could be 416, for now we'll just return 200
                    send_response({200, ReasonPhrase}, ReqState);
                PartList ->
                    {RangeHeaders, RangeBody} =
                        parts_to_body(PartList, Size, ReqState),
                    RespHdrsRD = wrq:set_resp_headers(
                             [{"Accept-Ranges", "bytes"} | RangeHeaders], RD0),
                    RespBodyRD = wrq:set_resp_body(
                                   RangeBody, RespHdrsRD),
                    NewState = State#wm_reqstate{reqdata=RespBodyRD},
                    send_response({206, ReasonPhrase}, NewState)
            end
    end.

send_response(CodeAndPhrase, PassedState=#wm_reqstate{reqdata=RD}) ->
    Body0 = wrq:resp_body(RD),
    {Body,Length} = case Body0 of
        {stream, StreamBody} -> {{stream, StreamBody}, chunked};
        {known_length_stream, Size, StreamBody} -> {{known_length_stream, StreamBody}, Size};
        {stream, Size, Fun} -> {{stream, Fun(0, Size-1)}, chunked};
        {writer, WriteBody} -> {{writer, WriteBody}, chunked};
        _ -> {Body0, iolist_size([Body0])}
    end,
    {Result, FinalLength} =
        case send_head(CodeAndPhrase, PassedState, Length) of
            ok ->
                case wrq:method(RD) of
                    'HEAD' ->
                        {ok, 0}; % no body sent
                    _ ->
                        send_body(Body, PassedState, Length)
                end;
            {error, _}=Error ->
                {Error, 0}
        end,
    RDNotes = maybe_log_stream_error(Result, RD),
    InitLogData = PassedState#wm_reqstate.log_data,
    FinalLogData = InitLogData#wm_log_data{response_code=CodeAndPhrase,
                                           response_length=FinalLength},
    {Result, PassedState#wm_reqstate{
               reqdata=wrq:set_response_code(CodeAndPhrase, RDNotes),
               log_data=FinalLogData}}.

maybe_log_stream_error(ok, ReqData) ->
    %% no error to log
    ReqData;
maybe_log_stream_error({error, Inets}, ReqData) when is_atom(Inets) ->
    %% A network problem occurred. We don't actually need to log this,
    %% because it's likely just that the client disconnected before we
    %% were done sending.
    ReqData;
maybe_log_stream_error({error, Reason}, ReqData) ->
    %% Close the connection, because we don't know if we left it in a
    %% state where the client would understand a new response.
    put(mochiweb_request_force_close, true),
    wrq:add_note(error, {stream_error, Reason}, ReqData).

send_head(CodeAndPhrase, #wm_reqstate{socket=Socket, reqdata=RD}, Length) ->
    send(Socket,
         [make_version(wrq:version(RD)),
          make_code(CodeAndPhrase), <<"\r\n">> |
          make_headers(
            webmachine_status_code:status_code(CodeAndPhrase), Length, RD)]).

send_body({stream, Body}, PassedState, _Length) ->
    send_stream_body(PassedState#wm_reqstate.socket, Body);
send_body({known_length_stream, Body}, PassedState, Length) ->
    {send_stream_body_no_chunk(PassedState#wm_reqstate.socket, Body),
     Length};
send_body({writer, Body}, PassedState, _Length) ->
    send_writer_body(PassedState#wm_reqstate.socket, Body);
send_body(Body, PassedState, Length) ->
    {send(PassedState#wm_reqstate.socket, Body), Length}.

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

expects_continue(PassedState) ->
    case get_header_value("expect", PassedState) of
        {undefined, _} ->
            false;
        {Continue, _} ->
            string:to_lower(Continue) =:= "100-continue"
    end.

sent_continue() ->
    get(webmachine_sent_continue) =:= true.

recv_stream_body(PassedState=#wm_reqstate{reqdata=RD}, MaxHunkSize) ->
    case expects_continue(PassedState) and (not sent_continue()) of
        true ->
            put(webmachine_sent_continue, true),
            send(PassedState#wm_reqstate.socket,
                 [make_version(wrq:version(RD)),
                  make_code(100), <<"\r\n\r\n">>]);
        _ ->
            ok
    end,
    case body_length(PassedState) of
        {unknown_transfer_encoding, X} -> exit({unknown_transfer_encoding, X});
        undefined ->
            record_stream_progress(done),
            {<<>>, done};
        0 ->
            record_stream_progress(done),
            {<<>>, done};
        chunked ->
            start_recv_chunked_body(PassedState#wm_reqstate.socket,
                                    MaxHunkSize);
        Length ->
            recv_unchunked_body(PassedState#wm_reqstate.socket,
                                MaxHunkSize, Length)
    end.

recv_unchunked_body(Socket, MaxHunk, DataLeft) ->
    case MaxHunk >= DataLeft of
        true ->
            case mochiweb_socket:recv(Socket,DataLeft,?IDLE_TIMEOUT) of
                {ok,Data1} ->
                    record_stream_progress(done),
                    {Data1, done};
                {error, Error} ->
                    throw({webmachine_recv_error, Error})
            end;
        false ->
            case mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT) of
                {ok,Data2} ->
                    Next = fun() ->
                                   recv_unchunked_body(Socket, MaxHunk,
                                                       DataLeft-MaxHunk)
                           end,
                    record_stream_progress(Next),
                    {Data2, Next};
                {error, Error} ->
                    throw({webmachine_recv_error, Error})
            end
    end.

start_recv_chunked_body(Socket, MaxHunk) ->
    case read_chunk_length(Socket, false) of
        0 ->
            record_stream_progress(done),
            {<<>>, done};
        ChunkLength ->
            recv_chunked_body(Socket,MaxHunk, ChunkLength)
    end.
recv_chunked_body(Socket, MaxHunk, LeftInChunk) ->
    case MaxHunk >= LeftInChunk of
        true ->
            case mochiweb_socket:recv(Socket,LeftInChunk,?IDLE_TIMEOUT) of
                {ok,Data1} ->
                    Next = fun() ->
                                   start_recv_chunked_body(Socket, MaxHunk)
                           end,
                    record_stream_progress(Next),
                    {Data1, Next};
                {error, Error} ->
                    throw({webmachine_recv_error, Error})
            end;
        false ->
            case mochiweb_socket:recv(Socket,MaxHunk,?IDLE_TIMEOUT) of
                {ok,Data2} ->
                    Next = fun() ->
                                   recv_chunked_body(Socket, MaxHunk,
                                                     LeftInChunk-MaxHunk)
                           end,
                    record_stream_progress(Next),
                    {Data2, Next};
                {error, Error} ->
                    throw({webmachine_recv_error, Error})
            end
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
        {error, Error} ->
            throw({webmachine_recv_error, Error})
    end.

record_stream_progress(Remaining) ->
    put(webmachine_stream_progress, Remaining).

get_stream_progress() ->
    get(webmachine_stream_progress).

maybe_flush_req_body(Req) ->
    case get_stream_progress() of
        done ->
            %% Let mochiweb know that we completed reading the body
            %% that came with the request, or it will close the
            %% socket.
            put(mochiweb_request_recv, true),
            true;
        undefined ->
            case expects_continue(Req) and (not sent_continue()) of
                true ->
                    %% If the request expected continue, but we didn't
                    %% send it, then tell mochiweb to ignore what the
                    %% content length or transfer encoding says about
                    %% a body.
                    put(mochiweb_request_recv, true),
                    true;
                false ->
                    MaxFlush = max_flush_bytes(),
                    case MaxFlush of
                        0 ->
                            %% this server has been configured to
                            %% close connections on clients who send
                            %% requests whose bodies are ignored
                            false;
                        _ ->
                            %% There might be a body sitting out there we
                            %% haven't read - give it a try.
                            ReadSize = erlang:min(65535, MaxFlush),
                            flush_req_body(
                              catch recv_stream_body(Req, ReadSize), MaxFlush)
                    end
            end;
        Next ->
            MaxFlush = max_flush_bytes(),
            case MaxFlush of
                0 ->
                    false;
                _ ->
                    %% request processing stopped in the middle of a stream -
                    %% can we finish it?
                    flush_req_body(catch Next(), MaxFlush)
            end
    end.

max_flush_bytes() ->
    application:get_env(webmachine, max_flush_bytes, 67108864).

flush_req_body({webmachine_recv_error, _}, _) ->
    false;
flush_req_body({_Bytes, done}, _) when is_binary(_Bytes) ->
    put(mochiweb_request_recv, true),
    true;
flush_req_body({Bytes, Next}, MaxFlush) when is_binary(Bytes),
                                             is_function(Next) ->
    Remaining = MaxFlush - size(Bytes),
    case Remaining > 0 of
        true ->
            flush_req_body(catch Next(), Remaining);
        false ->
            false
    end.


get_range(#wm_reqstate{reqdata = RD}=ReqState) ->
    case RD#wm_reqdata.resp_range of
        ignore_request ->
            {ignore, ReqState#wm_reqstate{range=undefined}};
        follow_request ->
            case get_header_value("range", ReqState) of
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
    Boundary = mochihex:to_hex(crypto:strong_rand_bytes(8)),
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

-spec make_code(webmachine_status_code:status_code_optional_phrase()) ->
          iolist().
make_code(CodeAndPhrase) ->
    [integer_to_list(webmachine_status_code:status_code(CodeAndPhrase)),
     " ", webmachine_status_code:reason_phrase(CodeAndPhrase)].

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

-spec update_header_with_content_length(number(), atom(), term()) -> term().
update_header_with_content_length(Code, _Length, RD) when (Code >= 100 andalso Code < 200) orelse
                                                           Code =:= 204 orelse
                                                           Code =:= 304 ->
    mochiweb_headers:make(wrq:resp_headers(RD));
update_header_with_content_length(_Code, Length, RD) ->
    case Length of
        chunked ->
            mochiweb_headers:enter(
              "Transfer-Encoding","chunked",
              mochiweb_headers:make(wrq:resp_headers(RD)));
        _ ->
            mochiweb_headers:enter(
              "Content-Length",integer_to_list(Length),
              mochiweb_headers:make(wrq:resp_headers(RD)))
    end.

make_headers(Code, Length, RD) when is_integer(Code) ->
    Hdrs0 = update_header_with_content_length(Code, Length, RD),
    %% server_name is guaranteed to be set by
    %% webmachine_app:load_default_app_config/0
    {ok, ServerHeader} = application:get_env(webmachine, server_name),
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

get_reqdata(Req) -> call(get_reqdata, Req).

set_reqdata(RD, Req) -> call({set_reqdata, RD}, Req).

socket(Req) -> call(socket, Req).

method(Req) -> call(method, Req).

version(Req) -> call(version, Req).

disp_path(Req) -> call(disp_path, Req).

path(Req) -> call(path, Req).

raw_path(Req) -> call(raw_path, Req).

req_headers(Req) -> call(req_headers, Req).
headers(Req) -> req_headers(Req).

req_body(MaxRevBody, Req) -> call({req_body,MaxRevBody}, Req).
stream_req_body(MaxHunk, Req) -> call({stream_req_body, MaxHunk}, Req).

resp_headers(Req) -> call(resp_headers, Req).
out_headers(Req) -> resp_headers(Req).

get_resp_header(HeaderName, Req) ->
    call({get_resp_header, HeaderName}, Req).
get_out_header(HeaderName, Req) -> get_resp_header(HeaderName, Req).

has_resp_header(HeaderName, Req) ->
    case get_out_header(HeaderName, Req) of
        {undefined, _} -> false;
        {_, _}         -> true
    end.
has_out_header(HeaderName, Req) -> has_resp_header(HeaderName, Req).

has_resp_body(Req) -> call(has_resp_body, Req).
has_response_body(Req) -> has_resp_body(Req).

response_code(Req) -> call(response_code, Req).
set_response_code(Code, ReqState) ->
    set_response_code({set_response_code, Code}, ReqState).

peer(Req) -> call(peer, Req).

range(Req) -> call(range, Req).

req_cookie(Req) -> call(req_cookie, Req).
parse_cookie(Req) -> req_cookie(Req).
get_cookie_value(Key, Req) ->
    {ReqCookie, NewReqState} = req_cookie(Req),
    case lists:keyfind(Key, 1, ReqCookie) of
        false -> {undefined, NewReqState};
        {Key, Value} -> {Value, NewReqState}
    end.

req_qs(Req) -> call(req_qs, Req).
parse_qs(Req) -> req_qs(Req).
get_qs_value(Key, Req) ->
    {ReqQS, NewReqState} = req_qs(Req),
    case lists:keyfind(Key, 1, ReqQS) of
        false -> {undefined, NewReqState};
        {Key, Value} -> {Value, NewReqState}
    end.
get_qs_value(Key, Default, Req) ->
    {ReqQS, NewReqState} = req_qs(Req),
    case lists:keyfind(Key, 1, ReqQS) of
        false -> {Default, NewReqState};
        {Key, Value} -> {Value, NewReqState}
    end.

set_resp_body(Body, Req) -> call({set_resp_body, Body}, Req).
resp_body(Req) -> call(resp_body, Req).
response_body(Req) -> resp_body(Req).

get_req_header(K, Req) -> call({get_req_header, K}, Req).

set_resp_header(K, V, Req) -> call({set_resp_header, K, V}, Req).
add_response_header(K, V, Req) -> set_resp_header(K, V, Req).

set_resp_headers(Hdrs, Req) -> call({set_resp_headers, Hdrs}, Req).
add_response_headers(Hdrs, Req) -> set_resp_headers(Hdrs, Req).

remove_resp_header(K, Req) -> call({remove_resp_header, K}, Req).
remove_response_header(K, Req) -> remove_resp_header(K, Req).

merge_resp_headers(Hdrs, Req) -> call({merge_resp_headers, Hdrs}, Req).
merge_response_headers(Hdrs, Req) -> merge_resp_headers(Hdrs, Req).

append_to_response_body(Data, Req) ->
    call({append_to_response_body, Data}, Req).

do_redirect(Req) -> call(do_redirect, Req).

resp_redirect(Req) -> call(resp_redirect, Req).

get_metadata(Key, Req) -> call({get_metadata, Key}, Req).

set_metadata(Key, Value, Req) -> call({set_metadata, Key, Value}, Req).

get_path_info(Req) -> call(get_path_info, Req).

get_path_info(Key, Req) -> call({get_path_info, Key}, Req).

path_tokens(Req) -> call(path_tokens, Req).
get_path_tokens(Req) -> path_tokens(Req).

app_root(Req) -> call(app_root, Req).
get_app_root(Req) -> app_root(Req).

load_dispatch_data(Bindings, HostTokens, Port, PathTokens,
                   AppRoot, DispPath, Req) ->
    call({load_dispatch_data, Bindings, HostTokens, Port,
          PathTokens, AppRoot, DispPath}, Req).

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

no_content_length_test() ->
    ReqData = #wm_reqdata{req_headers = mochiweb_headers:make([])},
    ?assertEqual(nomatch, re:run(make_headers(100, 56, ReqData), "content-length", [caseless])),
    ?assertEqual(nomatch, re:run(make_headers(204, 56, ReqData), "content-length", [caseless])),
    ?assertMatch({match, _}, re:run(make_headers(200, 56, ReqData), "content-length", [caseless])).

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
