%% @author Justin Sheehy <justin@basho.com>
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

-module(wrq).
-author('Justin Sheehy <justin@basho.com>').

-export([create/4, create/5,load_dispatch_data/7]).
-export([method/1,scheme/1,version/1,peer/1,disp_path/1,path/1,raw_path/1,
         path_info/1,response_code/1,req_cookie/1,req_qs/1,req_headers/1,
         req_body/1,stream_req_body/2,resp_redirect/1,resp_headers/1,
         resp_body/1,app_root/1,path_tokens/1, host_tokens/1, port/1,
         base_uri/1,sock/1]).
-export([path_info/2,get_req_header/2,do_redirect/2,fresh_resp_headers/2,
         get_resp_header/2,set_resp_header/3,set_resp_headers/2,
         set_disp_path/2,set_req_body/2,set_resp_body/2,set_response_code/2,
         merge_resp_headers/2,remove_resp_header/2,
         append_to_resp_body/2,append_to_response_body/2, set_resp_range/2,
         max_recv_body/1,set_max_recv_body/2,
         get_cookie_value/2,get_qs_value/2,get_qs_value/3,set_peer/2,
         set_sock/2,add_note/3, get_notes/1]).

% @type reqdata(). The opaque data type used for req/resp data structures.
-include("wm_reqdata.hrl").
-include("wm_reqstate.hrl").
-type t() :: #wm_reqdata{}.
-export_type([t/0]).

-type scheme() :: http | https.
-type method() ::'OPTIONS' % from erlang:decode_packet/3
               | 'GET'
               | 'HEAD'
               | 'POST'
               | 'PUT'
               | 'DELETE'
               | 'TRACE'
               | string()
               | binary ().
-type version() :: {non_neg_integer(), non_neg_integer()}.

-export_type([scheme/0, method/0, version/0]).

-spec create(method(),
             version(),
             string(),
             webmachine:headers()) ->
                    t().
create(Method,Version,RawPath,Headers) ->
    create(Method,http,Version,RawPath,Headers).
-spec create(method(),
             scheme(),
             version(),
             string(),
             webmachine:headers()) ->
                    t().
create(Method,Scheme,Version,RawPath,Headers) ->
    create(
      #wm_reqdata{
         method=Method,
         scheme=Scheme,
         version=Version,
         raw_path=RawPath,
         req_headers=Headers
        }).

-spec create(t()) -> t().
create(RD = #wm_reqdata{raw_path=RawPath}) ->
    {Path, _, _} = mochiweb_util:urlsplit_path(RawPath),
    Cookie = case get_req_header("cookie", RD) of
                 undefined -> [];
                 Value -> mochiweb_cookies:parse_cookie(Value)
             end,
    {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
    ReqQS = mochiweb_util:parse_qs(QueryString),
    RD#wm_reqdata{path=Path,req_cookie=Cookie,req_qs=ReqQS}.

load_dispatch_data(PathInfo, HostTokens, Port, PathTokens, AppRoot,
                   DispPath, RD) ->
    RD#wm_reqdata{path_info=PathInfo,host_tokens=HostTokens,
                  port=Port,path_tokens=PathTokens,
                  app_root=AppRoot,disp_path=DispPath}.

-spec method(t()) -> method().
method(_RD = #wm_reqdata{method=Method}) -> Method.

-spec scheme(t()) -> scheme().
scheme(_RD = #wm_reqdata{scheme=Scheme}) -> Scheme.

-spec version(t()) -> version().
version(_RD = #wm_reqdata{version=Version})
  when is_tuple(Version), size(Version) == 2,
     is_integer(element(1,Version)), is_integer(element(2,Version)) -> Version.

peer(_RD = #wm_reqdata{peer=Peer}) when is_list(Peer) -> Peer.

sock(_RD = #wm_reqdata{sock=Sock}) when is_list(Sock) -> Sock.

app_root(_RD = #wm_reqdata{app_root=AR}) when is_list(AR) -> AR.

-spec disp_path(t()) -> string().
disp_path(_RD = #wm_reqdata{disp_path=DP}) when is_list(DP) -> DP.

-spec path(t()) -> string().
path(_RD = #wm_reqdata{path=Path}) when is_list(Path) -> Path.

-spec raw_path(t()) -> string().
raw_path(_RD = #wm_reqdata{raw_path=RawPath}) when is_list(RawPath) -> RawPath.

path_info(_RD = #wm_reqdata{path_info=PathInfo}) -> PathInfo. % dict

path_tokens(_RD = #wm_reqdata{path_tokens=PathT}) -> PathT. % list of strings

host_tokens(_RD = #wm_reqdata{host_tokens=HostT}) -> HostT. % list of strings

-spec port(t()) -> inet:port_number().
port(_RD = #wm_reqdata{port=Port}) -> Port.

-spec response_code(t()) -> non_neg_integer().
response_code(#wm_reqdata{response_code={C,_ReasonPhrase}})
  when is_integer(C) -> C;
response_code(_RD = #wm_reqdata{response_code=C})
  when is_integer(C) -> C.

-spec req_cookie(t()) -> [{string(), string()}].
req_cookie(_RD = #wm_reqdata{req_cookie=C}) when is_list(C) -> C.

-spec req_qs(t()) -> [{string(), string()}].
req_qs(_RD = #wm_reqdata{req_qs=QS}) when is_list(QS) -> QS.

-spec req_headers(t()) -> webmachine:headers().
req_headers(_RD = #wm_reqdata{req_headers=ReqH}) -> ReqH.

req_body(_RD = #wm_reqdata{wm_state=ReqState0,max_recv_body=MRB}) ->
    Req = webmachine_request:new(ReqState0),
    {ReqResp, ReqState} = webmachine_request:req_body(MRB, Req),
    put(tmp_reqstate, ReqState),
    maybe_conflict_body(ReqResp).

stream_req_body(_RD = #wm_reqdata{wm_state=ReqState0}, MaxHunk) ->
    Req = webmachine_request:new(ReqState0),
    {ReqResp, ReqState} = webmachine_request:stream_req_body(MaxHunk, Req),
    put(tmp_reqstate, ReqState),
    maybe_conflict_body(ReqResp).

max_recv_body(_RD = #wm_reqdata{max_recv_body=X}) when is_integer(X) -> X.

set_max_recv_body(X, RD) when is_integer(X) -> RD#wm_reqdata{max_recv_body=X}.

maybe_conflict_body(BodyResponse) ->
    case BodyResponse of
        stream_conflict ->
            exit("wrq:req_body and wrq:stream_req_body conflict");
        {error, req_body_too_large} ->
            exit("request body too large");
        _ ->
            BodyResponse
    end.

-spec resp_redirect(t()) -> boolean().
resp_redirect(#wm_reqdata{resp_redirect=R}) -> R.

-spec resp_headers(t()) -> webmachine_headers:headers().
resp_headers(_RD = #wm_reqdata{resp_headers=RespH}) -> RespH. % mochiheaders

-spec resp_body(t()) -> webmachine:response_body().
resp_body(_RD = #wm_reqdata{resp_body={stream,X}}) -> {stream,X};
resp_body(_RD = #wm_reqdata{resp_body={known_length_stream,X,Y}}) -> {known_length_stream,X,Y};
resp_body(_RD = #wm_reqdata{resp_body={stream,X,Y}}) -> {stream,X,Y};
resp_body(_RD = #wm_reqdata{resp_body={writer,X}}) -> {writer,X};
resp_body(_RD = #wm_reqdata{resp_body=RespB}) when is_binary(RespB) -> RespB;
resp_body(_RD = #wm_reqdata{resp_body=RespB}) -> iolist_to_binary(RespB).

%% --

path_info(Key, RD) when is_atom(Key) ->
    case orddict:find(Key, path_info(RD)) of
        {ok, Value} when is_list(Value); is_integer(Value) ->
            Value; % string (for host or path match)
                   % or integer (for port match)
        error -> undefined
    end.

-spec get_req_header(webmachine_headers:name(), t()) ->
                            undefined | webmachine_headers:value().
get_req_header(HdrName, RD) -> % string->string
    mochiweb_headers:get_value(HdrName, req_headers(RD)).

-spec do_redirect(boolean(), t()) -> t().
do_redirect(Bool, RD) ->  RD#wm_reqdata{resp_redirect=Bool}.

set_peer(P, RD) when is_list(P) -> RD#wm_reqdata{peer=P}. % string

set_sock(S, RD) when is_list(S) -> RD#wm_reqdata{sock=S}. % string

set_disp_path(P, RD) when is_list(P) -> RD#wm_reqdata{disp_path=P}. % string

set_req_body(Body, RD) -> RD#wm_reqdata{req_body=Body}.

set_resp_body(Body, RD) -> RD#wm_reqdata{resp_body=Body}.

set_response_code({Code, _ReasonPhrase}=CodeAndReason, RD) when is_integer(Code) ->
    RD#wm_reqdata{response_code=CodeAndReason};
set_response_code(Code, RD) when is_integer(Code) ->
    RD#wm_reqdata{response_code=Code}.

get_resp_header(HdrName, _RD=#wm_reqdata{resp_headers=RespH}) ->
    mochiweb_headers:get_value(HdrName, RespH).

set_resp_header(K, V, RD=#wm_reqdata{resp_headers=RespH})
  when is_list(K),is_list(V) ->
    RD#wm_reqdata{resp_headers=mochiweb_headers:enter(K, V, RespH)}.

set_resp_headers(Hdrs, RD=#wm_reqdata{resp_headers=RespH}) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:enter(K, V, Acc) end,
    RD#wm_reqdata{resp_headers=lists:foldl(F, RespH, Hdrs)}.

fresh_resp_headers(Hdrs, RD) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:enter(K, V, Acc) end,
    RD#wm_reqdata{resp_headers=lists:foldl(F, mochiweb_headers:empty(), Hdrs)}.

remove_resp_header(K, RD=#wm_reqdata{resp_headers=RespH}) when is_list(K) ->
    RD#wm_reqdata{resp_headers=mochiweb_headers:from_list(
                                 proplists:delete(K,
                                     mochiweb_headers:to_list(RespH)))}.

merge_resp_headers(Hdrs, RD=#wm_reqdata{resp_headers=RespH}) ->
    F = fun({K, V}, Acc) -> mochiweb_headers:insert(K, V, Acc) end,
    NewHdrs = lists:foldl(F, RespH, Hdrs),
    RD#wm_reqdata{resp_headers=NewHdrs}.

-spec append_to_resp_body(iolist() | binary(), t()) -> t().
append_to_resp_body(Data, RD) -> append_to_response_body(Data, RD).

-spec append_to_response_body(iolist() | binary(), t()) -> t().
append_to_response_body(IOList, RD) when is_list(IOList) ->
    append_to_response_body(iolist_to_binary(IOList), RD);
append_to_response_body(Data, RD=#wm_reqdata{resp_body=RespB})
  when is_binary(Data) ->
    Data0 = RespB,
    Data1 = <<Data0/binary,Data/binary>>,
    RD#wm_reqdata{resp_body=Data1}.

-spec set_resp_range(follow_request | ignore_request, t()) -> t().
set_resp_range(RespRange, RD)
  when RespRange =:= follow_request orelse RespRange =:= ignore_request ->
    RD#wm_reqdata{resp_range = RespRange}.

-spec get_cookie_value(string(), t()) -> string() | undefined.
get_cookie_value(Key, RD) when is_list(Key) ->
    case lists:keyfind(Key, 1, req_cookie(RD)) of
        false -> undefined;
        {Key, Value} -> Value
    end.

-spec get_qs_value(string(), t()) -> string() | undefined.
get_qs_value(Key, RD) when is_list(Key) -> % string
    case lists:keyfind(Key, 1, req_qs(RD)) of
        false -> undefined;
        {Key, Value} -> Value
    end.

-spec get_qs_value(string(), string(), t()) -> string().
get_qs_value(Key, Default, RD) when is_list(Key) ->
    case lists:keyfind(Key, 1, req_qs(RD)) of
        false -> Default;
        {Key, Value} -> Value
    end.

-spec add_note(any(), any(), t()) -> t().
add_note(K, V, RD) -> RD#wm_reqdata{notes=[{K, V} | RD#wm_reqdata.notes]}.

-spec get_notes(t()) -> list().
get_notes(RD) -> RD#wm_reqdata.notes.

-spec base_uri(t()) -> string().
base_uri(RD) ->
    Scheme = erlang:atom_to_list(RD#wm_reqdata.scheme),
    Host = string:join(RD#wm_reqdata.host_tokens, "."),
    PortString = port_string(RD#wm_reqdata.scheme, RD#wm_reqdata.port),
    Scheme ++ "://" ++ Host ++ PortString.

-spec port_string(scheme(), inet:port_number()) -> string().
port_string(http, 80) -> "";
port_string(https, 443) -> "";
port_string(_, Port) ->
    ":" ++ erlang:integer_to_list(Port).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_wrq(Method, RawPath, Headers) ->
    make_wrq(Method, http, RawPath, Headers).

make_wrq(Method, Scheme, RawPath, Headers) ->
    create(Method, Scheme, {1,1}, RawPath, mochiweb_headers:from_list(Headers)).

accessor_test() ->
    R0 = make_wrq('GET', "/foo?a=1&b=2", [{"Cookie", "foo=bar"}]),
    R = set_peer("127.0.0.1", R0),
    ?assertEqual('GET', method(R)),
    ?assertEqual({1,1}, version(R)),
    ?assertEqual("/foo", path(R)),
    ?assertEqual("/foo?a=1&b=2", raw_path(R)),
    ?assertEqual([{"a", "1"}, {"b", "2"}], req_qs(R)),
    ?assertEqual({"1", "2"}, {get_qs_value("a", R), get_qs_value("b", R)}),
    ?assertEqual("3", get_qs_value("c", "3", R)),
    ?assertEqual([{"foo", "bar"}], req_cookie(R)),
    ?assertEqual("bar", get_cookie_value("foo", R)),
    ?assertEqual("127.0.0.1", peer(R)).

simple_dispatch_test() ->
    R0 = make_wrq('GET', "/foo?a=1&b=2", [{"Cookie", "foo=bar"}]),
    R1 = set_peer("127.0.0.1", R0),
    {_, _, HostTokens, Port, PathTokens, Bindings, AppRoot, StringPath} =
        webmachine_dispatcher:dispatch("127.0.0.1", "/foo",
                                       [{["foo"], foo_resource, []}], R1),
    R = load_dispatch_data(Bindings,
                           HostTokens,
                           Port,
                           PathTokens,
                           AppRoot,
                           StringPath,
                           R1),
    ?assertEqual(".", app_root(R)),
    ?assertEqual(80, port(R)),
    ?assertEqual("http://127.0.0.1", base_uri(R)).

base_uri_test_() ->
    Make_req =
        fun(Scheme, Host) ->
                R0 = make_wrq('GET', Scheme, "/foo?a=1&b=2",
                              [{"Cookie", "foo=bar"}]),
                R1 = set_peer("127.0.0.1", R0),
                DispatchRule = {["foo"], foo_resource, []},
                {_, _, HostTokens, Port, PathTokens,
                 Bindings, AppRoot,StringPath} =
                    webmachine_dispatcher:dispatch(Host, "/foo", [DispatchRule],
                                                   R1),
                load_dispatch_data(Bindings,
                                   HostTokens,
                                   Port,
                                   PathTokens,
                                   AppRoot,
                                   StringPath,
                                   R1)
        end,
    Tests = [{{http, "somewhere.com:8080"}, "http://somewhere.com:8080"},
             {{https, "somewhere.com:8080"}, "https://somewhere.com:8080"},

             {{http, "somewhere.com"}, "http://somewhere.com"},
             {{https, "somewhere.com"}, "https://somewhere.com"},

             {{http, "somewhere.com:80"}, "http://somewhere.com"},
             {{https, "somewhere.com:443"}, "https://somewhere.com"},
             {{https, "somewhere.com:80"}, "https://somewhere.com:80"},
             {{http, "somewhere.com:443"}, "http://somewhere.com:443"}],
    [ ?_assertEqual(Expect, base_uri(Make_req(Scheme, Host)))
      || {{Scheme, Host}, Expect} <- Tests ].

-endif.
