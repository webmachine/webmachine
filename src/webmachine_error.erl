%% Copyright (c) 2011-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Default HTTP error reasons for webmachine error responsesf

-module(webmachine_error).

-export([reason/1]).

-spec reason(pos_integer()) -> string().
reason(400) ->
    "Bad Request";
reason(401) ->
    "Unauthorized";
reason(402) ->
    "Payment Requested";
reason(403) ->
    "Forbidden";
reason(404) ->
    "Not Found";
reason(405) ->
    "Method Not Allowed";
reason(406) ->
    "Not Acceptable";
reason(407) ->
    "Proxy Authentication Required";
reason(408) ->
    "Request Timeout";
reason(409) ->
    "Conflict";
reason(410) ->
    "Gone";
reason(411) ->
    "Length Required";
reason(412) ->
    "Precondition Failed";
reason(413) ->
    "Request Entity Too Large";
reason(414) ->
    "Request-URI Too Long";
reason(415) ->
    "Unsupported Media Type";
reason(416) ->
    "Request Range Not Satisfiable";
reason(417) ->
    "Expectation Failed";
reason(500) ->
    "Internal Server Error";
reason(501) ->
    "Not Implemented";
reason(502) ->
    "Bad Gateway";
reason(503) ->
    "Service Unavailable";
reason(504) ->
    "Gateway Timeout";
reason(505) ->
    "HTTP Version Not Supported";
reason(Code) when Code >= 400, Code < 500 ->
    "Client Error";
reason(Code) when Code >= 500 ->
    "Server Error".
