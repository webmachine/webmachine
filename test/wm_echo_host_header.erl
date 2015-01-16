%%
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

-module(wm_echo_host_header).

-export([
         init/1,
         to_html/2,
         parse_body/1
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(Req, State) ->
    HostVal = wrq:get_req_header("host", Req),
    HostTokens = string:join(wrq:host_tokens(Req), "."),
    Body = "Host\t" ++ HostVal ++
        "\nHostTokens\t" ++ HostTokens ++ "\n",
    {Body, Req, State}.

%% Transform the body returned by this resource into a proplist for
%% testing.
parse_body(Body) ->
    Lines = re:split(Body, "\n"),
    [ erlang:list_to_tuple(re:split(Line, "\t")) || Line <- Lines ].
