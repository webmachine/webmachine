%% @copyright 2007-2013 Basho Technologies
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

-module(decision_core_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(port, 12001).
-define(url, "http://localhost:12001/decisioncore").

-define(html_content, "<html><body>Foo</body></html>").

decision_core_test_() ->
    {setup,
     fun() ->
             %% Spin up webmachine
             application:start(inets),
             WebConfig = [{ip, "0.0.0.0"},
                          {port, ?port},
                          {dispatch, [{["decisioncore", '*'],
                                       ?MODULE, []}]}
                         ],
             {ok, Pid0} = webmachine_sup:start_link(),
             {ok, Pid} = webmachine_mochiweb:start(WebConfig),
             link(Pid),
             {Pid0, Pid}
     end,
     fun({Pid0, Pid1}) ->
             %% clean up
             unlink(Pid0),
             exit(Pid0, kill),
             unlink(Pid1),
             exit(Pid1, kill),
             application:stop(inets)
     end,
     [
      {<<"405 method not allowed">>,
       fun head_method_not_allowed/0
      },
      {<<"200 from a get">>,
       fun simple_get/0
      },
      {<<"204 from a put">>,
       fun simple_put/0
      }
     ]}.

head_method_not_allowed() ->
    {ok, Result} = httpc:request(head, {?url, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 405, "Method Not Allowed"}, _, _}, Result).

simple_get() ->
    {ok, Result} = httpc:request(get, {?url, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, ?html_content}, Result).

simple_put() ->
    Body = {?url, [], "binary/octet-stream", <<1,1,2,3,5>>},
    {ok, Result} = httpc:request(put, Body, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result).

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET', 'PUT'], ReqData, Context}.

to_html(ReqData, Context) ->
    {?html_content, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"binary/octet-stream", on_put}], ReqData, Context}.

on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

-endif.
