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

-define(PORT, 12001).
-define(URL, "http://localhost:12001/decisioncore").
-define(HTML_CONTENT, "<html><body>Foo</body></html>").

decision_core_test_() ->
    {foreach,
     fun() ->
             %% Spin up webmachine
             application:start(inets),
             WebConfig = [{ip, "0.0.0.0"},
                          {port, ?PORT},
                          {dispatch, [{["decisioncore", '*'],
                                       ?MODULE, []}]}
                         ],
             {ok, Pid0} = webmachine_sup:start_link(),
             {ok, Pid} = webmachine_mochiweb:start(WebConfig),
             link(Pid),
             meck:new(webmachine_resource, [passthrough]),
             {Pid0, Pid}
     end,
     fun({Pid0, Pid1}) ->
             meck:unload(webmachine_resource),
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
      },
      {<<"a post">>,
       fun simple_post/0
      }
     ]}.

get_decision_ids() ->
    [DecisionID || {_, {webmachine_resource, log_d, [DecisionID|_]}, _}
                       <- meck:history(webmachine_resource)].


head_method_not_allowed() ->
    {ok, Result} = httpc:request(head, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 405, "Method Not Allowed"}, _, _}, Result),
    io:format(user, "~p~n", [get_decision_ids()]),
    ok.

simple_get() ->
    {ok, Result} = httpc:request(get, {?URL ++ "/foo", []}, [], []),
    ?assertMatch({{"HTTP/1.1", 200, "OK"}, _, ?HTML_CONTENT}, Result),
    io:format(user, "~p~n", [get_decision_ids()]),
    ok.

simple_put() ->
    Body = {?URL ++ "/bar", [], "binary/octet-stream", <<1,1,2,3,5>>},
    {ok, Result} = httpc:request(put, Body, [], []),
    ?assertMatch({{"HTTP/1.1", 204, "No Content"}, _, _}, Result),
    io:format(user, "~p~n", [get_decision_ids()]),
    ok.

simple_post() ->
    Body = {?URL ++ "/bar", [], "binary/octet-stream", <<1,1,2,3,5>>},
    {ok, Result} = httpc:request(post, Body, [], []),
    io:format(user, "\n<<~p>>\n", [Result]).

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['GET', 'POST', 'PUT'], ReqData, Context}.

to_html(ReqData, Context) ->
    {?HTML_CONTENT, ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"binary/octet-stream", on_put}], ReqData, Context}.

on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

ping(ReqData, State) ->
    {pong, ReqData, State}.

-endif.
