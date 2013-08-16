%% @author Macneil Shonle <mshonle@basho.com>
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

%% These utility functions can be used to set up Webmachine on an ephemeral
%% port that can be interacted with over HTTP requests. These utilities are
%% useful for writing integration tests, which test all of the Webmachine
%% stack, as a complement to the unit-testing and mocking strategies.
-module(wm_integration_test_util).

-ifdef(TEST).
-export([start/3, stop/1]).
-export([get_port/1, url/1, url/2]).

-export([init/1, service_available/2]).

-include("webmachine.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EPHEMERAL_PORT, 0).

-record(integration_state, {
          webmachine_sup,
          mochi_serv,
          port,
          resource_name,
          base_url = "http://localhost"
         }).

%% Returns an integration_state record that should be passed to get_port/1 and
%% stop/1. Starts up webmachine and the mochiweb server with the given name,
%% ip, and dispatch list. Communication is set up on an ephemeral port, which
%% can be accessed via get_port/1.
start(Name, IP, DispatchList) ->
    cleanup_previous_runs(),
    error_logger:tty(false),
    application:start(inets),
    {ok, WebmachineSup} = webmachine_sup:start_link(),
    WebConfig = [{name, Name}, {ip, IP}, {port, ?EPHEMERAL_PORT},
                 {dispatch, DispatchList}],
    {ok, MochiServ} = webmachine_mochiweb:start(WebConfig),
    link(MochiServ),
    Port = mochiweb_socket_server:get(MochiServ, port),
    #integration_state{webmachine_sup=WebmachineSup,
                       mochi_serv=MochiServ,
                       port=Port,
                       resource_name=Name}.

%% Receives the integration_state record returned by start/3
stop(Context) ->
    stop_supervisor(Context#integration_state.webmachine_sup),
    MochiServ = Context#integration_state.mochi_serv,
    {registered_name, MochiName} = process_info(MochiServ, registered_name),
    webmachine_mochiweb:stop(MochiName),
    stop_supervisor(MochiServ),
    application:stop(inets).

%% Receives the integration_state record returned by start_webmachine, returns
%% the port to use to communicate with Webmachine over HTTP.
get_port(Context) ->
    Context#integration_state.port.

%% Returns a URL to use for an HTTP request to communicate with Webmachine.
url(Context) ->
    Port = get_port(Context),
    Name = Context#integration_state.resource_name,
    Chars = io_lib:format("http://localhost:~b/~s", [Port, Name]),
    lists:flatten(Chars).

%% Returns a URL extended with the given path to use for an HTTP request to
%% communicate with Webmachine
url(Context, Path) ->
    url(Context) ++ "/" ++ Path.

stop_supervisor(Sup) ->
    unlink(Sup),
    exit(Sup, kill),
    wait_for_pid(Sup).

%% Wait for a pid to exit -- Copied from riak_kv_test_util.erl
wait_for_pid(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mref, process, _, _} ->
            ok
    after
        5000 ->
            {error, didnotexit, Pid, erlang:process_info(Pid)}
    end.

%% Sometimes the previous clean up didn't work, so we try again
cleanup_previous_runs() ->
    RegNames = [webmachine_sup, webmachine_router, webmachine_logger,
                webmachine_log_event, webmachine_logger_watcher_sup],
    UndefinedsOrPids = [whereis(RegName) || RegName <- RegNames],
    [wait_for_pid(Pid) || Pid <- UndefinedsOrPids, Pid /= undefined].


%%
%% EXAMPLE TEST CASE
%%
%% init and service_available are simple resource functions for a service that
%% is unavailable (a 503 error)
init([]) ->
    {ok, undefined}.

service_available(ReqData, Context) ->
    {false, ReqData, Context}.

integration_tests() ->
    [fun service_unavailable_test/1].

integration_test_() ->
    {foreach,
     %% Setup
     fun() ->
             DL = [{[atom_to_list(?MODULE), '*'], ?MODULE, []}],
             Ctx = wm_integration_test_util:start(?MODULE, "0.0.0.0", DL),
             Ctx
     end,
     %% Cleanup
     fun(Ctx) ->
             wm_integration_test_util:stop(Ctx)
     end,
     %% Test functions provided with context from setup
     [fun(Ctx) ->
              {spawn, {with, Ctx, integration_tests()}}
      end]}.

service_unavailable_test(Ctx) ->
    URL = wm_integration_test_util:url(Ctx, "foo"),
    {ok, Result} = httpc:request(head, {URL, []}, [], []),
    ?assertMatch({{"HTTP/1.1", 503, "Service Unavailable"}, _, _}, Result),
    ok.

-endif.
