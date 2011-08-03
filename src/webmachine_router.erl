%% @author Kevin A. Smith <ksmith@basho.com>
%% @copyright 2007-2010 Basho Technologies
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

%% @doc Module to add and remove dynamic routes to webmachine's routing
%%      table. Dynamic routes are not persistent between executions of
%%      a webmachine application. They will need to be added to the
%%      the table each time webmachine restarts.
-module(webmachine_router).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_route/1,
         remove_route/1,
         remove_resource/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @type hostmatchterm() = {hostmatch(), [pathmatchterm()]}.
% The dispatch configuration contains a list of these terms, and the
% first one whose host and one pathmatchterm match is used.

%% @type pathmatchterm() = {[pathterm()], matchmod(), matchopts()}.
% The dispatch configuration contains a list of these terms, and the
% first one whose list of pathterms matches the input path is used.

%% @type pathterm() = '*' | string() | atom().
% A list of pathterms is matched against a '/'-separated input path.
% The '*' pathterm matches all remaining tokens.
% A string pathterm will match a token of exactly the same string.
% Any atom pathterm other than '*' will match any token and will
% create a binding in the result if a complete match occurs.

%% @type matchmod() = atom().
% This atom, if present in a successful matchterm, will appear in
% the resulting dispterm.  In Webmachine this is used to name the
% resource module that will handle the matching request.

%% @type matchopts() = [term()].
% This term, if present in a successful matchterm, will appear in
% the resulting dispterm.  In Webmachine this is used to provide
% arguments to the resource module handling the matching request.

-define(SERVER, ?MODULE).

%% @spec add_route(hostmatchterm() | pathmatchterm()) -> ok
%% @doc Adds a route to webmachine's route table. The route should
%%      be the format documented here:
%% http://bitbucket.org/justin/webmachine/wiki/DispatchConfiguration
add_route(Route) ->
    gen_server:call(?SERVER, {add_route, Route}, infinity).

%% @spec remove_route(hostmatchterm() | pathmatchterm()) -> ok
%% @doc Removes a route from webamchine's route table. The route
%%      route must be properly formatted
%% @see add_route/2
remove_route(Route) ->
    gen_server:call(?SERVER, {remove_route, Route}, infinity).

%% @spec remove_resource(atom()) -> ok
%% @doc Removes all routes for a specific resource module.
remove_resource(Resource) when is_atom(Resource) ->
    gen_server:call(?SERVER, {remove_resource, Resource}, infinity).

%% @spec start_link() -> {ok, pid()} | {error, any()}
%% @doc Starts the webmachine_router gen_server.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init([]) ->
  {ok, []}.

%% @private
handle_call(get_routes, _From, State) ->
    {reply, get_dispatch_list(), State};

handle_call({remove_resource, Resource}, _From, State) ->
    DL = [D || D <- get_dispatch_list(),
               filter_by_resource(D, Resource)],
    {reply, set_dispatch_list(DL), State};

handle_call({remove_route, Route}, _From, State) ->
    DL = [D || D <- get_dispatch_list(),
               D /= Route],
    {reply, set_dispatch_list(DL), State};

handle_call({add_route, Route}, _From, State) ->
    DL = [Route|[D || D <- get_dispatch_list(),
                      D /= Route]],
    {reply, set_dispatch_list(DL), State};

handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
filter_by_resource({_, {_, Resource, _}}, Resource) ->
    false;
filter_by_resource({_, {_, _, _}}, _Resource) ->
    true;
filter_by_resource({_, Resource, _}, Resource) ->
    false;
filter_by_resource({_, _, _}, _Resource) ->
    true.

get_dispatch_list() ->
    case application:get_env(webmachine, dispatch_list) of
        {ok, Dispatch} ->
            Dispatch;
        undefined ->
            []
    end.

set_dispatch_list(DispatchList) ->
    ok = application:set_env(webmachine, dispatch_list, DispatchList),
    ok.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% For unit tests only
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

get_routes() ->
    gen_server:call(?SERVER, get_routes, infinity).


add_remove_route_test() ->
    application:set_env(webmachine, dispatch_list, []),
    {ok, Pid} = start(),
    PathSpec = {["foo"], foo, []},
    webmachine_router:add_route(PathSpec),
    [PathSpec] = get_routes(),
    webmachine_router:remove_route(PathSpec),
    [] = get_routes(),
    exit(Pid, kill).

add_remove_resource_test() ->
    application:set_env(webmachine, dispatch_list, []),
    {ok, Pid} = start(),
    PathSpec1 = {["foo"], foo, []},
    PathSpec2 = {["bar"], foo, []},
    PathSpec3 = {["baz"], bar, []},
    webmachine_router:add_route(PathSpec1),
    webmachine_router:add_route(PathSpec2),
    webmachine_router:add_route(PathSpec3),
    webmachine_router:remove_resource(foo),
    [PathSpec3] = get_routes(),
    exit(Pid, kill).

no_dupe_path_test() ->
    application:set_env(webmachine, dispatch_list, []),
    {ok, Pid} = start(),
    PathSpec = {["foo"], foo, []},
    webmachine_router:add_route(PathSpec),
    webmachine_router:add_route(PathSpec),
    [PathSpec] = get_routes(),
    exit(Pid, kill).

-endif.
