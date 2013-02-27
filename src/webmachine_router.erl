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
         add_route/2,
         remove_route/1,
         remove_route/2,
         remove_resource/1,
         remove_resource/2,
         get_routes/0,
         get_routes/1,
         init_routes/1,
         init_routes/2
        ]).

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
    add_route(default, Route).

add_route(Name, Route) ->
    gen_server:call(?SERVER, {add_route, Name, Route}, infinity).

%% @spec remove_route(hostmatchterm() | pathmatchterm()) -> ok
%% @doc Removes a route from webamchine's route table. The route
%%      route must be properly formatted
%% @see add_route/2
remove_route(Route) ->
    remove_route(default, Route).

remove_route(Name, Route) ->
    gen_server:call(?SERVER, {remove_route, Name, Route}, infinity).

%% @spec remove_resource(atom()) -> ok
%% @doc Removes all routes for a specific resource module.
remove_resource(Resource) when is_atom(Resource) ->
    remove_resource(default, Resource).

remove_resource(Name, Resource) when is_atom(Resource) ->
    gen_server:call(?SERVER, {remove_resource, Name, Resource}, infinity).

%% @spec get_routes() -> [{[], res, []}]
%% @doc Retrieve a list of routes and resources set in webmachine's
%%      route table.
get_routes() ->
    get_routes(default).

get_routes(Name) ->
    get_dispatch_list(Name).

%% @spec init_routes() -> ok
%% @doc Set the default routes, unless the routing table isn't empty.
init_routes(DefaultRoutes) ->
    init_routes(default, DefaultRoutes).

init_routes(Name, DefaultRoutes) ->
    gen_server:call(?SERVER, {init_routes, Name, DefaultRoutes}, infinity).

%% @spec start_link() -> {ok, pid()} | {error, any()}
%% @doc Starts the webmachine_router gen_server.
start_link() ->
    %% We expect to only be called from webmachine_sup
    %%
    %% Set up the ETS configuration table.
    try ets:new(?MODULE, [named_table, public, set, {keypos, 1},
                {read_concurrency, true}]) of
        _Result ->
            ok
    catch
        error:badarg ->
            %% The table already exists, which is fine. The webmachine_router
            %% probably crashed and this is a restart.
            ok
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init([]) ->
    {ok, undefined}.

%% @private
handle_call({remove_resource, Name, Resource}, _From, State) ->
    DL = filter_by_resource(Resource, get_dispatch_list(Name)),
    {reply, set_dispatch_list(Name, DL), State};

handle_call({remove_route, Name, Route}, _From, State) ->
    DL = [D || D <- get_dispatch_list(Name),
               D /= Route],
    {reply, set_dispatch_list(Name, DL), State};

handle_call({add_route, Name, Route}, _From, State) ->
    DL = [Route|[D || D <- get_dispatch_list(Name),
                      D /= Route]],
    {reply, set_dispatch_list(Name, DL), State};

handle_call({init_routes, Name, DefaultRoutes}, _From, State) ->
    %% if the table lacks a dispatch_list row, set it
    ets:insert_new(?MODULE, {Name, DefaultRoutes}),
    {reply, ok, State};

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

%% @doc Remove any dispatch rule that directs requests to `Resource'
filter_by_resource(Resource, Dispatch) ->
    lists:foldr(filter_by_resource(Resource), [], Dispatch).

filter_by_resource(Resource) ->
    fun({_, R, _}, Acc) when R == Resource -> % basic dispatch
            Acc;
       ({_, _, R, _}, Acc) when R == Resource -> % guarded dispatch
            Acc;
       ({Host, Disp}, Acc) -> % host-based dispatch
            [{Host, filter_by_resource(Resource, Disp)}|Acc];
       (Other, Acc) -> % dispatch not mentioning this resource
            [Other|Acc]
    end.

get_dispatch_list(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Dispatch}] ->
            Dispatch;
        [] ->
            []
    end.

set_dispatch_list(Name, DispatchList) ->
    true = ets:insert(?MODULE, {Name, DispatchList}),
    ok.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_remove_route_test() ->
    {ok, Pid} = webmachine_router:start_link(),
    unlink(Pid),
    PathSpec = {["foo"], foo, []},
    webmachine_router:add_route(PathSpec),
    [PathSpec] = get_routes(),
    webmachine_router:remove_route(PathSpec),
    [] = get_routes(),
    exit(Pid, kill).

add_remove_resource_test() ->
    {ok, Pid} = webmachine_router:start_link(),
    unlink(Pid),
    PathSpec1 = {["foo"], foo, []},
    PathSpec2 = {["bar"], foo, []},
    PathSpec3 = {["baz"], bar, []},
    PathSpec4 = {["foo"], fun(_) -> true end, foo, []},
    PathSpec5 = {["foo"], {webmachine_router, test_guard}, foo, []},
    webmachine_router:add_route(PathSpec1),
    webmachine_router:add_route(PathSpec2),
    webmachine_router:add_route(PathSpec3),
    webmachine_router:remove_resource(foo),
    [PathSpec3] = get_routes(),
    webmachine_router:add_route(PathSpec4),
    webmachine_router:remove_resource(foo),
    [PathSpec3] = get_routes(),
    webmachine_router:add_route(PathSpec5),
    webmachine_router:remove_resource(foo),
    [PathSpec3] = get_routes(),
    webmachine_router:remove_route(PathSpec3),
    [begin
         PathSpec = {"localhost", [HostPath]},
         webmachine_router:add_route(PathSpec),
         webmachine_router:remove_resource(foo),
         [{"localhost", []}] = get_routes(),
         webmachine_router:remove_route({"localhost", []})
     end || HostPath <- [PathSpec1, PathSpec4, PathSpec5]],
    exit(Pid, kill).

no_dupe_path_test() ->
    {ok, Pid} = webmachine_router:start_link(),
    unlink(Pid),
    PathSpec = {["foo"], foo, []},
    webmachine_router:add_route(PathSpec),
    webmachine_router:add_route(PathSpec),
    [PathSpec] = get_routes(),
    exit(Pid, kill).

supervisor_restart_keeps_routes_test() ->
    {ok, Pid} = webmachine_router:start_link(),
    unlink(Pid),
    PathSpec = {["foo"], foo, []},
    webmachine_router:add_route(PathSpec),
    [PathSpec] = get_routes(),
    OldRouter = whereis(webmachine_router),
    exit(whereis(webmachine_router), kill),
    timer:sleep(100),
    NewRouter = whereis(webmachine_router),
    ?assert(OldRouter /= NewRouter),
    [PathSpec] = get_routes(),
    exit(Pid, kill).

-endif.
