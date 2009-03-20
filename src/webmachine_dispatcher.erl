%% @author Robert Ahrens <rahrens@basho.com>
%% @copyright 2008 Basho Technologies
%% @copyright 2007-2008 Basho Technologies
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

%% @doc gen_server for dispatching page requests from webmachine.

-module(webmachine_dispatcher).
-author('Robert Ahrens <rahrens@basho.com>').
-behaviour(gen_server).

-define(SEPARATOR, $\/).
-define(MATCH_ALL, '*').

%% API
-export([start_link/0, start_link/1, stop/0, dispatch/1,
         set_dispatch_list/1, get_dispatch_list/0]).
-export([set_error_handler/1, get_error_handler/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {dispatchlist=[], error_handler}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the dispatch server
start_link() ->
    start_link([]).
start_link(DispatchList) when is_list(DispatchList) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DispatchList], []).

stop() ->
    gen_server:cast(?MODULE, stop).

dispatch(Req) ->
    gen_server:call(?MODULE, {dispatch, Req}).

set_dispatch_list(List) when is_list(List) ->
    gen_server:cast(?MODULE, {set_dispatch_list, List}).

get_dispatch_list() ->
    gen_server:call(?MODULE, get_dispatch_list).

set_error_handler(ErrorHandlerMod) when is_atom(ErrorHandlerMod) ->
    gen_server:cast(?MODULE, {set_error_handler, ErrorHandlerMod}).

get_error_handler() ->
    gen_server:call(?MODULE, get_error_handler).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
init([DispatchList]) ->
    {ok, #state{dispatchlist=DispatchList}}.

%% @spec %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call({dispatch, Req}, From, State) ->
    spawn(fun() -> binding_dispatch(State#state.dispatchlist, Req, From) end),
    {noreply, State};
handle_call(get_error_handler, _From, State) ->
    {reply, State#state.error_handler, State};
handle_call(get_dispatch_list, _From, State) ->
    {reply, State#state.dispatchlist, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({set_error_handler, ErrorHandlerMod}, State) ->
    {noreply, State#state{error_handler=ErrorHandlerMod}};
handle_cast({set_dispatch_list, List}, State) when is_list(List) ->
    {noreply, State#state{dispatchlist=List}}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

bind_path([], [], Bindings, Depth) ->
    {ok, [], Bindings, Depth};
bind_path([?MATCH_ALL], PathRest, Bindings, Depth) when is_list(PathRest) ->
    {ok, PathRest, Bindings, Depth + length(PathRest)};
bind_path(_, [], _, _) ->
    fail;
bind_path([Token|Rest], [Match|PathRest], Bindings, Depth) when is_atom(Token) ->
    bind_path(Rest, PathRest, [{Token, Match}|Bindings], Depth + 1);
bind_path([Token|Rest], [Token|PathRest], Bindings, Depth) ->
    bind_path(Rest, PathRest, Bindings, Depth + 1);
bind_path(_, _, _, _) ->
    fail.

try_binding([], PathTokens, _) ->
    {no_dispatch_match, PathTokens};
try_binding([{PathSchema, Mod, Props}|Rest], PathTokens, ExtraDepth) ->
    case bind_path(PathSchema, PathTokens, [], 0) of
	    {ok, Remainder, Bindings, Depth} ->
	        {Mod, Props, Remainder, Bindings, calculate_app_root(Depth + ExtraDepth), reconstitute(Remainder)};
	    fail -> 
	        try_binding(Rest, PathTokens, ExtraDepth)
    end.

reconstitute([]) ->
     "";
reconstitute(UnmatchedTokens) ->
    string:join(UnmatchedTokens, [?SEPARATOR]).

binding_dispatch(DispatchList, Req, Client) ->
    PathAsString = Req:path(),
    Path = string:tokens(PathAsString, [?SEPARATOR]),
    % URIs that end with a trailing slash are implicitly one token
    % "deeper" than we otherwise might think as we are "inside"
    % a directory named by the last token.
    ExtraDepth = case lists:last(PathAsString) == ?SEPARATOR of
		     true -> 1;
		     _ -> 0
		 end,
    gen_server:reply(Client, try_binding(DispatchList, Path, ExtraDepth)).

calculate_app_root(1) ->
    ".";
calculate_app_root(N) when N > 1 ->
    string:join(lists:duplicate(N, ".."), [?SEPARATOR]).
