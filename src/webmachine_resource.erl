%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
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

-module(webmachine_resource).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-behaviour(gen_server).
-export([behaviour_info/1]).
-export([start_link/1, start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([do/2,log_d/2]).
-define(TIMEOUT, 150000). %% this is just the internal gen_server timeout
-record(state, {mod, modstate, modexports, trace=false}).

behaviour_info(callbacks) -> [{ping, 2}].

default(ping) ->
    no_default;
default(service_available) ->
    true;
default(resource_exists) ->
    true;
default(auth_required) ->
    true;
default(is_authorized) ->
    true;
default(forbidden) ->
    false;
default(allow_missing_post) ->
    false;
default(malformed_request) ->
    false;
default(uri_too_long) ->
    false;
default(known_content_type) ->
    true;
default(valid_content_headers) ->
    true;
default(valid_entity_length) ->
    true;
default(options) ->
    [];
default(allowed_methods) ->
    ['GET', 'HEAD'];
default(known_methods) ->
    ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS'];
default(content_types_provided) ->
    [{"text/html", to_html}];
default(content_types_accepted) ->
    [];
default(delete_resource) ->
    false;
default(delete_completed) ->
    true;
default(post_is_create) ->
    false;
default(create_path) ->
    undefined;
default(process_post) ->
    false;
default(language_available) ->
    true;
default(charsets_provided) ->
    no_charset; % this atom causes charset-negotation to short-circuit
% the default setting is needed for non-charset responses such as image/png
%    an example of how one might do actual negotiation
%    [{"iso-8859-1", fun(X) -> X end}, {"utf-8", make_utf8}];
default(encodings_provided) ->
    [{"identity", fun(X) -> X end}];
% this is handy for auto-gzip of GET-only resources:
%    [{"identity", fun(X) -> X end}, {"gzip", fun(X) -> zlib:gzip(X) end}];
default(variances) ->
    [];
default(is_conflict) ->
    false;
default(multiple_choices) ->
    false;
default(previously_existed) ->
    false;
default(moved_permanently) ->
    false;
default(moved_temporarily) ->
    false;
default(last_modified) ->
    undefined;
default(expires) ->
    undefined;
default(generate_etag) ->
    undefined;
default(finish_request) ->
    true;
default(_) ->
    no_default.
          
start_link(Mod) ->
    start_link(Mod, []).

start_link(Mod, Args) ->
    gen_server:start_link(?MODULE, [Mod, Args], []).

%% @private
init([Mod, Args]) ->
    case Mod:init(Args) of
	{ok, State} ->
	    {ok, #state{mod=Mod, modstate=State, 
			modexports=dict:from_list(Mod:module_info(exports))}};
        {{trace, Dir}, State} ->
            {ok, File} = open_log_file(Dir, Mod),
            log_decision(File, v3b14),
            log_call(File, attempt, Mod, init, Args),
            log_call(File, result, Mod, init, {{trace, Dir}, State}),
            {ok, #state{mod=Mod, modstate=State, trace=File,
			modexports=dict:from_list(Mod:module_info(exports))}};
	_ ->
	    {stop, bad_init_arg}
    end.

do(Fun, ReqProps) when is_atom(Fun) andalso is_list(ReqProps) ->
    gen_server:call(proplists:get_value(pid, ReqProps), 
		    {do, Fun, ReqProps}, ?TIMEOUT).

log_d(DecisionID, ReqProps) ->
    gen_server:cast(proplists:get_value(pid, ReqProps), 
		    {log_d, DecisionID}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% @private
handle_call({do, Fun, ReqProps}, _From, 
	    State=#state{modstate=ModState, mod=Mod}) ->
    Req = proplists:get_value(req, ReqProps),
    RD0 = Req:get_reqdata(),
    {Reply, RD1, NewModState} =
        handle_wm_call({Fun, ReqProps, RD0}, Mod, ModState, State),
    case Reply of
        {error, Err} -> {reply, Err, State};
        _ -> 
            Req:set_reqdata(RD1),
            {reply,Reply,State#state{modstate=NewModState}}
    end.

%% @private
handle_cast(stop, State) ->
    close_log_file(State#state.trace),
    {stop, normal, State};
handle_cast({log_d, DecisionID}, State) ->
    case State#state.trace of
        false -> nop;
        File -> log_decision(File, DecisionID)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    close_log_file(State#state.trace),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_wm_call({Fun, _ReqProps, ReqData}, Mod, ModState, State) ->
    ModExports = State#state.modexports,
    case default(Fun) of
        no_default ->
            resource_call(Mod, Fun, ReqData, ModState, State#state.trace);
        Default ->
            case dict:is_key(Fun, ModExports) of
                true ->
                    resource_call(Mod, Fun, ReqData, ModState,
                                  State#state.trace);
                false ->
                    if is_pid(State#state.trace) ->
                            log_call(State#state.trace,
                                     not_exported,
                                     Mod, Fun, [ReqData, ModState]);
                       true -> ok
                    end,
                    {Default, ReqData, ModState}
            end
    end.

resource_call(M, F, ReqData, ModState, false) ->
    try
        apply(M, F, [ReqData, ModState])
    catch C:R ->
	    Reason = {C, R, erlang:get_stacktrace()},
            {{error, Reason}, ReqData, ModState}
    end;
resource_call(M, F, ReqData, ModState, File) ->
    log_call(File, attempt, M, F, [ReqData, ModState]),
    Result = resource_call(M, F, ReqData, ModState, false),
    log_call(File, result, M, F, Result),
    Result.

log_call(File, Type, M, F, Data) ->
    io:format(File,
              "{~p, ~p, ~p,~n ~p}.~n",
              [Type, M, F, Data]).

log_decision(File, DecisionID) ->
    io:format(File, "{decision, ~p}.~n", [DecisionID]).

open_log_file(Dir, Mod) ->
    Now = {_,_,US} = now(),
    {{Y,M,D},{H,I,S}} = calendar:now_to_universal_time(Now),
    Filename = io_lib:format(
                 "~s/~p-~4..0B-~2..0B-~2..0B"
                 "-~2..0B-~2..0B-~2..0B.~6..0B.wmtrace",
                 [Dir, Mod, Y, M, D, H, I, S, US]),
    file:open(Filename, [write]).

close_log_file(File) when is_pid(File) ->
    file:close(File);
close_log_file(_) ->
    ok.
