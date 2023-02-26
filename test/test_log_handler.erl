%% A simple log handler to support testing logging outcomes.
%%
%% This log handler just holds logs it receives in an in-memory list
%% until asked for them. Retrieve accumulated logs by calling either
%% `get_logs/0` or `wait_for_logs/2`.
-module(test_log_handler).
-export([
         init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         get_logs/0,
         clear_logs/0,
         wait_for_logs/2
        ]).
-record(state, {logs, waiter}).

-include_lib("eunit/include/eunit.hrl").

%% GEN EVENT CALLBACKS

init([]) ->
    {ok, #state{logs=[], waiter=undefined}}.

handle_call({get_logs, Waiter}, State) ->
    case State#state.logs of
        [] ->
            Ref = make_ref(),
            {ok, {wait, Ref}, State#state{waiter={Waiter, Ref}}};
        Logs ->
            {ok, Logs, State#state{logs=[]}}
    end.

handle_event({log_error, _}=Log, #state{logs=OldLogs}=State) ->
    {ok, maybe_send_logs(State#state{logs=[Log|OldLogs]})};
handle_event({log_error, _, _, _}=Log, #state{logs=OldLogs}=State) ->
    {ok, maybe_send_logs(State#state{logs=[Log|OldLogs]})};
handle_event({log_access, _}=Log, #state{logs=OldLogs}=State) ->
    {ok, maybe_send_logs(State#state{logs=[Log|OldLogs]})};
handle_event({log_info, _}=Log, #state{logs=OldLogs}=State) ->
    {ok, maybe_send_logs(State#state{logs=[Log|OldLogs]})};
handle_event(_Event, State) ->
    ?debugMsg("unknown logged!"),
    ?debugVal(_Event),
    {ok, State}.

maybe_send_logs(#state{logs=Logs, waiter={Waiter, Ref}}) ->
    Waiter ! {Ref, Logs},
    #state{logs=[], waiter=undefined};
maybe_send_logs(State) ->
    State.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TEST QUERY API

%% Get the accumulated logs. If no logs have been received by the
%% handler, a `{wait, ref()}` tuple is returned instead, and a
%% `{ref(), [Log]}` message will be sent when the next log is
%% received. The accumulated logs are cleared from the handler.
-spec get_logs() -> [any()] | {wait, reference()}.
get_logs() ->
    webmachine_log:call(test_log_handler, {get_logs, self()}).

%% Call `get_logs` until it tells us to wait.
-spec clear_logs() -> {wait, reference()}.
clear_logs() ->
    case get_logs() of
        {wait, _}=WaitRef ->
            WaitRef;
        _OldLogs ->
            clear_logs()
    end.

%% Keep reading logs until the specified amount of each type have been
%% read. The amount of each type is specified by a map passed as the
%% second parameter. The keys of the map should be the same as the
%% first element of the log event (i.e. log_info, log_error, or
%% log_access), and the values should be the number of logs of that
%% type to wait for.
wait_for_logs(RefOrLogs, TypeCounts) ->
    wait_for_logs(RefOrLogs, TypeCounts, []).

wait_for_logs({wait, Ref}, TypeCounts, OldLogs) ->
    receive {Ref, NewLogs} -> wait_for_logs(NewLogs, TypeCounts, OldLogs) end;
wait_for_logs(NewLogs, TypeCounts, OldLogs) ->
    {NewCounts, Logs} =
        lists:foldl(
          fun(Log, {AccCounts, AccLogs}) ->
              TypeCount = max(0, maps:get(element(1, Log), AccCounts, 0) - 1),
              NewCounts = AccCounts#{element(1, Log) => TypeCount},
              {NewCounts, [Log|AccLogs]}
          end,
          {TypeCounts, OldLogs},
          NewLogs),
    case maps:fold(fun(_, V, A) -> V+A end, 0, NewCounts) of
        0 ->
            Logs;
        _ ->
            wait_for_logs(get_logs(), NewCounts, Logs)
    end.
