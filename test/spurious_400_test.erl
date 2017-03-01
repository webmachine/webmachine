-module(spurious_400_test).

-include_lib("eunit/include/eunit.hrl").
-include("webmachine.hrl").

-compile(export_all).

http_request(_Timeout, _Value, 0) ->
    error;
http_request(Timeout, Value, Count) ->
    case httpc:request(put, {"http://localhost:12000/test400/foo",
                             [], % Headers
                             "binary/octet-stream", % Content-type
                             Value}, % body
                       [{timeout, Timeout}], % HTTPoptions
                       [{full_result, false}, {socket_opts, [{keepalive, true}]}]) of
        {ok, Result} ->
            {ok, Result};
        {error, socket_closed_remotely} ->
            io:format(user, "Retry!\n", []),
            http_request(Timeout, Value, Count-1)
    end.

provoke_400_test_() ->
    {spawn,
     [{setup,
       fun setup/0,
       fun cleanup/1,
       fun({_Pid0, MochiPid}) -> [{timeout, 60, ?_assert(provoke_400(MochiPid))}] end}]}.


handle_response(Data) ->
    error_logger:info_msg("Got ~p", [Data]),
    put(response, Data).

setup() ->
    error_logger:tty(false),

    %% Spin up webmachine
    application:start(inets),
    WebConfig = [{ip, "0.0.0.0"}, {port, 12000},
                 {dispatch, [{["test400", '*'], ?MODULE, []}]}],
    {ok, Pid0} = webmachine_sup:start_link(),
    {ok, Pid1} = webmachine_mochiweb:start(WebConfig),
    link(Pid1),
    {Pid0, Pid1}.

cleanup({Pid0, Pid1}) ->
    %% clean up
    unlink(Pid0),
    exit(Pid0, normal),
    unlink(Pid1),
    exit(Pid1, kill),
    application:stop(inets).

init([]) ->
    {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['PUT'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"binary/octet-stream", on_put}], ReqData, Context}.

on_put(ReqData, Context) ->
    {ok, ReqData, Context}.

provoke_400(MochiPid) ->
    Timeout = 30*1000,
    %% Get acceptor pool from mochiweb state
    State = sys:get_state(MochiPid),
    %% This is brittle - if record changes, element number might change too
    %% importing the record which also is bad, this will work.
    %% NB Acceptors is a sets data type - not a list
    Acceptors = element(14, State),
    {ok, {Status1, _}} = http_request(Timeout, "foobar", 3),
    ?assertEqual(204, Status1),
    send_everyone_crazy_msg(sets:to_list(Acceptors)),
    {ok, {Status2, _}} = http_request(Timeout, "barbaz", 3),
    ?assertEqual(204, Status2),
    true.

send_everyone_crazy_msg(Pids) ->
    lists:foreach(fun(P) -> P ! 'foo' end, Pids).
