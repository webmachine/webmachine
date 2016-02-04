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
                       [{sync, false}, {receiver, {?MODULE, handle_response, []}}]) of
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
       [
        {timeout, 60,
         ?_assert(provoke_400())}
       ]}]}.


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
    put(mw_pid, Pid1),
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

provoke_400() ->
    Timeout = 30*1000,
    %% Send an HTTP request, get the pid for the request, and then send a crazy message into it
    _ = http_request(Timeout, "foobar", 3),
    MochiPid = get(mw_pid),
    MochiPid ! 'foo',
    true.




