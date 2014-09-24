-export([ping/2]).

-spec ping(wrq:wm_reqdata(), term()) -> {pong, wrq:wm_reqdata(), term()}.
ping(ReqData, State) ->
    {pong, ReqData, State}.
