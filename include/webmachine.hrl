-export([ping/2]).

-include("wm_reqdata.hrl").

ping(ReqData, State) ->
    {pong, ReqData, State}.
