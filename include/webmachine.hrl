-export([ping/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").
-spec(ping(#wm_reqdata{}, any()) ->{pong, #wm_reqdata{}, any()}).
ping(ReqData, State) ->
    {pong, ReqData, State}.


