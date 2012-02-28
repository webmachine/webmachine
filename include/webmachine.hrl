-export([ping/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").

-spec ping(#wm_reqdata{}, term()) -> {pong, #wm_reqdata{}, term()}.
ping(ReqData, State) ->
    {pong, ReqData, State}.


