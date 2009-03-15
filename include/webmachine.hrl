-behaviour(webmachine_resource).
-export([start_link/1]).
-export([ping/2]).

-include_lib("webmachine/include/wm_reqdata.hrl").

start_link(Args) ->
    webmachine_resource:start_link(?MODULE, [Args]).

ping(ReqData, State) ->
    {pong, ReqData, State}.


