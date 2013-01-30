%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the mock_resource application.

-module(mock_resource_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mock_resource.
start(_Type, _StartArgs) ->
    mock_resource_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mock_resource.
stop(_State) ->
    ok.
