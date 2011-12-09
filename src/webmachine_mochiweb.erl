%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
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

%% @doc Mochiweb interface for webmachine.
-module(webmachine_mochiweb).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-export([start/1, stop/0, loop/1]).

start(Options) ->
    {DispatchList, Options1} = get_option(dispatch, Options),
    {ErrorHandler0, Options2} = get_option(error_handler, Options1),
    {EnablePerfLog, Options3} = get_option(enable_perf_logger, Options2),
    ErrorHandler =
        case ErrorHandler0 of
            undefined ->
                webmachine_error_handler;
            EH -> EH
        end,
    {LogDir, Options4} = get_option(log_dir, Options3),
    case whereis(webmachine_logger) of
      undefined ->
        webmachine_sup:start_logger(LogDir);
      _ ->
        ignore
    end,
    case EnablePerfLog of
        true ->
          case whereis(webmachine_perf_logger) of
            undefined ->
              application_set_unless_env(webmachine, enable_perf_logger, true),
              webmachine_sup:start_perf_logger(LogDir);
            _ ->
              ignore
          end;
        _ ->
            ignore
    end,
    {PName, Options5} = case get_option(name, Options4) of
      {undefined, _} -> {?MODULE, Options4};
      {PN, O5} -> {PN, O5}
    end,
    application_set_unless_env(webmachine, dispatch_list, DispatchList),
    application_set_unless_env(webmachine, error_handler, ErrorHandler),
    mochiweb_http:start([{name, PName}, {loop, fun loop/1} | Options5]).

stop() ->
    {registered_name, PName} = process_info(self(), registered_name),
    mochiweb_http:stop(PName).

loop(MochiReq) ->
    Req = webmachine:new_request(mochiweb, MochiReq),
    {ok, DispatchList} = application:get_env(webmachine, dispatch_list),
    Host = case host_headers(Req) of
               [H|_] -> H;
               [] -> []
           end,
    {Path, _} = Req:path(),
    {RD, _} = Req:get_reqdata(),

    %% Run the dispatch code, catch any errors...
    try webmachine_dispatcher:dispatch(Host, Path, DispatchList, RD) of
        {no_dispatch_match, _UnmatchedHost, _UnmatchedPathTokens} ->
            handle_error(404, {none, none, []}, Req);
        {Mod, ModOpts, HostTokens, Port, PathTokens, Bindings,
         AppRoot, StringPath} ->
            BootstrapResource = webmachine_resource:new(x,x,x,x),
            {ok,RS1} = Req:load_dispatch_data(Bindings,HostTokens,Port,
                                              PathTokens,AppRoot,StringPath),
            XReq1 = {webmachine_request,RS1},
            try
                {ok, Resource} = BootstrapResource:wrap(Mod, ModOpts),
                {ok,RS2} = XReq1:set_metadata('resource_module', Mod),
                webmachine_decision_core:handle_request(Resource, RS2)
            catch
                error:Error ->
                    handle_error(500, {error, Error}, Req)
            end
    catch
        Type : Error ->
            handle_error(500, {Type, Error}, Req)
    end.

handle_error(Code, Error, Req) ->
    {ok, ErrorHandler} = application:get_env(webmachine, error_handler),
    {ErrorHTML,ReqState1} =
        ErrorHandler:render_error(Code, Req, Error),
    Req1 = {webmachine_request,ReqState1},
    {ok,ReqState2} = Req1:append_to_response_body(ErrorHTML),
    Req2 = {webmachine_request,ReqState2},
    {ok,ReqState3} = Req2:send_response(Code),
    Req3 = {webmachine_request,ReqState3},
    {LogData,_ReqState4} = Req3:log_data(),
    case application:get_env(webmachine,webmachine_logger_module) of
        {ok, LogModule} ->
            spawn(LogModule, log_access, [LogData]);
        _ -> nop
    end.



get_option(Option, Options) ->
    case lists:keytake(Option, 1, Options) of
       false -> {undefined, Options};
       {value, {Option, Value}, NewOptions} -> {Value, NewOptions}
    end.

application_set_unless_env(App, Var, Value) ->
    Current = application:get_all_env(App),
    CurrentKeys = proplists:get_keys(Current),
    case lists:member(Var, CurrentKeys) of
        true ->
            ok;
        false ->
            application:set_env(App, Var, Value)
    end.

host_headers(Req) ->
    [ V || {V,_ReqState} <- [Req:get_header_value(H)
                             || H <- ["x-forwarded-host",
                                      "x-forwarded-server",
                                      "host"]],
           V /= undefined].
