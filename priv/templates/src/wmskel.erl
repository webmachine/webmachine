-module({{appid}}).
-export([
    start/0
]).

-spec start() -> ok.
start() ->
    _ = [ application:start(Dep) || Dep <- resolve_deps({{appid}}),
                                           not is_otp_base_app(Dep) ],
    ok.

-spec dep_apps(atom()) -> [atom()].
dep_apps(App) ->
    application:load(App),
    {ok, Apps} = application:get_key(App, applications),
    Apps.

-spec all_deps(atom(), [atom()]) -> [atom()].
all_deps(App, Deps) ->
    [[ all_deps(Dep, [App|Deps]) || Dep <- dep_apps(App),
                                           not lists:member(Dep, Deps)], App].

-spec resolve_deps(atom()) -> [atom()].
resolve_deps(App) ->
    DepList = all_deps(App, []),
    {AppOrder, _} = lists:foldl(fun(A,{List,Set}) ->
                                        case sets:is_element(A, Set) of
                                            true ->
                                                {List, Set};
                                            false ->
                                                {List ++ [A], sets:add_element(A, Set)}
                                        end
                                end,
                                {[], sets:new()},
                                lists:flatten(DepList)),
    AppOrder.

-spec is_otp_base_app(atom()) -> boolean().
is_otp_base_app(kernel) -> true;
is_otp_base_app(stdlib) -> true;
is_otp_base_app(_) -> false.
