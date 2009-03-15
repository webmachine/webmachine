%% @author Bryan Fink <bryan@basho.com>
%% @doc Webmachine trace file interpretter.
-module(wmtrace_resource).

-export([start_link/1,
         ping/2,
         init/1,
         resource_exists/2,
         content_types_provided/2,
         produce_html/2,
         produce_javascript/2,
         produce_map/2,
         produce_css/2]).

-include("include/wm_reqdata.hrl").

-record(ctx, {trace_dir, trace}).

-define(MAP_EXTERNAL, "static/map.png").
-define(MAP_INTERNAL, "deps/webmachine/docs/http-headers-status-v3.png").
-define(SCRIPT_EXTERNAL, "static/wmtrace.js").
-define(SCRIPT_INTERNAL, "deps/webmachine/trace/wmtrace.js").
-define(STYLE_EXTERNAL, "static/wmtrace.css").
-define(STYLE_INTERNAL, "deps/webmachine/trace/wmtrace.css").

start_link(Args) ->
    webmachine_resource:start_link(?MODULE, [Args]).

ping(ReqData, State) ->
    {pong, ReqData, State}.

init(Config) ->
    {trace_dir, TraceDir} = proplists:lookup(trace_dir, Config),
    {trace_dir_exists, true} = {trace_dir_exists, filelib:is_dir(TraceDir)},
    {ok, #ctx{trace_dir=TraceDir}}.

resource_exists(RD, Ctx) ->
    case wrq:disp_path(RD) of
        [] ->
            {true, RD, Ctx};
        ?MAP_EXTERNAL ->
            {filelib:is_file(?MAP_INTERNAL), RD, Ctx};
        ?SCRIPT_EXTERNAL ->
            {filelib:is_file(?SCRIPT_INTERNAL), RD, Ctx};
        ?STYLE_EXTERNAL ->
            {filelib:is_file(?STYLE_INTERNAL), RD, Ctx};
        TraceName ->
            TracePath = filename:join([Ctx#ctx.trace_dir, TraceName]),
            {filelib:is_file(TracePath), RD, Ctx#ctx{trace=TracePath}}
    end.

content_types_provided(RD, Ctx) ->
    case wrq:disp_path(RD) of
        ?MAP_EXTERNAL ->
            {[{"image/png", produce_map}], RD, Ctx};
        ?SCRIPT_EXTERNAL ->
            {[{"text/javascript", produce_javascript}], RD, Ctx};
        ?STYLE_EXTERNAL ->
            {[{"text/css", produce_css}], RD, Ctx};
        _ ->
            {[{"text/html", produce_html}], RD, Ctx}
    end.

produce_html(RD, Ctx=#ctx{trace=undefined}) ->
    Dir = filename:absname(Ctx#ctx.trace_dir),
    Files = lists:reverse(
              lists:sort(
                filelib:fold_files(Dir,
                                   ".*\.wmtrace",
                                   false,
                                   fun(F, Acc) ->
                                           [filename:basename(F)|Acc]
                                   end,
                                   []))),
    {trace_list_html(Dir, Files), RD, Ctx};
produce_html(RD, Ctx) ->
    Filename = filename:absname(Ctx#ctx.trace),
    {ok, Data} = file:consult(Filename),
    {trace_html(Filename, Data), RD, Ctx}.

trace_list_html(Dir, Files) ->
    html([],
         [head([],
               title([], ["Webmachine Trace List for ",Dir])),
          body([],
               [h1([], ["Traces in ",Dir]),
                ul([],
                   [ li([], a([{"href", F}], F)) || F <- Files ])
               ])
         ]).

trace_html(Filename, Data) ->
    {Request, Response, Trace} = encode_trace(Data),
    html([],
         [head([],
               [title([],["Webmachine Trace ",Filename]),
                linkblock([{"rel", "stylesheet"},
                           {"type", "text/css"},
                           {"href", "static/wmtrace.css"}],
                          []),
                script([{"type", "text/javascript"},
                        {"src", "static/wmtrace.js"}],
                       []),
                script([{"type", "text/javascript"}],
                       cdata(["var request=",Request,";\n"
                              "var response=",Response,";\n"
                              "var trace=",Trace,";"]))
               ]),
          body([],
               [canvas([{"id", "v3map"},
                        {"width", "3138"},
                        {"height", "2184"}],
                       []),
                divblock([{"id", "requestdetail"}],
                         [divblock([],
                                   [span([{"id", "requestmethod"}], []),
                                    " ",
                                    span([{"id", "requestpath"}], [])]),
                          divblock([{"id", "requestheaders"}],
                                   [ul([], [])]),
                          divblock([{"id", "requestbody"}],
                                   [])
                         ]),
                divblock([{"id", "responsedetail"}],
                         [divblock([{"id", "responsecode"}], []),
                          divblock([{"id", "responseheaders"}],
                                   [ul([], [])]),
                          divblock([{"id", "responsebody"}], [])
                         ]),
                divblock([{"id", "decisiondetail"}],
                         [divblock([],
                                   ["Decision: ",
                                    span([{"id", "decisionid"}], "&nbsp;")
                                   ]),
                          divblock([], "Calls:"),
                          select([{"id", "decisioncalls"}, {"size", "4"}], []),
                          divblock([], "Input:"),
                          pre([{"id", "callinput"}], []),
                          divblock([], "Output:"),
                          pre([{"id", "calloutput"}], [])
                         ])
               ])
         ]).

produce_javascript(RD, Ctx) ->
    {ok, Script} = file:read_file(?SCRIPT_INTERNAL),
    {Script, RD, Ctx}.

produce_map(RD, Ctx) ->
    {ok, Map} = file:read_file(?MAP_INTERNAL),
    {Map, RD, Ctx}.

produce_css(RD, Ctx) ->
    {ok, Script} = file:read_file(?STYLE_INTERNAL),
    {Script, RD, Ctx}.

%%
%% Trace Encoding
%%

encode_trace(Data) ->
    {Request, Response, Trace} = aggregate_trace(Data),
    {mochijson:encode(encode_request(Request)),
     mochijson:encode(encode_response(Response)),
     mochijson:encode({array, [ encode_trace_part(P) || P <- Trace ]})}.

aggregate_trace(RawTrace) ->
    {Request, Response, Trace} = lists:foldl(fun aggregate_trace_part/2,
                                             {undefined, 500, []},
                                             RawTrace),
    {Request, Response, lists:reverse(Trace)}.

aggregate_trace_part({decision, Decision}, {Q, R, Acc}) ->
    BDN = base_decision_name(Decision),
    case Acc of
        [{BDN,_}|_] -> {Q, R, Acc}; %% subdecision (ex. v3b13b)
        _ ->
            {Q, R, [{base_decision_name(Decision), []}|Acc]}
    end;
aggregate_trace_part({attempt, Module, Function, Args},
                     {Q, R, [{Decision,Calls}|Acc]}) ->
    {maybe_extract_request(Function, Args, Q),
     R, [{Decision,[{Module, Function, Args, wmtrace_null}|Calls]}|Acc]};
aggregate_trace_part({result, Module, Function, Result},
                     {Q, R, [{Decision,[{Module,Function,Args,_}|Calls]}|Acc]}) ->
    {Q, maybe_extract_response(Function, Result, R),
     [{Decision,[{Module, Function, Args, Result}|Calls]}|Acc]};
aggregate_trace_part({not_exported, Module, Function, Args},
                     {Q, R, [{Decision,Calls}|Acc]}) ->
    {Q, maybe_extract_response(Function, Args, R),
     [{Decision,[{Module, Function, Args, wmtrace_not_exported}|Calls]}
      |Acc]}.

maybe_extract_request(ping, [ReqData,_], _) ->
    ReqData;
maybe_extract_request(_, _, R) ->
    R.

maybe_extract_response(finish_request, [ReqData,_], _) ->
    ReqData;
maybe_extract_response(finish_request, {_, ReqData, _}, _) ->
    ReqData;
maybe_extract_response(_, _, R) ->
    R.

base_decision_name(Decision) ->
    [$v,$3|D] = atom_to_list(Decision), %% strip 'v3'
    case lists:reverse(D) of
        [A|RD] when A >= $a, A =< $z ->
            lists:reverse(RD); %% strip 'b' off end of some
        _ ->
            D
    end.

encode_request(ReqData) ->
    {struct, [{"method", atom_to_list(
                           wrq:method(ReqData))},
              {"path", wrq:raw_path(ReqData)},
              {"headers", encode_headers(wrq:req_headers(ReqData))},
              {"body", case wrq:req_body(ReqData) of
                           undefined -> [];
                           Body -> lists:flatten(io_lib:format("~s", [Body]))
                       end}]}.
    
encode_response(ReqData) ->
    {struct, [{"code", integer_to_list(
                         wrq:response_code(ReqData))},
              {"headers", encode_headers(wrq:resp_headers(ReqData))},
              {"body", lists:flatten(io_lib:format("~s", [wrq:resp_body(ReqData)]))}]}.

encode_headers(Headers) when is_list(Headers) ->
    {struct, [ {N, V} || {N, V} <- Headers ]};
encode_headers(Headers) ->
    encode_headers(mochiweb_headers:to_list(Headers)).

encode_trace_part({Decision, Calls}) ->
    {struct, [{"d", Decision},
              {"calls",
               {array, [ {struct,
                          [{"module", Module},
                           {"function", Function},
                           {"input", encode_trace_io(Input)},
                           {"output", encode_trace_io(Output)}]}
                         || {Module, Function, Input, Output}
                                <- lists:reverse(Calls) ]}}]}.

encode_trace_io(wmtrace_null) -> null;
encode_trace_io(wmtrace_not_exported) -> "wmtrace_not_exported";
encode_trace_io(Data) ->
    lists:flatten(io_lib:format("~p", [Data])).

%%
%% HTML Building
%%

-define(TAG(T), T(Attrs, Content) ->
                   tag(??T, Attrs, Content)).

?TAG(head).
?TAG(script).
?TAG(title).
?TAG(body).
?TAG(h1).
?TAG(ul).
?TAG(li).
?TAG(a).
?TAG(canvas).
?TAG(select).
?TAG(pre).
?TAG(span).

html(_Attrs, Content) ->
    [<<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">">>,
     <<"<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">">>,
     Content,
     <<"</html>">>].

divblock(Attrs, Content) ->
    tag("div", Attrs, Content). %% div is a reserved word

linkblock(Attrs, Content) ->
    tag("link", Attrs, Content). %% link is a reserved word

tag(Name, Attrs, Content) ->
    ["<",Name,
     [ [" ",K,"=\"",V,"\""] || {K, V} <- Attrs ],
     if Content == empty -> "/>";
        true ->
             [">",
              Content,
              "</",Name,">"]
     end].

cdata(Content) ->
    ["//<![CDATA[\n",Content,"\n//]]>"].
