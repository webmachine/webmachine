%% @author Voila <th3rac25@gmail.com>
%% @copyright 2007-2009 Basho Technologies, Inc.  All Rights Reserved.
%% @doc Example webmachine_resource.

-module(webmachine_demo_upload_resource).
-export([init/1, to_html/2, allowed_methods/2, content_types_provided/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-define(UPLOAD_DIR, 'priv/www/uploads').

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->    
    {['GET', 'POST', 'HEAD'], ReqData, Context}.
    
content_types_provided(ReqData, Context) ->
    {[{"text/html", to_html}], ReqData, Context}.

process_post(ReqData, Context) ->
    Body = wrq:req_body(ReqData),
    Boundary = webmachine_multipart:find_boundary(ReqData),
    Parts = webmachine_multipart:get_all_parts(Body, Boundary),
    {FileName, FileData} = get_file(Parts),
    ok = save_file(FileName, FileData),
    Msg = ["File '", FileName, "' successfully uploaded."],
    ReqData1 = wrq:set_resp_header("location", ["/upload?msg=", Msg], ReqData),
    ReqData2 = wrq:do_redirect(true, ReqData1),
    {true, ReqData2, Context}.


to_html(ReqData, Context) ->
    Vars = case wrq:get_qs_value("msg", ReqData) of
               undefined -> [];
               Msg -> [{msg, Msg}]
           end,
    {ok, Content} = form_dtl:render(Vars),
    {Content, ReqData, Context}.

%%-----------------------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------------------
binary_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).
 
save_file(FileName, Bin) ->
    FilePath = [?UPLOAD_DIR, '/', binary_to_atom(FileName)],
    file:write_file(FilePath, Bin).

get_file(Parts) -> 
    {_, {Params, _}, FileData} = lists:keyfind("file", 1, Parts),
    {_, FileName} = proplists:lookup(<<"filename">>, Params),
    {FileName, FileData}.
