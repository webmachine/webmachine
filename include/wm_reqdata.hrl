-type http_version() :: {non_neg_integer(), non_neg_integer()}.
-type path_info() :: undefined | string().
-type qs() :: list({string(), string()}).
-type body() :: undefined | binary().

-record(wm_reqdata, {
        method :: atom(),
        scheme :: atom(),
        version :: http_version(),
        peer :: string(),
        wm_state :: term(),
        disp_path :: string(),
        path :: string(),
        raw_path :: string(),
        path_info :: path_info(),
        path_tokens :: list(),
        app_root :: string(),
        response_code :: non_neg_integer(),
        max_recv_body :: integer(),
        max_recv_hunk :: integer(),
        req_cookie :: string(),
        req_qs :: qs(),
        req_headers :: term(),
        req_body :: body(),
        resp_redirect :: boolean(),
        resp_headers :: term(),
        resp_body :: body(),
        host_tokens :: list(),
        port :: integer(),
        notes :: string()
        }).
