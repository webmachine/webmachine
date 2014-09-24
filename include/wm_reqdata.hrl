-ifdef(old_gbtree_type).
-type req_headers() :: gb_tree().
-else.
-type req_headers() :: gb_trees:tree().
-endif.

-record(wm_reqdata, {method        :: 'GET' | 'HEAD' | 'PUT' | 'POST' | 'DELETE' |
                                      'OPTIONS' | 'TRACE' | 'CONNECT' | string(),
                     scheme        :: http | https | undefined,
                     version       :: wrq:version() | undefined,
                     peer          :: string() | atom() | undefined,
                     sock          :: string() | atom() | undefined,
                     wm_state,
                     disp_path,
                     path          :: string() | undefined,
                     raw_path      :: string() | undefined,
                     path_info     :: orddict:orddict() | undefined,
                     path_tokens   :: [string()] | atom() | undefined,
                     app_root      :: string() | undefined,
                     response_code :: pos_integer() | {pos_integer(),string()} |
                                      undefined,
                     max_recv_body :: pos_integer() | undefined,
                     max_recv_hunk :: pos_integer() | undefined,
                     req_cookie    :: [{string(),string()}] | atom() | undefined,
                     req_qs        :: [{string(),string()}] | atom() | undefined,
                     req_headers   :: req_headers() | undefined, %% mochiheaders
                     req_body,
                     resp_redirect :: boolean() | undefined,
                     resp_headers,
                     resp_body     :: any(),
                     resp_range    :: atom() | undefined,
                     host_tokens   :: [string()] | undefined,
                     port          :: inet:port_number() | undefined,
                     notes         :: list() | undefined
                    }).

