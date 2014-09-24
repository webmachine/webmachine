-ifdef(old_gbtree_type).
-type headers() :: gb_tree().
-else.
-type headers() :: gb_trees:tree().
-endif.

-record(wm_reqdata, {method                     :: 'GET' | 'HEAD' | 'PUT' | 'POST' | 'DELETE' |
                                                   'OPTIONS' | 'TRACE' | 'CONNECT' | string(),
                     scheme                     :: http | https | undefined,
                     version                    :: wrq:version() | undefined,
                     peer                       :: string() | atom() | undefined,
                     sock                       :: string() | atom() | undefined,
                     wm_state,                   
                     disp_path,                  
                     path                       :: string() | undefined,
                     raw_path                   :: string() | undefined,
                     path_info = orddict:new()  :: orddict:orddict(),
                     path_tokens                :: [string()] | atom() | undefined,
                     app_root                   :: string() | undefined,
                     response_code = 500        :: 200..599 | {200..599, string()},                
                     max_recv_body = 1073741224 :: pos_integer(), % 1GB default
                     %% 64MB default, stolen from R13B03 inet_drv.c's
                     %% TCP_MAX_PACKET_SIZE definition
                     max_recv_hunk = 67108864   :: pos_integer(), 
                     req_cookie                 :: [{string(),string()}] | atom() | undefined,
                     req_qs                     :: [{string(),string()}] | atom() | undefined,
                     req_headers                :: headers() | undefined, %% mochiheaders
                     req_body,
                     resp_redirect = false      :: boolean(),
                     resp_headers = mochiweb_headers:empty() 
                                                :: headers(),
                     resp_body                  :: wrq:response_body() | undefined,
                     resp_range                 :: atom() | undefined,
                     host_tokens                :: [string()] | undefined,
                     port                       :: inet:port_number() | undefined,
                     notes = []                 :: list()
                    }).

