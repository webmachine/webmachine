%% Reverse Engineering of these types. Right now based on what
%% mochiweb expected, and the assumptions it made from OTP

%% The following types are clear in erlang:decode_packet/3
%% https://github.com/erlang/otp/blob/86d1fb0865193cce4e308baa6472885a81033f10/erts/preloaded/src/erlang.erl#L537-L632
%% {http_request, Method, Url, Version}
%% Method  :: atom() | string()
%% Url     :: string()
%% Version :: {integer(), integer()}

-record(
   wm_reqdata,
   {
     method :: wrq:method(),
     scheme :: wrq:scheme(),
     version :: {non_neg_integer(), % decode_packet/3
                 non_neg_integer()},
     peer="defined_in_wm_req_srv_init",
     sock="defined_in_wm_req_srv_init",
     wm_state = defined_on_call,
     disp_path=defined_in_load_dispatch_data,
     path = "defined_in_create" :: string(),
     raw_path :: string(), % mochiweb:uri/1
     path_info = orddict:new() :: orddict:orddict(),
     path_tokens=defined_in_load_dispatch_data,
     app_root="defined_in_load_dispatch_data",
     response_code = 500 :: non_neg_integer()
                          | {non_neg_integer(), string()},
     max_recv_body = (1024*(1024*1024)) :: non_neg_integer(),
     % Stolen from R13B03 inet_drv.c's TCP_MAX_PACKET_SIZE definition
     max_recv_hunk = (64*(1024*1024)) :: non_neg_integer(),
     req_cookie = defined_in_create :: [{string(), string()}]
                                     | defined_in_create,
     req_qs = defined_in_create :: [{string(), string()}]
                                 | defined_in_create,
     req_headers :: webmachine:headers(),
     req_body=not_fetched_yet,
     resp_redirect = false :: boolean(),
     resp_headers = webmachine_headers:empty() :: webmachine:headers(),
     resp_body = <<>> :: webmachine:response_body(),
     %% follow_request : range responce for range request, normal responce for non-range one
     %% ignore_request : normal resopnse for either range reuqest or non-range one
     resp_range = follow_request :: follow_request | ignore_request,
     host_tokens,
     port :: undefined | inet:port_number(),
     notes = [] :: list()
   }).
