-record(wm_reqstate, {socket :: inet:socket() | {ssl, ssl:sslsocket()} |
                                atom() | undefined,
                      metadata=orddict:new() :: orddict:orddict(),
                      range  :: undefined | ignore | [{integer(),integer()|none}],
                      peer   :: undefined | inet:ip_address() | string(),
                      sock,
                      reqdata,  %% reqdata
                      bodyfetch,
                      reqbody :: undefined | binary(),
                      log_data
                     }).
