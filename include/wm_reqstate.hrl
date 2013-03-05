-record(wm_reqstate, {socket=undefined :: undefined | inet:socket(),
                      metadata=orddict:new() :: orddict:orddict(),
                      range=undefined     :: undefined | string(),
                      peer=undefined      :: undefined | inet:ip_address(),
                      sock=undefined,
                      reqdata=undefined,  %% reqdata
                      bodyfetch=undefined,
                      reqbody=undefined   :: undefined | binary(),
                      log_data=undefined
                     }).

