-record(wm_reqstate, {socket=undefined :: undefined | inet:socket(),
                      metadata=dict:new() :: dict(),
                      range=undefined     :: undefined | string(),
                      peer=undefined      :: undefined | inet:ip_address(),
                      reqdata=undefined,  %% reqdata
                      bodyfetch=undefined,
                      reqbody=undefined   :: undefined | binary(),
                      log_data=undefined
                     }).

