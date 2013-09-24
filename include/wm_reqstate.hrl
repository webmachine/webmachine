-type bodyfetch() :: undefined | stream | standard.

-record(wm_reqstate, {socket=undefined,
                   metadata=orddict:new(),
                   range=undefined,
                   peer=undefined,
                   sock=undefined,
                   reqdata=undefined,
                   bodyfetch=undefined :: bodyfetch(),
                   reqbody=undefined,
                   log_data=undefined
                  }).

