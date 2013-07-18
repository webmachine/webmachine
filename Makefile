ERL          ?= erl
APP          := webmachine

VSN := $(shell erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), init:stop().' | grep 'R' | sed -e 's,R\(..\)B.*,\1,')
OLD_HASH := $(shell expr $(VSN) \<= 15)
ifeq ($(OLD_HASH),1)
hash := "-Dold_hash"
endif

.PHONY: deps

all: deps
	@(./rebar compile $(hash))

deps:
	@(./rebar get-deps)

clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all	
	@(./rebar skip_deps=true $(hash) eunit)


