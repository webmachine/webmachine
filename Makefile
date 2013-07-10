ERL          ?= erl
APP          := webmachine

VSN := $(shell erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), init:stop().' | grep 'R' | sed -e 's,R\(..\)B.*,\1,')
NEW_HASH := $(shell expr $(VSN) \>= 16)

.PHONY: deps

all: deps
ifeq ($(NEW_HASH),1)
	@(./rebar compile -Dnew_hash)
else
	@(./rebar compile)
endif

deps:
	@(./rebar get-deps)

clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all	
ifeq ($(NEW_HASH),1)
	@(./rebar skip_deps=true -Dnew_hash eunit)
else
	@(./rebar skip_deps=true eunit)
endif

