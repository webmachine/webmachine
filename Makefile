ERL          ?= erl
APP          := webmachine

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
	@(./rebar skip_deps=true eunit)

