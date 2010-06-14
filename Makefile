ERL          ?= erl
APP          := webmachine

.PHONY: deps

all: deps
	@(./rebar compile)

deps:
	@(./rebar get-deps)

clean:
	@(./rebar clean)

distclean: clean
	@(./rebar delete-deps)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	scripts/run_tests.escript ebin | tee test.log

