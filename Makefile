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
	@(./rebar skip_deps=true eunit)

