ERL          ?= erl
APP          := webmachine
WEBMACHINE_SERVER := mochiweb

.PHONY: deps

all: deps
	@WEBMACHINE_SERVER=$(WEBMACHINE_SERVER) ./rebar compile

deps:
	@WEBMACHINE_SERVER=$(WEBMACHINE_SERVER) ./rebar get-deps

clean:
	@WEBMACHINE_SERVER=$(WEBMACHINE_SERVER) ./rebar clean

distclean: clean
	@WEBMACHINE_SERVER=$(WEBMACHINE_SERVER) ./rebar delete-deps

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	@WEBMACHINE_SERVER=$(WEBMACHINE_SERVER) ./rebar skip_deps=true eunit

