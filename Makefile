ERL          ?= erl
APP          := webmachine
WEBMACHINE_SERVER ?= mochiweb

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

# Meta-tasks for testing specific backends, does a clean between runs.
# If you want to test with a specific backend repeatedly without the clean,
# use `make test` with WEBMACHINE_SERVER set appropriately.
yaws-test: clean
	@WEBMACHINE_SERVER=yaws ${MAKE} test

cowboy-test: clean
	@WEBMACHINE_SERVER=cowboy ${MAKE} test

mochiweb-test: clean
	@WEBMACHINE_SERVER=mochiweb ${MAKE} test
