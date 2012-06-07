ERL ?= erl
APP := {{appid}}
WEBSERVER := {{webserver}}

.PHONY: deps

all: deps
	@WEBMACHINE_SERVER=$(WEBSERVER) ./rebar compile

deps:
	@WEBMACHINE_SERVER=$(WEBSERVER) ./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
