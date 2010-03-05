ERL ?= erl
APP := webmachine_demo

all:
	@../rebar compile

clean:
	@../rebar clean

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
