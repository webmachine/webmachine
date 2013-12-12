ERL          ?= erl
APP          := webmachine

REPO = ${shell echo `basename "$${PWD}"`}
ARTIFACTSFILE = ${shell echo ${REPO}-`date +%F_%H-%M-%S`.tgz}

.PHONY: deps

all: deps compile

compile: deps
	./rebar compile

deps: DEV_MODE
	@(./rebar get-deps)

clean:
	@(./rebar clean)

# nuke deps first to avoid wasting time having rebar recurse into deps
# for clean
distclean:
	@rm -rf deps ./rebar/DEV_MODE
	@(./rebar clean)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'
DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.webmachine_dialyzer_plt

include tools.mk

verbosetest: all
	@(./rebar -v skip_deps=true eunit)

travisupload:
	tar cvfz ${ARTIFACTSFILE} --exclude '*.beam' --exclude '*.erl' test.log .eunit
	travis-artifacts upload --path ${ARTIFACTSFILE}

DEV_MODE:
	@touch ./.rebar/DEV_MODE
