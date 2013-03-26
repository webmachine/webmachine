ERL          ?= erl
APP          := webmachine

REPO = ${shell echo `basename "$${PWD}"`}
ARTIFACTSFILE = ${shell echo ${REPO}-`date +%F-%a-%T`.tgz}

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

verbosetest: all
	@(./rebar -v skip_deps=true eunit)

travisupload:
	tar cvfz ${ARTIFACTSFILE} --exclude '*.beam' --exclude '*.erl' test.log .eunit
	travis-artifacts upload --path ${ARTIFACTSFILE}
