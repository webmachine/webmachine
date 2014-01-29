test: compile
	./rebar eunit skip_deps=true

docs:
	./rebar doc skip_deps=true

xref: compile
	./rebar xref skip_deps=true

PLT ?= $(HOME)/.riak_combo_dialyzer_plt
LOCAL_PLT = .local_dialyzer_plt
DIALYZER_FLAGS ?= -Wunmatched_returns

${PLT}: compile
ifneq (,$(wildcard $(PLT)))
	dialyzer --check_plt --plt $(PLT) --apps $(DIALYZER_APPS) && \
		dialyzer --add_to_plt --plt $(PLT) --output_plt $(PLT) --apps $(DIALYZER_APPS) ; test $$? -ne 1
else
	dialyzer --build_plt --output_plt $(PLT) --apps $(DIALYZER_APPS); test $$? -ne 1
endif

${LOCAL_PLT}: compile
ifneq (,$(wildcard deps/*))
ifneq (,$(wildcard $(LOCAL_PLT)))
	dialyzer --check_plt --plt $(LOCAL_PLT) deps/*/ebin  && \
		dialyzer --add_to_plt --plt $(LOCAL_PLT) --output_plt $(LOCAL_PLT) deps/*/ebin ; test $$? -ne 1
else
	dialyzer --build_plt --output_plt $(LOCAL_PLT) deps/*/ebin ; test $$? -ne 1
endif
endif

dialyzer: ${PLT} ${LOCAL_PLT}
	@echo "==> $(shell basename $(shell pwd)) (dialyzer)"
	@if [ -f $(LOCAL_PLT) ]; then \
		dialyzer $(DIALYZER_FLAGS) --plts $(PLT) $(LOCAL_PLT) -c ebin; \
	else \
		dialyzer $(DIALYZER_FLAGS) --plts $(PLT) -c ebin; \
	fi

cleanplt:
	@echo 
	@echo "Are you sure?  It takes several minutes to re-build."
	@echo Deleting $(PLT) and $(LOCAL_PLT) in 5 seconds.
	@echo 
	sleep 5
	rm $(PLT)
	rm $(LOCAL_PLT)

