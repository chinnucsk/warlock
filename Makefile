SHELL := /bin/bash

# Generic app Makefile 

REBAR=./rebar
APP=dlock

all: small_clean deps compile

docs:
	$(REBAR) skip_deps=true doc

deps:
	$(REBAR) get-deps

small_clean:
	$(REBAR) skip_deps=true clean

clean: testclean
	$(REBAR) clean

cleanfull: clean devclean relclean

compile:
	rm -rf .eunit
	$(REBAR) compile

#test:
#	$(REBAR) skip_deps=true eunit
#	rm -rf doc/coverage
#	mkdir doc/coverage
#	cp -r .eunit/*.html doc/coverage

TEST_LOG_FILE := eunit.log
testclean:
	  @rm -f $(TEST_LOG_FILE)

# Test each dependency individually in its own VM
test: deps compile testclean
	  @$(foreach dep, \
		  $(wildcard deps/*), \
			./rebar eunit app=$(notdir $(dep)) \
			  || echo "Eunit: $(notdir $(dep)) FAILED" >> $(TEST_LOG_FILE);)
	  ./rebar eunit skip_deps=true
	  @if test -s $(TEST_LOG_FILE) ; then \
		  cat $(TEST_LOG_FILE) && \
		  exit `wc -l < $(TEST_LOG_FILE)`; \
	  fi

complete: small_clean deps compile test docs dia
	@echo ok

## Release

rel: deps
	$(REBAR) compile generate

relclean:
	rm -rf rel/$(APP)

## Dev

devrel: dev1 dev2 dev3

dev1 dev2 dev3:
	mkdir -p dev
	(cd rel && ../rebar generate target_dir=../dev/$@ overlay_vars=vars/$@_vars.config)

devclean:
	rm -rf dev

## Dializer

APPS = kernel stdlib sasl erts tools os_mon runtime_tools crypto \
	   snmp eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.$(REPO)_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
	  deps/*/ebin

build_plt: compile
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin | \
	  fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)



.PHONY: all small_clean clean compile test docs dia complete rel deps
