build:
	@rebar compile skip_deps=true

compile:
	@rebar compile

deps:
	@TEST=$(TEST) rebar get-deps
	@TEST=$(TEST) rebar compile
clean:
	@rm -Rf ebin .eunit log

test: TEST=true
test: build
	@TEST=$(TEST) rebar ct skip_deps=true

.PHONY: deps compile all test
