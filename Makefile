

REBAR ?= rebar3

all: local

local:
	@$(REBAR) as local release

console:
	@$(REBAR) as local shell

clean:
	@$(REBAR) clean

distclean: clean
	@rm -rf _build/

test: local
	@$(REBAR) as local eunit
