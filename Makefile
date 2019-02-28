

REBAR ?= rebar3

all: local

local: src/aeb_fate_opcodes.erl src/aeb_fate_code.erl include/aeb_fate_opcodes.hrl src/aeb_fate_asm_scan.xrl
	@$(REBAR) as local release

console: local
	@$(REBAR) as local shell

clean:
	@$(REBAR) clean
	rm -f src/aeb_fate_opcodes.erl
	rm -f src/aeb_fate_code.erl
	rm -f include/aeb_fate_opcodes.hrl

dialyzer: local
	@$(REBAR) as local dialyzer



distclean: clean
	@rm -rf _build/

eunit: local
	@$(REBAR) as local eunit

test: local
	@$(REBAR) as local eunit


ebin/aeb_fate_generate_ops.beam: src/aeb_fate_generate_ops.erl ebin
	erlc -o $(dir $@) $<

src/aeb_fate_opcodes.erl src/aeb_fate_code.erl include/aeb_fate_opcodes.hrl src/aeb_fate_asm_scan.xrl: ebin/aeb_fate_generate_ops.beam
	erl -pa ebin/ -noshell -s aeb_fate_generate_ops gen_and_halt src/ include/

ebin:
	mkdir ebin
