GENERATED_SRC = src/aeb_fate_opcodes.erl src/aeb_fate_ops.erl include/aeb_fate_opcodes.hrl src/aeb_fate_asm_scan.xrl src/aeb_fate_pp.erl
GENERATOR_DEPS = ebin/aeb_fate_generate_ops.beam src/aeb_fate_asm_scan.template
REBAR ?= ./rebar3

all: local

sources: $(GENERATED_SRC)

local: $(GENERATED_SRC)
	@$(REBAR) as local release

console: local
	@$(REBAR) as local shell

clean:
	@$(REBAR) clean
	rm -f $(GENERATED_SRC)
	rm -f ebin/*

dialyzer: local
	@$(REBAR) as local dialyzer

distclean: clean
	@rm -rf _build/

eunit: local
	@$(REBAR) as local eunit

test: local
	@$(REBAR) as local eunit

ebin/%.beam: src/%.erl
	erlc +debug_info -o $(dir $@) $<

$(GENERATED_SRC): $(GENERATOR_DEPS)
	erl -pa ebin/ -noshell -s aeb_fate_generate_ops gen_and_halt src/ include/
