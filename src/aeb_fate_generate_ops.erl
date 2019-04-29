-module(aeb_fate_generate_ops).

-export([ gen_and_halt/1
        , generate/0
        , generate_documentation/1
        , get_ops/0
        , test_asm_generator/1]).

gen_and_halt([SrcDirArg, IncludeDirArg]) ->
    generate(atom_to_list(SrcDirArg),
             atom_to_list(IncludeDirArg)),
    halt().

generate() -> generate("src/", "include/").

get_ops()  -> gen(ops_defs()).

generate(Src, Include) ->
    Ops = get_ops(),
    %% io:format("ops: ~p\n", [Ops]),
    HrlFile = Include ++ "aeb_fate_opcodes.hrl",
    generate_header_file(HrlFile, Ops),
    generate_opcodes_ops(aeb_fate_opcodes, HrlFile, Src, Ops),
    generate_code_ops(aeb_fate_code, Src, Ops),
    generate_scanner("aeb_fate_asm_scan.template", "aeb_fate_asm_scan.xrl", Src, Ops),
    gen_asm_pp(aeb_fate_pp, Src, Ops).

%% TODO: Some real gas numbers...
ops_defs() ->
    %%  Opname,        Opcode, args, end_bb, gas, format,      Constructor, Documentation
    [ { 'RETURN',       16#00,    0,   true,   2, atomic,           return, "Return from function call pop stack to arg0. The type of the retun value has to match the return type of the function."}
    , { 'RETURNR',      16#01,    1,   true,   2, [a],             returnr, "Return from function call copy Arg0 to arg0. The type of the retun value has to match the return type of the function."}
    , { 'CALL',         16#02,    1,   true,   4, [is],               call, "Call given function with args on stack. The types of the arguments has to match the argument typs of the function."}
    , { 'CALL_R',       16#03,    2,   true,   8, [a,is],           call_r, "Remote call to given contract and function.  The types of the arguments has to match the argument typs of the function."}
    , { 'CALL_T',       16#04,    1,   true,   4, [is],             call_t, "Tail call to given function. The types of the arguments has to match the argument typs of the function. And the return type of the called function has to match the type of the current function."}
    , { 'CALL_TR',      16#05,    2,   true,   8, [a,is],          call_tr, "Remote tail call to given contract and function. The types of the arguments has to match the argument typs of the function. And the return type of the called function has to match the type of the current function."}
    , { 'JUMP',         16#06,    1,   true,   3, [ii],               jump, "Jump to a basic block. The basic block has to exist in the current function."}
    , { 'JUMPIF',       16#07,    2,   true,   4, [a,ii],           jumpif, "Conditional jump to a basic block. If Arg0 then jump to Arg1."}
    , { 'SWITCH_V2',    16#08,    3,   true,   4, [a,ii,ii],        switch, "Conditional jump to a basic block on variant tag."}
    , { 'SWITCH_V3',    16#09,    4,   true,   4, [a,ii,ii,ii],     switch, "Conditional jump to a basic block on variant tag."}
    , { 'SWITCH_VN',    16#0a,    2,   true,   4, [a, li],           switch, "Conditional jump to a basic block on variant tag."}
    , { 'PUSH',         16#0b,    1,  false,   2, [a],                push, "Push argument to stack."}
    , { 'DUPA',         16#0c,    0,  false,   3, atomic,              dup, "push copy of accumulator on stack."}
    , { 'DUP',          16#0d,    1,  false,   3, [a],                 dup, "push Arg0 stack pos on top of stack."}
    , { 'POP',          16#0e,    1,  false,   3, [a],                 pop, "Arg0 := top of stack."}
    , { 'STORE',        16#0f,    2,  false,   3, [a,a],             store, "Arg0 := Arg1."}
    , { 'INCA',         16#10,    0,  false,   2, atomic,              inc, "Increment accumulator."}
    , { 'INC',          16#11,    1,  false,   2, [a],                 inc, "Increment argument."}
    , { 'DECA',         16#12,    0,  false,   2, atomic,              dec, "Decrement accumulator."}
    , { 'DEC',          16#13,    1,  false,   2, [a],                 dec, "Decrement argument."}
    , { 'ADD',          16#14,    3,  false,   3, [a,a,a],             add, "Arg0 := Arg1 + Arg2."}
    , { 'SUB',          16#15,    3,  false,   3, [a,a,a],             sub, "Arg0 := Arg1 - Arg2."}
    , { 'MUL',          16#16,    3,  false,   3, [a,a,a],             mul, "Arg0 := Arg1 * Arg2."}
    , { 'DIV',          16#17,    3,  false,   3, [a,a,a],          divide, "Arg0 := Arg1 / Arg2."}
    , { 'MOD',          16#18,    3,  false,   3, [a,a,a],          modulo, "Arg0 := Arg1 mod Arg2."}
    , { 'POW',          16#19,    3,  false,   3, [a,a,a],             pow, "Arg0 := Arg1  ^ Arg2."}
    , { 'LT',           16#20,    3,  false,   3, [a,a,a],              lt, "Arg0 := Arg1  < Arg2."}
    , { 'GT',           16#21,    3,  false,   3, [a,a,a],              gt, "Arg0 := Arg1  > Arg2."}
    , { 'EQ',           16#22,    3,  false,   3, [a,a,a],              eq, "Arg0 := Arg1  = Arg2."}
    , { 'ELT',          16#23,    3,  false,   3, [a,a,a],             elt, "Arg0 := Arg1 =< Arg2."}
    , { 'EGT',          16#24,    3,  false,   3, [a,a,a],             egt, "Arg0 := Arg1 >= Arg2."}
    , { 'NEQ',          16#25,    3,  false,   3, [a,a,a],             neq, "Arg0 := Arg1 /= Arg2."}
    , { 'AND',          16#26,    3,  false,   3, [a,a,a],          and_op, "Arg0 := Arg1 and Arg2."}
    , { 'OR',           16#27,    3,  false,   3, [a,a,a],           or_op, "Arg0 := Arg1  or Arg2."}
    , { 'NOT',          16#28,    2,  false,   3, [a,a],            not_op, "Arg0 := not Arg1."}
    , { 'TUPLE',        16#29,    1,  false,   3, [ii],              tuple, "Create a tuple of size = Arg0. Elements on stack."}
    , { 'ELEMENT',      16#2a,    3,  false,   3, [a,a,a],      element_op, "Arg1 := element(Arg2, Arg3)."}
    , { 'MAP_EMPTY',    16#2b,    1,  false,   3, [a],           map_empty, "Arg0 := #{}."}
    , { 'MAP_LOOKUP',   16#2c,    3,  false,   3, [a,a,a],      map_lookup, "Arg0 := lookup key Arg2 in map Arg1."}
    , { 'MAP_LOOKUPD',  16#2d,    4,  false,   3, [a,a,a,a],    map_lookup, "Arg0 := lookup key Arg2 in map Arg1 if key exists in map otherwise Arg0 := Arg3."}
    , { 'MAP_UPDATE',   16#2e,    4,  false,   3, [a,a,a,a],    map_update, "Arg0 := update key Arg2 in map Arg1 with value Arg3."}
    , { 'MAP_DELETE',   16#2f,    3,  false,   3, [a,a,a],      map_delete, "Arg0 := delete key Arg2 from map Arg1."}
    , { 'MAP_MEMBER',   16#30,    3,  false,   3, [a,a,a],      map_member, "Arg0 := true if key Arg2 is in map Arg1."}
    , { 'MAP_FROM_LIST',16#31,    2,  false,   3, [a,a],     map_from_list, "Arg0 := make a map from (key, value) list in Arg1."}
    , { 'NIL',          16#32,    1,  false,   3, [a],                 nil, "Arg0 := []."}
    , { 'IS_NIL',       16#33,    2,  false,   3, [a,a],            is_nil, "Arg0 := true if Arg1 == []."}
    , { 'CONS',         16#34,    3,  false,   3, [a,a,a],            cons, "Arg0 := [Arg1|Arg2]."}
    , { 'HD',           16#35,    2,  false,   3, [a,a],                hd, "Arg0 := head of list Arg1."}
    , { 'TL',           16#36,    2,  false,   3, [a,a],                tl, "Arg0 := tail of list Arg1."}
    , { 'LENGTH',       16#37,    2,  false,   3, [a,a],            length, "Arg0 := length of list Arg1."}
    , { 'STR_EQ',       16#38,    3,  false,   3, [a,a,a],          str_eq, "Arg0 := true iff the strings Arg1 and Arg2 are the same."}
    , { 'STR_JOIN',     16#39,    3,  false,   3, [a,a,a],        str_join, "Arg0 := string Arg1 followed by string Arg2."}
    , { 'INT_TO_STR',   16#40,    2,  false,   3, [a,a],        int_to_str, "Arg0 := turn integer Arg1 into a string."}
    , { 'ADDR_TO_STR',  16#41,    2,  false,   3, [a,a],       addr_to_str, "Arg0 := turn address Arg1 into a string."}
    , { 'STR_REVERSE',  16#42,    2,  false,   3, [a,a],       str_reverse, "Arg0 := the reverse of string Arg1."}
    , { 'INT_TO_ADDR',  16#43,    2,  false,   3, [a,a],       int_to_addr, "Arg0 := turn integer Arg1 into an address."}
    , { 'VARIANT',      16#44,    4,  false,   3, [a,a,a,a],       variant, "Arg0 := create a variant of size Arg1 with the tag Arg2 (Arg2 < Arg1) and take Arg3 elements from the stack."}
    , { 'VARIANT_TEST', 16#45,    3,  false,   3, [a,a,a],    variant_test, "Arg0 := true if variant Arg1 has the tag Arg2."}
    , { 'VARIANT_ELEMENT',16#46,  3,  false,   3, [a,a,a], variant_element, "Arg0 := element number Arg2 from variant Arg1."}
    , { 'BITS_NONEA',   16#47,    0,  false,   3, atomic,        bits_none, "accumulator := empty bitmap."}
    , { 'BITS_NONE',    16#48,    1,  false,   3, [a],           bits_none, "Arg0 := empty bitmap."}
    , { 'BITS_ALLA',    16#49,    0,  false,   3, atomic,         bits_all, "accumulator := full bitmap."}
    , { 'BITS_ALL',     16#50,    1,  false,   3, [a],            bits_all, "Arg0 := full bitmap."}
    , { 'BITS_ALL_N',   16#51,    2,  false,   3, [a,a],        bits_all_n, "Arg0 := bitmap with Arg1 bits set."}
    , { 'BITS_SET',     16#52,    3,  false,   3, [a,a,a],        bits_set, "Arg0 := set bit Arg2 of bitmap Arg1."}
    , { 'BITS_CLEAR',   16#53,    3,  false,   3, [a,a,a],      bits_clear, "Arg0 := clear bit Arg2 of bitmap Arg1."}
    , { 'BITS_TEST',    16#54,    3,  false,   3, [a,a,a],       bits_test, "Arg0 := true if bit Arg2 of bitmap Arg1 is set."}
    , { 'BITS_SUM',     16#55,    2,  false,   3, [a,a],          bits_sum, "Arg0 := sum of set bits in bitmap Arg1. Exception if infinit bitmap."}
    , { 'BITS_OR',      16#56,    3,  false,   3, [a,a,a],         bits_or, "Arg0 := Arg1 v Arg2."}
    , { 'BITS_AND',     16#57,    3,  false,   3, [a,a,a],        bits_and, "Arg0 := Arg1 ^ Arg2."}
    , { 'BITS_DIFF',    16#58,    3,  false,   3, [a,a,a],       bits_diff, "Arg0 := Arg1 - Arg2."}
    , { 'ADDRESS',      16#59,    1,  false,   3, [a],             address, "Arg0 := The current contract address."}
    , { 'BALANCE',      16#5a,    1,  false,   3, [a],             balance, "Arg0 := The current contract balance."}
    , { 'ORIGIN',       16#5b,    1,  false,   3, [a],              origin, "Arg0 := Address of contract called by the call transaction."}
    , { 'CALLER',       16#5c,    1,  false,   3, [a],              caller, "Arg0 := The address that signed the call transaction."}
    , { 'GASPRICE',     16#5d,    1,  false,   3, [a],            gasprice, "Arg0 := The current gas price."}
    , { 'BLOCKHASH',    16#5e,    2,  false,   3, [a, a],        blockhash, "Arg0 := The blockhash at height."}
    , { 'BENEFICIARY',  16#5f,    1,  false,   3, [a],         beneficiary, "Arg0 := The address of the current beneficiary."}
    , { 'TIMESTAMP',    16#60,    1,  false,   3, [a],           timestamp, "Arg0 := The current timestamp. Unrelaiable, don't use for anything."}
    , { 'GENERATION',   16#61,    1,  false,   3, [a],          generation, "Arg0 := The block height of the cureent generation."}
    , { 'MICROBLOCK',   16#62,    1,  false,   3, [a],          microblock, "Arg0 := The current micro block number."}
    , { 'DIFFICULTY',   16#63,    1,  false,   3, [a],          difficulty, "Arg0 := The current difficulty."}
    , { 'GASLIMIT',     16#64,    1,  false,   3, [a],            gaslimit, "Arg0 := The current gaslimit."}
    , { 'GAS',          16#65,    1,  false,   3, [a],                 gas, "Arg0 := The amount of gas left."}

    , { 'LOG0',         16#66,    2,  false,   3, [a,a],               log, "Create a log message in the call object."}
    , { 'LOG1',         16#67,    3,  false,   3, [a,a,a],             log, "Create a log message with one topic in the call object."}
    , { 'LOG2',         16#68,    4,  false,   3, [a,a,a,a],           log, "Create a log message with two topics in the call object."}
    , { 'LOG3',         16#69,    5,  false,   3, [a,a,a,a,a],         log, "Create a log message with three topics in the call object."}
    , { 'LOG4',         16#6a,    6,  false,   3, [a,a,a,a,a,a],       log, "Create a log message with four topics in the call object."}
    , { 'DEACTIVATE',   16#6b,    0,  false,   3, atomic,       deactivate, "Mark the current contract for deactication."}
      %% Transaction ops
    , { 'SPEND',               16#6c, 2, false,3, [a,a],                       spend, "Transfer Arg0 tokens to account Arg1. (If the contract account has at least that many tokens."}
    , { 'ORACLE_REGISTER',     16#6d, 6, false,3, [a,a,a,a,a,a],     oracle_register, "Mark the current contract for deactication."}
      %% TODO:
    , { 'ORACLE_QUERY',        16#6e, 0, false,3, atomic,       oracle_query, ""}
    , { 'ORACLE_RESPOND',      16#6f, 0, false,3, atomic,     oracle_respond, ""}
    , { 'ORACLE_EXTEND',       16#70, 0, false,3, atomic,      oracle_extend, ""}
    , { 'ORACLE_GET_ANSWER',   16#71, 0, false,3, atomic,  oracle_get_answer, ""}
    , { 'ORACLE_GET_QUESTION', 16#72, 0, false,3, atomic,oracle_get_question, ""}
    , { 'ORACLE_QUERY_FEE',    16#73, 0, false,3, atomic,   oracle_query_fee, ""}
    , { 'AENS_RESOLVE',        16#74, 0, false,3, atomic,       aens_resolve, ""}
    , { 'AENS_PRECLAIM',       16#75, 0, false,3, atomic,      aens_preclaim, ""}
    , { 'AENS_CLAIM',          16#76, 0, false,3, atomic,         aens_claim, ""}
    , { 'AENS_UPDATE',         16#77, 0, false,3, atomic,        aend_update, ""}
    , { 'AENS_TRANSFER',       16#78, 0, false,3, atomic,      aens_transfer, ""}
    , { 'AENS_REVOKE',         16#79, 0, false,3, atomic,        aens_revoke, ""}
    , { 'ECVERIFY',            16#7a, 0, false,3, atomic,           ecverify, ""}
    , { 'SHA3',                16#7b, 0, false,3, atomic,               sha3, ""}
    , { 'SHA256',              16#7c, 0, false,3, atomic,             sha256, ""}
    , { 'BLAKE2B',             16#7d, 0, false,3, atomic,            blake2b, ""}
    , { 'BALANCE_OTHER',       16#7e, 2, false,3, [a,a],       balance_other, "Arg0 := The balance of address Arg1."}
    , { 'SETELEMENT',          16#7f, 4, false,3, [a,a,a,a],      setelement, "Arg0 := a new tuple similar to Arg2, but with element number Arg1 replaced by Arg3."}


    , { 'DUMMY7ARG',           16#f9, 7, false,3, [a,a,a,a,a,a,a],  dummyarg, "Temporary dummy instruction to test 7 args."}
    , { 'DUMMY8ARG',           16#fa, 8, false,3, [a,a,a,a,a,a,a,a],dummyarg, "Temporary dummy instruction to test 8 args."}
    , { 'ABORT',         16#fb,    1,  false,   3, [a],               abort, "Abort execution (dont use all gas) with error message in Arg0."}
    , { 'EXIT',          16#fc,    1,  false,   3, [a],                exit, "Abort execution (use upp all gas) with error message in Arg0."}
    , { 'NOP',          16#fd,    0,  false,   1, atomic,              nop, "The no op. does nothing."}
    %% FUNCTION         16#fe                                               "Function declaration and entrypoint."
    %% EXTEND           16#ff                                               "Reserved for future extensions beyond one byte opcodes."
    ].


generate_header_file(Filename, Ops) ->
    {ok, File} = file:open(Filename, [write]),
    Defines = lists:flatten([gen_defines(Op) || Op <- Ops]),
    io:format(File, "~s", [prelude("Provides opcode defines.\n")]),
    io:format(File, "%% FATE opcodes\n~s", [Defines]),
    io:format(File, "~s",
              ["-define('FUNCTION'                , 16#fe).\n"
               "-define('EXTEND'                  , 16#ff).\n\n"]),
    file:close(File).

generate_opcodes_ops(Modulename, HrlFile, SrcDir, Ops) ->
    Filename = SrcDir ++ atom_to_list(Modulename) ++ ".erl",

    {ok, File} = file:open(Filename, [write]),
    Mnemonic = lists:flatten([gen_mnemonic(Op) || Op <- Ops]),
    ToOp = lists:flatten([gen_m_to_op(Op) || Op <- Ops]),
    Args = lists:flatten([gen_args(Op) || Op <- Ops]),
    EndBB = lists:flatten([gen_bb(Op) || Op <- Ops]),

    io:format(File, "~s", [prelude("Provides opcode primitives.\n")]),
    io:format(File, "~s", [ops_exports(Modulename, HrlFile,
                                       ["args/1\n"
                                        "        , end_bb/1\n"
                                        "        , mnemonic/1\n"
                                        "        , m_to_op/1\n"
                                       ])]),

    io:format(File, "%% FATE mnemonics\n~s", [Mnemonic]),
    io:format(File, "mnemonic(Op) -> exit({bad_opcode, Op}).\n\n", []),

    io:format(File, "%% FATE opcodes\n~s", [ToOp]),
    io:format(File, "m_to_op(M) -> exit({bad_mnemonic, M}).\n\n", []),

    io:format(File, "%% FATE numbers of args to op.\n~s", [Args]),
    io:format(File, "args(Op) -> exit({bad_opcode, Op}).\n\n", []),

    io:format(File, "%% Does FATE Op end a Basic Block?\n~s", [EndBB]),
    io:format(File, "end_bb(_) -> false.\n\n", []),

    file:close(File).

generate_code_ops(Modulename, SrcDir, Ops) ->
    Filename = SrcDir ++ atom_to_list(Modulename) ++ ".erl",

    {ok, File} = file:open(Filename, [write]),
    Types = lists:flatten([gen_type(Op) || Op <- Ops]),
    TypeExports = lists:flatten([gen_type_exports(Op) || Op <- Ops]),
    [#{type_name := FirstType} | RestOfOps] = Ops,
    FateTypes = lists:flatten([gen_fate_code_type(Op) || Op <- RestOfOps]),
    ConstructorExports = lists:flatten([gen_constructor_exports(Op) || Op <- Ops]),
    Constructors = lists:flatten([gen_constructors(Op) || Op <- Ops]),

    io:format(File, "~s", [prelude(" Provide constructor functuions for "
                                   "Fate instructions.\n%%% Provide types"
                                   " and documentation for Fate "
                                   "instructions.\n")]),
    io:format(File, "-module(~w).\n\n", [Modulename]),
    io:format(File, "-include_lib(\"aebytecode/include/aeb_fate_data.hrl\").\n\n"
              "-define(i(__X__), {immediate, __X__ }).\n\n"
              "-type fate_arg_immediate(T) :: {immediate, T}.\n"
              "-type fate_arg_var()        :: {var, integer()}.\n"
              "-type fate_arg_arg()        :: {arg, integer()}.\n"
              "-type fate_arg_stack()      :: {stack, integer()}.\n"
              "-type fate_arg() :: fate_arg_immediate()\n"
              "                  | fate_arg_var()\n"
              "                  | fate_arg_arg()\n"
              "                  | fate_arg_stack().\n\n"
              "-type fate_arg_immediate() :: {immediate, aeb_fate_data:fate_type()}.\n"
             , []),
    io:format(File, "~s", [Types]),
    io:format(File, "-type fate_code() :: ~s\n~s                   .\n\n",
              [FirstType, FateTypes]),
    io:format(File, "-export_type([ fate_code/0\n~s            ]).\n\n", [TypeExports]),
    io:format(File, "-export([ foo/0\n~s       ]).\n\n", [ConstructorExports]),
    io:format(File, "~s\n", [Constructors]),

    io:format(File, "foo() -> \"A temp hack.\".\n", []),

    file:close(File).

gen_type(#{type_name := TypeName, type := Type}) ->
    lists:flatten(io_lib:format("-type ~-26s :: ~s.\n",
                                [TypeName, Type])).

gen_fate_code_type(#{type_name := TypeName}) ->
    lists:flatten(io_lib:format("                   | ~s\n", [TypeName])).

gen_type_exports(#{type_name := TypeName}) ->
    lists:flatten(io_lib:format("             , ~s/0\n", [TypeName--"()"])).

gen_constructor_exports(#{constructor_type := Function}) ->
    lists:flatten(io_lib:format("        , ~s\n", [Function])).

gen_constructors(#{constructor := Function, format := atomic,
                   type_name := Type, opname := Name}) ->
    lists:flatten(io_lib:format("-spec ~s() -> ~s.\n"
                                "~s() ->\n"
                                "    ~w.\n\n",
                                [Function, Type, Function, Name]));
gen_constructors(#{constructor := Function, format := ArgSpec,
                   type_name := Type, opname := Name}) ->
    ArgTypeSpecs = gen_arg_type_specs(ArgSpec),
    Args = gen_arg_names(0, ArgSpec),
    UseArgs = gen_arg_uses(0, ArgSpec),
    lists:flatten(io_lib:format("-spec ~s(~s) -> ~s.\n"
                                "~s(~s) ->\n"
                                "    {~w, ~s}.\n\n",
                                [Function, ArgTypeSpecs, Type,
                                 Function, Args, Name, UseArgs])).

gen_arg_type_specs([]) -> [];
gen_arg_type_specs([a]) -> "fate_arg()";
gen_arg_type_specs([is]) -> "aeb_fate_data:fate_string()";
gen_arg_type_specs([ii]) -> "aeb_fate_data:fate_integer()";
gen_arg_type_specs([li]) -> "[aeb_fate_data:fate_integer()]";
gen_arg_type_specs([t]) -> "aeb_fate_data:fate_type_type()";
gen_arg_type_specs([a | Args]) -> "fate_arg(), " ++ gen_arg_type_specs(Args);
gen_arg_type_specs([is | Args]) -> "aeb_fate_data:fate_string(), " ++ gen_arg_type_specs(Args);
gen_arg_type_specs([ii | Args]) -> "aeb_fate_data:fate_integer(), " ++ gen_arg_type_specs(Args);
gen_arg_type_specs([li | Args]) -> "[aeb_fate_data:fate_integer()], " ++ gen_arg_type_specs(Args);
gen_arg_type_specs([t | Args]) -> "aeb_fate_data:fate_type_type(), " ++ gen_arg_type_specs(Args).


gen_arg_names(_, []) ->
    [];
gen_arg_names(N, [_]) -> io_lib:format("Arg~w", [N]);
gen_arg_names(N, [_|Args]) ->
    io_lib:format("Arg~w, ", [N]) ++ gen_arg_names(N+1, Args).

gen_arg_uses(_, []) ->
    [];
gen_arg_uses(N, [a]) -> io_lib:format("Arg~w", [N]);
gen_arg_uses(N, [is]) -> io_lib:format("{immediate, Arg~w}", [N]);
gen_arg_uses(N, [ii]) -> io_lib:format("{immediate, Arg~w}", [N]);
gen_arg_uses(N, [li]) -> io_lib:format("[{immediate, I} || I <- Arg~w]", [N]);
gen_arg_uses(N, [t]) -> io_lib:format("Arg~w", [N]);
gen_arg_uses(N, [a | Args]) ->
    io_lib:format("Arg~w, ", [N]) ++ gen_arg_uses(N+1, Args);
gen_arg_uses(N, [is | Args]) ->
    io_lib:format("{immediate, Arg~w}, ", [N]) ++ gen_arg_uses(N+1, Args);
gen_arg_uses(N, [ii | Args]) ->
    io_lib:format("{immediate, Arg~w}, ", [N]) ++ gen_arg_uses(N+1, Args);
gen_arg_uses(N, [li | Args]) ->
    io_lib:format("[{immediate, I} || I <- Arg~w], ", [N]) ++ gen_arg_uses(N+1, Args);
gen_arg_uses(N, [t | Args]) ->
    io_lib:format("Arg~w, ", [N]) ++ gen_arg_uses(N+1, Args).


ops_exports(Module, HrlFile, Exports) ->
    lists:flatten(io_lib:format(
                    "-module(~w).\n\n"
                    "-export([ ~s         ]).\n\n"
                    "-include_lib(\"aebytecode/" ++ HrlFile ++"\").\n\n"
                    "%%====================================================================\n"
                    "%% API\n"
                    "%%====================================================================\n",
                    [Module, Exports])).

gen_mnemonic(#{opname := Name, macro := Macro}) ->
    lists:flatten(io_lib:format("mnemonic(~21s) -> ~21w ;\n",
                                [Macro, Name])).


gen_m_to_op(#{opname := Name, macro := Macro}) ->
    lists:flatten(io_lib:format("m_to_op(~21w) -> ~21s ;\n",
                                [Name, Macro])).

gen_args(#{macro := Macro, args := Args}) ->
    lists:flatten(io_lib:format("args(~21s) -> ~2w ;\n",
                                [Macro, Args])).

gen_bb(#{macro := Macro, end_bb := EndBB}) ->
    lists:flatten(io_lib:format("end_bb(~21s) -> ~w ;\n",
                                [Macro, EndBB])).


prelude(Doc) ->
    "%%%-------------------------------------------------------------------\n"
        "%%% @copyright (C) 2019, Aeternity Anstalt\n"
        "%%%\n"
        "%%%   === ===  N O T E :   This file is generated do not edit. === ===\n"
        "%%%\n"
        "%%% Source is in aeb_fate_generate_ops.erl\n"
        "%%% @doc\n"
        "%%%     "++Doc++
        "%%% @end\n"
        "%%%-------------------------------------------------------------------\n\n".


gen_defines(#{opname := Name, opcode := OpCode}) ->
    lists:flatten(io_lib:format("-define(~-26w, 16#~2.16.0b).\n", [Name, OpCode])).

gen([]) ->
    [];
gen([{OpName, OpCode, Args, EndBB, Gas, FateFormat, Constructor, Doc} | Rest]) ->
    Name = atom_to_list(OpName),
    LowerName = string:to_lower(Name),
    TypeName = "fate_" ++ LowerName ++ "()",
    Macro = "?" ++ Name,
    Type = case FateFormat of
               atomic -> io_lib:format("~w", [OpName]);
               ArgTypes  ->
                    io_lib:format("{~w, ~s}", [OpName, expand_types(ArgTypes)])
           end,
    ConstructorType = atom_to_list(Constructor) ++ "/" ++ io_lib:format("~w", [Args]),

    [#{ opname            => OpName
      , opcode            => OpCode
      , args              => Args
      , end_bb            => EndBB
      , format            => FateFormat
      , macro             => Macro
      , type_name         => TypeName
      , doc               => Doc
      , gas               => Gas
      , type              => Type
      , constructor       => Constructor
      , constructor_type  => ConstructorType
      }| gen(Rest)].


expand_types([]) -> "";
expand_types([T]) -> expand_type(T);
expand_types([T|Ts]) ->expand_type(T) ++ ", " ++ expand_types(Ts).

expand_type(a)  -> "fate_arg()";
expand_type(is) -> "fate_arg_immediate(aeb_fate_data:fate_string())";
expand_type(ii) -> "fate_arg_immediate(aeb_fate_data:fate_integer())";
expand_type(li) -> "[fate_arg_immediate(aeb_fate_data:fate_integer())]";
expand_type(t)  -> "aeb_fate_data:fate_type_type()".

generate_scanner(TemplateFile, Outfile, Path, Ops) ->
    {ok, Template} = file:read_file(filename:join(Path,TemplateFile)),
    Tokens = lists:flatten([gen_token(Op) || Op <- Ops]),
    NewFile = insert_tokens_in_template(Template, Tokens),
    file:write_file(filename:join(Path, Outfile), NewFile).

gen_token(#{opname := OpName}) ->
    Name = atom_to_list(OpName),
    io_lib:format("~-28s: {token, {mnemonic, TokenLine, ~w}}.\n",
                  [Name, OpName]).

insert_tokens_in_template(<<"%% ###REPLACEWITHOPTOKENS###", Rest/binary >>, Tokens) ->
    [Tokens, Rest];
insert_tokens_in_template(<<"%%% ###REPLACEWITHNOTE###", Rest/binary >>, Tokens) ->
    [
     "%%%\n"
     "%%%   === ===  N O T E :   This file is generated do not edit. === ===\n"
     "%%%\n"
     "%%% Source is in aeb_fate_generate_ops.erl\n"
     "%%%          and aeb_fate_asm_scan.template"
     | insert_tokens_in_template(Rest, Tokens)];
insert_tokens_in_template(<<B,Rest/binary>>, Tokens) ->
    [B|insert_tokens_in_template(Rest, Tokens)].

gen_asm_pp(Module, Path, Ops) ->
    Filename = filename:join(Path, atom_to_list(Module)) ++ ".erl",
    {ok, File} = file:open(Filename, [write]),
    Formats = lists:flatten([gen_format(Op)++"\n" || Op <- Ops]),

    io:format(File, "~s", [prelude(" Provide pretty printing functuions for "
                                   "Fate instructions.\n")]),
    io:format(File, "-module(~w).\n\n", [Module]),
    io:format(File,
              "-export([format_op/2]).\n\n"
              "format_arg(li, {immediate, LI}) ->\n"
              "    aeb_fate_data:format(LI);\n"
              "format_arg(_, {immediate, I}) ->\n"
              "    aeb_fate_data:format(I);\n"
              "format_arg(a, {arg, N}) -> io_lib:format(\"arg~~p\", [N]);\n"
              "format_arg(a, {var, N}) -> io_lib:format(\"var~~p\", [N]);\n"
              "format_arg(a, {stack, 0}) -> \"a\";\n"
              "format_arg(a, {stack, N}) -> io_lib:format(\"a~~p\", [N]).\n\n"
              "lookup(Name, Symbols) ->\n"
              "    maps:get(Name, Symbols, io_lib:format(\"~~p\",[Name])).\n\n"
              "~s"
             , [Formats]),

    io:format(File, "format_op(Op, _Symbols) -> io_lib:format(\";; Bad Op: ~~w\\n\", [Op]).\n", []),
    file:close(File).

gen_format(#{opname := Name}) when ('CALL' =:= Name) or (Name =:= 'CALL_T') ->
    io_lib:format("format_op({~w, {immediate, Function}}, Symbols) ->\n"
                  "[\"~s \", lookup(Function, Symbols)];",
                  [Name, atom_to_list(Name)]);
gen_format(#{opname := Name}) when (Name =:= 'CALL_R') or (Name =:= 'CALL_TR') ->
    io_lib:format("format_op({~w, {immediate, Contract}, {immediate, Function}}, Symbols) ->\n"
                  "[\"~s \", lookup(Contract, Symbols), \".\", lookup(Function, Symbols)];\n"
                  "format_op({~w, Contract, {immediate, Function}}, Symbols) ->\n"
                  "[\"~s \", format_arg(a, Contract), \".\", lookup(Function, Symbols)];",
                  [Name, atom_to_list(Name), Name, atom_to_list(Name)]);
gen_format(#{opname := Name, format := atomic}) ->
    io_lib:format("format_op(~w, _) -> [\"~s\"];", [Name, atom_to_list(Name)]);
gen_format(#{opname := Name, format := Args}) ->
    NameAsString = atom_to_list(Name),
    case Args of
        [T0] ->
            io_lib:format(
              "format_op({~w, Arg0}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0)];",
              [Name, NameAsString, T0]);
        [T0, T1] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1)];",
              [Name, NameAsString, T0, T1]);
        [T0, T1, T2] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1, Arg2}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1),"
              "\" \",  format_arg(~w, Arg2)];",
              [Name, NameAsString, T0, T1, T2]);
        [T0, T1, T2, T3] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1, Arg2, Arg3}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1),"
              "\" \",  format_arg(~w, Arg2),"
              "\" \",  format_arg(~w, Arg3)];",
              [Name, NameAsString, T0, T1, T2, T3]);
        [T0, T1, T2, T3, T4] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1, Arg2, Arg3, Arg4}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1),"
              "\" \",  format_arg(~w, Arg2),"
              "\" \",  format_arg(~w, Arg3),"
              "\" \",  format_arg(~w, Arg4)];",
              [Name, NameAsString, T0, T1, T2, T3, T4]);
        [T0, T1, T2, T3, T4, T5] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1, Arg2, Arg3, Arg4, Arg5}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1),"
              "\" \",  format_arg(~w, Arg2),"
              "\" \",  format_arg(~w, Arg3),"
              "\" \",  format_arg(~w, Arg4),"
              "\" \",  format_arg(~w, Arg5)];",
              [Name, NameAsString, T0, T1, T2, T3, T4, T5]);
        [T0, T1, T2, T3, T4, T5, T6] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1),"
              "\" \",  format_arg(~w, Arg2),"
              "\" \",  format_arg(~w, Arg3),"
              "\" \",  format_arg(~w, Arg4),"
              "\" \",  format_arg(~w, Arg5),"
              "\" \",  format_arg(~w, Arg6)];",
              [Name, NameAsString, T0, T1, T2, T3, T4, T5, T6]);
        [T0, T1, T2, T3, T4, T5, T6, T7] ->
            io_lib:format(
              "format_op({~w, Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7}, _) ->\n"
              "    [\"~s \", format_arg(~w, Arg0), "
              "\" \",  format_arg(~w, Arg1),"
              "\" \",  format_arg(~w, Arg2),"
              "\" \",  format_arg(~w, Arg3),"
              "\" \",  format_arg(~w, Arg4),"
              "\" \",  format_arg(~w, Arg5),"
              "\" \",  format_arg(~w, Arg6),"
              "\" \",  format_arg(~w, Arg7)];",
              [Name, NameAsString, T0, T1, T2, T3, T4, T5, T6, T7])
    end.

test_asm_generator(Filename) ->
    {ok, File} = file:open(Filename, [write]),
    Instructions = lists:flatten([gen_instruction(Op)++"\n" || Op <- get_ops()]),
    io:format(File,
              ";; CONTRACT all_instructions\n\n"
              ";; Dont expect this contract to typecheck or run.\n"
              ";; Just used to check assembler rountrip of all instruction.\n\n"
              "FUNCTION foo () : {tuple, []}\n"
              "~s"
             , [Instructions]),
    io:format(File, "  RETURNR ()\n", []),
    file:close(File).


gen_instruction(#{opname := Name, format := atomic}) ->
    io_lib:format("  ~s\n", [Name]);
gen_instruction(#{opname := Name, format := ArgTypes}) ->
    Args = lists:flatten(lists:join(" ", [gen_arg(A) || A <- ArgTypes])),
    I = io_lib:format("  ~s ~s\n", [Name, Args]),
    I.

%% This should be done with a Quick Check generator...
gen_arg(a) -> any_arg();
gen_arg(is) -> "foo";
gen_arg(ii) -> gen_int();
gen_arg(li) -> "[1, 2, 3]";
gen_arg(t) -> "integer".

any_arg() ->
    element(rand:uniform(5), {"a", stack_arg(), var_arg(), arg_arg(), imm_arg()}).
stack_arg() -> "a" ++ integer_to_list(rand:uniform(255)-1).
arg_arg() ->  "arg" ++ integer_to_list(rand:uniform(256)-1).
var_arg() ->  "var" ++ integer_to_list(rand:uniform(256)-1).
imm_arg() ->
    case rand:uniform(15) of
        1 -> gen_int();
        2 -> gen_int();
        3 -> gen_int();
        4 -> gen_int();
        5 -> gen_int();
        6 -> gen_int();
        7 -> gen_int();
        8 -> gen_address();
        9 -> gen_boolean();
        10 -> gen_string();
        11 -> gen_map();
        12 -> gen_list();
        13 -> gen_bits();
        14 -> gen_tuple();
        15 -> gen_variant()
    end.

gen_key() ->
    case rand:uniform(15) of
        1 -> gen_int();
        2 -> gen_int();
        3 -> gen_int();
        4 -> gen_int();
        5 -> gen_int();
        6 -> gen_int();
        7 -> gen_int();
        8 -> gen_address();
        9 -> gen_boolean();
        10 -> gen_string();
        11 -> gen_string();
        12 -> gen_list();
        13 -> gen_bits();
        14 -> gen_tuple();
        15 -> gen_variant()
    end.

gen_boolean() ->
    element(rand:uniform(2), {"true", "false"}).

gen_int() ->
    element(rand:uniform(4),
            { integer_to_list(rand:uniform(round(math:pow(10,40))))
            , integer_to_list(rand:uniform(10))
            , integer_to_list(rand:uniform(100))
            , io_lib:format("0x~.16b",[rand:uniform(round(math:pow(10,10)))])}).

gen_address() -> "#nv5B93FPzRHrGNmMdTDfGdd5xGZvep3MVSpJqzcQmMp59bBCv".
gen_string() -> "\"foo\"".
gen_map() -> "{ " ++ gen_key() ++ " => " ++ imm_arg() ++ "}".
gen_list() ->
    case rand:uniform(4) of
        1 -> "[]";
        2 -> "[" ++ lists:join(", ", gen_list_elements()) ++ " ]";
        3 -> "[ " ++ imm_arg() ++ " ]";
        4 -> "[ " ++ imm_arg() ++ ", " ++ imm_arg() ++ " ]"
    end.

%% Not type correct.
gen_list_elements() ->
    case rand:uniform(3) of
        1 -> [imm_arg() |  gen_list_elements()];
        2 -> [];
        3 -> [imm_arg()]
    end.

gen_bits() ->
    element(rand:uniform(3),
            {"<>"
            ,"!<>"
            , "101010"}).

gen_tuple() ->
    case rand:uniform(3) of
        1 -> "()";
        2 -> "(42)";
        3 -> "(" ++ imm_arg() ++ ")"
    end.

gen_variant() ->
    case rand:uniform(3) of
        1 -> "(| 5 | 2 | (1, \"foo\", ()) |)";
        2 -> "(| 2 | 1 | ( " ++ imm_arg() ++ " ) |)";
        3 -> "(| 2 | 0 | ( " ++ imm_arg() ++ ", " ++ imm_arg() ++ " ) |)"
    end.


%% TODO: add gas cost.
generate_documentation(Filename) ->
    {ok, File} = file:open(Filename, [write]),
    Instructions = lists:flatten([gen_doc(Op)++"\n" || Op <- get_ops()]),
    io:format(File,
              "### Operations\n\n"
              "| OpCode | Name | Args | Description |\n"
              "| ---    | ---  | ---  |        ---  |\n"
              "~s"
             , [Instructions]),
    io:format(File, "\n", []),
    file:close(File).

gen_doc(#{ opname            := Name
         , opcode            := OpCode
         , args              := _Args
         , end_bb            := _EndBB
         , format            := FateFormat
         , macro             := _Macro
         , type_name         := _TypeName
         , doc               := Doc
         , gas               := _Gas
         , type              := _Type
         , constructor       := _Constructor
         , constructor_type  := _ConstructorType
         }) ->
    Arguments =
        case FateFormat of
            atomic -> "";
            _ ->  lists:join(" ",
                             [format_arg_doc(A) ||
                                 A <-
                                     lists:zip(FateFormat,
                                               lists:seq(0,length(FateFormat)-1))])
        end,
    io_lib:format("| 0x~.16b | ~w | ~s | ~s |\n",
                  [ OpCode
                  , Name
                  , Arguments
                  , Doc]).

format_arg_doc({a, N}) -> io_lib:format("Arg~w", [N]);
format_arg_doc({is,_N}) -> "Identifier";
format_arg_doc({ii,_N}) -> "Integer";
format_arg_doc({li,_N}) -> "[Integers]";
format_arg_doc({t,_N}) -> "Type".

