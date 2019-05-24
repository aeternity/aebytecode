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
    check_defs(ops_defs()),
    Ops = get_ops(),
    %% io:format("ops: ~p\n", [Ops]),
    HrlFile = Include ++ "aeb_fate_opcodes.hrl",
    generate_header_file(HrlFile, Ops),
    generate_opcodes_ops(aeb_fate_opcodes, HrlFile, Src, Ops),
    generate_code_ops(aeb_fate_ops, Src, Ops),
    generate_scanner("aeb_fate_asm_scan.template", "aeb_fate_asm_scan.xrl", Src, Ops),
    gen_asm_pp(aeb_fate_pp, Src, Ops).

check_defs(List) ->
    true = check_numbering(0, lists:keysort(2, List)).

check_numbering(N, [T|Rest]) ->
    OpCode = element(2, T),
    case OpCode of
        N -> check_numbering(N+1, Rest);
        16#fa -> check_numbering(16#fa+1, Rest);
        _ when OpCode < N -> {duplicate_opcode, OpCode};
        _ when OpCode > N -> {missing_opcode, N}
    end;
check_numbering(_, []) -> true.


%% TODO: Some real gas numbers...
ops_defs() ->
    %%  Opname,               Opcode, end_bb, gas, format,      Constructor,                              ArgType, ResType, Documentation
    [ { 'RETURN',              16#00,   true,   2, [],               return,                                   {},     any, "Return from function call, top of stack is return value . The type of the retun value has to match the return type of the function."}
    , { 'RETURNR',             16#01,   true,   2, [a],             returnr,                                {any},     any, "Push Arg0 and return from function. The type of the retun value has to match the return type of the function."}
    , { 'CALL',                16#02,   true,   4, [a],                call,                             {string},     any, "Call the function Arg0 with args on stack. The types of the arguments has to match the argument typs of the function."}
    , { 'CALL_R',              16#03,   true,   8, [a,is,a],         call_r,            {string, string, integer},     any, "Remote call to contract Arg0 and function Arg1 with value Arg2. The types of the arguments has to match the argument typs of the function."}
    , { 'CALL_T',              16#04,   true,   4, [a],              call_t,                             {string},     any, "Tail call to function Arg0. The types of the arguments has to match the argument typs of the function. And the return type of the called function has to match the type of the current function."}
    , { 'CALL_TR',             16#05,   true,   8, [a,is,a],        call_tr,            {string, string, integer},     any, "Remote tail call to contract Arg0 and function Arg1 with value Arg2. The types of the arguments has to match the argument typs of the function. And the return type of the called function has to match the type of the current function."}
    , { 'CALL_GR',             16#06,   true,   8, [a,is,a,a],      call_gr,   {string, string, integer, integer},     any, "Remote call with gas cap in Arg3. Otherwise as CALL_R."}
    , { 'CALL_GTR',            16#07,   true,   8, [a,is,a,a],     call_gtr,   {string, string, integer, integer},     any, "Remote tail call with gas cap in Arg3. Otherwise as CALL_TR."}
    , { 'JUMP',                16#08,   true,   3, [ii],               jump,                            {integer},    none, "Jump to a basic block. The basic block has to exist in the current function."}
    , { 'JUMPIF',              16#09,   true,   4, [a,ii],           jumpif,                   {boolean, integer},    none, "Conditional jump to a basic block. If Arg0 then jump to Arg1."}
    , { 'SWITCH_V2',           16#0a,   true,   4, [a,ii,ii],        switch,          {variant, integer, ingeger},    none, "Conditional jump to a basic block on variant tag."}
    , { 'SWITCH_V3',           16#0b,   true,   4, [a,ii,ii,ii],     switch, {variant, integer, integer, ingeger},    none, "Conditional jump to a basic block on variant tag."}
    , { 'SWITCH_VN',           16#0c,   true,   4, [a, li],          switch,           {variant, {list, integer}},    none, "Conditional jump to a basic block on variant tag."}
    , { 'CALL_VALUE',          16#0d,  false,   3, [a],          call_value,                                   {}, integer, "The value sent in the current remote call."}
    , { 'PUSH',                16#0e,  false,   2, [a],                push,                                {any},     any, "Push argument to stack."}
    , { 'DUPA',                16#0f,  false,   3, [],                  dup,                                {any},     any, "Duplicate top of stack."}
    , { 'DUP',                 16#10,  false,   3, [a],                 dup,                                {any},     any, "push Arg0 stack pos on top of stack."}
    , { 'POP',                 16#11,  false,   3, [a],                 pop,                            {integer}, integer, "Arg0 := top of stack."}
    , { 'INCA',                16#12,  false,   2, [],                  inc,                            {integer}, integer, "Increment accumulator."}
    , { 'INC',                 16#13,  false,   2, [a],                 inc,                            {integer}, integer, "Increment argument."}
    , { 'DECA',                16#14,  false,   2, [],                  dec,                            {integer}, integer, "Decrement accumulator."}
    , { 'DEC',                 16#15,  false,   2, [a],                 dec,                            {integer}, integer, "Decrement argument."}
    , { 'ADD',                 16#16,  false,   3, [a,a,a],             add,                   {integer, integer}, integer, "Arg0 := Arg1 + Arg2."}
    , { 'SUB',                 16#17,  false,   3, [a,a,a],             sub,                   {integer, integer}, integer, "Arg0 := Arg1 - Arg2."}
    , { 'MUL',                 16#18,  false,   3, [a,a,a],             mul,                   {integer, integer}, integer, "Arg0 := Arg1 * Arg2."}
    , { 'DIV',                 16#19,  false,   3, [a,a,a],          divide,                   {integer, integer}, integer, "Arg0 := Arg1 / Arg2."}
    , { 'MOD',                 16#1a,  false,   3, [a,a,a],          modulo,                   {integer, integer}, integer, "Arg0 := Arg1 mod Arg2."}
    , { 'POW',                 16#1b,  false,   3, [a,a,a],             pow,                   {integer, integer}, integer, "Arg0 := Arg1  ^ Arg2."}
    , { 'STORE',               16#1c,  false,   3, [a,a],             store,                   {integer, integer}, integer, "Arg0 := Arg1."}
    , { 'SHA3',                16#1d,  false,   3, [a],                sha3,                             {string},    hash, "Arg0 := sha3(Arg1)."}
    , { 'SHA256',              16#1e,  false,   3, [a],              sha256,                             {string},    hash, "Arg0 := sha256(Arg1)."}
    , { 'BLAKE2B',             16#1f,  false,   3, [a],             blake2b,                             {string},    hash, "Arg0 := blake2b(Arg1)."}
    , { 'LT',                  16#20,  false,   3, [a,a,a],              lt,                   {integer, integer}, boolean, "Arg0 := Arg1  < Arg2."}
    , { 'GT',                  16#21,  false,   3, [a,a,a],              gt,                   {integer, integer}, boolean, "Arg0 := Arg1  > Arg2."}
    , { 'EQ',                  16#22,  false,   3, [a,a,a],              eq,                   {integer, integer}, boolean, "Arg0 := Arg1  = Arg2."}
    , { 'ELT',                 16#23,  false,   3, [a,a,a],             elt,                   {integer, integer}, boolean, "Arg0 := Arg1 =< Arg2."}
    , { 'EGT',                 16#24,  false,   3, [a,a,a],             egt,                   {integer, integer}, boolean, "Arg0 := Arg1 >= Arg2."}
    , { 'NEQ',                 16#25,  false,   3, [a,a,a],             neq,                   {integer, integer}, boolean, "Arg0 := Arg1 /= Arg2."}
    , { 'AND',                 16#26,  false,   3, [a,a,a],          and_op,                   {boolean, boolean}, boolean, "Arg0 := Arg1 and Arg2."}
    , { 'OR',                  16#27,  false,   3, [a,a,a],           or_op,                   {boolean, boolean}, boolean, "Arg0 := Arg1  or Arg2."}
    , { 'NOT',                 16#28,  false,   3, [a,a],            not_op,                            {boolean}, boolean, "Arg0 := not Arg1."}
    , { 'TUPLE',               16#29,  false,   3, [ii],              tuple,                            {integer},   tuple, "Create a tuple of size = Arg0. Elements on stack."}
    , { 'ELEMENT',             16#2a,  false,   3, [a,a,a],      element_op,                     {integer, tuple},     any, "Arg1 := element(Arg2, Arg3)."}
    , { 'SETELEMENT',          16#2b,  false,   3, [a,a,a,a],    setelement,               {integer, tuple, any},   tuple, "Arg0 := a new tuple similar to Arg2, but with element number Arg1 replaced by Arg3."}
    , { 'MAP_EMPTY',           16#2c,  false,   3, [a],           map_empty,                                   {},     map, "Arg0 := #{}."}
    , { 'MAP_LOOKUP',          16#2d,  false,   3, [a,a,a],      map_lookup,                           {map, any},     any, "Arg0 := lookup key Arg2 in map Arg1."}
    , { 'MAP_LOOKUPD',         16#2e,  false,   3, [a,a,a,a],    map_lookup,                      {map, any, any},     any, "Arg0 := lookup key Arg2 in map Arg1 if key exists in map otherwise Arg0 := Arg3."}
    , { 'MAP_UPDATE',          16#2f,  false,   3, [a,a,a,a],    map_update,                      {map, any, any},     map, "Arg0 := update key Arg2 in map Arg1 with value Arg3."}
    , { 'MAP_DELETE',          16#30,  false,   3, [a,a,a],      map_delete,                           {map, any},     map, "Arg0 := delete key Arg2 from map Arg1."}
    , { 'MAP_MEMBER',          16#31,  false,   3, [a,a,a],      map_member,                           {map, any}, boolean, "Arg0 := true if key Arg2 is in map Arg1."}
    , { 'MAP_FROM_LIST',       16#32,  false,   3, [a,a],     map_from_list,        {{list, {tuple, [any, any]}}},     map, "Arg0 := make a map from (key, value) list in Arg1."}
    , { 'IS_NIL',              16#33,  false,   3, [a,a],            is_nil,                               {list}, boolean, "Arg0 := true if Arg1 == []."}
    , { 'CONS',                16#34,  false,   3, [a,a,a],            cons,                          {any, list},    list, "Arg0 := [Arg1|Arg2]."}
    , { 'HD',                  16#35,  false,   3, [a,a],                hd,                               {list},     any, "Arg0 := head of list Arg1."}
    , { 'TL',                  16#36,  false,   3, [a,a],                tl,                               {list},    list, "Arg0 := tail of list Arg1."}
    , { 'LENGTH',              16#37,  false,   3, [a,a],            length,                               {list}, integer, "Arg0 := length of list Arg1."}
    , { 'NIL',                 16#38,  false,   3, [a],                 nil,                                   {},    list, "Arg0 := []."}
    , { 'STR_JOIN',            16#39,  false,   3, [a,a,a],        str_join,                     {string, string},  string, "Arg0 := string Arg1 followed by string Arg2."}
    , { 'INT_TO_STR',          16#3a,  false,   3, [a,a],        int_to_str,                            {integer},  string, "Arg0 := turn integer Arg1 into a string."}
    , { 'ADDR_TO_STR',         16#3b,  false,   3, [a,a],       addr_to_str,                            {address},  string, "Arg0 := turn address Arg1 into a string."}
    , { 'STR_REVERSE',         16#3c,  false,   3, [a,a],       str_reverse,                             {string},  string, "Arg0 := the reverse of string Arg1."}
    , { 'APPEND',              16#3d,  false,   3, [a,a,a],          append,                         {list, list},    list, "Arg0 := Arg1 ++ Arg2."}
    , { 'INT_TO_ADDR',         16#3e,  false,   3, [a,a],       int_to_addr,                            {integer}, address, "Arg0 := turn integer Arg1 into an address."}
    , { 'VARIANT',             16#3f,  false,   3, [a,a,a,a],       variant,          {integer, integer, integer}, variant, "Arg0 := create a variant of size Arg1 with the tag Arg2 (Arg2 < Arg1) and take Arg3 elements from the stack."}
    , { 'VARIANT_TEST',        16#40,  false,   3, [a,a,a],    variant_test,                   {variant, integer}, boolean, "Arg0 := true if variant Arg1 has the tag Arg2."}
    , { 'VARIANT_ELEMENT',     16#41,  false,   3, [a,a,a], variant_element,                   {variant, integer},     any, "Arg0 := element number Arg2 from variant Arg1."}
    , { 'BITS_NONEA',          16#42,  false,   3, [],            bits_none,                                   {},    bits, "push an empty bitmap on the stack."}
    , { 'BITS_NONE',           16#43,  false,   3, [a],           bits_none,                                   {},    bits, "Arg0 := empty bitmap."}
    , { 'BITS_ALLA',           16#44,  false,   3, [],             bits_all,                                   {},    bits, "push a full bitmap on the stack."}
    , { 'BITS_ALL',            16#45,  false,   3, [a],            bits_all,                                   {},    bits, "Arg0 := full bitmap."}
    , { 'BITS_ALL_N',          16#46,  false,   3, [a,a],        bits_all_n,                            {integer},    bits, "Arg0 := bitmap with Arg1 bits set."}
    , { 'BITS_SET',            16#47,  false,   3, [a,a,a],        bits_set,                      {bits, integer},    bits, "Arg0 := set bit Arg2 of bitmap Arg1."}
    , { 'BITS_CLEAR',          16#48,  false,   3, [a,a,a],      bits_clear,                      {bits, integer},    bits, "Arg0 := clear bit Arg2 of bitmap Arg1."}
    , { 'BITS_TEST',           16#49,  false,   3, [a,a,a],       bits_test,                      {bits, integer}, boolean, "Arg0 := true if bit Arg2 of bitmap Arg1 is set."}
    , { 'BITS_SUM',            16#4a,  false,   3, [a,a],          bits_sum,                               {bits}, integer, "Arg0 := sum of set bits in bitmap Arg1. Exception if infinit bitmap."}
    , { 'BITS_OR',             16#4b,  false,   3, [a,a,a],         bits_or,                         {bits, bits},    bits, "Arg0 := Arg1 v Arg2."}
    , { 'BITS_AND',            16#4c,  false,   3, [a,a,a],        bits_and,                         {bits, bits},    bits, "Arg0 := Arg1 ^ Arg2."}
    , { 'BITS_DIFF',           16#4d,  false,   3, [a,a,a],       bits_diff,                         {bits, bits},    bits, "Arg0 := Arg1 - Arg2."}
    , { 'BALANCE',             16#4e,  false,   3, [a],             balance,                                   {}, integer, "Arg0 := The current contract balance."}
    , { 'ORIGIN',              16#4f,  false,   3, [a],              origin,                                   {}, address, "Arg0 := Address of contract called by the call transaction."}
    , { 'CALLER',              16#50,  false,   3, [a],              caller,                                   {}, address, "Arg0 := The address that signed the call transaction."}
    , { 'GASPRICE',            16#51,  false,   3, [a],            gasprice,                                   {}, integer, "Arg0 := The current gas price."}
    , { 'BLOCKHASH',           16#52,  false,   3, [a,a],         blockhash,                            {integer},    hash, "Arg0 := The blockhash at height."}
    , { 'BENEFICIARY',         16#53,  false,   3, [a],         beneficiary,                                   {}, address, "Arg0 := The address of the current beneficiary."}
    , { 'TIMESTAMP',           16#54,  false,   3, [a],           timestamp,                                   {}, integer, "Arg0 := The current timestamp. Unrelaiable, don't use for anything."}
    , { 'GENERATION',          16#55,  false,   3, [a],          generation,                                   {}, integer, "Arg0 := The block height of the cureent generation."}
    , { 'MICROBLOCK',          16#56,  false,   3, [a],          microblock,                                   {}, integer, "Arg0 := The current micro block number."}
    , { 'DIFFICULTY',          16#57,  false,   3, [a],          difficulty,                                   {}, integer, "Arg0 := The current difficulty."}
    , { 'GASLIMIT',            16#58,  false,   3, [a],            gaslimit,                                   {}, integer, "Arg0 := The current gaslimit."}
    , { 'GAS',                 16#59,  false,   3, [a],                 gas,                                   {}, integer, "Arg0 := The amount of gas left."}
    , { 'ADDRESS',             16#5a,  false,   3, [a],             address,                                   {}, address, "Arg0 := The current contract address."}

    , { 'LOG0',                16#5b,  false,   3, [a],                 log,                             {string},    none, "Create a log message in the call object."}
    , { 'LOG1',                16#5c,  false,   3, [a,a],               log,                    {integer, string},    none, "Create a log message with one topic in the call object."}
    , { 'LOG2',                16#5d,  false,   3, [a,a,a],             log,           {integer, integer, string},    none, "Create a log message with two topics in the call object."}
    , { 'LOG3',                16#5e,  false,   3, [a,a,a,a],           log,  {integer, integer, integer, string},    none, "Create a log message with three topics in the call object."}
    , { 'LOG4',                16#5f,  false,   3, [a,a,a,a,a],         log, {integer, integer, integer, integer, string},    none, "Create a log message with four topics in the call object."}
      %% Transaction ops
    , { 'SPEND',               16#60,  false,   3, [a,a],             spend,                  {address, integer},    none, "Transfer Arg1 tokens to account Arg0. (If the contract account has at least that many tokens."}
    , { 'ORACLE_REGISTER',     16#61,  false,   3, [a,a,a,a,a,a], oracle_register,         {any,any,any,any,any},     any, "NYI"}
      %% TODO:
    , { 'ORACLE_QUERY',        16#62,  false,   3, [],         oracle_query,                                  {},    none, "NYI"}
    , { 'ORACLE_RESPOND',      16#63,  false,   3, [],       oracle_respond,                                  {},    none, "NYI"}
    , { 'ORACLE_EXTEND',       16#64,  false,   3, [],        oracle_extend,                                  {},    none, "NYI"}
    , { 'ORACLE_GET_ANSWER',   16#65,  false,   3, [],    oracle_get_answer,                                  {},    none, "NYI"}
    , { 'ORACLE_GET_QUESTION', 16#66,  false,   3, [],  oracle_get_question,                                  {},    none, "NYI"}
    , { 'ORACLE_QUERY_FEE',    16#67,  false,   3, [],     oracle_query_fee,                                  {},    none, "NYI"}
    , { 'AENS_RESOLVE',        16#68,  false,   3, [],         aens_resolve,                                  {},    none, "NYI"}
    , { 'AENS_PRECLAIM',       16#69,  false,   3, [],        aens_preclaim,                                  {},    none, "NYI"}
    , { 'AENS_CLAIM',          16#6a,  false,   3, [],           aens_claim,                                  {},    none, "NYI"}
    , { 'AENS_UPDATE',         16#6b,  false,   3, [],          aend_update,                                  {},    none, "NYI"}
    , { 'AENS_TRANSFER',       16#6c,  false,   3, [],        aens_transfer,                                  {},    none, "NYI"}
    , { 'AENS_REVOKE',         16#6d,  false,   3, [],          aens_revoke,                                  {},    none, "NYI"}
    , { 'ECVERIFY',            16#6e,  false,   3, [],             ecverify,                                  {},    none, "NYI"}
    , { 'BALANCE_OTHER',       16#6f,  false,   3, [a,a],     balance_other,                           {address}, integer, "Arg0 := The balance of address Arg1."}

    , { 'DEACTIVATE',          16#fa,  false,   3, [],           deactivate,                                  {},    none, "Mark the current contract for deactivation."}
    , { 'ABORT',               16#fb,   true,   3, [a],               abort,                            {string},    none, "Abort execution (dont use all gas) with error message in Arg0."}
    , { 'EXIT',                16#fc,   true,   3, [a],                exit,                            {string},    none, "Abort execution (use upp all gas) with error message in Arg0."}
    , { 'NOP',                 16#fd,  false,   1, [],                  nop,                                  {},    none, "The no op. does nothing."}
    %% FUNCTION                16#fe                                         "Function declaration and entrypoint."
    %% EXTEND                  16#ff                                         "Reserved for future extensions beyond one byte opcodes."
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
              "-type fate_arg_stack()      :: {stack, 0}.\n"
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

gen_constructors(#{constructor := Function, format := [],
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
gen_arg_uses(N, [li]) -> io_lib:format("{immediate, Arg~w}", [N]);
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

gen_args(#{macro := Macro, arity := Arity}) ->
    lists:flatten(io_lib:format("args(~21s) -> ~2w ;\n",
                                [Macro, Arity])).

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
gen([{OpName, OpCode, EndBB, Gas, FateFormat, Constructor, ArgTypes, ResType, Doc} | Rest]) ->
    Arity = length(FateFormat),
    Name = atom_to_list(OpName),
    LowerName = string:to_lower(Name),
    TypeName = "fate_" ++ LowerName ++ "()",
    Macro = "?" ++ Name,
    Type = case FateFormat of
               [] -> io_lib:format("~w", [OpName]);
               Args ->
                    io_lib:format("{~w, ~s}", [OpName, expand_types(Args)])
           end,
    ConstructorType = atom_to_list(Constructor) ++ "/" ++ io_lib:format("~w", [Arity]),

    [#{ opname            => OpName
      , opcode            => OpCode
      , arity             => Arity
      , end_bb            => EndBB
      , format            => FateFormat
      , macro             => Macro
      , type_name         => TypeName
      , doc               => Doc
      , gas               => Gas
      , type              => Type
      , constructor       => Constructor
      , constructor_type  => ConstructorType
      , arg_types         => ArgTypes
      , res_type          => ResType
      }| gen(Rest)].


expand_types([]) -> "";
expand_types([T]) -> expand_type(T);
expand_types([T|Ts]) ->expand_type(T) ++ ", " ++ expand_types(Ts).

expand_type(a)  -> "fate_arg()";
expand_type(is) -> "fate_arg_immediate(aeb_fate_data:fate_string())";
expand_type(ii) -> "fate_arg_immediate(aeb_fate_data:fate_integer())";
expand_type(li) -> "fate_arg_immediate([aeb_fate_data:fate_integer()])";
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
              "format_arg(a, {stack, 0}) -> \"a\".\n\n"
              "lookup(Name, Symbols) ->\n"
              "    maps:get(Name, Symbols, io_lib:format(\"~~p\",[Name])).\n\n"
              "~s"
             , [Formats]),

    io:format(File, "format_op(Op, _Symbols) -> io_lib:format(\";; Bad Op: ~~w\\n\", [Op]).\n", []),
    file:close(File).

gen_format(#{opname := Name}) when (Name =:= 'CALL_R') or (Name =:= 'CALL_TR') ->
    io_lib:format("format_op({~w, {immediate, Contract}, {immediate, Function}, Value}, Symbols) ->\n"
                  "    [\"~s \", lookup(Contract, Symbols), \".\", "
                  "lookup(Function, Symbols), \" \", "
                  "format_arg(a, Value)];\n"
                  "format_op({~w, Contract, {immediate, Function}, Value}, Symbols) ->\n"
                  "[\"~s \", format_arg(a, Contract), \".\", "
                  "lookup(Function, Symbols), \" \", "
                  "format_arg(a, Value)];\n",
                  [Name, atom_to_list(Name), Name, atom_to_list(Name)]);
gen_format(#{opname := Name}) when (Name =:= 'CALL_GR') or (Name =:= 'CALL_GTR') ->
    io_lib:format("format_op({~w, {immediate, Contract}, {immediate, Function}, Value, Gas}, Symbols) ->\n"
                  "    [\"~s \", lookup(Contract, Symbols), \".\", "
                  "lookup(Function, Symbols), \" \", "
                  "format_arg(a, Value),  \" \", "
                  "format_arg(a, Gas)];\n"
                  "format_op({~w, Contract, {immediate, Function}, Value, Gas}, Symbols) ->\n"
                  "[\"~s \", format_arg(a, Contract), \".\", "
                  "lookup(Function, Symbols), \" \", "
                  "format_arg(a, Value),  \" \", "
                  "format_arg(a, Gas)];\n",
                  [Name, atom_to_list(Name), Name, atom_to_list(Name)]);
gen_format(#{opname := Name, format := []}) ->
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


gen_instruction(#{opname := Name, format := []}) ->
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
stack_arg() -> "a".
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
         , arity             := _Arity
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
            [] -> "";
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

