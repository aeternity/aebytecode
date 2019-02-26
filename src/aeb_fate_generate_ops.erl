

-module(aeb_fate_generate_ops).

-export([generate/0]).

-define(ati(__X__), {immediate, __X__}).
-define(atv(__X__), {var, __X__}).
-define(ata(__X__), {arg, __X__}).
-define(ats(__X__), {stack, __X__}).

-define(ta(), "fate_arg()").
-define(ti(__X__), "fate_arg_immediate(" __X__ ")").
-define(ts, "aeb_fate_data:fate_string()").

generate() ->
    Ops = gen(ops_defs()),
    io:format("ops: ~p\n", [Ops]),
    HrlFile = "aeb_new_fate_opcodes.hrl",
    generate_header_file(HrlFile, Ops),
    generate_opcodes_ops(aeb_new_fate_opcodes, HrlFile, Ops),
    generate_code_ops(aeb_new_fate_code, Ops).

ops_defs() ->
    %%  Opname,    Opcode, args, end_bb, gas, format,   Constructor,  Documentation
    [ { 'NOP',     16#00,    0,  false,   1, atomic,    nop,         "The no op. does nothing."}
    , { 'RETURN',  16#01,    0,   true,   2, atomic,    return,      "Return from function call pop stack to arg0."}
    , { 'RETURNR', 16#68,    1,   true,   2, [a],       returnr,     "Return from function call pop stack to arg0."}
    , { 'CALL',    16#02,    1,   true,   4, [is],      call,        "Call given function with args on stack."}
    , { 'CALL_T',  16#04,    1,   true,   4, [is],      call_t,      "Tail call to given function."}
    , { 'CALL_R',  16#03,    2,   true,   8, [a, is],   call_r,      "Remote call to given contract and function."}
    , { 'CALL_TR', 16#05,    2,   true,   8, [a, is],   call_tr,     "Remote tail call to given contract and function."}
    , { 'JUMP',    16#06,    1,   true,   3, [ii],      jump,        "Jump to a basic block."}
    , { 'JUMPIF',  16#07,    2,   true,   4, [a, ii],   jumpif,      "Conditional jump to a basic block."}
    , { 'SWITCH_V2',16#6a,   3,   true,   4, [a, ii, ii], switch,    "Conditional jump to a basic block on variant tag."}
    , { 'SWITCH_V3',16#6b,   4,   true,   4, [a, ii, ii, ii], switch,"Conditional jump to a basic block on variant tag."}
    , { 'SWITCH_VN',16#6c,   2,   true,   4, [a, li], switch,"Conditional jump to a basic block on variant tag."}
    , { 'PUSH',     16#09,   1,  false,   2, [a],       push,        "Push argument to stack."}
    , { 'INCA',     16#71,   0,  false,   2, atomic,    inc,         "Increment accumulator."}
    , { 'INC',      16#53,   1,  false,   2, [a],       inc,         "Increment argument."}
    , { 'DECA',     16#72,   0,  false,   2, atomic,    dec,         "Decrement accumulator."}
    , { 'DEC',      16#54,   1,  false,   2, [a],       dec,         "Decrement argument."}
    , { 'ADD',      16#11,   3,  false,   3, [a,a,a],   add,         "Arg0 := Arg1 + Arg2."}
    , { 'SUB',      16#13,   3,  false,   3, [a,a,a],   sub,         "Arg0 := Arg1 - Arg2."}
    , { 'MUL',      16#12,   3,  false,   3, [a,a,a],   mul,         "Arg0 := Arg1 * Arg2."}
    , { 'DIV',      16#14,   3,  false,   3, [a,a,a],   divide,      "Arg0 := Arg1 / Arg2."}
    , { 'MOD',      16#15,   3,  false,   3, [a,a,a],   modulo,      "Arg0 := Arg1 mod Arg2."}
    , { 'POW',      16#16,   3,  false,   3, [a,a,a],   pow,         "Arg0 := Arg1  ^ Arg2."}
    , { 'LT',       16#17,   3,  false,   3, [a,a,a],   lt,          "Arg0 := Arg1  < Arg2."}
    , { 'GT',       16#18,   3,  false,   3, [a,a,a],   gt,          "Arg0 := Arg1  > Arg2."}
    , { 'EQ',       16#19,   3,  false,   3, [a,a,a],   eq,          "Arg0 := Arg1  = Arg2."}
    , { 'ELT',      16#1a,   3,  false,   3, [a,a,a],   elt,         "Arg0 := Arg1 =< Arg2."}
    , { 'EGT',      16#1b,   3,  false,   3, [a,a,a],   egt,         "Arg0 := Arg1 >= Arg2."}
    , { 'NEQ',      16#1c,   3,  false,   3, [a,a,a],   neq,         "Arg0 := Arg1 /= Arg2."}
    , { 'AND',      16#1d,   3,  false,   3, [a,a,a],   and_op,      "Arg0 := Arg1 and Arg2."}
    , { 'OR',       16#1e,   3,  false,   3, [a,a,a],   or_op,       "Arg0 := Arg1  or Arg2."}
    , { 'NOT',      16#1f,   2,  false,   3, [a,a],     not_op,      "Arg0 := not Arg1."}
    , { 'TUPLE',    16#20,   1,  false,   3, [ii],      tuple,       "Create a tuple of size = Arg0. Elements on stack."}
    , { 'ELEMET',   16#21,   4,  false,   3, [t,a,a,a], element_op,  "Arg1 := element(Arg2, Arg3). The element should be of type Arg1"}
    , { 'MAP_EMPTY',16#22,   1,  false,   3, [a],       map_empty,   "Arg0 := #{}."}
    , { 'MAP_LOOKUP',16#23,  3,  false,   3, [a, a, a], map_lookup,  "Arg0 := lookup key Arg2 in map Arg1."}
    , { 'MAP_LOOKUPD',16#69, 4,  false,   3, [a, a, a, a], map_lookup,  "Arg0 := lookup key Arg2 in map Arg1 if key exists in map otherwise Arg0 := Arg3."}
    , { 'MAP_UPDATE',16#24,  4,  false,   3, [a, a, a, a], map_update,  "Arg0 := update key Arg2 in map Arg1 with value Arg3."}
    , { 'MAP_DELETE',16#25,  3,  false,   3, [a, a, a], map_delete,  "Arg0 := delete key Arg2 from map Arg1."}
    , { 'MAP_MEMBER',16#26,  3,  false,   3, [a, a, a], map_member,  "Arg0 := true if key Arg2 is in map Arg1."}
    , { 'NIL',       16#28,  1,  false,   3, [a],       nil,         "Arg0 := []."}
    , { 'IS_NIL',    16#29,  2,  false,   3, [a, a],    is_nil,      "Arg0 := true if Arg1 == []."}
    , {'CONS',       16#2a,  3,  false,   3, [a, a, a], cons,        "Arg0 := [Arg1|Arg2]."}
    , {'HD',         16#2b,  2,  false,   3, [a, a],    hd,          "Arg0 := head of list Arg1."}
    , {'TL',         16#2c,  2,  false,   3, [a, a],    tl,          "Arg0 := tail of list Arg1."}
    , {'LENGTH',     16#2d,  2,  false,   3, [a, a],    length,      "Arg0 := length of list Arg1."}
      
    , {'STR_EQ',     16#2e,  3,  false,   3, [a, a, a], str_eq,      "Arg0 := true iff the strings Arg1 and Arg2 are the same."}
    , {'STR_JOIN',   16#2f,  3,  false,   3, [a, a, a], str_join,    "Arg0 := string Arg1 followed by string Arg2."}
    , {'INT_TO_STR', 16#55,  2,  false,   3, [a, a],    int_to_str,  "Arg0 := turn integer Arg1 into a string."}
    , {'ADDR_TO_STR', 16#30, 2,  false,   3, [a, a],    addr_to_str, "Arg0 := turn address Arg1 into a string."}
    , {'STR_REVERSE', 16#31, 2,  false,   3, [a, a],    str_reverse, "Arg0 := the reverse of string Arg1."}
    , {'INT_TO_ADDR', 16#32, 2,  false,   3, [a, a],    int_to_addr, "Arg0 := turn integer Arg1 into an address."}
    , {'VARIANT',     16#33, 4,  false,   3, [a, a, a, a], variant,  "Arg0 := create a variant of size Arg1 with the tag Arg2 (Arg2 < Arg1) and take Arg3 elements from the stack."}
    , {'VARIANT_TEST', 16#34,3,  false,   3, [a, a, a], variant_test,"Arg0 := true if variant Arg1 has the tag Arg2."}
    , {'VARIANT_ELEMENT',16#35,3,false,   3, [a, a, a], variant_element,"Arg0 := element number Arg2 from variant Arg1."}
    , {'BITS_NONEA', 16#6e,  0,  false,   3, atomic,    bits_none,   "accumulator := empty bitmap."}
    , {'BITS_NONE',  16#36,  1,  false,   3, [a],       bits_none,   "Arg0 := empty bitmap."}
    , {'BITS_ALLA', 16#6f,   0,  false,   3, atomic,    bits_all,    "accumulator := full bitmap."}
    , {'BITS_ALL',  16#37,   1,  false,   3, [a],       bits_all,    "Arg0 := full bitmap."}
    , {'BITS_ALL_N', 16#6d,  2,  false,   3, [a, a],    bits_all_n,  "Arg0 := bitmap with Arg1 bits set."}
    , {'BITS_SET',  16#38,   3,  false,   3, [a, a, a], bits_set,    "Arg0 := set bit Arg2 of bitmap Arg1."}
    , {'BITS_CLEAR',16#39,   3,  false,   3, [a, a, a], bits_clear,  "Arg0 := clear bit Arg2 of bitmap Arg1."}
    , {'BITS_TEST', 16#3a,   3,  false,   3, [a, a, a], bits_test,   "Arg0 := true if bit Arg2 of bitmap Arg1 is set."}   , {'BITS_SUM',  16#3b,   2,  false,   3, [a, a],    bits_sum,    "Arg0 := sum of set bits in bitmap Arg1. Exception if infinit bitmap."}
    , {'BITS_OR',   16#3c,   3,  false,   3, [a, a, a], bits_or,     "Arg0 := Arg1 v Arg2."}
    , {'BITS_AND',  16#3d,   3,  false,   3, [a, a, a], bits_and,    "Arg0 := Arg1 ^ Arg2."}
    , {'BITS_DIFF', 16#3e,   3,  false,   3, [a, a, a], bits_diff,   "Arg0 := Arg1 - Arg2."}
    , {'DUPA',      16#70,   0,  false,   3, atomic,    dup,         "push copy of accumulator on stack."}
    , {'DUP',       16#0a,   1,  false,   3, [a],       dup,         "push Arg0 stack pos on top of stack."}
    , {'POP',       16#0b,   1,  false,   3, [a],       pop,         "Arg0 := top of stack."}
    , {'STORE',     16#10,   2,  false,   3, [a, a],    store,       "Arg0 := Arg1."}
    ].


generate_header_file(Filename, Ops) ->
    {ok, File} = file:open(Filename, [write]),
    Defines = lists:flatten([gen_defines(Op) || Op <- Ops]),
    io:format(File, "~s", [prelude("Provides opcode defines.\n")]),
    io:format(File, "%% FATE opcodes\n~s", [Defines]),
    file:close(File).

generate_opcodes_ops(Modulename, HrlFile, Ops) ->
    Filename = atom_to_list(Modulename) ++ ".erl",

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

generate_code_ops(Modulename, Ops) ->
    Filename = atom_to_list(Modulename) ++ ".erl",

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
    lists:flatten(io_lib:format("-type ~-25s :: ~s.\n",
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
                    "-include_lib(\"aebytecode/include/" ++ HrlFile ++"\").\n\n"
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
    lists:flatten(io_lib:format("-define(~-17w, 16#~2.16.0b).\n", [Name, OpCode])).

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

