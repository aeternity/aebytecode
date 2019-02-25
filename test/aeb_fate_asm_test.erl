%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate serialization
%%%
%%% To run:
%%%  TEST=aeb_fate_asm_test rebar3 eunit
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeb_fate_asm_test).

-include_lib("eunit/include/eunit.hrl").

asm_path() ->
    filename:join(code:lib_dir(aebytecode, test), "asm_code").


file_path(File) ->
   filename:join(asm_path(), File) ++ ".fate".

read_file(File) ->
    FilePath = file_path(File),
    Asm = aeb_fate_asm:read_file(FilePath),
    Asm.

assemble(Asm) ->
    {Env, BC} = aeb_fate_asm:asm_to_bytecode(Asm, []),
    {Env, BC}.

disassemble(BC) ->
    aeb_fate_asm:bytecode_to_fate_code(BC, []).


asm_disasm_idenity_test() ->
    check_roundtrip(identity).

asm_disasm_files_test_() ->
    [{lists:flatten(io_lib:format("~p", [X])),
      fun() -> check_roundtrip(X) end}
     || X <- sources()].

sources() ->
    [ "arith"
    , "bool"
    , "comp"
    , "jumpif"
    , "map"
    , "memory"
    , "remote"
    , "test"
    , "tuple"
    ].

check_roundtrip(File) ->
    AssemblerCode = read_file(File),
    {Env, ByteCode} = assemble(AssemblerCode),
    FateCode = disassemble(ByteCode),
    DissasmCode = aeb_fate_asm:to_asm(FateCode),
    io:format("~s~n", [AssemblerCode]),
    io:format("~s~n", [DissasmCode]),
    {Env2, ByteCode2} = assemble(DissasmCode),
    Code1 = aeb_fate_asm:strip(ByteCode),
    Code2 = aeb_fate_asm:strip(ByteCode2),
    ?assertEqual(Code1, Code2).
