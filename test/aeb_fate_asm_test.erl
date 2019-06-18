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
    aeb_fate_asm:asm_to_bytecode(Asm, []).

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
    , "mapofmap"
    , "immediates"
    , "oracles"
%%    , "all_instructions"
    ].

check_roundtrip(File) ->
    AssemblerCode = read_file(File),
    {_Env, ByteCode} = assemble(AssemblerCode),
    FateCode = aeb_fate_code:deserialize(ByteCode),
    DissasmCode = aeb_fate_asm:to_asm(FateCode),
    {_Env2, ByteCode2} = assemble(DissasmCode),
    ByteCode3 = aeb_fate_code:serialize(FateCode),
    Code1 = aeb_fate_asm:strip(ByteCode),
    Code2 = aeb_fate_asm:strip(ByteCode2),
    Code3 = aeb_fate_asm:strip(ByteCode3),
    ?assertEqual(Code1, Code2),
    ?assertEqual(Code1, Code3).
