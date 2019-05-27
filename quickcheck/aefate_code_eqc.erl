%%% @author Thomas Arts
%%% @doc Use `rebar3 as eqc shell` to run properties in the shell
%%%
%%% We want to be sure that we can deserialize all FATE assembler that is accepted on chain.
%%%
%%% We test something slightly weaker here,
%%%    viz. All FATE assembler we serialize, we can deserialize
%%%
%%% @end
%%% Created : 13 Dec 2018 by Thomas Arts <thomas@SpaceGrey.lan>

-module(aefate_code_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

prop_serializes() ->
    in_parallel(
    ?FORALL(FateCode, fate_code(),
           ?WHENFAIL(eqc:format("Trying to serialize/deserialize ~p failed~n", [FateCode]),
                      begin
                          {T1, Binary} =
                              timer:tc( fun() -> aeb_fate_code:serialize(FateCode) end),
                          {T2, Decoded} =
                              timer:tc(fun() -> aeb_fate_code:deserialize(Binary) end),
                          measure(binary_size, size(Binary),
                          measure(encode, T1,
                          measure(decode, T2,
                                  conjunction([{equal, equals(Decoded, FateCode)},
                                              {decoding_time, true}]))))
                      end))).

prop_opcodes() ->
    ?FORALL(Opcode, choose(0, 16#ff),
            try M = aeb_fate_opcodes:mnemonic(Opcode),
                ?WHENFAIL(eqc:format("opcode ~p -> ~p", [Opcode, M]),
                          conjunction([{valid, lists:member(Opcode, valid_opcodes())},
                                       {eq, equals(aeb_fate_opcodes:m_to_op(M), Opcode)}]))
            catch
                _:_ ->
                    not lists:member(Opcode, valid_opcodes())
            end).


valid_opcodes() ->
    lists:seq(0, 16#6f) ++ lists:seq(16#fa, 16#ff).


fate_code() ->
    ?SIZED(Size,
           ?LET({FMap, SMap, AMap},
                {map(binary(4), {{list(aefate_type_eqc:fate_type(Size div 3)), aefate_type_eqc:fate_type(Size div 3)}, bb_code()}),
                 map(resize(Size div 5, aefate_eqc:fate_data()), resize(Size div 3, aefate_eqc:fate_data())),
                 map(resize(Size div 5, aefate_eqc:fate_data()), resize(Size div 4, aefate_eqc:fate_data()))},
                aeb_fate_code:update_annotations(
                  aeb_fate_code:update_symbols(
                    aeb_fate_code:update_functions(
                      aeb_fate_code:new(), FMap), SMap), AMap))).

bb_code() ->
    #{}.
