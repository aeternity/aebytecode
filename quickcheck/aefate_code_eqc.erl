%%% @author Thomas Arts
%%% @doc Use `rebar3 as eqc shell` to run properties in the shell
%%%
%%% We want to be sure that we can deserialize all FATE assembler that is accepted on chain.
%%%
%%% We test something slightly weaker here,
%%%    viz. All FATE assembler we serialize, we can deserialize
%%%
%%%    Negative testing modelled:
%%%       Failure 1: function names differ from 4 bytes
%%%       Failure 2: pointer to empty code block
%%%       Failure 3: end_BB operation as not ending block or not at end of block
%%%       - empty code blocks
%%%       - blocks that are not of the form (not end_bb)* end_bb.
%%%
%%% @end
%%% Created : 13 Dec 2018 by Thomas Arts <thomas@SpaceGrey.lan>

-module(aefate_code_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).
%%-define(Failure(Failures, Number), case lists:member(Number, Failures) of true -> 1; false -> 0 end)


prop_serializes() ->
    in_parallel(
    ?FORALL(FateCode, fate_code(0),
           ?WHENFAIL(eqc:format("Trying to serialize/deserialize ~p failed~n", [FateCode]),
                      begin
                          Binary = aeb_fate_code:serialize(FateCode),
                          ?WHENFAIL(eqc:format("serialized: ~p~n", [Binary]),
                                    begin
                                        Decoded = aeb_fate_code:deserialize(Binary),
                                        measure(binary_size, size(Binary),
                                                equals(Decoded, FateCode))
                                    end)
                      end))).

prop_fail_serializes() ->
    conjunction([{Failure, eqc:counterexample(
                             ?FORALL(FateCode, fate_code(Failure),
                               ?FORALL(Binary, catch aeb_fate_code:serialize(FateCode),
                                         is_binary(aeb_fate_code:serialize(FateCode)))))
                             =/= true} || Failure <- [1,2,3,4] ]).

prop_fuzz() ->
    in_parallel(
    ?FORALL(Binary, ?LET(FateCode, fate_code(0), aeb_fate_code:serialize(FateCode)),
            ?FORALL(InjectedBin, injection(Binary),
                    try Org = aeb_fate_code:deserialize(InjectedBin),
                         NewBin = aeb_fate_code:serialize(Org),
                         NewOrg = aeb_fate_code:deserialize(NewBin),
                         ?WHENFAIL(eqc:format("Deserialize ~p gives\n~p\nSerializes to ~p\n", [InjectedBin, Org, NewOrg]),
                                   equals(NewBin, InjectedBin))
                    catch _:_ ->
                            true
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
    lists:seq(0, 16#72) ++ lists:seq(16#fa, 16#fd).


fate_code(Failure) ->
    ?SIZED(Size,
           ?LET({FMap, SMap, AMap},
                {non_empty(map(if Failure == 1 -> binary(1);
                        true -> binary(4) end,
                     {{list(aefate_type_eqc:fate_type(Size div 3)), aefate_type_eqc:fate_type(Size div 3)}, bbs_code(Failure)})),
                 map(resize(Size div 5, aefate_eqc:fate_data()), resize(Size div 3, aefate_eqc:fate_data())),
                 map(resize(Size div 5, aefate_eqc:fate_data()), resize(Size div 4, aefate_eqc:fate_data()))},
                aeb_fate_code:update_annotations(
                  aeb_fate_code:update_symbols(
                    aeb_fate_code:update_functions(
                      aeb_fate_code:new(), FMap), SMap), AMap))).

bbs_code(Failure) ->
    frequency([{if Failure == 2 -> 5; true -> 0 end, #{0 => []}},
               {10, ?LET(BBs, list(bb_code(Failure)),
                          maps:from_list(
                            lists:zip(lists:seq(0, length(BBs)-1), BBs)))}]).

bb_code(Failure) ->
    EndBB = [ Op || Op <- valid_opcodes(), aeb_fate_opcodes:end_bb(Op) ],
    NonEndBB = valid_opcodes() -- EndBB,
    frequency(
      [{if Failure == 3 -> 5; true -> 0 end, ?LET(Ops, non_empty(list(elements(valid_opcodes()))), bblock(Failure, Ops))},
       {10, ?LET({Ops, Op}, {list(elements(NonEndBB)), elements(EndBB)},
                  bblock(Failure, Ops ++ [Op]))}]).

bblock(Failure, Ops) ->
    [ begin
          Mnemonic = aeb_fate_opcodes:mnemonic(Op),
          Arity = aeb_fate_opcodes:args(Op),
          case Arity of
              0 -> Mnemonic;
              _ -> list_to_tuple([Mnemonic |
                                  [ frequency([{if Failure == 4 -> 5; true -> 0 end, {stack, nat()}},
                                               {5, {stack, 0}},
                                               {5, {arg, nat()}},
                                               {5, {var, nat()}},
                                               {5, {immediate, small_fate_data(4)}}]) ||
                                      _ <- lists:seq(1, Arity) ]])
          end
      end || Op <- Ops ].

injection(Binary) ->
    ?LET({N, Inj}, {choose(0, byte_size(Binary) - 1), choose(0,255)},
         begin
             M = N * 8,
             <<X:M, _:8, Z/binary>> = Binary,
             <<X:M, Inj:8, Z/binary>>
         end).

small_fate_data(N) ->
    ?SIZED(Size, resize(Size div N, aefate_eqc:fate_data())).
