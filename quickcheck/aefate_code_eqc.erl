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
    begin
        {T0, Binary} = timer:tc(fun() -> aeb_fate_code:serialize(FateCode) end),
        ?WHENFAIL(eqc:format("serialized:\n  ~120p~n", [Binary]),
        begin
            {T1, Decoded} = timer:tc(fun() -> aeb_fate_code:deserialize(Binary) end),
            measure(binary_size, size(Binary),
            measure(serialize, T0 / 1000,
            measure(deserialize, T1 / 1000,
            conjunction([{equal, equals(Decoded, FateCode)},
                         {serialize_time, T0 / 1000 < 500},
                         {deserialize_time, T1 / 1000 < 500}]))))
        end)
    end)).

prop_fail_serializes() ->
    conjunction([{Failure, eqc:counterexample(
                             ?FORALL(FateCode, fate_code(Failure),
                               ?FORALL(Binary, catch aeb_fate_code:serialize(FateCode),
                                         is_binary(Binary))))
                             =/= true} || Failure <- [1, 2, 3, 4, 5] ]).

prop_fuzz() ->
    in_parallel(
    ?FORALL(Binary, ?LET(FateCode, fate_code(0), aeb_fate_code:serialize(FateCode)),
    ?FORALL(FuzzedBin, fuzz(Binary),
    try aeb_fate_code:deserialize(FuzzedBin) of
        Code ->
            ?WHENFAIL(eqc:format("Code:\n  ~p\n", [Code]),
            begin
                Bin1  = aeb_fate_code:serialize(Code),
                Code1 = aeb_fate_code:deserialize(Bin1),
                ?WHENFAIL(eqc:format("Reserialized\n  ~120p\n", [Bin1]),
                equals(Code, Code1))
            end)
    catch _:_ -> true
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
    [ Op || #{opcode := Op} <- aeb_fate_generate_ops:get_ops() ].


fate_code(Failure) ->
    ?SIZED(Size,
           ?LET({FMap, SMap, AMap},
                {non_empty(map(if Failure == 1 -> binary(1);
                        true -> binary(4) end,
                     {sublist(lists:sort([private, payable])),  %% deserialize sorts them
                      {list(aefate_type_eqc:fate_type(Size div 3)), aefate_type_eqc:fate_type(Size div 3)}, bbs_code(Failure)})),
                 small_map(small_fate_data_key(5), small_fate_data(4)),
                 small_map(small_fate_data_key(5), small_fate_data(4))},
                aeb_fate_code:update_annotations(
                  aeb_fate_code:update_symbols(
                    aeb_fate_code:update_functions(
                      aeb_fate_code:new(), FMap), SMap), AMap))).

short_list(Max, Gen) ->
    ?LET(N, choose(0, Max), eqc_gen:list(N, Gen)).

small_map(KeyGen, ValGen) ->
    ?LET(KeyVals, short_list(6, {KeyGen, ValGen}),
    return(maps:from_list(KeyVals))).

bbs_code(Failure) ->
    frequency([{if Failure == 2 -> 5; true -> 0 end, #{0 => []}},
               {10, ?LET(BBs, short_list(6, bb_code(Failure)),
                          maps:from_list(
                            lists:zip(lists:seq(0, length(BBs)-1), BBs)))}]).

bb_code(Failure) ->
    EndBB = [ Op || Op <- valid_opcodes(), aeb_fate_opcodes:end_bb(Op) ],
    NonEndBB = valid_opcodes() -- EndBB,
    frequency(
      [{if Failure == 3 -> 5; true -> 0 end, ?LET(Ops, non_empty(short_list(6, elements(NonEndBB))), bblock(Failure, Ops))},
       {if Failure == 4 -> 5; true -> 0 end, ?LET({Ops, Op}, {short_list(6, elements(valid_opcodes())), elements(EndBB)}, bblock(Failure, Ops ++ [Op]))},
       {10, ?LET({Ops, Op}, {short_list(6, elements(NonEndBB)), elements(EndBB)},
                  bblock(Failure, Ops ++ [Op]))}]).

bblock(Failure, Ops) ->
    [ begin
          Mnemonic = aeb_fate_opcodes:mnemonic(Op),
          Arity = aeb_fate_opcodes:args(Op),
          case Arity of
              0 -> Mnemonic;
              _ -> list_to_tuple([Mnemonic |
                                  [ frequency([{if Failure == 5 -> 5; true -> 0 end, {stack, nat()}},
                                               {5, {stack, 0}},
                                               {5, {arg, nat()}},
                                               {5, {var, nat()}},
                                               {5, {immediate, small_fate_data(4)}}]) ||
                                      _ <- lists:seq(1, Arity) ]])
          end
      end || Op <- Ops ].

fuzz(Binary) ->
    ?LET({N, Inj}, {choose(0, byte_size(Binary) - 1), choose(0, 255)},
         begin
             M = N * 8,
             <<X:M, _:8, Z/binary>> = Binary,
             <<X:M, Inj:8, Z/binary>>
         end).

prop_small() ->
    ?FORALL(Value, small_fate_data(4),
    begin
        Bin = aeb_fate_encoding:serialize(Value),
        Size = byte_size(Bin),
        measure(size, Size,
        ?WHENFAIL(eqc:format("Size: ~p\n", [Size]),
            Size < 1000))
    end).

prop_small_type() ->
    ?FORALL(Type, ?SIZED(Size, aefate_type_eqc:fate_type(Size div 3)),
    begin
        Bin = iolist_to_binary(aeb_fate_encoding:serialize_type(Type)),
        Size = byte_size(Bin),
        measure(size, Size,
        ?WHENFAIL(eqc:format("Size: ~p\n", [Size]),
            Size < 1000))
    end).

small_fate_data(N) ->
    ?SIZED(Size, resize(Size div N, aefate_eqc:fate_data())).

small_fate_data_key(N) ->
    ?SIZED(Size, ?LET(Data, aefate_eqc:fate_data(Size div N, []), eqc_symbolic:eval(Data))).
