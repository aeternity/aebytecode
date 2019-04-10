%%% @author Thomas Arts
%%% @copyright (C) 2018, Thomas Arts
%%% @doc Use `rebar3 as eqc shell` to run properties in the shell
%%%
%%%
%%% @end
%%% Created : 13 Dec 2018 by Thomas Arts <thomas@SpaceGrey.lan>

-module(aefate_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

prop_roundtrip() ->
    ?FORALL(FateData, fate_data(),
            measure(bytes, size(term_to_binary(FateData)),
                    begin
                        Serialized = aeb_fate_encoding:serialize(FateData),
                        ?WHENFAIL(eqc:format("Serialized ~p to ~p~n", [FateData, Serialized]),
                                  equals(aeb_fate_encoding:deserialize(Serialized), FateData))
                    end)).

prop_format() ->
    ?FORALL(FateData, fate_data(),
            ?WHENFAIL(eqc:format("Trying to format ~p failed~n",[FateData]),
                      begin
                          String = aeb_fate_data:format(FateData),
                          collect([FateData, unicode:characters_to_binary(String, latin1)], true)
                      end)).

prop_format_scan() ->
    ?FORALL(FateData, fate_data(),
            ?WHENFAIL(eqc:format("Trying to format ~p failed~n", [FateData]),
                      begin
                          String = aeb_fate_data:format(FateData),
                          {ok, _Scanned, _} = aeb_fate_asm_scan:scan(unicode:characters_to_list(String)),
                          true
                      end)).

fate_data() ->
    ?SIZED(Size, ?LET(Data, fate_type(Size, [map]), eqc_symbolic:eval(Data))).

fate_type(0, _Options) ->
    ?LAZY(
       oneof([fate_integer(),
              fate_boolean(),
              fate_nil(),
              fate_unit(),
              fate_string(),
              fate_address(),
              fate_hash(),
              fate_signature(),
              fate_contract(),
              fate_oracle(),
              fate_name(),
              fate_bits(),
              fate_channel()]));
fate_type(Size, Options) ->
    ?LETSHRINK([Smaller], [?LAZY(fate_type(Size div 5, Options))],
    oneof([?LAZY(fate_type(Size - 1, Options)),
           ?LAZY(fate_list( Smaller )),
           ?LAZY(fate_tuple( list( Smaller ))),
           ?LAZY(fate_variant(?LET(L, list( Smaller), list_to_tuple(L))))] ++
              [
               ?LAZY(fate_map( fate_type(Size div 3, Options -- [map]),
                               Smaller))
              || lists:member(map, Options)
              ])).


fate_integer()    -> {call, aeb_fate_data, make_integer, [oneof([int(), largeint()])]}.
fate_bits()       -> {call, aeb_fate_data, make_bits, [oneof([int(), largeint()])]}.
fate_boolean()    -> {call, aeb_fate_data, make_boolean, [elements([true, false])]}.
fate_nil()        -> {call, aeb_fate_data, make_list, [[]]}.
fate_unit()       -> {call, aeb_fate_data, make_unit, []}.
fate_string()     -> {call, aeb_fate_data, make_string, [?SUCHTHAT(S, utf8(), [ quote || <<34>> <= S ] == [])]}.
fate_address()    -> {call, aeb_fate_data, make_address, [non_zero_binary(256 div 8)]}.
fate_hash()       -> {call, aeb_fate_data, make_hash, [non_zero_binary(32)]}.
fate_signature()  -> {call, aeb_fate_data, make_signature, [non_zero_binary(64)]}.
fate_contract()   -> {call, aeb_fate_data, make_contract, [non_zero_binary(256 div 8)]}.
fate_oracle()     -> {call, aeb_fate_data, make_oracle, [non_zero_binary(256 div 8)]}.
fate_name()       -> {call, aeb_fate_data, make_name, [non_zero_binary(256 div 8)]}.
fate_channel()    -> {call, aeb_fate_data, make_channel, [non_zero_binary(256 div 8)]}.

%% May shrink to fate_unit
fate_tuple(ListGen) ->
    {call, aeb_fate_data, make_tuple, [?LET(Elements, ListGen, list_to_tuple(Elements))]}.

fate_variant(TupleGen) ->
    ?LET({L1, L2, Tuple}, {list(choose(0, 255)), list(choose(0,255)), TupleGen},
         {call, aeb_fate_data, make_variant, [L1 ++ [size(Tuple)] ++ L2, length(L1), Tuple]}).

fate_list(Gen) ->
    {call, aeb_fate_data, make_list, [oneof([fate_nil(), ?SHRINK(list(Gen), [fate_nil()])])]}.

fate_map(KeyGen, ValGen) ->
    {call, aeb_fate_data, make_map, [map(KeyGen, ValGen)]}.


non_zero_binary(N) ->
    Bits = N*8,
    ?SUCHTHAT(Bin, binary(N), begin <<V:Bits>> = Bin, V =/= 0 end).

char() ->
    choose(1, 255).
