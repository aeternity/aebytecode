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
    ?FORALL(FateData, fate_type(),
            measure(bytes, size(term_to_binary(FateData)),
                    begin
                        Serialized = aeb_fate_encoding:serialize(FateData),
                        ?WHENFAIL(eqc:format("Serialized ~p to ~p~n", [FateData, Serialized]),
                                  equals(aeb_fate_encoding:deserialize(Serialized), FateData))
                    end
                   )).

prop_format() ->
    ?FORALL(FateData, fate_type(),
            ?WHENFAIL(eqc:format("Trying to format ~p failed~n",[FateData]),
                      begin
                          String = aeb_fate_data:format(FateData),
                          collect([FateData, unicode:characters_to_binary(String, latin1)], true)
                      end)).

prop_format_scan() ->
    ?FORALL(FateData, fate_type(),
            ?WHENFAIL(eqc:format("Trying to format ~p failed~n~p~n", [FateData, catch unicode:characters_to_list(aeb_fate_data:format(FateData), utf8) ]),
                      begin
                          String = aeb_fate_data:format(FateData),
                          {ok, _Scanned, _} = aeb_fate_asm_scan:scan(unicode:characters_to_list(String)),
                          true
                      end)).

fate_type() ->
    ?SIZED(Size, fate_type(Size, [map])).

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


fate_integer()    -> oneof([int(), largeint()]).
fate_bits()       -> {bits, oneof([int(), largeint()])}.
fate_boolean()    -> elements([true, false]).
fate_nil()        -> [].
fate_unit()       -> {tuple, {}}.
fate_string()     -> ?SUCHTHAT(S, utf8(), string:find(S, "\"") == nomatch).
fate_address()    -> {address, non_zero_binary(256 div 8)}.
fate_hash()       -> {hash, non_zero_binary(32)}.
fate_signature()  -> {signature, non_zero_binary(64)}.
fate_contract()   -> {contract, non_zero_binary(256 div 8)}.
fate_oracle()     -> {oracle, non_zero_binary(256 div 8)}.
fate_name()       -> {name, non_zero_binary(256 div 8)}.
fate_channel()    -> {channel, non_zero_binary(256 div 8)}.

%% May shrink to fate_unit
fate_tuple(ListGen) ->
    {tuple, ?LET(Elements, ListGen, list_to_tuple(Elements))}.

fate_variant(TupleGen) ->
    ?LET({L1, L2, Tuple}, {list(choose(0, 255)), list(choose(0,255)), TupleGen},
         {variant, L1 ++ [size(Tuple)] ++ L2, length(L1), Tuple}).

fate_list(Gen) ->
    oneof([fate_nil(), ?SHRINK(list(Gen), [fate_nil()])]).

fate_map(KeyGen, ValGen) ->
    map(KeyGen, ValGen).


non_zero_binary(N) ->
    Bits = N*8,
    ?SUCHTHAT(Bin, binary(N), begin <<V:Bits>> = Bin, V =/= 0 end).

char() ->
    choose(1, 255).
