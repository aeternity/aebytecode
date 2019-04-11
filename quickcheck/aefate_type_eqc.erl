%%% @author Thomas Arts
%%% @doc Use `rebar3 as eqc shell` to run properties in the shell
%%%      Properties for testing Fate type representations
%%%
%%% @end
%%% Created : 13 Dec 2018 by Thomas Arts <thomas@SpaceGrey.lan>

-module(aefate_type_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

prop_roundtrip() ->
    ?FORALL(FateType, fate_type(),
            collect(FateType,
            begin
                Serialized = aeb_fate_encoding:serialize_type(FateType),
                BinSerialized = list_to_binary(Serialized),
                ?WHENFAIL(eqc:format("Serialized ~p to ~p (~p)~n", [FateType, Serialized, BinSerialized]),
                          begin
                              {Type, <<>>} = aeb_fate_encoding:deserialize_type(BinSerialized),
                              equals(Type, FateType)
                          end)
            end)).


fate_type() ->
    ?SIZED(Size, fate_type(Size)).

fate_type(0) ->
    oneof([integer,
           boolean,
           address,
           hash,
           signature,
           contract,
           oracle,
           name,
           channel,
           bits,
           string]);
fate_type(Size) ->
    oneof([?LAZY(fate_type(Size div 2)),
           {list, ?LAZY(fate_type(Size div 2))},
           {tuple, list(?LAZY(fate_type(Size div 2)))},
           {variant, list(?LAZY(fate_type(Size div 2)))},
           ?LETSHRINK([T1, T2], [?LAZY(fate_type(Size div 2)), ?LAZY(fate_type(Size div 2))],
                      {map, T1, T2})]).
