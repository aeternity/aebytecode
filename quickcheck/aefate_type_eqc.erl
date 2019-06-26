%%% @author Thomas Arts
%%% @doc Use `rebar3 as eqc shell` to run properties in the shell
%%%      Properties for testing Fate type representations
%%%
%%% @end
%%% Created : 13 Dec 2018 by Thomas Arts <thomas@SpaceGrey.lan>

-module(aefate_type_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

kind(X) when is_atom(X) -> X;
kind(T) when is_tuple(T) -> element(1, T).

prop_roundtrip() ->
    ?FORALL(FateType, fate_type(),
            collect(kind(FateType),
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
           {bytes, nat()},
           contract,
           oracle,
           channel,
           bits,
           string]);
fate_type(Size) ->
    ?LAZY(
    oneof([fate_type(0),
           {list, fate_type(Size div 2)},
           ?LETSHRINK(Ts, fate_types(Size), {tuple, Ts}),
           ?LETSHRINK(Ts, fate_types(Size), {variant, Ts}),
           ?LETSHRINK([T1, T2], vector(2, fate_type(Size div 2)),
                      {map, T1, T2})])).

fate_types(Size) ->
    ?LET(N, choose(0, 6),
    eqc_gen:list(N, fate_type(Size div max(2, N)))).

