%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Functions for manipulating FATE maps. In particular for mediating
%%%     between plain map values (represented by Erlang maps) and maps that are
%%%     fully or partially saved in the contract store.
%%%  @end
%%%    -------------------------------------------------------------------
-module(aeb_fate_maps).

-include("aeb_fate_data.hrl").

-export([allocate_store_maps/2, no_used_ids/0]).

-export_type([used_ids/0, maps/0]).

%% Size in bytes of serialization of a map for which we turn it into a store
%% map. It's not worth turning small maps into store maps.
%% Under consensus!
-define(STORE_MAP_THRESHOLD, 500).

-type fate_value() :: aeb_fate_data:fate_type().
-type id() :: integer().
-type used_ids() :: list(id()).    %% TODO: more clever representation
-type maps() :: #{ id() => aeb_fate_data:fate_map() | aeb_fate_data:fate_store_map() }.

-spec allocate_store_maps(used_ids(), [fate_value()]) ->
        {[fate_value()], maps()}.
allocate_store_maps(Used, Vals) ->
    allocate_store_maps_l(Used, Vals, #{}).

allocate_store_maps(Used, ?FATE_MAP_TOMBSTONE = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_TRUE          = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_FALSE         = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_UNIT          = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_BITS(_)       = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_BYTES(_)      = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_ADDRESS(_)    = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_CONTRACT(_)   = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_ORACLE(_)     = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_ORACLE_Q(_)   = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_CHANNEL(_)    = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_TYPEREP(_)    = Val, Maps) -> {Used, Val, Maps};
allocate_store_maps(Used, Val, Maps) when ?IS_FATE_INTEGER(Val) -> {Used, Val, Maps};
allocate_store_maps(Used, Val, Maps) when ?IS_FATE_STRING(Val)  -> {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_TUPLE(Val), Maps) ->
    {Used1, Vals, Maps1} = allocate_store_maps_l(Used, tuple_to_list(Val), Maps),
    {Used1, ?FATE_TUPLE(list_to_tuple(Vals)), Maps1};
allocate_store_maps(Used, Val, Maps) when ?IS_FATE_LIST(Val) ->
    {Used1, Vals, Maps1} = allocate_store_maps_l(Used, ?FATE_LIST_VALUE(Val), Maps),
    {Used1, ?MAKE_FATE_LIST(Vals), Maps1};
allocate_store_maps(Used, ?FATE_VARIANT(Arities, Tag, Vals), Maps) ->
    {Used1, Vals1, Maps1} = allocate_store_maps_l(Used, tuple_to_list(Vals), Maps),
    {Used1, ?FATE_VARIANT(Arities, Tag, list_to_tuple(Vals1)), Maps1};
allocate_store_maps(Used, Val, Maps) when ?IS_FATE_MAP(Val) ->
    {Used1, KVs, Maps1} = allocate_store_maps_m(Used, ?FATE_MAP_VALUE(Val), Maps),
    Val1 = ?MAKE_FATE_MAP(KVs),
    case byte_size(aeb_fate_encoding:serialize(Val1)) < ?STORE_MAP_THRESHOLD of
        true -> {Used1, Val1, Maps1};
        false ->
            {Id, Used2} = next_id(Used1),
            {Used2, ?FATE_STORE_MAP(#{}, Id), Maps1#{Id => Val1}}
    end;
allocate_store_maps(Used, ?FATE_STORE_MAP(Cache, _Id) = Val, Maps) when Cache =:= #{} ->
    {Used, Val, Maps};
allocate_store_maps(Used, ?FATE_STORE_MAP(Cache, Id), Maps) ->
    {NewId, Used1} = next_id(Used),
    {Used1, Cache1, Maps1} = allocate_store_maps_m(Used1, Cache, Maps),
    {Used1, ?FATE_STORE_MAP(#{}, NewId), Maps1#{NewId => ?FATE_STORE_MAP(Cache1, Id)}}.

allocate_store_maps_l(Used, [], Maps) -> {Used, [], Maps};
allocate_store_maps_l(Used, [H | T], Maps) ->
    {Used1, H1, Maps1} = allocate_store_maps(Used, H, Maps),
    {Used2, T1, Maps2} = allocate_store_maps(Used1, T, Maps1),
    {Used2, [H1 | T1], Maps2}.

allocate_store_maps_m(Used, Val, Maps) ->
    KVs = [ ?FATE_TUPLE(KV) || KV <- maps:to_list(Val) ],
    {Used1, KVs1, Maps1} = allocate_store_maps_l(Used, KVs, Maps),
    {Used1, maps:from_list([ KV || ?FATE_TUPLE(KV) <- KVs1 ]), Maps1}.

%% -- Map id allocation ------------------------------------------------------

-spec no_used_ids() -> used_ids().
no_used_ids() -> [].

-spec next_id(used_ids()) -> {id(), used_ids()}.
next_id(UsedIds) ->
    next_id(UsedIds, 0, []).

next_id(Used, J, Acc) when Used == []; J < hd(Used) ->
    {J, lists:reverse(Acc) ++ [J | Used]};
next_id([I | Used], I, Acc) ->
    next_id(Used, I + 1, [I | Acc]);
next_id([I | Used], J, Acc) when J > I ->
    next_id(Used, J, [I | Acc]).
