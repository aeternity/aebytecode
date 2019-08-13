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

-export([ allocate_store_maps/2
        , has_store_maps/1
        , unfold_store_maps/2
        , refcount/1
        , refcount_zero/0
        , refcount_diff/2
        , refcount_union/1
        , refcount_union/2
        , no_used_ids/0 ]).

-export_type([used_ids/0, maps/0, refcount/0]).

%% Size in bytes of serialization of a map for which we turn it into a store
%% map. It's not worth turning small maps into store maps.
%% Under consensus!
-define(STORE_MAP_THRESHOLD, 500).

-type fate_value() :: aeb_fate_data:fate_type().
-type id() :: integer().
-type used_ids() :: list(id()).
-type maps() :: #{ id() => aeb_fate_data:fate_map() | aeb_fate_data:fate_store_map() }.

%% -- Allocating store maps --------------------------------------------------

-spec allocate_store_maps(used_ids(), [fate_value()]) -> {[fate_value()], maps()}.
allocate_store_maps(Used, Vals) ->
    {_Used, Vals1, Maps} = allocate_store_maps_l(Used, Vals, #{}),
    {Vals1, Maps}.

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
    maps:fold(fun(K, V, {Us, M, Ms}) ->
                    {Us1, V1, Ms1} = allocate_store_maps(Us, V, Ms),
                    {Us1, M#{ K => V1 }, Ms1}
              end, {Used, #{}, Maps}, Val).

%% -- Unfolding store maps ---------------------------------------------------

-type unfold_fun() :: fun((id()) -> aeb_fate_data:fate_map()).

-spec unfold_store_maps(unfold_fun(), fate_value()) -> fate_value().
unfold_store_maps(_Unfold, ?FATE_TRUE          = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_FALSE         = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_UNIT          = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_BITS(_)       = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_BYTES(_)      = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_ADDRESS(_)    = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_CONTRACT(_)   = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_ORACLE(_)     = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_ORACLE_Q(_)   = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_CHANNEL(_)    = Val) -> Val;
unfold_store_maps(_Unfold, ?FATE_TYPEREP(_)    = Val) -> Val;
unfold_store_maps(_Unfold, Val) when ?IS_FATE_INTEGER(Val) -> Val;
unfold_store_maps(_Unfold, Val) when ?IS_FATE_STRING(Val)  -> Val;
unfold_store_maps(Unfold, ?FATE_TUPLE(Val)) ->
    Vals = unfold_store_maps_l(Unfold, tuple_to_list(Val)),
    ?FATE_TUPLE(list_to_tuple(Vals));
unfold_store_maps(Unfold, Val) when ?IS_FATE_LIST(Val) ->
    ?MAKE_FATE_LIST(unfold_store_maps_l(Unfold, ?FATE_LIST_VALUE(Val)));
unfold_store_maps(Unfold, ?FATE_VARIANT(Arities, Tag, Vals)) ->
    Vals1 = unfold_store_maps_l(Unfold, tuple_to_list(Vals)),
    ?FATE_VARIANT(Arities, Tag, list_to_tuple(Vals1));
unfold_store_maps(Unfold, Val) when ?IS_FATE_MAP(Val) ->
    ?MAKE_FATE_MAP(unfold_store_maps_m(Unfold, ?FATE_MAP_VALUE(Val)));
unfold_store_maps(Unfold, ?FATE_STORE_MAP(Cache, Id)) ->
    StoreMap = Unfold(Id),
    maps:fold(fun write_cache/3, unfold_store_maps(Unfold, StoreMap), Cache).

unfold_store_maps_l(Unfold, Vals) ->
    [ unfold_store_maps(Unfold, Val) || Val <- Vals ].

unfold_store_maps_m(Unfold, Val) ->
    maps:map(fun(_, V) -> unfold_store_maps(Unfold, V) end, Val).

write_cache(Key, ?FATE_MAP_TOMBSTONE, Map) ->
    maps:remove(Key, Map);
write_cache(Key, Val, Map) ->
    Map#{ Key => Val }.

%% -- Reference counting -----------------------------------------------------

-type refcount() :: #{id() => pos_integer()}.

-spec refcount_zero() -> refcount().
refcount_zero() -> #{}.

-spec refcount_diff(refcount(), refcount()) -> refcount().
refcount_diff(New, Old) ->
    maps:fold(fun(K, N, C) -> maps:update_with(K, fun(M) -> M - N end, -N, C) end,
              New, Old).

-spec refcount_union([refcount()]) -> refcount().
refcount_union(Counts) -> lists:foldl(fun refcount_union/2, #{}, Counts).

-spec refcount_union(refcount(), refcount()) -> refcount().
refcount_union(A, B) ->
    maps:fold(fun(K, N, C) -> maps:update_with(K, fun(M) -> M + N end, N, C) end,
              B, A).

-spec has_store_maps(fate_value()) -> boolean().
has_store_maps(Val) ->
    refcount_zero() /= refcount(Val).

-spec refcount(fate_value()) -> refcount().
refcount(Val) -> refcount(Val, #{}).

-spec refcount(fate_value(), refcount()) -> refcount().
refcount(?FATE_MAP_TOMBSTONE, Count) -> Count;
refcount(?FATE_TRUE,          Count) -> Count;
refcount(?FATE_FALSE,         Count) -> Count;
refcount(?FATE_UNIT,          Count) -> Count;
refcount(?FATE_BITS(_),       Count) -> Count;
refcount(?FATE_BYTES(_),      Count) -> Count;
refcount(?FATE_ADDRESS(_),    Count) -> Count;
refcount(?FATE_CONTRACT(_),   Count) -> Count;
refcount(?FATE_ORACLE(_),     Count) -> Count;
refcount(?FATE_ORACLE_Q(_),   Count) -> Count;
refcount(?FATE_CHANNEL(_),    Count) -> Count;
refcount(?FATE_TYPEREP(_),    Count) -> Count;
refcount(Val, Count) when ?IS_FATE_INTEGER(Val) -> Count;
refcount(Val, Count) when ?IS_FATE_STRING(Val)  -> Count;
refcount(?FATE_TUPLE(Val), Count) ->
    refcount_l(tuple_to_list(Val), Count);
refcount(Val, Count) when ?IS_FATE_LIST(Val) ->
    refcount_l(?FATE_LIST_VALUE(Val), Count);
refcount(?FATE_VARIANT(_Arities, _Tag, Vals), Count) ->
    refcount_l(tuple_to_list(Vals), Count);
refcount(Val, Count) when ?IS_FATE_MAP(Val) ->
    refcount_m(?FATE_MAP_VALUE(Val), Count);
refcount(?FATE_STORE_MAP(Cache, Id), Count) ->
    refcount_m(Cache, maps:update_with(Id, fun(N) -> N + 1 end, 1, Count)).

refcount_l(Vals, Count) ->
    lists:foldl(fun refcount/2, Count, Vals).

refcount_m(Val, Count) ->
    %% No maps in map keys
    maps:fold(fun(_, ?FATE_MAP_TOMBSTONE, C) -> C;
                 (_, V, C) -> refcount(V, C) end, Count, Val).

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
