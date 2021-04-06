%% FATE data representation.
%%
-include("aeb_fate_data.hrl").

-module(aeb_fate_data).

-type fate_integer()   :: ?FATE_INTEGER_T.
-type fate_boolean()   :: ?FATE_BOOLEAN_T.
-type fate_nil()       :: ?FATE_NIL_T.
-type fate_list()      :: ?FATE_LIST_T.
-type fate_unit()      :: ?FATE_UNIT_T.
-type fate_map()       :: ?FATE_MAP_T.
-type fate_store_map() :: ?FATE_STORE_MAP_T.
-type fate_string()    :: ?FATE_STRING_T.
-type fate_address()   :: ?FATE_ADDRESS_T.
-type fate_hash()      :: ?FATE_BYTES_T(32).
-type fate_signature() :: ?FATE_BYTES_T(64).
-type fate_contract()  :: ?FATE_CONTRACT_T.
-type fate_oracle()    :: ?FATE_ORACLE_T.
-type fate_oracle_q()  :: ?FATE_ORACLE_Q_T.
-type fate_channel()   :: ?FATE_CHANNEL_T.
-type fate_variant()   :: ?FATE_VARIANT_T.
-type fate_tuple()     :: ?FATE_TUPLE_T.
-type fate_bits()      :: ?FATE_BITS_T.
-type fate_typerep()   :: ?FATE_TYPEREP_T.
-type fate_contract_bytearray() :: ?FATE_CONTRACT_BYTEARRAY_T.

-type fate_type_type() :: integer
                        | boolean
                        | {list, fate_type_type()}
                        | {map, fate_type_type(), fate_type_type()}
                        | {tuple, [fate_type_type()]}
                        | address
                        | hash
                        | signature
                        | contract
                        | oracle
                        | oracle_query
                        | channel
                        | bits
                        | string
                        | {variant, [fate_type_type()]}
                        | contract_bytearray.


-type fate_type() ::
        fate_boolean()
      | fate_integer()
      | fate_nil()
      | fate_list()
      | fate_unit()
      | fate_tuple()
      | fate_string()
      | fate_address()
      | fate_hash()
      | fate_signature()
      | fate_contract()
      | fate_oracle()
      | fate_oracle_q()
      | fate_channel()
      | fate_variant()
      | fate_map()
      | fate_bits()
      | fate_typerep()
      | fate_contract_bytearray().

-export_type([fate_type/0
             , fate_boolean/0
             , fate_integer/0
             , fate_nil/0
             , fate_list/0
             , fate_unit/0
             , fate_tuple/0
             , fate_string/0
             , fate_address/0
             , fate_hash/0
             , fate_signature/0
             , fate_contract/0
             , fate_oracle/0
             , fate_channel/0
             , fate_variant/0
             , fate_map/0
             , fate_store_map/0
             , fate_bits/0
             , fate_type_type/0
             ]).

-export([ make_integer/1
        , make_boolean/1
        , make_list/1
        , make_variant/3
        , make_tuple/1
        , make_string/1
        , make_map/1
        , make_store_map/1
        , make_store_map/2
        , make_address/1
        , make_bytes/1
        , make_hash/1
        , make_signature/1
        , make_contract/1
        , make_oracle/1
        , make_oracle_query/1
        , make_channel/1
        , make_bits/1
        , make_unit/0
        , make_typerep/1
        , make_contract_bytearray/1
        ]).
-export([
         elt/2
        , lt/2
        , format/1
        , ordinal/1]).


make_boolean(true)  -> ?FATE_TRUE;
make_boolean(false) -> ?FATE_FALSE.
make_list([]) ->       ?FATE_NIL;
make_list(L) ->        ?MAKE_FATE_LIST(L).
make_unit() ->         ?FATE_UNIT.
make_tuple(T) ->       ?FATE_TUPLE(T).
make_map(M) ->         ?MAKE_FATE_MAP(M).
make_store_map(Id) ->  make_store_map(#{}, Id).
make_store_map(Cache, Id) -> ?FATE_STORE_MAP(Cache, Id).
make_address(X) ->     ?FATE_ADDRESS(X).
make_bytes(X) ->       ?FATE_BYTES(X).
make_hash(X) ->        make_bytes(X).
make_signature(X) ->   make_bytes(X).
make_contract(X) ->    ?FATE_CONTRACT(X).
make_oracle(X) ->      ?FATE_ORACLE(X).
make_oracle_query(X) -> ?FATE_ORACLE_Q(X).
make_channel(X) ->     ?FATE_CHANNEL(X).
make_integer(I) when is_integer(I) -> ?MAKE_FATE_INTEGER(I).
make_bits(I)    when is_integer(I) -> ?FATE_BITS(I).
make_string(S)  when is_list(S) ->
    ?FATE_STRING(iolist_to_binary(S));
make_string(S)  when is_binary(S) -> ?FATE_STRING(S).
make_typerep(T) -> ?FATE_TYPEREP(T).
make_contract_bytearray(B) -> ?FATE_CONTRACT_BYTEARRAY(B).

%% Tag points to the selected variant (zero based)
%% The arity of this variant is read from the list of provided arities
%% and should match the size of the given tuple.
make_variant(Arities, Tag, Values) ->
    Arities = [A || A <- Arities, is_integer(A), A < 256],
    Size = length(Arities),
    if is_integer(Tag)
       , 0 =< Tag
       , Tag < Size
       , is_tuple(Values) ->
            Arity = lists:nth(Tag+1, Arities),
            if size(Values) =:= Arity ->
                    ?FATE_VARIANT(Arities, Tag, Values)
            end
    end.



-spec format(fate_type()) -> iolist().
format(I) when ?IS_FATE_INTEGER(I) -> integer_to_list(?MAKE_FATE_INTEGER(I));
format(?FATE_TRUE) -> "true";
format(?FATE_FALSE) -> "false";
format(?FATE_NIL) -> "[]";
format(L) when ?IS_FATE_LIST(L) -> format_list(?FATE_LIST_VALUE(L));
format(?FATE_UNIT) -> "()";
format(?FATE_TUPLE(T)) ->
    ["( ", lists:join(", ", [ format(E) || E <- erlang:tuple_to_list(T)]), " )"];
format(S) when ?IS_FATE_STRING(S) -> ["\"", S, "\""];
format(?FATE_BITS(B)) when B >= 0 ->
    ["<", format_bits(B, "") , ">"];
format(?FATE_BITS(B)) when B < 0 ->
    ["!< ", format_nbits(-B-1, "") , " >"];
format(?FATE_VARIANT(Arities, Tag, T)) ->
    ["(| ",
      lists:join("| ",
                 [format_arities(Arities),
                  integer_to_list(Tag) |
                  [format(make_tuple(T))]]),
     " |)"];
format(M) when ?IS_FATE_MAP(M) ->
    ["{ ", format_kvs(maps:to_list(?FATE_MAP_VALUE(M))), " }"];
format(?FATE_BYTES(X))      -> ["#", base64:encode(X)];
format(?FATE_ADDRESS(X))    ->
    ["@", aeser_api_encoder:encode(account_pubkey, X)];
format(?FATE_CONTRACT(X))   ->
    ["@", aeser_api_encoder:encode(contract_pubkey, X)];
format(?FATE_ORACLE(X))     ->
    ["@", aeser_api_encoder:encode(oracle_pubkey, X)];
format(?FATE_ORACLE_Q(X))   ->
    ["@", aeser_api_encoder:encode(oracle_query_id, X)];
format(?FATE_CHANNEL(X))    ->
    ["@", aeser_api_encoder:encode(channel, X)];
format(?FATE_TYPEREP(X))    ->
    ["'", io_lib:format("~p", [X])];
format(?FATE_CONTRACT_BYTEARRAY(B))   ->
    ["@", aeser_api_encoder:encode(contract_bytearray, B)];
format(V) -> exit({not_a_fate_type, V}).

format_bits(0, Acc) -> Acc;
format_bits(N, Acc) ->
    Bit = $0 + (N band 1),
    format_bits(N bsr 1, [Bit|Acc]).

format_nbits(0, Acc) -> Acc;
format_nbits(N, Acc) ->
    Bit = $1 - (N band 1),
    format_nbits(N bsr 1, [Bit|Acc]).

format_arities(Arities) ->
    ["[ ", lists:join(", ", [integer_to_list(E) || E <- Arities]), " ]"].

format_list(List) ->
    ["[ ", lists:join(", ", [format(E) || E <- List]), " ]"].

format_kvs(List) ->
    lists:join(", ", [ [format(K), " => ",  format(V)] || {K, V} <- List]).


%% Total order of FATE terms.
%%  Integers < Booleans < Address < Channel < Contract < Oracle
%%   < Hash < Signature < Bits < String < Tuple < Map < List < Variant
%%   < Oracle query < FATE code
-define(ORD_INTEGER  , 0).
-define(ORD_BOOLEAN  , 1).
-define(ORD_ADDRESS  , 2).
-define(ORD_CHANNEL  , 3).
-define(ORD_CONTRACT , 4).
-define(ORD_ORACLE   , 5).
-define(ORD_BYTES    , 6).
-define(ORD_BITS     , 7).
-define(ORD_STRING   , 8).
-define(ORD_TUPLE    , 9).
-define(ORD_MAP      , 10).
-define(ORD_LIST     , 11).
-define(ORD_VARIANT  , 12).
-define(ORD_ORACLE_Q , 13).
-define(ORD_CONTRACT_BYTEARRAY , 14).

-spec ordinal(fate_type()) -> integer().
ordinal(T) when ?IS_FATE_INTEGER(T)   -> ?ORD_INTEGER;
ordinal(T) when ?IS_FATE_BOOLEAN(T)   -> ?ORD_BOOLEAN;
ordinal(T) when ?IS_FATE_ADDRESS(T)   -> ?ORD_ADDRESS;
ordinal(T) when ?IS_FATE_CHANNEL(T)   -> ?ORD_CHANNEL;
ordinal(T) when ?IS_FATE_CONTRACT(T)  -> ?ORD_CONTRACT;
ordinal(T) when ?IS_FATE_ORACLE(T)    -> ?ORD_ORACLE;
ordinal(T) when ?IS_FATE_BYTES(T)     -> ?ORD_BYTES;
ordinal(T) when ?IS_FATE_BITS(T)      -> ?ORD_BITS;
ordinal(T) when ?IS_FATE_STRING(T)    -> ?ORD_STRING;
ordinal(T) when ?IS_FATE_TUPLE(T)     -> ?ORD_TUPLE;
ordinal(T) when ?IS_FATE_MAP(T)       -> ?ORD_MAP;
ordinal(T) when ?IS_FATE_LIST(T)      -> ?ORD_LIST;
ordinal(T) when ?IS_FATE_VARIANT(T)   -> ?ORD_VARIANT;
ordinal(T) when ?IS_FATE_ORACLE_Q(T)  -> ?ORD_ORACLE_Q;
ordinal(T) when ?IS_FATE_CONTRACT_BYTEARRAY(T) -> ?ORD_CONTRACT_BYTEARRAY.


-spec lt(fate_type(), fate_type()) -> boolean().
lt(A, B) ->
    O1 = ordinal(A),
    O2 = ordinal(B),
    if O1 == O2 -> lt(O1, A, B);
       true -> O1 < O2
    end.

%% Integers are ordered as usual.
lt(?ORD_INTEGER, A, B) when ?IS_FATE_INTEGER(A), ?IS_FATE_INTEGER(B) ->
    ?FATE_INTEGER_VALUE(A) < ?FATE_INTEGER_VALUE(B);
%% false is smaller than true (true also for erlang booleans).
lt(?ORD_BOOLEAN, A, B) when ?IS_FATE_BOOLEAN(A), ?IS_FATE_BOOLEAN(B) ->
    ?FATE_BOOLEAN_VALUE(A) < ?FATE_BOOLEAN_VALUE(B);
lt(?ORD_BITS, A, B) when ?IS_FATE_BITS(A), ?IS_FATE_BITS(B) ->
    BitsA = ?FATE_BITS_VALUE(A),
    BitsB = ?FATE_BITS_VALUE(B),
    if BitsA < 0 ->
            if BitsB < 0 -> BitsA < BitsB;
               true -> false
            end;
       BitsB < 0 ->
            true;
       true -> BitsA < BitsB
    end;
lt(?ORD_STRING, ?FATE_STRING(A), ?FATE_STRING(B)) ->
    compare_bytes(A, B);

lt(?ORD_TUPLE, ?FATE_TUPLE(A), ?FATE_TUPLE(B)) ->
    SizeA = size(A),
    SizeB = size(B),
    case SizeA - SizeB of
        0 -> tuple_elements_lt(0, A, B, SizeA);
        N -> N < 0
    end;
lt(?ORD_MAP, ?FATE_MAP_VALUE(A), ?FATE_MAP_VALUE(B)) ->
    SizeA = maps:size(A),
    SizeB = maps:size(B),
    case SizeA - SizeB of
        0 -> maps_lt(A, B);
        N -> N < 0
    end;
lt(?ORD_LIST, ?FATE_LIST_VALUE(_), ?FATE_LIST_VALUE([])) -> false;
lt(?ORD_LIST, ?FATE_LIST_VALUE([]), ?FATE_LIST_VALUE(_)) -> true;
lt(?ORD_LIST, ?FATE_LIST_VALUE([A|RA]), ?FATE_LIST_VALUE([B|RB])) ->
    O1 = ordinal(A),
    O2 = ordinal(B),
    if O1 == O2 ->
            if A == B -> lt(RA, RB);
               true -> A < B
            end;
       true -> O1 < O2
    end;
lt(?ORD_VARIANT, ?FATE_VARIANT(AritiesA, TagA, TA),
   ?FATE_VARIANT(AritiesB, TagB, TB)) ->
    if length(AritiesA) < length(AritiesB) -> true;
       length(AritiesA) > length(AritiesB) -> false;
       true ->
            if AritiesA < AritiesB -> true;
               AritiesA > AritiesB -> false;
               true ->
                    if TagA < TagB -> true;
                       TagA > TagB -> false;
                       true -> lt(make_tuple(TA), make_tuple(TB))
                    end
            end
    end;
lt(?ORD_ADDRESS, ?FATE_ADDRESS(A), ?FATE_ADDRESS(B)) ->
  A < B;
lt(?ORD_CHANNEL, ?FATE_CHANNEL(A), ?FATE_CHANNEL(B)) ->
  A < B;
lt(?ORD_CONTRACT, ?FATE_CONTRACT(A), ?FATE_CONTRACT(B)) ->
  A < B;
lt(?ORD_ORACLE, ?FATE_ORACLE(A), ?FATE_ORACLE(B)) ->
  A < B;
lt(?ORD_ORACLE_Q, ?FATE_ORACLE_Q(A), ?FATE_ORACLE_Q(B)) ->
  A < B;
lt(?ORD_BYTES, ?FATE_BYTES(A), ?FATE_BYTES(B)) ->
  compare_bytes(A, B);
lt(?ORD_CONTRACT_BYTEARRAY, ?FATE_CONTRACT_BYTEARRAY(A), ?FATE_CONTRACT_BYTEARRAY(B)) ->
  compare_bytes(A, B).

compare_bytes(A, B) ->
  SizeA = byte_size(A),
  SizeB = byte_size(B),
  case SizeA - SizeB of
    0 -> A < B;
    N -> N < 0
  end.

tuple_elements_lt(N,_A,_B, N) ->
    false;
tuple_elements_lt(N, A, B, Size) ->
    E = N + 1,
    EA = element(E, A),
    EB = element(E, B),
    if EA =:= EB -> tuple_elements_lt(E, A, B, Size);
       true -> lt(EA, EB)
    end.

maps_lt(A, B) ->
    IA = maps_iterator(A),
    IB = maps_iterator(B),
    maps_i_lt(IA, IB).

maps_i_lt(IA, IB) ->
    case {maps_next(IA), maps_next(IB)} of
        {none, none} -> false;
        {_, none} -> false;
        {none, _} -> true;
        {{KA1, VA1, IA2},  {KB1, VB1, IB2}} ->
            case lt(KA1, KB1) of
                true -> true;
                false ->
                    case lt(KB1, KA1) of
                        true -> false;
                        false ->
                            case lt(VA1, VB1) of
                                true -> true;
                                false ->
                                    case lt(VB1, VA1) of
                                        true -> false;
                                        false ->
                                            maps_i_lt(IA2, IB2)
                                    end
                            end
                    end
            end
    end.

maps_iterator(M) -> lists:sort(fun ({K1,_}, {K2,_}) -> lt(K1, K2) end, maps:to_list(M)).
maps_next([]) -> none;
maps_next([{K,V}|Rest]) -> {K, V, Rest}.


-spec elt(fate_type(), fate_type()) -> boolean().
elt(A, A) -> true;
elt(A, B) ->
    R = lt(A, B),
    R.

