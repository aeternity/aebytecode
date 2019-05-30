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
-type fate_string()    :: ?FATE_STRING_T.
-type fate_address()   :: ?FATE_ADDRESS_T.
-type fate_hash()      :: ?FATE_HASH_T.
-type fate_contract()  :: ?FATE_CONTRACT_T.
-type fate_oracle()    :: ?FATE_ORACLE_T.
-type fate_name()      :: ?FATE_NAME_T.
-type fate_channel()   :: ?FATE_CHANNEL_T.
-type fate_signature() :: ?FATE_SIGNATURE_T.
-type fate_variant()   :: ?FATE_VARIANT_T.
-type fate_tuple()     :: ?FATE_TUPLE_T.
-type fate_bits()      :: ?FATE_BITS_T.

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
                        | name
                        | channel
                        | bits
                        | string
                        | {variant, [fate_type_type()]}.


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
      | fate_name()
      | fate_channel()
      | fate_variant()
      | fate_map()
      | fate_bits().

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
             , fate_name/0
             , fate_channel/0
             , fate_variant/0
             , fate_map/0
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
        , make_address/1
        , make_hash/1
        , make_signature/1
        , make_contract/1
        , make_oracle/1
        , make_name/1
        , make_channel/1
        , make_bits/1
        , make_unit/0
        ]).
-export([
         elt/2
        , lt/2
        , format/1]).


make_boolean(true)  -> ?FATE_TRUE;
make_boolean(false) -> ?FATE_FALSE.
make_list([]) ->       ?FATE_NIL;
make_list(L) ->        ?MAKE_FATE_LIST(L).
make_unit() ->         ?FATE_UNIT.
make_tuple(T) ->       ?FATE_TUPLE(T).
make_map(M) ->         ?MAKE_FATE_MAP(M).
make_address(X) ->     ?FATE_ADDRESS(X).
make_hash(X) ->        ?FATE_HASH(X).
make_signature(X) ->   ?FATE_SIGNATURE(X).
make_contract(X) ->    ?FATE_CONTRACT(X).
make_oracle(X) ->      ?FATE_ORACLE(X).
make_name(X) ->        ?FATE_NAME(X).
make_channel(X) ->     ?FATE_CHANNEL(X).
make_integer(I) when is_integer(I) -> ?MAKE_FATE_INTEGER(I).
make_bits(I)    when is_integer(I) -> ?FATE_BITS(I).
make_string(S)  when is_list(S) ->
    ?FATE_STRING(iolist_to_binary(S));
make_string(S)  when is_binary(S) -> ?FATE_STRING(S).

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
format(?FATE_HASH(X))       -> ["#", base64:encode(X)];
format(?FATE_ADDRESS(X))    ->
    ["@", aeser_api_encoder:encode(account_pubkey, X)];
format(?FATE_SIGNATURE(X))  ->
    ["$", aeser_api_encoder:encode(signature, X)];
format(?FATE_CONTRACT(X))   ->
    ["@", aeser_api_encoder:encode(contract_pubkey, X)];
format(?FATE_ORACLE(X))     ->
    ["@", aeser_api_encoder:encode(oracle_pubkey, X)];
format(?FATE_NAME(X))       ->
    ["@", aeser_api_encoder:encode(name, X)];
format(?FATE_CHANNEL(X))    ->
    ["@", aeser_api_encoder:encode(channel, X)];
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
%%  Integers < Booleans < Address < Channel < Contract < Name < Oracle
%%   < Hash < Signature < Bits < Tuple < Map < List
ordinal(T) when ?IS_FATE_INTEGER(T)   -> 0;
ordinal(T) when ?IS_FATE_BOOLEAN(T)   -> 1;
ordinal(T) when ?IS_FATE_ADDRESS(T)   -> 2;
ordinal(T) when ?IS_FATE_CHANNEL(T)   -> 3;
ordinal(T) when ?IS_FATE_CONTRACT(T)  -> 4;
ordinal(T) when ?IS_FATE_NAME(T)      -> 5;
ordinal(T) when ?IS_FATE_ORACLE(T)    -> 6;
ordinal(T) when ?IS_FATE_HASH(T)      -> 7;
ordinal(T) when ?IS_FATE_SIGNATURE(T) -> 8;
ordinal(T) when ?IS_FATE_BITS(T)      -> 9;
ordinal(T) when ?IS_FATE_TUPLE(T)     -> 10;
ordinal(T) when ?IS_FATE_MAP(T)       -> 11;
ordinal(T) when ?IS_FATE_LIST(T)      -> 12.


-spec lt(fate_type(), fate_type()) -> boolean.
%% This function assumes that all lists and maps are monomorphic,
%% and only tests one element of a list or a map.
%% If there is a risc that the term is not monomorphic,
%% use safe_lt.
lt(A, B) ->
    O1 = ordinal(A),
    O2 = ordinal(B),
    if O1 == O2 -> lt(O1, A, B);
       true -> O1 < O2
    end.

%% Integer is the smallest FATE type.
%% Integers themselves are ordered as usual.
lt(0, A, B) when ?IS_FATE_INTEGER(A), ?IS_FATE_INTEGER(B) ->
    ?FATE_INTEGER_VALUE(A) < ?FATE_INTEGER_VALUE(B);
%% Boolean is the second smallest FATE type.
%% false is smaller than true (true also for erlang booleans).
lt(1, A, B) when ?IS_FATE_BOOLEAN(A), ?IS_FATE_INTEGER(B) -> false;
lt(1, A, B) when ?IS_FATE_BOOLEAN(A), ?IS_FATE_BOOLEAN(B) ->
    ?FATE_BOOLEAN_VALUE(A) < ?FATE_BOOLEAN_VALUE(B);
lt(_, A, B) -> A < B.


-spec elt(fate_type(), fate_type()) -> boolean.
elt(A, A) -> true;
elt(A, B) ->
    R = lt(A, B),
    io:format("~w < ~w : ~w~n", [A, B, R]),
    R.

