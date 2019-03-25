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

-type fate_type_type() :: integer
                        | boolean
                        | {list, fate_type()}
                        | {map, fate_type(), fate_type()}
                        | {tuple, [fate_type()]}
                        | address
                        | hash
                        | signature
                        | contract
                        | oracle
                        | name
                        | channel
                        | bits
                        | {variant, integer()}.


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
      | fate_type_type().

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
        , tuple_to_list/1
        , decode/1
        , encode/1
        ]).
-export([format/1]).



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
    ?FATE_STRING(list_to_binary(lists:flatten(S)));
make_string(S)  when is_binary(S) -> ?FATE_STRING(S).

make_variant(Size, Tag, Values) when is_integer(Size), is_integer(Tag)
                                     , 0 =< Size
                                     , 0 =< Tag
                                     , Tag < Size
                               , is_tuple(Values) ->
    ?FATE_VARIANT(Size, Tag, Values).

tuple_to_list(?FATE_TUPLE(T)) -> erlang:tuple_to_list(T).

%% Encode is a convinience function for testing, encoding an Erlang term
%% to a Fate term, but it can not distinguish between e.g. 32-byte strings
%% and addresses. Therfore an extra tuple layer on the erlang side for
%% addresses and bits.
encode({bits, Term}) when is_integer(Term) -> make_bits(Term);
%% TODO: check that each byte is in base58
encode({address, B}) when is_binary(B)  -> make_address(B);
encode({address, I}) when is_integer(I)  -> B = <<I:256>>, make_address(B);
encode({address, S}) when is_list(S)  ->
    make_address(encode_address(account_pubkey, S));
encode({hash, H}) when is_binary(H)  -> make_hash(H);
encode({hash, H}) when is_list(H)  -> make_hash(base64:decode(H));
encode({signature, S}) when is_binary(S)  -> make_signature(S);
encode({signature, S}) when is_list(S)  ->
    make_signature(encode_address(signature, S));
encode({contract, B}) when is_binary(B)  -> make_contract(B);
encode({contract, I}) when is_integer(I)  -> B = <<I:256>>, make_contract(B);
encode({contract, S}) when is_list(S)  ->
    make_contract(encode_address(contract_pubkey, S));
encode({oracle, B}) when is_binary(B)  -> make_oracle(B);
encode({oracle, I}) when is_integer(I)  -> B = <<I:256>>, make_oracle(B);
encode({oracle, S}) when is_list(S)  ->
    make_oracle(encode_address(oracle_pubkey, S));
encode({name, B}) when is_binary(B)  -> make_name(B);
encode({name, I}) when is_integer(I)  -> B = <<I:256>>, make_name(B);
encode({name, S}) when is_list(S)  ->
    make_name(encode_address(name, S));
encode({channel, B}) when is_binary(B)  -> make_channel(B);
encode({channel, I}) when is_integer(I)  -> B = <<I:256>>, make_channel(B);
encode({channel, S}) when is_list(S)  ->
    make_channel(encode_address(channel, S));
encode({variant, Size, Tag, Values}) -> make_variant(Size, Tag, Values);
encode(Term) when is_integer(Term) -> make_integer(Term);
encode(Term) when is_boolean(Term) -> make_boolean(Term);
encode(Term) when is_list(Term) -> make_list([encode(E) || E <- Term]);
encode(Term) when is_tuple(Term) ->
    make_tuple(list_to_tuple([encode(E) || E <- erlang:tuple_to_list(Term)]));
encode(Term) when is_map(Term) ->
    make_map(maps:from_list([{encode(K), encode(V)} || {K,V} <- maps:to_list(Term)]));
encode(Term) when is_binary(Term) -> make_string(Term).



decode(I) when ?IS_FATE_INTEGER(I) -> I;
decode(?FATE_TRUE)  -> true;
decode(?FATE_FALSE) -> false;
decode(L) when ?IS_FATE_LIST(L) -> [decode(E) || E <- L];
decode(?FATE_ADDRESS(<<Address:256>>)) -> {address, Address};
decode(?FATE_HASH(H))      -> {hash, H};
decode(?FATE_SIGNATURE(S)) -> {signature, S};
decode(?FATE_CONTRACT(X))  -> {contract, X};
decode(?FATE_ORACLE(X))    -> {oracle, X};
decode(?FATE_NAME(X))      -> {name, X};
decode(?FATE_CHANNEL(X))   -> {channel, X};
decode(?FATE_BITS(Bits))   -> {bits, Bits};
decode(?FATE_TUPLE(T))     -> erlang:list_to_tuple([decode(E) || E <- T]);
decode(?FATE_VARIANT(Size, Tag, Values)) -> {variant, Size, Tag, Values};
decode(S) when ?IS_FATE_STRING(S) -> binary_to_list(S);
decode(M) when ?IS_FATE_MAP(M) ->
    maps:from_list([{decode(K), decode(V)} || {K, V} <- maps:to_list(M)]).

-spec format(fate_type()) -> iolist().
format(I) when ?IS_FATE_INTEGER(I) -> integer_to_list(?MAKE_FATE_INTEGER(I));
format(?FATE_TRUE) -> "true";
format(?FATE_FALSE) -> "false";
format(?FATE_NIL) -> "[]";
format(L) when ?IS_FATE_LIST(L) -> format_list(?FATE_LIST_VALUE(L));
format(?FATE_UNIT) -> "()";
format(?FATE_TUPLE(T)) ->
    ["( ", lists:join(", ", [ format(E) || E <- erlang:tuple_to_list(T)]), " )"];
format(S) when ?IS_FATE_STRING(S) -> [S];
format(?FATE_BITS(B)) when B >= 0 ->
    ["<", format_bits(B, "") , ">"];
format(?FATE_BITS(B)) when B < 0 ->
    ["!< ", format_nbits(-B-1, "") , " >"];
format(?FATE_VARIANT(Size, Tag, T)) ->
    ["(| ",
      lists:join("| ", [integer_to_list(Size), integer_to_list(Tag) |
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
    ["@", eser_api_encoder:encode(oracle_pubkey, X)];
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

format_list(List) ->
    ["[ ", lists:join(", ", [format(E) || E <- List]), " ]"].

format_kvs(List) ->
    lists:join(", ", [ [format(K), " => ",  format(V)] || {K, V} <- List]).

encode_address(Type, S) when is_list(S) ->
    B = list_to_binary(S),
    try aeser_api_encoder:decode(B) of
        {Type, Encoding} ->
            Encoding;
        _ -> erlang:error({bad_address_encoding, Type, S})
    catch _:_ ->
            erlang:error({bad_address_encoding, Type, S})
    end.
