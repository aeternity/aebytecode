%% Fate data (and instruction) serialization.
%%
%% Assuming
%%   S is seralize/1 (fate_type() -> binary())
%%   D is deserialize/1 (binary) -> fate_type())
%%   V, V1, V2 are of the type fate_type()
%%   B is of the type binary()
%% Then
%%  The FATE serialization has to fullfill the following properties:
%%   * For each value (V) in FATE there has to be a bytecode sequence (B)
%%     representing that value.
%%   * A valid byte sequence has to be deserializable to a FATE value.
%%   * A valid byte sequence must not contain any trailing bytes.
%%   * A serialization is a sequence of 8-bit bytes.
%%  The serialization function (S) should fullfill the following:
%%   * A valid FATE value should be serialized to a byte sequence.
%%   * Any other argument, not representing a valid FATE value should
%%     throw an exception
%%  The deserialization function (D) should fullfill the following:
%%   * A valid byte sequence should be deserialized to a valid FATE value.
%%   * Any other argument, not representing a valid byte sequence should
%%     throw an exception
%%  The following equalities should hold:
%%   * D(S(V)) == V
%%   * if V1 == V2 then S(V1) == S(V2)
%%
%%
%% History
%% * First draft of FATE serialization encoding/decoding.
%%   Initial experiment with tags
%% * Second draft
%%  * FATE data is now defined in aefa_data.erl
%% * Third draft
%%  * Added Bit strings
%%
%% TODO:
%%   * Make the code production ready.
%%       (add tests, document exported functions).
%%   * Handle instructions.
%%
%% ------------------------------------------------------------------------
-module(aeb_fate_encoding).

-export([ deserialize/1
        , deserialize_one/1
        , deserialize_type/1
        , serialize/1
        , serialize_type/1
        ]).

-ifdef(EQC).
-export([sort/1]).
-endif.

-include("aeb_fate_data.hrl").

%% Definition of tag scheme.
%% This has to follow the protocol specification.

-define(SMALL_INT    ,        2#0). %% sxxxxxx 0 - 6 bit integer with sign bit
%%                                             1 Set below
-define(LONG_STRING  , 2#00000001). %% 000000 01 | RLP encoded array - when size >= 64
-define(SHORT_STRING ,       2#01). %% xxxxxx 01 | [bytes] - when 0 < xxxxxx:size < 64
%%                                            11  Set below
-define(SHORT_LIST   ,     2#0011). %% xxxx 0011 | [encoded elements] when  0 < length < 16
%%                                     xxxx 0111
-define(TYPE_INTEGER , 2#00000111). %% 0000 0111 - Integer typedef
-define(TYPE_BOOLEAN , 2#00010111). %% 0001 0111 - Boolean typedef
-define(TYPE_LIST    , 2#00100111). %% 0010 0111 | Type
-define(TYPE_TUPLE   , 2#00110111). %% 0011 0111 | Size | [Element Types]
-define(TYPE_OBJECT  , 2#01000111). %% 0100 0111 | ObjectType
-define(TYPE_BITS    , 2#01010111). %% 0101 0111 - Bits typedef
-define(TYPE_MAP     , 2#01100111). %% 0110 0111 | Type | Type
-define(TYPE_STRING  , 2#01110111). %% 0111 0111 - string typedef
-define(TYPE_VARIANT , 2#10000111). %% 1000 0111 | [Arities] | [Type]
-define(TYPE_BYTES   , 2#10010111). %% 1001 0111 - Bytes typedef
                                    %% 1010 0111
                                    %% 1011 0111
                                    %% 1100 0111
                                    %% 1101 0111
-define(TYPE_VAR     , 2#11100111). %% 1110 0111 | Id when 0 =< Id < 256 (type variable)
-define(TYPE_ANY     , 2#11110111). %% 1111 0111 - Any typedef
-define(LONG_TUPLE   , 2#00001011). %% 0000 1011 | RLP encoded (size - 16) | [encoded elements],
-define(SHORT_TUPLE  ,     2#1011). %% xxxx 1011 | [encoded elements] when 0  <  size < 16
%%                                          1111 Set below
-define(LONG_LIST    , 2#00011111). %% 0001 1111 | RLP encoded (length - 16) | [encoded lements]
-define(MAP          , 2#00101111). %% 0010 1111 | RLP encoded size | [encoded key, encoded value]
-define(EMPTY_TUPLE  , 2#00111111). %% 0011 1111
-define(POS_BITS     , 2#01001111). %% 0100 1111 | RLP encoded integer (to be interpreted as bitfield)
-define(EMPTY_STRING , 2#01011111). %% 0101 1111
-define(POS_BIG_INT  , 2#01101111). %% 0110 1111 | RLP encoded (integer - 64)
-define(FALSE        , 2#01111111). %% 0111 1111
%%                                  %% 1000 1111 - FREE (Possibly for bytecode in the future.)
-define(OBJECT       , 2#10011111). %% 1001 1111 | ObjectType | RLP encoded Array
-define(VARIANT      , 2#10101111). %% 1010 1111 | [encoded arities] | encoded tag | [encoded values]
-define(MAP_ID       , 2#10111111). %% 1011 1111 | RLP encoded integer (store map id)
-define(NEG_BITS     , 2#11001111). %% 1100 1111 | RLP encoded integer (infinite 1:s bitfield)
-define(EMPTY_MAP    , 2#11011111). %% 1101 1111
-define(NEG_BIG_INT  , 2#11101111). %% 1110 1111 | RLP encoded (integer - 64)
-define(TRUE         , 2#11111111). %% 1111 1111

-define(SHORT_TUPLE_SIZE,  16).
-define(SHORT_LIST_SIZE,   16).
-define(SMALL_INT_SIZE,    64).
-define(SHORT_STRING_SIZE, 64).

-define(POS_SIGN, 0).
-define(NEG_SIGN, 1).

%% Object types
-define(OTYPE_ADDRESS,   0).
-define(OTYPE_BYTES,     1).
-define(OTYPE_CONTRACT,  2).
-define(OTYPE_ORACLE,    3).
-define(OTYPE_ORACLE_Q,  4).
-define(OTYPE_CHANNEL,   5).

-define(IS_TYPE_TAG(X), (X =:= ?TYPE_INTEGER orelse
                         X =:= ?TYPE_BOOLEAN orelse
                         X =:= ?TYPE_ANY orelse
                         X =:= ?TYPE_VAR orelse
                         X =:= ?TYPE_LIST orelse
                         X =:= ?TYPE_TUPLE orelse
                         X =:= ?TYPE_OBJECT orelse
                         X =:= ?TYPE_BITS orelse
                         X =:= ?TYPE_MAP orelse
                         X =:= ?TYPE_STRING orelse
                         X =:= ?TYPE_VARIANT)).

%% --------------------------------------------------
%% Serialize
%% Serialized a Fate data value into a sequence of bytes
%% according to the Fate serialization specification.
%% TODO: The type Fate Data is not final yet.
-spec serialize(aeb_fate_data:fate_type()) -> binary().
serialize(?FATE_TRUE)        -> <<?TRUE>>;
serialize(?FATE_FALSE)       -> <<?FALSE>>;
serialize(?FATE_UNIT)        -> <<?EMPTY_TUPLE>>;  %% ! Untyped
serialize(?FATE_EMPTY_STRING) -> <<?EMPTY_STRING>>;
serialize(I) when ?IS_FATE_INTEGER(I) -> serialize_integer(I);
serialize(?FATE_BITS(Bits)) when is_integer(Bits) -> serialize_bits(Bits);
serialize(String) when ?IS_FATE_STRING(String),
                       ?FATE_STRING_SIZE(String) > 0,
                       ?FATE_STRING_SIZE(String) < ?SHORT_STRING_SIZE ->
    Size = ?FATE_STRING_SIZE(String),
    Bytes = ?FATE_STRING_VALUE(String),
    <<Size:6, ?SHORT_STRING:2, Bytes/binary>>;
serialize(String) when ?IS_FATE_STRING(String),
                       ?FATE_STRING_SIZE(String) > 0,
                       ?FATE_STRING_SIZE(String) >= ?SHORT_STRING_SIZE ->
    Bytes = ?FATE_STRING_VALUE(String),
    <<?LONG_STRING,
      (serialize_integer(?FATE_STRING_SIZE(String) - ?SHORT_STRING_SIZE))/binary
     , Bytes/binary>>;
serialize(?FATE_BYTES(Bytes)) when is_binary(Bytes) ->
    <<?OBJECT, ?OTYPE_BYTES, (serialize(?FATE_STRING(Bytes)))/binary>>;
serialize(?FATE_ADDRESS(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_ADDRESS, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_CONTRACT(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_CONTRACT, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_ORACLE(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_ORACLE, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_ORACLE_Q(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_ORACLE_Q, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_CHANNEL(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_CHANNEL, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_TUPLE(T)) when size(T) > 0 ->
    S = size(T),
    L = tuple_to_list(T),
    Rest = << <<(serialize(E))/binary>> || E <- L >>,
    if S < ?SHORT_TUPLE_SIZE ->
            <<S:4, ?SHORT_TUPLE:4, Rest/binary>>;
       true ->
            Size = rlp_encode_int(S - ?SHORT_TUPLE_SIZE),
            <<?LONG_TUPLE:8, Size/binary, Rest/binary>>
    end;
serialize(L) when ?IS_FATE_LIST(L) ->
    List = ?FATE_LIST_VALUE(L),
    S = length(List),
    Rest = << <<(serialize(El))/binary>> || El <- List >>,
    if S < ?SHORT_LIST_SIZE ->
            <<S:4, ?SHORT_LIST:4, Rest/binary>>;
       true ->
            Val = rlp_encode_int(S - ?SHORT_LIST_SIZE),
            <<?LONG_LIST, Val/binary, Rest/binary>>
    end;
serialize(Map) when ?IS_FATE_MAP(Map) ->
    L = maps:to_list(?FATE_MAP_VALUE(Map)),
    Size = length(L),
    %% TODO:  check all K same type, and all V same type
    %%        check K =/= map
    Elements =
        list_to_binary([ <<(serialize(K))/binary, (serialize(V))/binary>> || {K, V} <- sort_and_check(L) ]),
    <<?MAP,
      (rlp_encode_int(Size))/binary,
      (Elements)/binary>>;
serialize(?FATE_STORE_MAP(Cache, Id)) when Cache =:= #{} ->
    %% We should never get to serialization without having flushed the caches.
    <<?MAP_ID, (rlp_encode_int(Id))/binary>>;
serialize(?FATE_VARIANT(Arities, Tag, Values)) ->
    Arities = [A || A <- Arities, is_integer(A), A < 256],
    Size = length(Arities),
    if is_integer(Tag)
       , 0 =< Tag
       , Tag < Size
       , is_tuple(Values) ->
            Arity  =  lists:nth(Tag+1, Arities),
            if size(Values) =:= Arity ->
                    EncodedArities = aeser_rlp:encode(list_to_binary(Arities)),
                    <<?VARIANT,
                      EncodedArities/binary,
                      Tag:8,
                      (serialize(?FATE_TUPLE(Values)))/binary
                    >>
            end
    end;
serialize(?FATE_TYPEREP(T)) ->
    iolist_to_binary(serialize_type(T)).


%% -----------------------------------------------------

-spec serialize_type(aeb_fate_data:fate_type_type()) -> [byte()].
serialize_type(integer)     -> [?TYPE_INTEGER];
serialize_type(boolean)     -> [?TYPE_BOOLEAN];
serialize_type(any)         -> [?TYPE_ANY];
serialize_type({tvar, N}) when 0 =< N, N =< 255 -> [?TYPE_VAR, N];
serialize_type({list, T})   -> [?TYPE_LIST | serialize_type(T)];
serialize_type({tuple, Ts}) ->
    case length(Ts) of
        N when N =< 255 ->
            [?TYPE_TUPLE, N | [serialize_type(T) || T <- Ts]]
    end;
serialize_type({bytes, N}) when 0 =< N ->
    [?TYPE_BYTES | binary_to_list(serialize_integer(N))];
serialize_type(address)     -> [?TYPE_OBJECT, ?OTYPE_ADDRESS];
serialize_type(contract)    -> [?TYPE_OBJECT, ?OTYPE_CONTRACT];
serialize_type(oracle)      -> [?TYPE_OBJECT, ?OTYPE_ORACLE];
serialize_type(oracle_query)-> [?TYPE_OBJECT, ?OTYPE_ORACLE_Q];
serialize_type(channel)     -> [?TYPE_OBJECT, ?OTYPE_CHANNEL];
serialize_type(bits)        -> [?TYPE_BITS];
serialize_type({map, K, V}) -> [?TYPE_MAP
                                | serialize_type(K) ++ serialize_type(V)];
serialize_type(string)      -> [?TYPE_STRING];
serialize_type({variant, ListOfVariants}) ->
    Size = length(ListOfVariants),
    if Size < 256 ->
            [?TYPE_VARIANT, Size | [serialize_type(T) || T <- ListOfVariants]]
    end.


-spec deserialize_type(binary()) -> {aeb_fate_data:fate_type_type(), binary()}.
deserialize_type(<<?TYPE_INTEGER, Rest/binary>>) -> {integer, Rest};
deserialize_type(<<?TYPE_BOOLEAN, Rest/binary>>) -> {boolean, Rest};
deserialize_type(<<?TYPE_ANY,     Rest/binary>>) -> {any, Rest};
deserialize_type(<<?TYPE_VAR, Id, Rest/binary>>) -> {{tvar, Id}, Rest};
deserialize_type(<<?TYPE_LIST, Rest/binary>>) ->
    {T, Rest2} = deserialize_type(Rest),
    {{list, T}, Rest2};
deserialize_type(<<?TYPE_TUPLE, N, Rest/binary>>) ->
    {Ts, Rest2} = deserialize_types(N, Rest, []),
    {{tuple, Ts}, Rest2};
deserialize_type(<<?TYPE_BYTES, Rest/binary>>) ->
    {N, Rest2} = deserialize_one(Rest),
    true       = is_integer(N) andalso N >= 0,
    {{bytes, N}, Rest2};
deserialize_type(<<?TYPE_OBJECT, ObjectType, Rest/binary>>) ->
    case ObjectType of
        ?OTYPE_ADDRESS   -> {address, Rest};
        ?OTYPE_CONTRACT  -> {contract, Rest};
        ?OTYPE_ORACLE    -> {oracle, Rest};
        ?OTYPE_ORACLE_Q  -> {oracle_query, Rest};
        ?OTYPE_CHANNEL   -> {channel, Rest}
    end;
deserialize_type(<<?TYPE_BITS, Rest/binary>>) -> {bits, Rest};
deserialize_type(<<?TYPE_MAP, Rest/binary>>) ->
    {K, Rest2} = deserialize_type(Rest),
    {V, Rest3} = deserialize_type(Rest2),
    {{map, K, V}, Rest3};
deserialize_type(<<?TYPE_STRING, Rest/binary>>) ->
    {string, Rest};
deserialize_type(<<?TYPE_VARIANT, Size, Rest/binary>>) ->
    {Variants, Rest2} = deserialize_variants(Size, Rest, []),
    {{variant, Variants}, Rest2}.

deserialize_variants(0, Rest, Variants) ->
    {lists:reverse(Variants), Rest};
deserialize_variants(N, Rest, Variants) ->
    {T, Rest2} = deserialize_type(Rest),
    deserialize_variants(N-1, Rest2, [T|Variants]).



deserialize_types(0, Binary, Acc) ->
    {lists:reverse(Acc), Binary};
deserialize_types(N, Binary, Acc) ->
    {T, Rest} = deserialize_type(Binary),
    deserialize_types(N-1, Rest, [T | Acc]).


%% -----------------------------------------------------

rlp_encode_int(S) when S >= 0 ->
    aeser_rlp:encode(binary:encode_unsigned(S)).


%% first byte of the binary gives the number of bytes we need <<129>> is 1, <<130>> = 2,
%% so <<129, 0>> is <<0>> and <<130, 0, 0>> is <<0, 0>>
rlp_decode_int(Binary) ->
    {Bin1, Rest} = aeser_rlp:decode_one(Binary),
    Int = binary:decode_unsigned(Bin1),
    ReEncode = rlp_encode_int(Int),
    case <<ReEncode/binary, Rest/binary>> == Binary of
        true ->
            {Int, Rest};
        false ->
            error({none_unique_encoding, Bin1, ReEncode})
    end.

serialize_integer(I) when ?IS_FATE_INTEGER(I) ->
    V = ?FATE_INTEGER_VALUE(I),
    Abs = abs(V),
    Sign = case V < 0 of
               true  -> ?NEG_SIGN;
               false -> ?POS_SIGN
           end,
    if Abs < ?SMALL_INT_SIZE -> <<Sign:1, Abs:6, ?SMALL_INT:1>>;
       Sign =:= ?NEG_SIGN -> <<?NEG_BIG_INT,
                               (rlp_encode_int(Abs - ?SMALL_INT_SIZE))/binary>>;
       Sign =:= ?POS_SIGN -> <<?POS_BIG_INT,
                               (rlp_encode_int(Abs - ?SMALL_INT_SIZE))/binary>>
    end.

serialize_bits(B) when is_integer(B) ->
    Abs = abs(B),
    if
        B < 0 -> <<?NEG_BITS, (rlp_encode_int(Abs))/binary>>;
        B >= 0 -> <<?POS_BITS, (rlp_encode_int(Abs))/binary>>
    end.

-spec deserialize(binary()) -> aeb_fate_data:fate_type().
deserialize(B) ->
    {T, <<>>} = deserialize2(B),
    T.

deserialize_one(B) -> deserialize2(B).

deserialize2(<<?POS_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {?MAKE_FATE_INTEGER(I), Rest};
deserialize2(<<?NEG_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    if I =/= 0  ->  {?MAKE_FATE_INTEGER(-I), Rest};
       I == 0 -> error({illegal_sign, I})
    end;
deserialize2(<<?NEG_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = rlp_decode_int(Rest),
    {?MAKE_FATE_INTEGER(-Bint - ?SMALL_INT_SIZE),
     Rest2};
deserialize2(<<?POS_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = rlp_decode_int(Rest),
    {?MAKE_FATE_INTEGER(Bint + ?SMALL_INT_SIZE),
     Rest2};
deserialize2(<<?NEG_BITS, Rest/binary>>) ->
    case rlp_decode_int(Rest) of
        {Pos, Rest2} when Pos > 0 ->
            {?FATE_BITS(-Pos), Rest2};
        {N, _} ->
            error({illegal_parameter, neg_bits, N})
    end;
deserialize2(<<?POS_BITS, Rest/binary>>) ->
    {Bint, Rest2} = rlp_decode_int(Rest),
    {?FATE_BITS(Bint), Rest2};
deserialize2(<<?LONG_STRING, Rest/binary>>) ->
    {S, Rest2} = deserialize_one(Rest),
    true = is_integer(S) andalso S >= 0,
    Size = S + ?SHORT_STRING_SIZE,
    String = binary:part(Rest2, 0, Size),
    Rest3 = binary:part(Rest2, byte_size(Rest2), - (byte_size(Rest2) - Size)),
    {?MAKE_FATE_STRING(String), Rest3};
deserialize2(<<S:6, ?SHORT_STRING:2, Rest/binary>>) ->
    String = binary:part(Rest, 0, S),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - S)),
    {?MAKE_FATE_STRING(String), Rest2};
deserialize2(<<?OBJECT, ?OTYPE_BYTES, Rest/binary>>) ->
    {String, Rest2} = deserialize_one(Rest),
    true = ?IS_FATE_STRING(String),
    {?FATE_BYTES(?FATE_STRING_VALUE(String)), Rest2};
deserialize2(<<?OBJECT, ObjectType, Rest/binary>>) ->
    {A, Rest2} = aeser_rlp:decode_one(Rest),
    Val =
        case ObjectType of
            ?OTYPE_ADDRESS   -> ?FATE_ADDRESS(A);
            ?OTYPE_CONTRACT  -> ?FATE_CONTRACT(A);
            ?OTYPE_ORACLE    -> ?FATE_ORACLE(A);
            ?OTYPE_ORACLE_Q  -> ?FATE_ORACLE_Q(A);
            ?OTYPE_CHANNEL   -> ?FATE_CHANNEL(A)
        end,
    {Val, Rest2};
deserialize2(<<?TRUE, Rest/binary>>) ->
    {?FATE_TRUE, Rest};
deserialize2(<<?FALSE, Rest/binary>>) ->
    {?FATE_FALSE, Rest};
deserialize2(<<?EMPTY_TUPLE, Rest/binary>>) ->
    {?FATE_UNIT, Rest};
deserialize2(<<?EMPTY_STRING, Rest/binary>>) ->
    {?FATE_EMPTY_STRING, Rest};
deserialize2(<<?LONG_TUPLE, Rest/binary>>) ->
    {Size, Rest1} = rlp_decode_int(Rest),
    N = Size + ?SHORT_TUPLE_SIZE,
    {List, Rest2} = deserialize_elements(N, Rest1),
    {?FATE_TUPLE(list_to_tuple(List)), Rest2};
deserialize2(<<S:4, ?SHORT_TUPLE:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {?FATE_TUPLE(list_to_tuple(List)), Rest1};
deserialize2(<<?LONG_LIST, Rest/binary>>) ->
    {Size, Rest1} = rlp_decode_int(Rest),
    Length = Size + ?SHORT_LIST_SIZE,
    {List, Rest2} = deserialize_elements(Length, Rest1),
    {?MAKE_FATE_LIST(List), Rest2};
deserialize2(<<S:4, ?SHORT_LIST:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {?MAKE_FATE_LIST(List), Rest1};
deserialize2(<<?MAP, Rest/binary>>) ->
    {Size, Rest1} = rlp_decode_int(Rest),
    {List, Rest2} = deserialize_elements(2*Size, Rest1),
    KVList = insert_kv(List),
    case sort_and_check(KVList) == KVList of
        true ->
            Map = maps:from_list(KVList),
            {?MAKE_FATE_MAP(Map), Rest2};
        false ->
            error({unknown_map_serialization_format, KVList})
    end;
deserialize2(<<?MAP_ID, Rest/binary>>) ->
    {Id, Rest1} = rlp_decode_int(Rest),
    {?FATE_STORE_MAP(#{}, Id), Rest1};
deserialize2(<<?VARIANT, Rest/binary>>) ->
    {AritiesBin, <<Tag:8, Rest2/binary>>} = aeser_rlp:decode_one(Rest),
    Arities = binary_to_list(AritiesBin),
    Size = length(Arities),
    if Tag > Size -> exit({too_large_tag_in_variant, Tag, Size});
       true ->
            {?FATE_TUPLE(T), Rest3} = deserialize2(Rest2),
            Arity = lists:nth(Tag+1, Arities),
            NumElements = size(T),
            if NumElements =/= Arity ->
                    exit({tag_does_not_match_type_in_variant, Tag, Arity});
               true ->
                    {?FATE_VARIANT(Arities, Tag, T), Rest3}
            end
    end;
deserialize2(<<TypeTag, _/binary>> = Bin) when ?IS_TYPE_TAG(TypeTag) ->
    {Type, Rest} = deserialize_type(Bin),
    {?FATE_TYPEREP(Type), Rest}.

insert_kv([]) -> [];
insert_kv([K, V | R]) -> [{K, V} | insert_kv(R)].

deserialize_elements(0, Rest) ->
    {[], Rest};
deserialize_elements(N, Es) ->
    {E, Rest} = deserialize2(Es),
    {Tail, Rest2} = deserialize_elements(N-1, Rest),
    {[E|Tail], Rest2}.


%% It is important to remove duplicated keys.
%% For deserialize this check is needed to observe illegal duplicates.
sort_and_check(List) ->
    UniqKeyList =
        lists:foldr(fun({K, V}, Acc) ->
                            case valid_key_type(K) andalso not lists:keymember(K, 1, Acc) of
                                true -> [{K,V}|Acc];
                                false -> Acc
                            end
                    end, [], List),
    sort(UniqKeyList).

%% Sorting is used to get a unique result.
%% Deserialization is checking whether the provided key-value pairs are sorted
%% and raises an exception if not.

sort(KVList) ->
    SortFun = fun({K1, _}, {K2, _}) ->
                      aeb_fate_data:elt(K1, K2)
              end,
    lists:sort(SortFun, KVList).

valid_key_type(K) when ?IS_FATE_MAP(K) ->
    error({map_as_key_in_map, K});
valid_key_type(_K) ->
    true.
