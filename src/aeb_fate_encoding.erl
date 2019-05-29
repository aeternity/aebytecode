%% Fate data (and instruction) serialization.
%%
%% The FATE serialization has to fullfill the following properties:
%% * There has to be 1 and only 1 byte sequence
%%     representing each unique value in FATE.
%% * A valid byte sequence has to be deserializable to a FATE value.
%% * A valid byte sequence must not contain any trailing bytes.
%% * A serialization is a sequence of 8-bit bytes.
%%
%% The serialization function should fullfill the following:
%% * A valid FATE value should be serialized to a byte sequence.
%% * Any other argument, not representing a valid FATE value should
%%     throw an exception
%%
%% The deserialization function should fullfill the following:
%% * A valid byte sequence should be deserialized to a valid FATE value.
%% * Any other argument, not representing a valid byte sequence should
%%     throw an exception
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
                                    %% 1001 0111
                                    %% 1010 0111
                                    %% 1011 0111
                                    %% 1100 0111
                                    %% 1101 0111
                                    %% 1110 0111
                                    %% 1111 0111
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
-define(NIL          , 2#10111111). %% 1011 1111 - Empty list
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
-define(OTYPE_HASH,      1).
-define(OTYPE_SIGNATURE, 2).
-define(OTYPE_CONTRACT,  3).
-define(OTYPE_ORACLE,    4).
-define(OTYPE_NAME,      5).
-define(OTYPE_CHANNEL,   6).

%% --------------------------------------------------
%% Serialize
%% Serialized a Fate data value into a sequence of bytes
%% according to the Fate serialization specification.
%% TODO: The type Fate Data is not final yet.
-spec serialize(aeb_fate_data:fate_type()) -> binary().
serialize(?FATE_TRUE)        -> <<?TRUE>>;
serialize(?FATE_FALSE)       -> <<?FALSE>>;
serialize(?FATE_NIL)         -> <<?NIL>>;     %% ! Untyped
serialize(?FATE_UNIT)        -> <<?EMPTY_TUPLE>>;  %% ! Untyped
serialize(M) when ?IS_FATE_MAP(M), ?FATE_MAP_SIZE(M) =:= 0 -> <<?EMPTY_MAP>>;  %% ! Untyped
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
serialize(?FATE_ADDRESS(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_ADDRESS, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_HASH(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_HASH, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_SIGNATURE(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_SIGNATURE, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_CONTRACT(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_CONTRACT, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_ORACLE(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_ORACLE, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_NAME(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_NAME, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_CHANNEL(Address)) when is_binary(Address) ->
    <<?OBJECT, ?OTYPE_CHANNEL, (aeser_rlp:encode(Address))/binary>>;
serialize(?FATE_TUPLE(T)) when size(T) > 0 ->
    S = size(T),
    L = tuple_to_list(T),
    Rest = << <<(serialize(E))/binary>> || E <- L >>,
    if S < ?SHORT_TUPLE_SIZE ->
            <<S:4, ?SHORT_TUPLE:4, Rest/binary>>;
       true ->
            Size = rlp_integer(S - ?SHORT_TUPLE_SIZE),
            <<?LONG_TUPLE:8, Size/binary, Rest/binary>>
    end;
serialize(L) when ?IS_FATE_LIST(L) ->
    [_E|_] = List = ?FATE_LIST_VALUE(L),
    S = length(List),
    Rest = << <<(serialize(El))/binary>> || El <- List >>,
    if S < ?SHORT_LIST_SIZE ->
            <<S:4, ?SHORT_LIST:4, Rest/binary>>;
       true ->
            Val = rlp_integer(S - ?SHORT_LIST_SIZE),
            <<?LONG_LIST, Val/binary, Rest/binary>>
    end;
serialize(Map) when ?IS_FATE_MAP(Map) ->
    L = [{_K,_V}|_] = lists:sort(maps:to_list(?FATE_MAP_VALUE(Map))),
    Size = length(L),
    %% TODO:  check all K same type, and all V same type
    %%        check K =/= map
    Elements = << <<(serialize(K1))/binary, (serialize(V1))/binary>> || {K1,V1} <- L >>,
    <<?MAP,
      (rlp_integer(Size))/binary,
      (Elements)/binary>>;
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
    end.


%% -----------------------------------------------------

-spec serialize_type(aeb_fate_data:fate_type_type()) -> [byte()].
serialize_type(integer)     -> [?TYPE_INTEGER];
serialize_type(boolean)     -> [?TYPE_BOOLEAN];
serialize_type({list, T})   -> [?TYPE_LIST | serialize_type(T)];
serialize_type({tuple, Ts}) ->
    case length(Ts) of
        N when N =< 255 ->
            [?TYPE_TUPLE, N | [serialize_type(T) || T <- Ts]]
    end;
serialize_type(address)     -> [?TYPE_OBJECT, ?OTYPE_ADDRESS];
serialize_type(hash)        -> [?TYPE_OBJECT, ?OTYPE_HASH];
serialize_type(signature)   -> [?TYPE_OBJECT, ?OTYPE_SIGNATURE];
serialize_type(contract)    -> [?TYPE_OBJECT, ?OTYPE_CONTRACT];
serialize_type(oracle)      -> [?TYPE_OBJECT, ?OTYPE_ORACLE];
serialize_type(name)        -> [?TYPE_OBJECT, ?OTYPE_NAME];
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
deserialize_type(<<?TYPE_LIST, Rest/binary>>) ->
    {T, Rest2} = deserialize_type(Rest),
    {{list, T}, Rest2};
deserialize_type(<<?TYPE_TUPLE, N, Rest/binary>>) ->
    {Ts, Rest2} = deserialize_types(N, Rest, []),
    {{tuple, Ts}, Rest2};
deserialize_type(<<?TYPE_OBJECT, ObjectType, Rest/binary>>) ->
    case ObjectType of
        ?OTYPE_ADDRESS   -> {address, Rest};
        ?OTYPE_HASH      -> {hash, Rest};
        ?OTYPE_SIGNATURE -> {signature, Rest};
        ?OTYPE_CONTRACT  -> {contract, Rest};
        ?OTYPE_ORACLE    -> {oracle, Rest};
        ?OTYPE_NAME      -> {name, Rest};
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

rlp_integer(S) when S >= 0 ->
    aeser_rlp:encode(binary:encode_unsigned(S)).

serialize_integer(I) when ?IS_FATE_INTEGER(I) ->
    V = ?FATE_INTEGER_VALUE(I),
    Abs = abs(V),
    Sign = case V < 0 of
               true  -> ?NEG_SIGN;
               false -> ?POS_SIGN
           end,
    if Abs < ?SMALL_INT_SIZE -> <<Sign:1, Abs:6, ?SMALL_INT:1>>;
       Sign =:= ?NEG_SIGN -> <<?NEG_BIG_INT,
                               (rlp_integer(Abs - ?SMALL_INT_SIZE))/binary>>;
       Sign =:= ?POS_SIGN -> <<?POS_BIG_INT,
                               (rlp_integer(Abs - ?SMALL_INT_SIZE))/binary>>
    end.

serialize_bits(B) when is_integer(B) ->
    Abs = abs(B),
    Sign = case B < 0 of
               true  -> ?NEG_SIGN;
               false -> ?POS_SIGN
           end,
    if
        Sign =:= ?NEG_SIGN -> <<?NEG_BITS, (rlp_integer(Abs))/binary>>;
        Sign =:= ?POS_SIGN -> <<?POS_BITS, (rlp_integer(Abs))/binary>>
    end.

-spec deserialize(binary()) -> aeb_fate_data:fate_type().
deserialize(B) ->
    {T, <<>>} = deserialize2(B),
    T.

deserialize_one(B) -> deserialize2(B).

deserialize2(<<?POS_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {?MAKE_FATE_INTEGER(I), Rest};
deserialize2(<<?NEG_SIGN:1, I:6, ?SMALL_INT:1, Rest/binary>>) ->
    {?MAKE_FATE_INTEGER(-I), Rest};
deserialize2(<<?NEG_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = aeser_rlp:decode_one(Rest),
    {?MAKE_FATE_INTEGER(-binary:decode_unsigned(Bint) - ?SMALL_INT_SIZE),
     Rest2};
deserialize2(<<?POS_BIG_INT, Rest/binary>>) ->
    {Bint, Rest2} = aeser_rlp:decode_one(Rest),
    {?MAKE_FATE_INTEGER(binary:decode_unsigned(Bint) + ?SMALL_INT_SIZE),
     Rest2};
deserialize2(<<?NEG_BITS, Rest/binary>>) ->
    {Bint, Rest2} = aeser_rlp:decode_one(Rest),
    case binary:decode_unsigned(Bint) of
        Pos when Pos > 0 ->
            {?FATE_BITS(-Pos), Rest2};
        N ->
            error({illegal_parameter, neg_bits, N})
    end;
deserialize2(<<?POS_BITS, Rest/binary>>) ->
    {Bint, Rest2} = aeser_rlp:decode_one(Rest),
    {?FATE_BITS(binary:decode_unsigned(Bint)), Rest2};
deserialize2(<<?LONG_STRING, Rest/binary>>) ->
    {S, Rest2} = deserialize_one(Rest),
    Size = S + ?SHORT_STRING_SIZE,
    String = binary:part(Rest2, 0, Size),
    Rest3 = binary:part(Rest2, byte_size(Rest2), - (byte_size(Rest2) - Size)),
    {?MAKE_FATE_STRING(String), Rest3};
deserialize2(<<S:6, ?SHORT_STRING:2, Rest/binary>>) ->
    String = binary:part(Rest, 0, S),
    Rest2 = binary:part(Rest, byte_size(Rest), - (byte_size(Rest) - S)),
    {?MAKE_FATE_STRING(String), Rest2};
deserialize2(<<?OBJECT, ObjectType, Rest/binary>>) ->
    {A, Rest2} = aeser_rlp:decode_one(Rest),
    Val =
        case ObjectType of
            ?OTYPE_ADDRESS   -> ?FATE_ADDRESS(A);
            ?OTYPE_HASH      -> ?FATE_HASH(A);
            ?OTYPE_SIGNATURE -> ?FATE_SIGNATURE(A);
            ?OTYPE_CONTRACT  -> ?FATE_CONTRACT(A);
            ?OTYPE_ORACLE    -> ?FATE_ORACLE(A);
            ?OTYPE_NAME      -> ?FATE_NAME(A);
            ?OTYPE_CHANNEL   -> ?FATE_CHANNEL(A)
        end,
    {Val, Rest2};
deserialize2(<<?TRUE, Rest/binary>>) ->
    {?FATE_TRUE, Rest};
deserialize2(<<?FALSE, Rest/binary>>) ->
    {?FATE_FALSE, Rest};
deserialize2(<<?NIL, Rest/binary>>) ->
    {?FATE_NIL, Rest};
deserialize2(<<?EMPTY_TUPLE, Rest/binary>>) ->
    {?FATE_UNIT, Rest};
deserialize2(<<?EMPTY_MAP, Rest/binary>>) ->
    {?MAKE_FATE_MAP(#{}), Rest};
deserialize2(<<?EMPTY_STRING, Rest/binary>>) ->
    {?FATE_EMPTY_STRING, Rest};
deserialize2(<<?LONG_TUPLE, Rest/binary>>) ->
    {BSize, Rest1} = aeser_rlp:decode_one(Rest),
    N = binary:decode_unsigned(BSize) + ?SHORT_TUPLE_SIZE,
    {List, Rest2} = deserialize_elements(N, Rest1),
    {?FATE_TUPLE(list_to_tuple(List)), Rest2};
deserialize2(<<S:4, ?SHORT_TUPLE:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {?FATE_TUPLE(list_to_tuple(List)), Rest1};
deserialize2(<<?LONG_LIST, Rest/binary>>) ->
    {BLength, Rest1} = aeser_rlp:decode_one(Rest),
    Length = binary:decode_unsigned(BLength) + ?SHORT_LIST_SIZE,
    {List, Rest2} = deserialize_elements(Length, Rest1),
    {?MAKE_FATE_LIST(List), Rest2};
deserialize2(<<S:4, ?SHORT_LIST:4, Rest/binary>>) ->
    {List, Rest1} = deserialize_elements(S, Rest),
    {?MAKE_FATE_LIST(List), Rest1};
deserialize2(<<?MAP, Rest/binary>>) ->
    {BSize, Rest1} = aeser_rlp:decode_one(Rest),
    Size = binary:decode_unsigned(BSize),
    {List, Rest2} = deserialize_elements(2*Size, Rest1),
    Map = insert_kv(List, #{}),
    {?MAKE_FATE_MAP(Map), Rest2};
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
    end.

insert_kv([], M) -> M;
insert_kv([K,V|R], M) -> insert_kv(R, maps:put(K, V, M)).

deserialize_elements(0, Rest) ->
    {[], Rest};
deserialize_elements(N, Es) ->
    {E, Rest} = deserialize2(Es),
    {Tail, Rest2} = deserialize_elements(N-1, Rest),
    {[E|Tail], Rest2}.
