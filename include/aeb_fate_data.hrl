-define(FATE_INTEGER_T,   integer()).
-define(FATE_BYTE_T,      0..255).
-define(FATE_BOOLEAN_T,   true | false).
-define(FATE_NIL_T,       []).
-define(FATE_LIST_T,      list()).
-define(FATE_UNIT_T,      {tuple, {}}).
-define(FATE_MAP_T,       #{ fate_type() => fate_type() }).
-define(FATE_STORE_MAP_T, {store_map, #{ fate_type() => fate_type() | ?FATE_MAP_TOMBSTONE }, integer()}).
-define(FATE_STRING_T,    binary()).
-define(FATE_ADDRESS_T,   {address, <<_:256>>}).
-define(FATE_BYTES_T(N),  {bytes, binary()}).
-define(FATE_CONTRACT_T,  {contract, <<_:256>>}).
-define(FATE_ORACLE_T,    {oracle, <<_:256>>}).
-define(FATE_ORACLE_Q_T,  {oracle_query, <<_:256>>}).
-define(FATE_CHANNEL_T,   {channel, <<_:256>>}).
-define(FATE_VARIANT_T,   {variant, [byte()], ?FATE_BYTE_T, tuple()}).
-define(FATE_VOID_T,      void).
-define(FATE_TUPLE_T,     {tuple, tuple()}).
-define(FATE_BITS_T,      {bits, integer()}).
-define(FATE_TYPEREP_T,   {typerep, fate_type_type()}).

-define(IS_FATE_INTEGER(X), (is_integer(X))).
-define(IS_FATE_LIST(X),    (is_list(X))).
-define(IS_FATE_STRING(X),  (is_binary(X))).
-define(IS_FATE_STORE_MAP(X), (is_tuple(X) andalso tuple_size(X) == 3
                                           andalso store_map == element(1, X)
                                           andalso is_map(element(2, X))
                                           andalso is_integer(element(3, X)))).
-define(IS_FATE_MAP(X),     (is_map(X))).
-define(IS_FATE_TUPLE(X),   (is_tuple(X) andalso (tuple == element(1, X) andalso is_tuple(element(2, X))))).
-define(IS_FATE_ADDRESS(X), (is_tuple(X) andalso (address == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_BYTES(X),   (is_tuple(X) andalso (bytes == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_BYTES(N, X), (?IS_FATE_BYTES(X) andalso byte_size(element(2, X)) == (N))).
-define(IS_FATE_CONTRACT(X), (is_tuple(X) andalso (contract == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_ORACLE(X), (is_tuple(X) andalso (oracle == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_ORACLE_Q(X), (is_tuple(X) andalso (oracle_query == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_CHANNEL(X), (is_tuple(X) andalso (channel == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_BITS(X), (is_tuple(X) andalso (bits == element(1, X) andalso is_integer(element(2, X))))).
-define(IS_FATE_VARIANT(X), (is_tuple(X)
                             andalso
                               (variant == element(1, X)
                                andalso is_list(element(2, X))
                                andalso is_integer(element(3, X))
                                andalso is_tuple(element(4, X))
                               ))).
-define(IS_FATE_BOOLEAN(X), is_boolean(X)).
-define(IS_FATE_TYPEREP(X), (is_tuple(X) andalso tuple_size(X) =:= 2 andalso element(1, X) =:= typerep)).

-define(FATE_UNIT,         {tuple, {}}).
-define(FATE_TUPLE(T),     {tuple, T}).
-define(FATE_ADDRESS(A),   {address, A}).
-define(FATE_BYTES(X),     {bytes, X}).
-define(FATE_CONTRACT(X),  {contract, X}).
-define(FATE_ORACLE(X),    {oracle, X}).
-define(FATE_ORACLE_Q(X),  {oracle_query, X}).
-define(FATE_CHANNEL(X),   {channel, X}).
-define(FATE_BITS(B),      {bits, B}).
-define(FATE_TYPEREP(T),   {typerep, T}).
-define(FATE_STORE_MAP(Cache, Id), {store_map, Cache, Id}).
-define(FATE_MAP_TOMBSTONE, '__DELETED__').

-define(FATE_INTEGER_VALUE(X), (X)).
-define(FATE_BOOLEAN_VALUE(X), (X)).
-define(FATE_LIST_VALUE(X), (X)).
-define(FATE_TUPLE_ELEMENTS(X), (tuple_to_list(element(2, X)))).
-define(FATE_STRING_VALUE(X), (X)).
-define(FATE_ADDRESS_VALUE(X), (element(2, X))).
-define(FATE_BYTES_VALUE(X), (element(2, X))).
-define(FATE_CONTRACT_VALUE(X), (element(2, X))).
-define(FATE_ORACLE_VALUE(X), (element(2, X))).
-define(FATE_CHANNEL_VALUE(X), (element(2, X))).
-define(FATE_BITS_VALUE(X), (element(2, X))).
-define(FATE_MAP_VALUE(X), (X)).
-define(FATE_STORE_MAP_CACHE(X), (element(2, X))).
-define(FATE_STORE_MAP_ID(X), (element(3, X))).
-define(FATE_MAP_SIZE(X), (map_size(X))).
-define(FATE_STRING_SIZE(X), (byte_size(X))).
-define(FATE_TRUE,  true).
-define(FATE_FALSE, false).
-define(FATE_NIL,   []).
-define(FATE_VOID,  void).
-define(FATE_EMPTY_STRING, <<>>).
-define(FATE_STRING(S), S).
-define(FATE_VARIANT(Arity, Tag,T), {variant, Arity, Tag, T}).

-define(MAKE_FATE_INTEGER(X), X).
-define(MAKE_FATE_LIST(X), X).
-define(MAKE_FATE_MAP(X),  X).
-define(MAKE_FATE_STRING(X),  X).
