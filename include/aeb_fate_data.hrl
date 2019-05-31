-define(FATE_INTEGER_T,   integer()).
-define(FATE_BYTE_T,      0..255).
-define(FATE_BOOLEAN_T,   true | false).
-define(FATE_NIL_T,       []).
-define(FATE_LIST_T,      list()).
-define(FATE_UNIT_T,      {tuple, {}}).
-define(FATE_MAP_T,       #{ fate_type() => fate_type() }).
-define(FATE_STRING_T,    binary()).
-define(FATE_ADDRESS_T,   {address, <<_:256>>}).
-define(FATE_HASH_T,      {hash, binary()}).
-define(FATE_SIGNATURE_T, {signature, binary()}).
-define(FATE_CONTRACT_T,  {contract, <<_:256>>}).
-define(FATE_ORACLE_T,    {oracle, <<_:256>>}).
-define(FATE_NAME_T,      {name, <<_:256>>}).
-define(FATE_CHANNEL_T,   {channel, <<_:256>>}).
-define(FATE_VARIANT_T,   {variant, [byte()], ?FATE_BYTE_T, tuple()}).
-define(FATE_VOID_T,      void).
-define(FATE_TUPLE_T,     {tuple, tuple()}).
-define(FATE_BITS_T,      {bits, integer()}).

-define(IS_FATE_INTEGER(X), (is_integer(X))).
-define(IS_FATE_LIST(X),    (is_list(X))).
-define(IS_FATE_STRING(X),  (is_binary(X))).
-define(IS_FATE_MAP(X),     (is_map(X))).
-define(IS_FATE_TUPLE(X),   (is_tuple(X) andalso (tuple == element(1, X) andalso is_tuple(element(2, X))))).
-define(IS_FATE_ADDRESS(X), (is_tuple(X) andalso (address == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_HASH(X),    (is_tuple(X) andalso (hash == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_SIGNATURE(X), (is_tuple(X) andalso (signature == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_CONTRACT(X), (is_tuple(X) andalso (contract == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_ORACLE(X), (is_tuple(X) andalso (oracle == element(1, X) andalso is_binary(element(2, X))))).
-define(IS_FATE_NAME(X), (is_tuple(X) andalso (name == element(1, X) andalso is_binary(element(2, X))))).
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

-define(FATE_UNIT,         {tuple, {}}).
-define(FATE_TUPLE(T),     {tuple, T}).
-define(FATE_ADDRESS(A),   {address, A}).
-define(FATE_HASH(X),      {hash, X}).
-define(FATE_SIGNATURE(S), {signature, S}).
-define(FATE_CONTRACT(X),  {contract, X}).
-define(FATE_ORACLE(X),    {oracle, X}).
-define(FATE_NAME(X),      {name, X}).
-define(FATE_CHANNEL(X),   {channel, X}).
-define(FATE_BITS(B),      {bits, B}).


-define(FATE_INTEGER_VALUE(X), (X)).
-define(FATE_BOOLEAN_VALUE(X), (X)).
-define(FATE_LIST_VALUE(X), (X)).
-define(FATE_TUPLE_ELEMENTS(X), (tuple_to_list(element(2, X)))).
-define(FATE_STRING_VALUE(X), (X)).
-define(FATE_ADDRESS_VALUE(X), (element(2, X))).
-define(FATE_HASH_VALUE(X), (element(2, X))).
-define(FATE_SIGNATURE_VALUE(X), (element(2, X))).
-define(FATE_CONTRACT_VALUE(X), (element(2, X))).
-define(FATE_ORACLE_VALUE(X), (element(2, X))).
-define(FATE_NAME_VALUE(X), (element(2, X))).
-define(FATE_CHANNEL_VALUE(X), (element(2, X))).
-define(FATE_BITS_VALUE(X), (element(2, X))).
-define(FATE_MAP_VALUE(X), (X)).
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
