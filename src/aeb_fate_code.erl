-module(aeb_fate_code).

-export([ return/0
        , return/1
        , call/1
        , call_t/1
        , call_r/2
        , call_tr/2
        , jump/1
        , jumpif/2
        , switch/3
        , switch/4
        , switch/5
        , switch/6
        , push/1
        , inc/0
        , inc/1
        , dec/0
        , dec/1
        , add/3
        , sub/3
        , mul/3
        , divide/3
        , modulo/3
        , pow/3
        , lt/3
        , gt/3
        , elt/3
        , egt/3
        , eq/3
        , neq/3
        , and_op/3
        , or_op/3
        , not_op/2
        , tuple/1
        , element_op/4
        , map_empty/1
        , map_lookup/3
        , map_lookup/4
        , map_update/4
        , map_member/3
        , map_from_list/2
        , nil/1
        , is_nil/2
        , cons/3
        , hd/2
        , tl/2
        , length/2
        , str_eq/3
        , str_join/3
        , int_to_str/2
        , addr_to_str/2
        , str_reverse/2
        , int_to_addr/2
        , variant_test/3
        , variant_element/3
        , variant/4
        , bits_none/0
        , bits_none/1
        , bits_all/0
        , bits_all/1
        , bits_all_n/2
        , bits_set/3
        , bits_clear/3
        , bits_test/3
        , bits_sum/2
        , bits_or/3
        , bits_and/3
        , bits_diff/3
        , dup/0
        , dup/1
        , pop/0
        , store/2
        , nop/0
        ]).

-define(i(__X__), {immediate, __X__ }).

return() ->
    'RETURN'.

return(Arg) ->
    {'RETURNR', Arg}.

call(Function) when is_binary(Function)->
    {'CALL', ?i(Function) }.

call_t(Function) when is_binary(Function) ->
    {'CALL_T', ?i(Function)}.

call_r(Contract, Function) when is_binary(Function) ->
    {'CALL_R', Contract, ?i(Function)}.

call_tr(Contract, Function) when is_binary(Function) ->
    {'CALL_TR', Contract, ?i(Function)}.

jump(BB) when is_integer(BB) ->
    {'JUMP', ?i(BB)}.

jumpif(Arg, BB) when is_integer(BB) ->
    {'JUMPIF', Arg, ?i(BB)}.

switch(Arg, BB1, BB2) when is_integer(BB1),
                           is_integer(BB2) ->
    {'SWITCH_V2', Arg, ?i(BB1), ?i(BB2)}.

switch(Arg, BB1, BB2, BB3) when is_integer(BB1),
                                is_integer(BB2),
                                is_integer(BB3) ->
    {'SWITCH_V3', Arg, ?i(BB1), ?i(BB2), ?i(BB3)}.

switch(Arg, BB1, BB2, BB3, BB4) when is_integer(BB1),
                                     is_integer(BB2),
                                     is_integer(BB3),
                                     is_integer(BB4) ->
    {'SWITCH_V4', Arg, ?i(BB1), ?i(BB2), ?i(BB3), ?i(BB4)}.

switch(Arg, BB1, BB2, BB3, BB4, BB5) when is_integer(BB1),
                                          is_integer(BB2),
                                          is_integer(BB3),
                                          is_integer(BB4),
                                          is_integer(BB5) ->
    {'SWITCH_V5', Arg, ?i(BB1), ?i(BB2), ?i(BB3), ?i(BB4), ?i(BB5)}.

push(Arg) ->
    {'PUSH', Arg}.

inc() ->
    'INCA'.

inc(Arg) ->
    {'INC', Arg}.

dec() ->
    'DECA'.

dec(Arg) ->
    {'DEC', Arg}.

add(Dest, Left, Right) ->
    {'ADD', Dest, Left, Right}.

sub(Dest, Left, Right) ->
    {'SUB', Dest, Left, Right}.

mul(Dest, Left, Right) ->
    {'MUL', Dest, Left, Right}.

divide(Dest, Left, Right) ->
    {'DIV', Dest, Left, Right}.

modulo(Dest, Left, Right) ->
    {'MOD', Dest, Left, Right}.

pow(Dest, Left, Right) ->
    {'POW', Dest, Left, Right}.

lt(Dest, Left, Right) ->
    {'LT', Dest, Left, Right}.

gt(Dest, Left, Right) ->
    {'GT', Dest, Left, Right}.

elt(Dest, Left, Right) ->
    {'ELT', Dest, Left, Right}.

egt(Dest, Left, Right) ->
    {'EGT', Dest, Left, Right}.

eq(Dest, Left, Right) ->
    {'EQ', Dest, Left, Right}.

neq(Dest, Left, Right) ->
    {'NEQ', Dest, Left, Right}.

and_op(Dest, Left, Right) ->
    {'AND', Dest, Left, Right}.

or_op(Dest, Left, Right) ->
    {'OR', Dest, Left, Right}.

not_op(Dest, Arg) ->
    {'NOT', Dest, Arg}.

tuple(Size) when is_integer(Size) ->
    {'TUPLE', ?i(Size)}.

element_op(Type, Dest, N, T) ->
    {'ELEMENT', Type, Dest, N, T}.

map_empty(Dest) ->
    {'MAP_EMPTY', Dest}.

map_lookup(Dest, Map, Key) ->
    {'MAP_LOOKUP', Dest, Map, Key}.

map_lookup(Dest, Map, Key, Default) ->
    {'MAP_LOOKUPD', Dest, Map, Key, Default}.

map_update(Dest, Map, Key, Value) ->
    {'MAP_UPDATE', Dest, Map, Key, Value}.

map_member(Dest, Map, Key) ->
    {'MAP_MEMBER', Dest, Map, Key}.

map_from_list(Dest, List) ->
    {'MAP_MEMBER', Dest, List}.

nil(Dest) ->
    {'NIL', Dest}.

is_nil(Dest, List) ->
    {'IS_NIL', Dest, List}.

cons(Dest, Hd, Tl) ->
    {'CONS', Dest, Hd, Tl}.

hd(Dest, List) ->
    {'HD', Dest, List}.

tl(Dest, List) ->
    {'TL', Dest, List}.

length(Dest, List) ->
    {'LENGTH', Dest, List}.

str_eq(Dest, Str1, Str2) ->
    {'STR_EQ', Dest, Str1, Str2}.

str_join(Dest, Str1, Str2) ->
    {'STR_JOIN', Dest, Str1, Str2}.

int_to_str(Dest, Str) ->
    {'INT_TO_STR', Dest, Str}.

addr_to_str(Dest, Str) ->
    {'ADDR_TO_STR', Dest, Str}.

str_reverse(Dest, Str) ->
    {'STR_REVERSE', Dest, Str}.

int_to_addr(Dest, Str) ->
    {'INT_TO_ADDR', Dest, Str}.

variant_test(Dest, Variant, Tag) ->
    {'VARIANT_TEST', Dest, Variant, Tag}.

variant_element( Dest, Variant, Index) ->
    {'VARIANT_ELEMENT', Dest, Variant, Index}.

variant(Dest, SizeA, TagA, ElementsA) ->
    {'VARIANT', Dest, SizeA, TagA, ElementsA}.

bits_none() ->
    'BITS_NONEA'.

bits_none(To) ->
    {'BITS_NONE', To}.

bits_all() ->
    'BITS_ALLA'.

bits_all(To) ->
    {'BITS_ALL', To}.

bits_all_n(To, N) ->
    {'BITS_ALL_N', To, N}.

bits_set(To, Bits, Bit) ->
    {'BITS_SET', To, Bits, Bit}.

bits_clear(To, Bits, Bit) ->
    {'BITS_CLEAR', To, Bits, Bit}.

bits_test(To, Bits, Bit) ->
    {'BITS_TEST', To, Bits, Bit}.

bits_sum(To, Bits) ->
    {'BITS_SUM', To, Bits}.

bits_or(To, Bits, Bit) ->
    {'BITS_OR', To, Bits, Bit}.

bits_and(To, Bits, Bit) ->
    {'BITS_AND', To, Bits, Bit}.

bits_diff(To, Bits, Bit) ->
    {'BITS_DIFF', To, Bits, Bit}.

dup() ->
    'DUPA'.

dup(N) when is_integer(N) ->
    {'DUP', ?i(N)}.

pop() ->
    'POP'.

store(Var, What) ->
    {'STORE', Var, What}.

nop() ->
    'NOP'.
