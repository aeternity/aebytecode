%% Provide constructor functuions for Fate instructions.
%% Provide types and documentation for Fate instructions.


-module(aeb_fate_code).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-type fate_arg_immediate(T) :: {immediate, T}.
-type fate_arg_var()        :: {var, integer()}.
-type fate_arg_arg()        :: {arg, integer()}.
-type fate_arg_stack()      :: {stack, integer()}.
-type fate_arg() :: fate_arg_immediate()
                  | fate_arg_var()
                  | fate_arg_arg()
                  | fate_arg_stack().

-type fate_arg_immediate() :: {immediate, aeb_fate_data:fate_type()}.

-type fate_return()     ::  'RETURN'.
-type fate_returnr()    :: {'RETURNR', fate_arg()}.
-type fate_call()       :: {'CALL',
                            fate_arg_immediate(aeb_fate_data:fate_string())}.
-type fate_call_t()     :: {'CALL_T',
                            fate_arg_immediate(aeb_fate_data:fate_string())}.
-type fate_call_r()     :: {'CALL_R', fate_arg(),
                            fate_arg_immediate(aeb_fate_data:fate_string())}.
-type fate_call_tr()    :: {'CALL_TR', fate_arg(),
                            fate_arg_immediate(aeb_fate_data:fate_string())}.
-type fate_jump()       :: {'JUMP',
                            fate_arg_immediate(aeb_fate_data:fate_integer())}.
-type fate_jumpif()     :: {'JUMPIF', fate_arg(),
                            fate_arg_immediate(aeb_fate_data:fate_integer())}.
-type fate_switch_v2()  :: {'SWITCH_V2', fate_arg(),
                            fate_arg_immediate(aeb_fate_data:fate_integer()),
                            fate_arg_immediate(aeb_fate_data:fate_integer())}.
-type fate_switch_v3()  :: {'SWITCH_V3', fate_arg(),
                            fate_arg_immediate(aeb_fate_data:fate_integer()),
                            fate_arg_immediate(aeb_fate_data:fate_integer()),
                            fate_arg_immediate(aeb_fate_data:fate_integer())}.
-type fate_switch_vn()  :: {'SWITCH_VN', fate_arg(),
                            fate_arg_immediate(aeb_fate_data:fate_integer()),
                            [fate_arg_immediate(aeb_fate_data:fate_integer())]}.
-type fate_push()       :: {'PUSH', fate_arg()}.
-type fate_inca()       ::  'INCA'.
-type fate_inc()        :: {'INC', fate_arg()}.
-type fate_deca()       ::  'DECA'.
-type fate_dec()        :: {'DEC', fate_arg()}.


-type fate_code() :: fate_return()
                   | fate_returnr()
                   | fate_call()
                   | fate_call_t()
                   | fate_call_r()
                   | fate_call_tr()
                   | fate_jump()
                   | fate_jumpif()
                   | fate_switch_v2()
                   | fate_switch_v3()
                   | fate_switch_vn()
                   | fate_push()
                   | fate_inca()
                   | fate_inc()
                   | fate_deca()
                   | fate_dec()
                     .

-export_type([ fate_code/0

             , fate_arg/0
             ]).

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
        , switch_n/2
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

-spec return() -> fate_return().
return() ->
    'RETURN'.

-spec return(fate_arg()) -> fate_returnr().
return(Arg) ->
    {'RETURNR', Arg}.

-spec call(aeb_fate_data:fate_string()) -> fate_call().
call(Function) when ?IS_FATE_STRING(Function)->
    {'CALL', ?i(Function) }.

-spec call_t(aeb_fate_data:fate_string()) -> fate_call_t().
call_t(Function) when ?IS_FATE_STRING(Function) ->
    {'CALL_T', ?i(Function)}.

-spec call_r(fate_arg(), aeb_fate_data:fate_string()) -> fate_call_r().
call_r(Contract, Function) when ?IS_FATE_STRING(Function) ->
    {'CALL_R', Contract, ?i(Function)}.

-spec call_tr(fate_arg(), aeb_fate_data:fate_string()) -> fate_call_tr().
call_tr(Contract, Function) when ?IS_FATE_STRING(Function) ->
    {'CALL_TR', Contract, ?i(Function)}.

-spec jump(aeb_fate_data:fate_integer()) -> fate_jump().
jump(BB) when ?IS_FATE_INTEGER(BB) ->
    {'JUMP', ?i(BB)}.

-spec jumpif(fate_arg(), aeb_fate_data:fate_integer()) -> fate_jumpif().
jumpif(Arg, BB) when  ?IS_FATE_INTEGER(BB) ->
    {'JUMPIF', Arg, ?i(BB)}.

-spec switch(fate_arg(),
             aeb_fate_data:fate_integer(),
             aeb_fate_data:fate_integer())
            -> fate_switch_v2().
switch(Arg, BB1, BB2) when ?IS_FATE_INTEGER(BB1),
                           ?IS_FATE_INTEGER(BB2) ->
    {'SWITCH_V2', Arg, ?i(BB1), ?i(BB2)}.

-spec switch(fate_arg(),
             aeb_fate_data:fate_integer(),
             aeb_fate_data:fate_integer(),
             aeb_fate_data:fate_integer())
            -> fate_switch_v3().
switch(Arg, BB1, BB2, BB3) when ?IS_FATE_INTEGER(BB1),
                                ?IS_FATE_INTEGER(BB2),
                                ?IS_FATE_INTEGER(BB3) ->
    {'SWITCH_V3', Arg, ?i(BB1), ?i(BB2), ?i(BB3)}.

-spec switch_n(fate_arg(),
             [aeb_fate_data:fate_integer()])
            -> fate_switch_vn().
switch_n(Arg, BBS) when is_list(BBS) ->
    N = length(BBS),
    {'SWITCH_VN', Arg, ?i(N), [?i(BB) || BB <- BBS]}.

-spec push(fate_arg()) -> fate_push().
push(Arg) ->
    {'PUSH', Arg}.

-spec inc() -> fate_inca().
inc() ->
    'INCA'.

-spec inc(fate_arg()) -> fate_inc().
inc(Arg) ->
    {'INC', Arg}.

-spec dec() -> fate_deca().
dec() ->
    'DECA'.

-spec dec(fate_arg()) -> fate_dec().
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

tuple(Size) when ?IS_FATE_INTEGER(Size) ->
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

dup(N) when ?IS_FATE_INTEGER(N) ->
    {'DUP', ?i(N)}.

pop() ->
    'POP'.

store(Var, What) ->
    {'STORE', Var, What}.

nop() ->
    'NOP'.
