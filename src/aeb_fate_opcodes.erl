%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%%-------------------------------------------------------------------

-module(aeb_fate_opcodes).

-export([ args/1
        , end_bb/1
        , mnemonic/1
        , m_to_op/1
        , opcode/1
        ]).

-include_lib("aebytecode/include/aeb_fate_opcodes.hrl").


%%====================================================================
%% API
%%====================================================================

opcode(X) when X >= 0, X =< 255 -> X;
opcode({comment,X}) -> ?COMMENT(X).

mnemonic(?NOP)         -> 'NOP'        ;
mnemonic(?RETURN)      -> 'RETURN'     ;
mnemonic(?RETURNR)     -> 'RETURNR'    ;
mnemonic(?PUSH)        -> 'PUSH'       ;
mnemonic(?JUMP)        -> 'JUMP'       ;
mnemonic(?JUMPIF)      -> 'JUMPIF'     ;
mnemonic(?INC)         -> 'INC'        ;
mnemonic(?CALL)        -> 'CALL'       ;
mnemonic(?CALL_T)      -> 'CALL_T'     ;
mnemonic(?CALL_R)      -> 'CALL_R'     ;
mnemonic(?CALL_TR)     -> 'CALL_TR'    ;
mnemonic(?ADD)         -> 'ADD'        ;
mnemonic(?SUB)         -> 'SUB'        ;
mnemonic(?MUL)         -> 'MUL'        ;
mnemonic(?DIV)         -> 'DIV'        ;
mnemonic(?MOD)         -> 'MOD'        ;
mnemonic(?POW)         -> 'POW'        ;
mnemonic(?AND)         -> 'AND'        ;
mnemonic(?OR)          -> 'OR'         ;
mnemonic(?NOT)         -> 'NOT'        ;
mnemonic(?LT)          -> 'LT'         ;
mnemonic(?GT)          -> 'GT'         ;
mnemonic(?EGT)         -> 'EGT'        ;
mnemonic(?ELT)         -> 'ELT'        ;
mnemonic(?EQ)          -> 'EQ'         ;
mnemonic(?NEQ)         -> 'NEQ'        ;
mnemonic(?STORE)       -> 'STORE'      ;
mnemonic(?TUPLE)       -> 'TUPLE'      ;
mnemonic(?ELEMENT)     -> 'ELEMENT'    ;
mnemonic(?MAP_EMPTY)   -> 'MAP_EMPTY'  ;
mnemonic(?MAP_UPDATE)  -> 'MAP_UPDATE' ;
mnemonic(?MAP_DELETE)  -> 'MAP_DELETE' ;
mnemonic(?MAP_MEMBER)  -> 'MAP_MEMBER' ;
mnemonic(?MAP_LOOKUP)  -> 'MAP_LOOKUP' ;
mnemonic(?MAP_LOOKUPD) -> 'MAP_LOOKUPD';
mnemonic(?MAP_FROM_LIST)->'MAP_FROM_LIST' ;

mnemonic(OP)           -> {OP, nothandled}.

m_to_op('NOP')         -> ?NOP         ;
m_to_op('COMMENT')     -> ?COMMENT("") ;
m_to_op('RETURN')      -> ?RETURN      ;
m_to_op('RETURNR')     -> ?RETURNR     ;
m_to_op('PUSH')        -> ?PUSH        ;
m_to_op('JUMP')        -> ?JUMP        ;
m_to_op('JUMPIF')      -> ?JUMPIF      ;
m_to_op('INC')         -> ?INC         ;
m_to_op('ADD')         -> ?ADD         ;
m_to_op('SUB')         -> ?SUB         ;
m_to_op('MUL')         -> ?MUL         ;
m_to_op('DIV')         -> ?DIV         ;
m_to_op('MOD')         -> ?MOD         ;
m_to_op('POW')         -> ?POW         ;
m_to_op('AND')         -> ?AND         ;
m_to_op('OR')          -> ?OR          ;
m_to_op('NOT')         -> ?NOT         ;
m_to_op('LT')          -> ?LT          ;
m_to_op('GT')          -> ?GT          ;
m_to_op('ELT')         -> ?ELT         ;
m_to_op('EGT')         -> ?EGT         ;
m_to_op('EQ')          -> ?EQ          ;
m_to_op('NEQ')         -> ?NEQ         ;
m_to_op('STORE')       -> ?STORE       ;
m_to_op('TUPLE')       -> ?TUPLE       ;
m_to_op('ELEMENT')     -> ?ELEMENT     ;
m_to_op('MAP_EMPTY')   -> ?MAP_EMPTY   ;
m_to_op('MAP_UPDATE')  -> ?MAP_UPDATE  ;
m_to_op('MAP_DELETE')  -> ?MAP_DELETE  ;
m_to_op('MAP_MEMBER')  -> ?MAP_MEMBER  ;
m_to_op('MAP_LOOKUP')  -> ?MAP_LOOKUP  ;
m_to_op('MAP_LOOKUPD') -> ?MAP_LOOKUPD ;
m_to_op('MAP_FROM_LIST')->?MAP_FROM_LIST ;
m_to_op('CALL')        -> ?CALL        ;
m_to_op('CALL_T')      -> ?CALL_T      ;
m_to_op('CALL_R')      -> ?CALL_R      ;
m_to_op('CALL_TR')     -> ?CALL_TR     .

args(?NOP)     -> 0;
args(?RETURN)  -> 0;
args(?INC)     -> 0;

args(?RETURNR)   -> 1;
args(?PUSH)      -> 1;
args(?JUMP)      -> 1;
args(?CALL)      -> 1;
args(?CALL_T)    -> 1;
args(?TUPLE)     -> 1;
args(?MAP_EMPTY) -> 1;

args(?JUMPIF)        -> 2;
args(?CALL_R)        -> 2;
args(?CALL_TR)       -> 2;
args(?NOT)           -> 2;
args(?STORE)         -> 2;
args(?MAP_FROM_LIST) -> 2;

args(?ADD)        -> 3;
args(?SUB)        -> 3;
args(?MUL)        -> 3;
args(?DIV)        -> 3;
args(?MOD)        -> 3;
args(?POW)        -> 3;
args(?AND)        -> 3;
args(?OR)         -> 3;
args(?LT)         -> 3;
args(?GT)         -> 3;
args(?EGT)        -> 3;
args(?ELT)        -> 3;
args(?EQ)         -> 3;
args(?NEQ)        -> 3;
args(?MAP_MEMBER) -> 3;
args(?MAP_LOOKUP) -> 3;
args(?MAP_DELETE) -> 3;

args(?ELEMENT)     -> 4;
args(?MAP_UPDATE)  -> 4;
args(?MAP_LOOKUPD) -> 4;

args(_) -> 0. %% TODO do not allow this

end_bb(?RETURN) -> true;
end_bb(?RETURNR)-> true;
end_bb(?JUMP)   -> true;
end_bb(?JUMPIF) -> true;
end_bb(?CALL)   -> true;
end_bb(?CALL_T) -> true;
end_bb(?CALL_R) -> true;
end_bb(?CALL_TR)-> true;
end_bb(_)       -> false.

