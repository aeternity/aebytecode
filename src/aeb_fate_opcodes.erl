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

mnemonic(?NOP)         -> 'NOP' ;
mnemonic(?RETURN)      -> 'RETURN' ;
mnemonic(?CALL)        -> 'CALL' ;
mnemonic(?CALL_R)      -> 'CALL_R' ;
mnemonic(?CALL_T)      -> 'CALL_T' ;
mnemonic(?CALL_TR)     -> 'CALL_TR' ;
mnemonic(?JUMP)        ->    'JUMP' ;
mnemonic(?JUMPIF)      ->    'JUMPIF' ;
mnemonic(?PUSH)        ->    'PUSH' ;
mnemonic(?DUP)         ->    'DUP' ;
mnemonic(?DUPA)        ->    'DUPA' ;
mnemonic(?POP)         ->    'POP' ;
mnemonic(?STORE)       ->    'STORE' ;
mnemonic(?ADD)         ->    'ADD' ;
mnemonic(?MUL)         ->    'MUL' ;
mnemonic(?SUB)         ->    'SUB' ;
mnemonic(?DIV)         ->    'DIV' ;
mnemonic(?MOD)         ->    'MOD' ;
mnemonic(?POW)         ->    'POW' ;
mnemonic(?LT)          ->    'LT' ;
mnemonic(?GT)          ->    'GT' ;
mnemonic(?EQ)          ->    'EQ' ;
mnemonic(?ELT)         ->    'ELT' ;
mnemonic(?EGT)         ->    'EGT' ;
mnemonic(?NEQ)         ->    'NEQ' ;
mnemonic(?AND)         ->    'AND' ;
mnemonic(?OR)          ->    'OR' ;
mnemonic(?NOT)         ->    'NOT' ;
mnemonic(?TUPLE)       ->    'TUPLE' ;
mnemonic(?ELEMENT)     ->    'ELEMENT' ;
mnemonic(?MAP_EMPTY)   ->    'MAP_EMPTY' ;
mnemonic(?MAP_LOOKUP)  ->    'MAP_LOOKUP' ;
mnemonic(?MAP_UPDATE)  ->    'MAP_UPDATE' ;
mnemonic(?MAP_DELETE)  ->    'MAP_DELETE' ;
mnemonic(?MAP_MEMBER)  ->    'MAP_MEMBER' ;
mnemonic(?MAP_FROM_LIST) ->    'MAP_FROM_LIST' ;
mnemonic(?NIL)         ->    'NIL' ;
mnemonic(?IS_NIL)      ->    'IS_NIL' ;
mnemonic(?CONS)        ->    'CONS' ;
mnemonic(?HD)          ->    'HD' ;
mnemonic(?TL)          ->    'TL' ;
mnemonic(?LENGTH)      ->    'LENGTH' ;
mnemonic(?STR_EQ)      ->    'STR_EQ' ;
mnemonic(?STR_JOIN)    ->    'STR_JOIN' ;
mnemonic(?ADDR_TO_STR) ->    'ADDR_TO_STR' ;
mnemonic(?STR_REVERSE) ->    'STR_REVERSE' ;
mnemonic(?INT_TO_ADDR) ->    'INT_TO_ADDR' ;
mnemonic(?VARIANT)     ->    'VARIANT' ;
mnemonic(?VARIANT_TEST) ->    'VARIANT_TEST' ;
mnemonic(?VARIANT_ELEMENT) ->    'VARIANT_ELEMENT' ;
mnemonic(?BITS_NONE)   ->    'BITS_NONE' ;
mnemonic(?BITS_NONEA)  ->    'BITS_NONEA' ;
mnemonic(?BITS_ALL)    ->    'BITS_ALL' ;
mnemonic(?BITS_ALLA)   ->    'BITS_ALLA' ;
mnemonic(?BITS_SET)    ->    'BITS_SET' ;
mnemonic(?BITS_CLEAR)  ->    'BITS_CLEAR' ;
mnemonic(?BITS_TEST)   ->    'BITS_TEST' ;
mnemonic(?BITS_SUM)    ->    'BITS_SUM' ;
mnemonic(?BITS_OR)     ->    'BITS_OR' ;
mnemonic(?BITS_AND)    ->    'BITS_AND' ;
mnemonic(?BITS_DIFF)   ->    'BITS_DIFF' ;
mnemonic(?ADDRESS)     ->    'ADDRESS' ;
mnemonic(?BALANCE)     ->    'BALANCE' ;
mnemonic(?ORIGIN)      ->    'ORIGIN' ;
mnemonic(?CALLER)      ->    'CALLER' ;
mnemonic(?GASPRICE)    ->    'GASPRICE' ;
mnemonic(?BLOCKHASH)   ->    'BLOCKHASH' ;
mnemonic(?BENEFICIARY) ->    'BENEFICIARY' ;
mnemonic(?TIMESTAMP)   ->    'TIMESTAMP' ;
mnemonic(?NUMBER)      ->    'NUMBER' ;
mnemonic(?DIFFICULTY)  ->    'DIFFICULTY' ;
mnemonic(?GASLIMIT)    ->    'GASLIMIT' ;
mnemonic(?GAS)         ->    'GAS' ;
mnemonic(?LOG0)        ->    'LOG0' ;
mnemonic(?LOG1)        ->    'LOG1' ;
mnemonic(?LOG2)        ->    'LOG2' ;
mnemonic(?LOG3)        ->    'LOG3' ;
mnemonic(?LOG4)        ->    'LOG4' ;
mnemonic(?ABORT)       ->    'ABORT' ;
mnemonic(?EXIT)        ->    'EXIT' ;
mnemonic(?DEACTIVATE)  ->    'DEACTIVATE' ;
mnemonic(?INC)         ->    'INC' ;
mnemonic(?DEC)         ->    'DEC' ;
mnemonic(?INCA)        ->    'INCA' ;
mnemonic(?DECA)        ->    'DECA' ;
mnemonic(?INT_TO_STR)  ->    'INT_TO_STR' ;
mnemonic(?SPEND)       ->    'SPEND' ;
mnemonic(?ORACLE_REGISTER) ->    'ORACLE_REGISTER' ;
mnemonic(?ORACLE_QUERY)    ->    'ORACLE_QUERY' ;
mnemonic(?ORACLE_RESPOND)  ->    'ORACLE_RESPOND' ;
mnemonic(?ORACLE_EXTEND)   ->    'ORACLE_EXTEND' ;
mnemonic(?ORACLE_GET_ANSWER)   ->    'ORACLE_GET_ANSWER' ;
mnemonic(?ORACLE_GET_QUESTION) ->    'ORACLE_GET_QUESTION' ;
mnemonic(?ORACLE_QUERY_FEE)    ->    'ORACLE_QUERY_FEE' ;
mnemonic(?AENS_RESOLVE)    ->    'AENS_RESOLVE' ;
mnemonic(?AENS_PRECLAIM)   ->    'AENS_PRECLAIM' ;
mnemonic(?AENS_CLAIM)      ->    'AENS_CLAIM' ;
mnemonic(?AENS_UPDATE)     ->    'AENS_UPDATE' ;
mnemonic(?AENS_TRANSFER)   ->    'AENS_TRANSFER' ;
mnemonic(?AENS_REVOKE)     ->    'AENS_REVOKE' ;
mnemonic(?ECVERIFY)        ->    'ECVERIFY' ;
mnemonic(?SHA3)            ->    'SHA3' ;
mnemonic(?SHA256)          ->    'SHA256' ;
mnemonic(?BLAKE2B)         ->    'BLAKE2B' ;
mnemonic(?RETURNR)         ->    'RETURNR' ;
mnemonic(?MAP_LOOKUPD)     ->    'MAP_LOOKUPD' ;
mnemonic(?SWITCH_V2)       ->    'SWITCH_V2' ;
mnemonic(?SWITCH_V3)       ->    'SWITCH_V3' ;
mnemonic(?SWITCH_V4)       ->    'SWITCH_V4' ;
mnemonic(?SWITCH_V5)       ->    'SWITCH_V5' ;
mnemonic(?BITS_ALL_N)      ->    'BITS_ALL_N' ;
mnemonic(?FUNCTION)        ->    'FUNCTION' ;
mnemonic(?EXTEND)          ->    'EXTEND'.


m_to_op('NOP')         -> ?NOP ;
m_to_op('RETURN')      -> ?RETURN ;
m_to_op('CALL')        -> ?CALL ;
m_to_op('CALL_R')      -> ?CALL_R ;
m_to_op('CALL_T')      -> ?CALL_T ;
m_to_op('CALL_TR')     -> ?CALL_TR ;
m_to_op('JUMP')        ->    ?JUMP ;
m_to_op('JUMPIF')      ->    ?JUMPIF ;
m_to_op('PUSH')        ->    ?PUSH ;
m_to_op('DUP')         ->    ?DUP ;
m_to_op('DUPA')        ->    ?DUPA ;
m_to_op('POP')         ->    ?POP ;
m_to_op('STORE')       ->    ?STORE ;
m_to_op('ADD')         ->    ?ADD ;
m_to_op('MUL')         ->    ?MUL ;
m_to_op('SUB')         ->    ?SUB ;
m_to_op('DIV')         ->    ?DIV ;
m_to_op('MOD')         ->    ?MOD ;
m_to_op('POW')         ->    ?POW ;
m_to_op('LT')          ->    ?LT ;
m_to_op('GT')          ->    ?GT ;
m_to_op('EQ')          ->    ?EQ ;
m_to_op('ELT')         ->    ?ELT ;
m_to_op('EGT')         ->    ?EGT ;
m_to_op('NEQ')         ->    ?NEQ ;
m_to_op('AND')         ->    ?AND ;
m_to_op('OR')          ->    ?OR ;
m_to_op('NOT')         ->    ?NOT ;
m_to_op('TUPLE')       ->    ?TUPLE ;
m_to_op('ELEMENT')     ->    ?ELEMENT ;
m_to_op('MAP_EMPTY')   ->    ?MAP_EMPTY ;
m_to_op('MAP_LOOKUP')  ->    ?MAP_LOOKUP ;
m_to_op('MAP_UPDATE')  ->    ?MAP_UPDATE ;
m_to_op('MAP_DELETE')  ->    ?MAP_DELETE ;
m_to_op('MAP_MEMBER')  ->    ?MAP_MEMBER ;
m_to_op('MAP_FROM_LIST') ->    ?MAP_FROM_LIST ;
m_to_op('NIL')         ->    ?NIL ;
m_to_op('IS_NIL')      ->    ?IS_NIL ;
m_to_op('CONS')        ->    ?CONS ;
m_to_op('HD')          ->    ?HD ;
m_to_op('TL')          ->    ?TL ;
m_to_op('LENGTH')      ->    ?LENGTH ;
m_to_op('STR_EQ')      ->    ?STR_EQ ;
m_to_op('STR_JOIN')    ->    ?STR_JOIN ;
m_to_op('ADDR_TO_STR') ->    ?ADDR_TO_STR ;
m_to_op('STR_REVERSE') ->    ?STR_REVERSE ;
m_to_op('INT_TO_ADDR') ->    ?INT_TO_ADDR ;
m_to_op('VARIANT')     ->    ?VARIANT ;
m_to_op('VARIANT_TEST') ->    ?VARIANT_TEST ;
m_to_op('VARIANT_ELEMENT') ->    ?VARIANT_ELEMENT ;
m_to_op('BITS_NONEA')   ->    ?BITS_NONEA ;
m_to_op('BITS_ALL')    ->    ?BITS_ALL ;
m_to_op('BITS_ALLA')   ->    ?BITS_ALLA ;
m_to_op('BITS_SET')    ->    ?BITS_SET ;
m_to_op('BITS_CLEAR')  ->    ?BITS_CLEAR ;
m_to_op('BITS_TEST')   ->    ?BITS_TEST ;
m_to_op('BITS_SUM')    ->    ?BITS_SUM ;
m_to_op('BITS_OR')     ->    ?BITS_OR ;
m_to_op('BITS_AND')    ->    ?BITS_AND ;
m_to_op('BITS_DIFF')   ->    ?BITS_DIFF ;
m_to_op('ADDRESS')     ->    ?ADDRESS ;
m_to_op('BALANCE')     ->    ?BALANCE ;
m_to_op('ORIGIN')      ->    ?ORIGIN ;
m_to_op('CALLER')      ->    ?CALLER ;
m_to_op('GASPRICE')    ->    ?GASPRICE ;
m_to_op('BLOCKHASH')   ->    ?BLOCKHASH ;
m_to_op('BENEFICIARY') ->    ?BENEFICIARY ;
m_to_op('TIMESTAMP')   ->    ?TIMESTAMP ;
m_to_op('NUMBER')      ->    ?NUMBER ;
m_to_op('DIFFICULTY')  ->    ?DIFFICULTY ;
m_to_op('GASLIMIT')    ->    ?GASLIMIT ;
m_to_op('GAS')         ->    ?GAS ;
m_to_op('LOG0')        ->    ?LOG0 ;
m_to_op('LOG1')        ->    ?LOG1 ;
m_to_op('LOG2')        ->    ?LOG2 ;
m_to_op('LOG3')        ->    ?LOG3 ;
m_to_op('LOG4')        ->    ?LOG4 ;
m_to_op('ABORT')       ->    ?ABORT ;
m_to_op('EXIT')        ->    ?EXIT ;
m_to_op('DEACTIVATE')  ->    ?DEACTIVATE ;
m_to_op('INC')         ->    ?INC ;
m_to_op('DEC')         ->    ?DEC ;
m_to_op('INCA')        ->    ?INCA ;
m_to_op('DECA')        ->    ?DECA ;
m_to_op('INT_TO_STR')  ->    ?INT_TO_STR ;
m_to_op('SPEND')       ->    ?SPEND ;
m_to_op('ORACLE_REGISTER') ->    ?ORACLE_REGISTER ;
m_to_op('ORACLE_QUERY')    ->    ?ORACLE_QUERY ;
m_to_op('ORACLE_RESPOND')  ->    ?ORACLE_RESPOND ;
m_to_op('ORACLE_EXTEND')   ->    ?ORACLE_EXTEND ;
m_to_op('ORACLE_GET_ANSWER')   ->    ?ORACLE_GET_ANSWER ;
m_to_op('ORACLE_GET_QUESTION') ->    ?ORACLE_GET_QUESTION ;
m_to_op('ORACLE_QUERY_FEE')    ->    ?ORACLE_QUERY_FEE ;
m_to_op('AENS_RESOLVE')    ->    ?AENS_RESOLVE ;
m_to_op('AENS_PRECLAIM')   ->    ?AENS_PRECLAIM ;
m_to_op('AENS_CLAIM')      ->    ?AENS_CLAIM ;
m_to_op('AENS_UPDATE')     ->    ?AENS_UPDATE ;
m_to_op('AENS_TRANSFER')   ->    ?AENS_TRANSFER ;
m_to_op('AENS_REVOKE')     ->    ?AENS_REVOKE ;
m_to_op('ECVERIFY')        ->    ?ECVERIFY ;
m_to_op('SHA3')            ->    ?SHA3 ;
m_to_op('SHA256')          ->    ?SHA256 ;
m_to_op('BLAKE2B')         ->    ?BLAKE2B ;
m_to_op('RETURNR')         ->    ?RETURNR ;
m_to_op('MAP_LOOKUPD')     ->    ?MAP_LOOKUPD ;
m_to_op('SWITCH_V2')       ->    ?SWITCH_V2 ;
m_to_op('SWITCH_V3')       ->    ?SWITCH_V3 ;
m_to_op('SWITCH_V4')       ->    ?SWITCH_V4 ;
m_to_op('SWITCH_V5')       ->    ?SWITCH_V5 ;
m_to_op('BITS_ALL_N')      ->    ?BITS_ALL_N ;
m_to_op('FUNCTION')        ->    ?FUNCTION ;
m_to_op('EXTEND')          ->    ?EXTEND.



args(?NOP)        -> 0;
args(?RETURN)     -> 0;
args(?INCA)       -> 0;
args(?DECA)       -> 0;
args(?DUPA)       -> 0;
args(?BITS_NONEA) -> 0;
args(?BITS_ALLA)  -> 0;

args(?INC)       -> 1;
args(?DEC)       -> 1;
args(?RETURNR)   -> 1;
args(?PUSH)      -> 1;
args(?JUMP)      -> 1;
args(?CALL)      -> 1;
args(?CALL_T)    -> 1;
args(?TUPLE)     -> 1;
args(?MAP_EMPTY) -> 1;
args(?DUP)       -> 1;
args(?POP)       -> 1;
args(?NIL)       -> 1;
args(?BITS_NONE) -> 1;
args(?BITS_ALL)  -> 1;
args(?ADDRESS)   -> 1;
args(?BALANCE)   -> 1;
args(?ORIGIN)    -> 1;
args(?CALLER)    -> 1;
args(?GASPRICE)  -> 1;
args(?BLOCKHASH) -> 1;
args(?BENEFICIARY) -> 1;
args(?TIMESTAMP) -> 1;
args(?NUMBER)    -> 1;
args(?DIFFICULTY)-> 1;
args(?GASLIMIT)  -> 1;
args(?GAS)       -> 1;
args(?ABORT)     -> 1;
args(?EXIT)      -> 1;

args(?JUMPIF)        -> 2;
args(?CALL_R)        -> 2;
args(?CALL_TR)       -> 2;
args(?HD)            -> 2;
args(?TL)            -> 2;
args(?NOT)           -> 2;
args(?STORE)         -> 2;
args(?LENGTH)        -> 2;
args(?IS_NIL)        -> 2;
args(?BITS_SUM)      -> 2;
args(?BITS_ALL_N)    -> 2;
args(?ADDR_TO_STR)   -> 2;
args(?STR_REVERSE)   -> 2;
args(?INT_TO_ADDR)   -> 2;
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
args(?CONS)       -> 3;
args(?STR_EQ)     -> 3;
args(?STR_JOIN)   -> 3;
args(?MAP_MEMBER) -> 3;
args(?MAP_LOOKUP) -> 3;
args(?MAP_DELETE) -> 3;
args(?BITS_OR)    -> 3;
args(?BITS_AND)   -> 3;
args(?BITS_SET)   -> 3;
args(?BITS_DIFF)  -> 3;
args(?BITS_TEST)  -> 3;
args(?BITS_CLEAR) -> 3;
args(?VARIANT_TEST)    -> 3;
args(?VARIANT_ELEMENT) -> 3;
args(?INT_TO_STR)  -> 3;
args(?SWITCH_V2)   -> 3;

args(?SWITCH_V3)   -> 4;
args(?ELEMENT)     -> 4;
args(?VARIANT)     -> 4;
args(?MAP_UPDATE)  -> 4;
args(?MAP_LOOKUPD) -> 4;

args(?SWITCH_V4)   -> 5;

args(?SWITCH_V5)   -> 6;

args(_) -> 0. %% TODO do not allow this

end_bb(?RETURN)    -> true;
end_bb(?RETURNR)   -> true;
end_bb(?JUMP)      -> true;
end_bb(?JUMPIF)    -> true;
end_bb(?CALL)      -> true;
end_bb(?CALL_T)    -> true;
end_bb(?CALL_R)    -> true;
end_bb(?CALL_TR)   -> true;
end_bb(?SWITCH_V2) -> true;
end_bb(?SWITCH_V3) -> true;
end_bb(?SWITCH_V4) -> true;
end_bb(?SWITCH_V5) -> true;
end_bb(?ABORT)     -> true;
end_bb(?EXIT)      -> true;

end_bb(_)          -> false.
