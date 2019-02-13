%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%%-------------------------------------------------------------------

-module(aefa_opcodes).

-export([ args/1
        , end_bb/1
        , mnemonic/1
        , m_to_op/1
        , opcode/1
        ]).

-include_lib("aebytecode/include/aefa_opcodes.hrl").


%%====================================================================
%% API
%%====================================================================

opcode(X) when X >= 0, X =< 255 -> X;
opcode({comment,X}) -> ?COMMENT(X).

mnemonic(?NOP)         -> 'NOP'        ;
mnemonic(?RETURN)      -> 'RETURN'     ;
mnemonic(?PUSH)        -> 'PUSH'       ;
mnemonic(?JUMP)        -> 'JUMP'       ;
mnemonic(?INC)         -> 'INC'        ;
mnemonic(?CALL)        -> 'CALL'       ;
mnemonic(?CALL_T)      -> 'CALL_T'     ;
mnemonic(OP)           -> {OP, nothandled} ;
mnemonic({comment,_})  -> 'COMMENT'    .

m_to_op('NOP')         -> ?NOP         ;
m_to_op('COMMENT')     -> ?COMMENT("") ;
m_to_op('RETURN')      -> ?RETURN      ;
m_to_op('PUSH')        -> ?PUSH        ;
m_to_op('JUMP')        -> ?JUMP        ;
m_to_op('INC')         -> ?INC         ;
m_to_op('CALL')        -> ?CALL        ;
m_to_op('CALL_T')      -> ?CALL_T      ;
m_to_op(Data) when 0=<Data, Data=<255 -> Data.

args(?NOP) -> 0;
args(?RETURN) -> 0;
args(?PUSH) -> 1;
args(?JUMP) -> 1;
args(?INC) -> 0;
args(?CALL) -> hash;
args(?CALL_T) -> hash;
args(_) -> 0. %% TODO do not allow this

end_bb(?RETURN) -> true;
end_bb(?JUMP)   -> true;
end_bb(?CALL)   -> true;
end_bb(?CALL_T) -> true;
end_bb(_)       -> false.

