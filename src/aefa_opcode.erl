%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Opcodes
%%% @end
%%%-------------------------------------------------------------------

-module(aefa_opcode).

-export([ mnemonic/1
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
mnemonic({comment,_})  -> 'COMMENT'    .

m_to_op('NOP')         -> ?NOP         ;
m_to_op('COMMENT')     -> ?COMMENT("") ;
m_to_op('RETURN')      -> ?RETURN      ;
m_to_op('PUSH')        -> ?PUSH        ;
m_to_op('JUMP')        -> ?JUMP        ;
m_to_op(Data) when 0=<Data, Data=<255 -> Data.

