%%% -*- erlang-indent-level:4; indent-tabs-mode: nil -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, aeternity Anstalt
%%% @doc
%%%     Handling FATE code.
%%% @end
%%% Created : 9 Jan 2019
%%%-------------------------------------------------------------------

Definitions.
DIGIT    = [0-9]
HEXDIGIT = [0-9a-fA-F]
LOWER    = [a-z_]
UPPER    = [A-Z]
INT      = {DIGIT}+
HEX      = 0x{HEXDIGIT}+
HASH     = #{HEXDIGIT}+
WS       = [\000-\s]
ID       = {LOWER}[a-zA-Z0-9_]*


Rules.
%%{ID}           : {token, {id, TokenLine, TokenChars }}.

RETURN         : {token, {mnemonic, TokenLine, 'RETURN'}}.
CALL           : {token, {mnemonic, TokenLine, 'CALL'}}.
FUNCTION       : {token, {function, TokenLine, 'FUNCTION' }}.
NOP            : {token, {mnemonic, TokenLine, 'NOP'}}.

CALL_R         : {token, {mnemonic, TokenLine, 'CALL_R'}}.
CALL_T         : {token, {mnemonic, TokenLine, 'CALL_T'}}.
CALL_TR        : {token, {mnemonic, TokenLine, 'CALL_TR'}}.
JUMP           : {token, {mnemonic, TokenLine, 'JUMP'}}.
JUMPI          : {token, {mnemonic, TokenLine, 'JUMPI'}}.
SWITCH         : {token, {mnemonic, TokenLine, 'SWITCH'}}.

PUSH           : {token, {mnemonic, TokenLine, 'PUSH'}}.
DUP            : {token, {mnemonic, TokenLine, 'DUP'}}.
POP            : {token, {mnemonic, TokenLine, 'POP'}}.

STORE          : {token, {mnemonic, TokenLine, 'STORE'}}.

ADD            : {token, {mnemonic, TokenLine, 'ADD'}}.
MUL            : {token, {mnemonic, TokenLine, 'MUL'}}.
SUB            : {token, {mnemonic, TokenLine, 'SUB'}}.
DIV            : {token, {mnemonic, TokenLine, 'DIV'}}.
MOD            : {token, {mnemonic, TokenLine, 'MOD'}}.
POW            : {token, {mnemonic, TokenLine, 'POW'}}.

LT             : {token, {mnemonic, TokenLine, 'LT'}}.
GT             : {token, {mnemonic, TokenLine, 'GT'}}.
EQ             : {token, {mnemonic, TokenLine, 'EQ'}}.
ELT            : {token, {mnemonic, TokenLine, 'ELT'}}.
EGT            : {token, {mnemonic, TokenLine, 'EGT'}}.
NEQ            : {token, {mnemonic, TokenLine, 'NEQ'}}.

AND            : {token, {mnemonic, TokenLine, 'AND'}}.
OR             : {token, {mnemonic, TokenLine, 'OR'}}.
NOT            : {token, {mnemonic, TokenLine, 'NOT'}}.

TUPLE          : {token, {mnemonic, TokenLine, 'TUPLE'}}.
ELEMENT        : {token, {mnemonic, TokenLine, 'ELEMENT'}}.

MAP_EMPTY      : {token, {mnemonic, TokenLine, 'MAP_EMPTY'}}.
MAP_LOOKUP     : {token, {mnemonic, TokenLine, 'MAP_LOOKUP'}}.
MAP_UPDATE     : {token, {mnemonic, TokenLine, 'MAP_UPDATE'}}.
MAP_MEMBER     : {token, {mnemonic, TokenLine, 'MAP_MEMBER'}}.
MAP_FROM_LIST  : {token, {mnemonic, TokenLine, 'MAP_FROM_LIST'}}.

NIL            : {token, {mnemonic, TokenLine, 'NIL'}}.
IS_NIL         : {token, {mnemonic, TokenLine, 'IS_NIL'}}.
CONS           : {token, {mnemonic, TokenLine, 'CONS'}}.
HD             : {token, {mnemonic, TokenLine, 'HD'}}.
TL             : {token, {mnemonic, TokenLine, 'TL'}}.
LENGTH         : {token, {mnemonic, TokenLine, 'LENGTH'}}.

STR_EQ         : {token, {mnemonic, TokenLine, 'STR_EQ'}}.
STR_JOIN       : {token, {mnemonic, TokenLine, 'STR_JOIN'}}.
INT_TO_STR     : {token, {mnemonic, TokenLine, 'INT_TO_STR'}}.
ADDR_TO_STR    : {token, {mnemonic, TokenLine, 'ADDR_TO_STR'}}.
STR_REVERSE    : {token, {mnemonic, TokenLine, 'STR_REVERSE'}}.

INT_TO_ADDR    : {token, {mnemonic, TokenLine, 'INT_TO_ADDR'}}.

VARIANT        : {token, {mnemonic, TokenLine, 'VARIANT'}}.
VARIANT_TEST   : {token, {mnemonic, TokenLine, 'VARIANT_TEST'}}.
VARIANT_ELEMENT : {token, {mnemonic, TokenLine, 'VARIANT_ELEMENT'}}.

BITS_NONE      : {token, {mnemonic, TokenLine, 'BITS_NONE'}}.
BITS_ALL       : {token, {mnemonic, TokenLine, 'BITS_ALL'}}.
BITS_SET       : {token, {mnemonic, TokenLine, 'BITS_SET'}}.
BITS_CLEAR     : {token, {mnemonic, TokenLine, 'BITS_CLEAR'}}.
BITS_TEST      : {token, {mnemonic, TokenLine, 'BITS_TEST'}}.
BITS_SUM       : {token, {mnemonic, TokenLine, 'BITS_SUM'}}.
BITS_OR        : {token, {mnemonic, TokenLine, 'BITS_OR'}}.
BITS_AND       : {token, {mnemonic, TokenLine, 'BITS_AND'}}.
BITS_DIFF      : {token, {mnemonic, TokenLine, 'BITS_DIFF'}}.


ADDRESS        : {token, {mnemonic, TokenLine, 'ADDRESS'}}.
BALANCE        : {token, {mnemonic, TokenLine, 'BALANCE'}}.
ORIGIN         : {token, {mnemonic, TokenLine, 'ORIGIN'}}.
CALLER         : {token, {mnemonic, TokenLine, 'CALLER'}}.
GASPRICE       : {token, {mnemonic, TokenLine, 'GASPRICE'}}.
BLOCKHASH      : {token, {mnemonic, TokenLine, 'BLOCKHASH'}}.
BENEFICIARY    : {token, {mnemonic, TokenLine, 'BENEFICIARY'}}.
TIMESTAMP      : {token, {mnemonic, TokenLine, 'TIMESTAMP'}}.
NUMBER         : {token, {mnemonic, TokenLine, 'NUMBER'}}.
DIFFICULTY     : {token, {mnemonic, TokenLine, 'DIFFICULTY'}}.
GASLIMIT       : {token, {mnemonic, TokenLine, 'GASLIMIT'}}.
GAS            : {token, {mnemonic, TokenLine, 'GAS'}}.
LOG0           : {token, {mnemonic, TokenLine, 'LOG0'}}.
LOG1           : {token, {mnemonic, TokenLine, 'LOG1'}}.
LOG2           : {token, {mnemonic, TokenLine, 'LOG2'}}.
LOG3           : {token, {mnemonic, TokenLine, 'LOG3'}}.
LOG4           : {token, {mnemonic, TokenLine, 'LOG4'}}.
ABORT          : {token, {mnemonic, TokenLine, 'ABORT'}}.
EXIT           : {token, {mnemonic, TokenLine, 'EXIT'}}.
DEACTIVATE     : {token, {mnemonic, TokenLine, 'DEACTIVATE'}}.
COMMENT        : {token, {mnemonic, TokenLine, 'COMMENT'}}.
{ID} :
 {token, {id, TokenLine, TokenChars}}.
{HEX} :
 {token, {int, TokenLine, parse_hex(TokenChars)}}.
{INT} :
 {token, {int, TokenLine, parse_int(TokenChars)}}.
{HASH} :
 {token, {hash, TokenLine, parse_hash(TokenChars)}}.


%% Symbols
\-\>  : {token, {'->', TokenLine}}.
,   : {token, {',', TokenLine}}.
\.  : {token, {'.', TokenLine}}.
\(  : {token, {'(', TokenLine}}.
\)  : {token, {')', TokenLine}}.
\[  : {token, {'[', TokenLine}}.
\]  : {token, {']', TokenLine}}.
\{  : {token, {'{', TokenLine}}.
\}  : {token, {'}', TokenLine}}.


%% Whitespace ignore
{WS} : skip_token.

%% Comments (TODO: nested comments)
;;.*                     : skip_token.

. : {error, "Unexpected token: " ++ TokenChars}.

Erlang code.

-export([scan/1]).

-dialyzer({nowarn_function, yyrev/2}).

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

-include_lib("aebytecode/include/aefa_opcodes.hrl").


parse_hex("0x" ++ Chars) -> list_to_integer(Chars, 16).

parse_int(Chars) -> list_to_integer(Chars).

parse_hash("#" ++ Chars) ->
    N = list_to_integer(Chars, 16),
    <<N:256>>.

scan(S) ->
    string(S).

