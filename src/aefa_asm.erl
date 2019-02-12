%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Assembler for Fate machine code.
%%%
%%%      Assembler code can be read from a file.
%%%      The assembler has the following format
%%%      Comments start with 2 semicolons and runs till end of line
%%%         ;; This is a comment
%%%      Opcode mnemonics start with an upper case letter.
%%%         DUP
%%%      Identifiers start with a lower case letter
%%%         an_identifier
%%%      Immediates can be of 9 types:
%%%       1. Integers
%%%          42
%%%          -2374683271468723648732648736498712634876147
%%%       2. Hexadecimal integers starting with 0x
%%%          0x0deadbeef0
%%%       3. addresses, a 256-bit hash strings starting with #
%%%          followed by up to 64 hex chars
%%%          #00000deadbeef
%%%       4. Boolean
%%%          true
%%%          false
%%%       5. Strings
%%%          "Hello"
%%%       6. Empty map
%%%          {}
%%%       7. Lists
%%%          []
%%%          [1, 2]
%%%       8. Bit field
%%%          <000>
%%%          <1010>
%%%          <>
%%%          !<>
%%%       9. Tuples
%%%          ()
%%%          (1, "foo")
%%% @end
%%% Created : 21 Dec 2017
%%%-------------------------------------------------------------------

-module(aefa_asm).

-export([ file/2
        , pp/1
        , to_hexstring/1
        ]).

-include_lib("aebytecode/include/aefa_opcodes.hrl").


pp(Asm) ->
    Listing = format(Asm),
    io:format("~s~n", [Listing]).

format(Asm) -> format(Asm, 0).

format([{comment, Comment} | Rest], Address) ->
    ";; " ++ Comment ++ "\n" ++ format(Rest, Address);
format([Mnemonic | Rest], Address) ->
    _Op = aefa_opcode:m_to_op(Mnemonic),
    "        " ++ atom_to_list(Mnemonic) ++ "\n"
        ++ format(Rest, Address + 1);
format([],_) -> [].




file(Filename, Options) ->
    {ok, File} = file:read_file(Filename),
    {ok, Tokens, _} = aefa_asm_scan:scan(binary_to_list(File)),

    case proplists:lookup(pp_tokens, Options) of
        {pp_tokens, true} ->
            io:format("Tokens ~p~n",[Tokens]);
        none ->
            ok
    end,

    Env = to_bytecode(Tokens, none, #{}, [], Options),

    ByteList = serialize(Env),

    case proplists:lookup(pp_hex_string, Options) of
        {pp_hex_string, true} ->
            io:format("Code: ~s~n",[to_hexstring(ByteList)]);
        none ->
            ok
    end,

    {Env, list_to_binary(ByteList)}.

serialize(Env) ->
    %% TODO: add serialization of immediates
    %% TODO: add serialization of function definitions
    Code = [C || {_Name, {_Sig, C}} <- maps:to_list(Env)],
    Code.

to_hexstring(ByteList) ->
    "0x" ++ lists:flatten(
              [io_lib:format("~2.16.0b", [X])
               || X <- ByteList]).

to_bytecode([{function,_line, 'FUNCTION'}|Rest], Address, Env, Code, Opts) ->
    Env2 = insert_fun(Address, Code, Env),
    {Fun, Rest2} = to_fun_def(Rest),
    to_bytecode(Rest2, Fun, Env2, [], Opts);
to_bytecode([{mnemonic,_line, Op}|Rest], Address, Env, Code, Opts) ->
    OpCode = aefa_opcode:m_to_op(Op),
    %% TODO: arguments
    to_bytecode(Rest, Address, Env, [OpCode|Code], Opts);
to_bytecode([{int,_line, Int}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [Int|Code], Opts);
to_bytecode([{hash,_line, Hash}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [Hash|Code], Opts);
to_bytecode([{id,_line, ID}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{ref, ID}|Code], Opts);
to_bytecode([{label,_line, Label}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env#{Label => Address}, Code, Opts);
to_bytecode([], Address, Env, Code, Opts) ->
    Env2 = insert_fun(Address, Code, Env),
    case proplists:lookup(pp_opcodes, Opts) of
        {pp_opcodes, true} ->
            Ops = [C || {_Name, {_Sig, C}} <- maps:to_list(Env2)],
            io:format("opcodes ~p~n", [Ops]);
        none ->
            ok
    end,
    Env2.


to_fun_def([{id, _, Name}, {'(', _} | Rest]) ->
    {ArgsType, [{'->', _} | Rest2]} = to_arg_types(Rest),
    {RetType, Rest3} = to_type(Rest2),
    {{Name, ArgsType, RetType}, Rest3}.

to_arg_types([{')', _} | Rest]) -> {[], Rest};
to_arg_types(Tokens) ->
    case to_type(Tokens) of
        {Type, [{',', _} |  Rest]} ->
            {MoreTypes, Rest2} = to_arg_types(Rest),
            {[Type|MoreTypes], Rest2};
        {Type, [{')', _} |  Rest]} ->
            {[Type], Rest}
    end.

to_type([{id, _, "integer"} | Rest]) -> {integer, Rest};
to_type([{id, _, "boolean"} | Rest]) -> {boolean, Rest};
to_type([{id, _, "string"}  | Rest]) -> {string, Rest};
to_type([{id, _, "address"} | Rest]) -> {address, Rest};
to_type([{id, _, "bits"}    | Rest]) -> {bits, Rest};
to_type([{'{', _}, {id, _, "list"}, {',', _} | Rest]) ->
    %% TODO: Error handling
    {ListType, [{'}', _}| Rest2]} = to_type(Rest),
    {{list, ListType}, Rest2};
to_type([{'{', _}, {id, _, "tuple"}, {',', _}, {'[', _} | Rest]) ->
    %% TODO: Error handling
    {ElementTypes, [{'}', _}| Rest2]} = to_list_of_types(Rest),
    {{tuple, ElementTypes}, Rest2};
to_type([{'{', _}, {id, _, "map"}, {',', _} | Rest]) ->
    %% TODO: Error handling
    {KeyType, [{',', _}| Rest2]} = to_type(Rest),
    {ValueType, [{'}', _}| Rest3]} = to_type(Rest2),
    {{map, KeyType, ValueType}, Rest3}.

to_list_of_types([{']', _} | Rest]) -> {[], Rest};
to_list_of_types(Tokens) ->
    case to_type(Tokens) of
        {Type, [{',', _} |  Rest]} ->
            {MoreTypes, Rest2} = to_list_of_types(Rest),
            {[Type|MoreTypes], Rest2};
        {Type, [{']', _} |  Rest]} ->
            {[Type], Rest}
    end.



%% Also reverses the code (back to unreversed state).
resolve_refs([{ref, ID} | Rest], Env, Code) ->
    Address = maps:get(ID, Env),
    resolve_refs(Rest, Env, [Address | Code]);
resolve_refs([Op | Rest], Env, Code) ->
    resolve_refs(Rest, Env, [Op | Code]);
resolve_refs([],_Env, Code) -> Code.

expand_args([OP | Rest]) ->
    [OP | expand_args(Rest)];
expand_args([]) -> [].

insert_fun(none, [], Env) -> Env;
insert_fun({Name, Type, RetType}, Code, Env) ->
    Env#{Name => {{Type, RetType}, lists:reverse(Code)}}.
