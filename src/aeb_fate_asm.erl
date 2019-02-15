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
%%%      References to function arguments start with arg
%%%          arg0
%%%      References to variables/registers start with var
%%%          var0
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

-module(aeb_fate_asm).

-export([ assemble_file/3
        , asm_to_bytecode/2
        , bytecode_to_fate_code/2
        , function_call/1
        , pp/1
        , read_file/1
        , to_hexstring/1
        ]).

-include_lib("aebytecode/include/aeb_fate_opcodes.hrl").
-define(HASH_BYTES, 32).

assemble_file(InFile, OutFile, Options) ->
    Asm = read_file(InFile),
    {_Env, BC} = asm_to_bytecode(Asm, Options),
    ok = file:write_file(OutFile, BC).

function_call(String) ->
    {ok, Tokens, _} = aeb_fate_asm_scan:scan(String),
    parse_function_call(Tokens).

parse_function_call([{id,_,Name}, {'(',_}| Rest]) ->
    {Args, []} = to_args(Rest),
    aeb_fate_encoding:serialize(
      {tuple, {mk_hash(Name), {tuple, list_to_tuple(Args)}}}).


to_args([{')', _}]) -> {[], []};
to_args(Tokens) ->
    case to_data(Tokens) of
        {Arg, [{',', _} |  Rest]} ->
            {More, Rest2} = to_args(Rest),
            {[Arg|More], Rest2};
        {Arg, [{')', _} |  Rest]} ->
            {[Arg], Rest}
    end.
    
to_data([{int,_line, Int}|Rest]) ->
    {Int, Rest};
to_data([{boolean,_line, Bool}|Rest]) ->
    {Bool, Rest};
to_data([{hash,_line, Hash}|Rest]) ->
    {Hash, Rest}.


pp(Asm) ->
    Listing = format(Asm),
    io:format("~s~n", [Listing]).

format(Asm) -> format(Asm, 0).

format([{comment, Comment} | Rest], Address) ->
    ";; " ++ Comment ++ "\n" ++ format(Rest, Address);
format([Mnemonic | Rest], Address) ->
    _Op = aeb_fate_opcodes:m_to_op(Mnemonic),
    "        " ++ atom_to_list(Mnemonic) ++ "\n"
        ++ format(Rest, Address + 1);
format([],_) -> [].


read_file(Filename) ->
    {ok, File} = file:read_file(Filename),
    binary_to_list(File).

asm_to_bytecode(AssemblerCode, Options) ->
    {ok, Tokens, _} = aeb_fate_asm_scan:scan(AssemblerCode),

    case proplists:lookup(pp_tokens, Options) of
        {pp_tokens, true} ->
            io:format("Tokens ~p~n",[Tokens]);
        none ->
            ok
    end,

    Env = to_bytecode(Tokens, none, #{ functions => #{}
                                     , symbols => #{}
                                     }, [], Options),

    ByteList = serialize(Env),

    case proplists:lookup(pp_hex_string, Options) of
        {pp_hex_string, true} ->
            io:format("Code: ~s~n",[to_hexstring(ByteList)]);
        none ->
            ok
    end,

    {Env, list_to_binary(ByteList)}.

bytecode_to_fate_code(ByteCode,_Options) ->
    deserialize(ByteCode, #{ function => none
                           , bb => 0
                           , current_bb_code => []
                           , functions => #{}
                           , code => #{}
                           }).

deserialize(<<?FUNCTION:8, A, B, C, D, Rest/binary>>,
            #{ function := none
             , bb := 0
             , current_bb_code := []
             } = Env) ->
    {Sig, Rest2} = deserialize_signature(Rest),
    Env2 = Env#{function => {<<A,B,C,D>>, Sig}},
    deserialize(Rest2, Env2);
deserialize(<<?FUNCTION:8, A, B, C, D, Rest/binary>>,
            #{ function := {F, Sig}
             , bb := BB
             , current_bb_code := Code
             , code := Program
             , functions := Funs} = Env) ->
    {NewSig, Rest2} = deserialize_signature(Rest),
    case Code of
        [] ->
            Env2 = Env#{ bb => 0
                       , current_bb_code => []
                       , function => {<<A,B,C,D>>, NewSig}
                       , code => #{}
                       , functions => Funs#{F => {Sig, Program}}},
            deserialize(Rest2, Env2);
        _ ->
            Env2 = Env#{ bb => 0
                       , current_bb_code => []
                       , function => {<<A,B,C,D>>, NewSig}
                       , code => #{}
                       , functions =>
                             Funs#{F => {Sig,
                                         Program#{ BB => lists:reverse(Code)}}}},
            deserialize(Rest2, Env2)
    end;
deserialize(<<Op:8, Rest/binary>>,
            #{ bb := BB
             , current_bb_code := Code
             , code := Program} = Env) ->
    {Rest2, OpCode} = deserialize_op(Op, Rest, Code),
    case aeb_fate_opcodes:end_bb(Op) of
        true ->
            deserialize(Rest2, Env#{ bb => BB+1
                                   , current_bb_code => []
                                   , code => Program#{BB =>
                                                          lists:reverse(OpCode)}});
        false ->
            deserialize(Rest2, Env#{ current_bb_code => OpCode})
    end;
deserialize(<<>>, #{ function := {F, Sig}
                   , bb := BB
                   , current_bb_code := Code
                   , code := Program
                   , functions := Funs} = Env) ->
    FunctionCode =
        case Code of
            [] -> Program;
            _ -> Program#{ BB => lists:reverse(Code)}
        end,
    Env#{ bb => 0
        , current_bb_code => []
        , function => none
        , code => #{}
        , functions => Funs#{F => {Sig, FunctionCode}}}.

deserialize_op(?ELEMENT, Rest, Code) ->
    {Type, Rest2} = deserialize_type(Rest),
    <<ArgType:8, Rest3/binary>> = Rest2,
    {Arg0, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
    {Arg1, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
    {Arg2, Rest6} = aeb_fate_encoding:deserialize_one(Rest5),
    Modifier0 = bits_to_modifier(ArgType band 2#11),
    Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
    Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
    {Rest6, [{ aeb_fate_opcodes:mnemonic(?ELEMENT)
             , Type
             , {Modifier0, Arg0}
             , {Modifier1, Arg1}
             , {Modifier2, Arg2}}
             | Code]};
deserialize_op(Op, Rest, Code) ->
    OpName = aeb_fate_opcodes:mnemonic(Op),
    case aeb_fate_opcodes:args(Op) of
        0 -> {Rest, [OpName | Code]};
        1 ->
            <<ArgType:8, Rest2/binary>> = Rest,
            {Arg, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            Modifier = bits_to_modifier(ArgType),
            {Rest3, [{OpName, {Modifier, Arg}} | Code]};
        2 ->
            <<ArgType:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            {Rest4, [{OpName, {Modifier0, Arg0},
                      {Modifier1, Arg1}} | Code]};
        3 ->
            <<ArgType:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            {Arg2, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
            {Rest5, [{ OpName
                     , {Modifier0, Arg0}
                     , {Modifier1, Arg1}
                     , {Modifier2, Arg2}}
                     | Code]};
        4 ->
            <<ArgType:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            {Arg2, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
            {Arg3, Rest6} = aeb_fate_encoding:deserialize_one(Rest5),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
            Modifier3 = bits_to_modifier((ArgType bsr 6) band 2#11),
            {Rest6, [{ OpName
                     , {Modifier0, Arg0}
                     , {Modifier1, Arg1}
                     , {Modifier2, Arg2}
                     , {Modifier3, Arg3}}
                     | Code]}
    end.


serialize(#{functions := Functions} =_Env) ->
    Code = [[?FUNCTION, Name, serialize_signature(Sig), C]  ||
               {Name, {Sig, C}} <- maps:to_list(Functions)],
    serialize_code(lists:flatten(Code)).


%% Argument encoding
%% Agument Specification Byte
%% bitpos:  6    4    2    0
%%         xx   xx   xx   xx
%%       Arg3 Arg2 Arg1 Arg0
%% Bit pattern
%% 00 : stack/unused (depending on instruction)
%% 01 : argN
%% 10 : varN
%% 11 : immediate

serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
               , {Arg2Type, Arg2}
               , {Arg3Type, Arg3}| Rest]) ->
    ArgSpec =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2) bor
        (modifier_bits(Arg2Type) bsl 4) bor
        (modifier_bits(Arg3Type) bsl 6),
    [ ArgSpec
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
    , serialize_data(Arg2Type, Arg2)
    , serialize_data(Arg3Type, Arg3)
      | serialize_code(Rest)];
serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
               , {Arg2Type, Arg2}
                 | Rest]) ->
    ArgSpec =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2) bor
        (modifier_bits(Arg2Type) bsl 4),
    [ArgSpec
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
    , serialize_data(Arg2Type, Arg2)
     | serialize_code(Rest)];
serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
                 | Rest]) ->
    ArgSpec =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2),
    [ArgSpec
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
     | serialize_code(Rest)];
serialize_code([ {Arg0Type, Arg0} | Rest]) ->
    ArgSpec =
        modifier_bits(Arg0Type),
    [ArgSpec
    , serialize_data(Arg0Type, Arg0)
     | serialize_code(Rest)];
serialize_code([ ?ELEMENT
               , ResType
               | Rest]) ->
    [?ELEMENT, 
     serialize_type(ResType)
     | serialize_code(Rest)];
serialize_code([B|Rest]) ->
    [B | serialize_code(Rest)];
serialize_code([]) -> [].

%% 00 : stack/unused (depending on instruction)
%% 01 : argN
%% 10 : varN
%% 11 : immediate
modifier_bits(immediate) -> 2#11;
modifier_bits(var)       -> 2#10;
modifier_bits(arg)       -> 2#01;
modifier_bits(stack)     -> 2#00.

bits_to_modifier(2#11) -> immediate;
bits_to_modifier(2#10) -> var;
bits_to_modifier(2#01) -> arg;
bits_to_modifier(2#00) -> stack.

serialize_data(_, Data) ->
    aeb_fate_encoding:serialize(Data).

serialize_signature({Args, RetType}) ->
    [serialize_type({tuple, Args}) |
     serialize_type(RetType)].

serialize_type(integer) -> [0];
serialize_type(boolean) -> [1];
serialize_type({list, T}) -> [2 | serialize_type(T)];
serialize_type({tuple, Ts}) ->
    case length(Ts) of
        N when N =< 255 ->
            [3, N | [serialize_type(T) || T <- Ts]]
    end;
serialize_type(address) -> 4;
serialize_type(bits) -> 5;
serialize_type({map, K, V}) -> [6 | serialize_type(K) ++ serialize_type(V)].


deserialize_signature(Binary) ->
    {{tuple, Args}, Rest}  = deserialize_type(Binary),
    {RetType, Rest2} = deserialize_type(Rest),
    {{Args, RetType}, Rest2}.

deserialize_type(<<0, Rest/binary>>) -> {integer, Rest};
deserialize_type(<<1, Rest/binary>>) -> {boolean, Rest};
deserialize_type(<<2, Rest/binary>>) ->
    {T, Rest2} = deserialize_type(Rest),
    {{list, T}, Rest2};
deserialize_type(<<3, N, Rest/binary>>) ->
    {Ts, Rest2} = deserialize_types(N, Rest, []),
    {{tuple, Ts}, Rest2};
deserialize_type(<<4, Rest/binary>>) -> {address, Rest};
deserialize_type(<<5, Rest/binary>>) -> {bits, Rest};
deserialize_type(<<6, Rest/binary>>) ->
    {K, Rest2} = deserialize_type(Rest),
    {V, Rest3} = deserialize_type(Rest2),
    {{map, K, V}, Rest3}.

deserialize_types(0, Binary, Acc) ->
    {lists:reverse(Acc), Binary};
deserialize_types(N, Binary, Acc) ->
    {T, Rest} = deserialize_type(Binary),
    deserialize_types(N-1, Rest, [T | Acc]).


to_hexstring(ByteList) ->
    "0x" ++ lists:flatten(
              [io_lib:format("~2.16.0b", [X])
               || X <- ByteList]).

to_bytecode([{function,_line, 'FUNCTION'}|Rest], Address, Env, Code, Opts) ->
    Env2 = insert_fun(Address, Code, Env),
    {Fun, Rest2} = to_fun_def(Rest),
    to_bytecode(Rest2, Fun, Env2, [], Opts);
to_bytecode([{mnemonic,_line, 'ELEMENT'}|Rest], Address, Env, Code, Opts) ->
    OpCode = aeb_fate_opcodes:m_to_op('ELEMENT'),
    {RetType, Rest2} = to_type(Rest),
    to_bytecode(Rest2, Address, Env, [RetType, OpCode|Code], Opts);
to_bytecode([{mnemonic,_line, Op}|Rest], Address, Env, Code, Opts) ->
    OpCode = aeb_fate_opcodes:m_to_op(Op),
    to_bytecode(Rest, Address, Env, [OpCode|Code], Opts);
to_bytecode([{arg,_line, N}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{arg, N}|Code], Opts);
to_bytecode([{var,_line, N}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{var, N}|Code], Opts);
to_bytecode([{stack,_line, N}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{stack, N}|Code], Opts);
to_bytecode([{int,_line, Int}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{immediate, Int}|Code], Opts);
to_bytecode([{boolean,_line, Bool}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{immediate, Bool}|Code], Opts);
to_bytecode([{hash,_line, Hash}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env, [{immediate, Hash}|Code], Opts);
to_bytecode([{id,_line, ID}|Rest], Address, Env, Code, Opts) ->
    {Hash, Env2} = insert_symbol(ID, Env),
    to_bytecode(Rest, Address, Env2, [{immediate, Hash}|Code], Opts);
to_bytecode([], Address, Env, Code, Opts) ->
    Env2 = insert_fun(Address, Code, Env),
     #{functions := Funs} = Env2,
    case proplists:lookup(pp_opcodes, Opts) of
        {pp_opcodes, true} ->
            Ops = [C || {_Name, {_Sig, C}} <- maps:to_list(Funs)],
            io:format("opcodes ~p~n", [Ops]);
        none ->
            ok
    end,
    Env2.


to_fun_def([{id, _, Name}, {'(', _} | Rest]) ->
    {ArgsType, [{'to', _} | Rest2]} = to_arg_types(Rest),
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



insert_fun(none, [], Env) -> Env;
insert_fun({Name, Type, RetType}, Code, #{functions := Functions} = Env) ->
    {Hash, Env2} = insert_symbol(Name, Env),
    Env2#{
      functions => Functions#{Hash => {{Type, RetType}, lists:reverse(Code)}}
     }.

mk_hash(Id) ->
    %% Use first 4 bytes of blake hash
    {ok, <<A:8, B:8, C:8, D:8,_/binary>> } = aeb_blake2:blake2b(?HASH_BYTES, list_to_binary(Id)),
    <<A,B,C,D>>.

insert_symbol(Id, Env) ->
    Hash = mk_hash(Id),
    insert_symbol(Id, Hash, Env).

insert_symbol(Id, Hash, #{symbols := Symbols} = Env) ->
    case maps:find(Hash, Symbols) of
        {ok, Id} -> {Hash, Env};
        {ok, Id2} ->
            %% Very unlikely...
            exit({two_symbols_with_same_hash, Id, Id2});
        error ->
            {Hash, Env#{symbols => Symbols#{ Id => Hash
                                           , Hash => Id}}}
    end.

