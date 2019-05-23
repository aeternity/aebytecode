%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Assembler for Fate machine code.
%%% @end
%%%
%%%  Fate code exists in 3 formats:
%%%
%%%   1. Fate byte code. This format is under consensus.
%%%   2. Fate assembler. This is a text represenation of fate code.
%%%                      This is not under consensus and other
%%%                      implemenation and toolchains could have
%%%                      their own format.
%%%   3. Internal. This is an Erlang representation of fate code
%%%                Used by this particular engin implementation.
%%%
%%%  This library handles all tree representations.
%%%  The byte code format is described in a separate document.
%%%  The internal format is described in a separate document.
%%%  The text representation is described here:
%%%
%%%      Assembler code can be read from a file.
%%%      The assembler has the following format
%%%      Comments start with 2 semicolons and runs till end of line
%%%         ;; This is a comment
%%%      Opcode mnemonics start with an upper case letter.
%%%         DUP
%%%      Identifiers start with a lower case letter
%%%         an_identifier
%%%      References to function arguments start with arg followed by an integer
%%%          arg0
%%%      References to variables/registers start with var followed by an integer
%%%          var0
%%%      References to stack postions is either a (for stack 0)
%%%       or start with stack followed by an integer
%%%          stack1
%%%          a
%%%
%%%      Immediate values can be of 11 types:
%%%      1a. Integers as decimals: {Digits} or -{Digits}
%%%          42
%%%          -2374683271468723648732648736498712634876147
%%%      1b. Integers as Hexadecimals::  0x{Hexdigits}
%%%          0x0deadbeef0
%%%      2a. account addresses, a base58c encoded string prefixed with @ak_
%%%      2b. contract address: @ct_{base58char}+
%%%      2c. oracle address:   @ok_{base58char}+
%%%      2d. name address:     @nm_{base58char}+
%%%      2e. channel address:  @ch_{base58char}+
%%%       3. Boolean  true or false
%%%          true
%%%          false
%%%       4. Strings  "{Characters}"
%%%          "Hello"
%%%       5. Map  { Key => Value }
%%%          {}
%%%          { 1 => { "foo" => true, "bar" => false}
%%%       6. Lists [ Elements ]
%%%          []
%%%          [1, 2]
%%%       7. Bit field < Bits > or !< Bits >
%%%          <000>
%%%          <1010 1010>
%%%          <>
%%%          !<>
%%%       8. Tuples ( Elements )
%%%          ()
%%%          (1, "foo")
%%%       9. Variants: (| [Arities] | Tag | ( Elements ) |)
%%%          (| [0,1,2] | 2 | ( "foo", 12) |)
%%%      10. Hashes: #{base64char}+
%%%          #AQIDCioLFQ==
%%%      11. Signatures: $sg_{base58char}+
%%%
%%%       Where Digits: [0123456789]
%%%             Hexdigits:  [0123456789abcdef]
%%%             base58char:  [123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]
%%%             base64char:  [ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy0123456789+/=]
%%%             Characters any printable ascii character 0..255 (except " no quoting yet)
%%%             Key: any value except for a map
%%%             Bits: 01 or space
%%%             Elements: Nothing or Value , Elements
%%%             Size: Digits
%%%             Tag: Digits
%%%
%%% Created : 21 Dec 2017
%%%-------------------------------------------------------------------

-module(aeb_fate_asm).

-export([ assemble_file/3
        , asm_to_bytecode/2
        , function_call/1
        , pp/1
        , read_file/1
        , strip/1
        , to_asm/1
        ]).

-include_lib("aebytecode/include/aeb_fate_opcodes.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").
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
    case parse_value(Tokens) of
        {Arg, [{',', _} |  Rest]} ->
            {More, Rest2} = to_args(Rest),
            {[Arg|More], Rest2};
        {Arg, [{')', _} |  Rest]} ->
            {[Arg], Rest}
    end.

pp(FateCode) ->
    Listing = to_asm(FateCode),
    io_lib:format("~ts~n",[Listing]).


to_asm(FateCode) ->
    Functions   = aeb_fate_code:functions(FateCode),
    Symbols     = aeb_fate_code:symbols(FateCode),
    Annotations = aeb_fate_code:annotations(FateCode),
    insert_comments(get_comments(Annotations), 1,
                    lists:flatten(
                      io_lib:format("~s",
                                    [format_functions(Functions, Symbols)]))).

insert_comments([{L,C}|Comments], L, String) ->
    ";; " ++ C ++ "\n" ++ insert_comments(Comments, L + 1, String);
insert_comments(Comments, L, [$\n|String]) ->
    "\n" ++ insert_comments(Comments, L+1, String);
insert_comments(Comments, L, [C|Rest]) ->
    [C|insert_comments(Comments, L, Rest)];
insert_comments([],_,[]) -> [];
insert_comments([{L,C}|Rest], _, []) ->
    ";; " ++ C ++ "\n" ++ insert_comments(Rest, L + 1, []).

format_functions(Functions, Symbols) ->
    [format(lookup(Name, Symbols),
            Sig,
            lists:sort(maps:to_list(CodeMap)),
            Symbols)
     ||
        {Name, {Sig, CodeMap}} <- maps:to_list(Functions)].


format(Name, Sig, BBs, Symbols) ->
    [ "FUNCTION "
    , Name
    , format_sig(Sig)
    , "\n"
    , format_bbs(BBs, Symbols)].

format_sig({Args, RetType}) ->
    [ "( "
    , format_arg_types(Args)
    , ") : "
    , format_type(RetType)].

format_arg_types([]) -> "";
format_arg_types([T]) -> format_type(T);
format_arg_types([T|Ts]) ->
    [format_type(T)
    , ", "
    , format_arg_types(Ts)].

format_type(T) ->
    %% TODO: Limit to ok types.
    io_lib:format("~p", [T]).

format_bbs([], _) ->
    [];
format_bbs([{BB, Code}|Rest], Symbols) ->
    [ io_lib:format("  ;; BB : ~p~n", [BB])
    , format_code(Code, Symbols)
      | format_bbs(Rest, Symbols)].

format_code([], _) ->
    "";
format_code([Op|Rest], Symbols) ->
    ["          ",
     aeb_fate_pp:format_op(Op, Symbols),
     "\n",
     format_code(Rest, Symbols)].


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
    Env = #{ fate_code => aeb_fate_code:new()
           , functions => #{}
           },

    Env1 = to_bytecode(Tokens, none, Env, [], Options),
    FateCode  = maps:get(fate_code, Env1),
    FunctionsMap = maps:get(functions, Env1),
    Functions = [X || {_, X} <- lists:sort(maps:to_list(FunctionsMap))],
    FunctionsBin = iolist_to_binary(Functions),
    ByteCode = aeb_fate_code:serialize(FateCode, FunctionsBin, Options),
    {Env, ByteCode}.

strip(ByteCode) ->
    {Code, _Rest} = aeser_rlp:decode_one(ByteCode),
    Code.

%% -------------------------------------------------------------------
%% Parser
%% Asm tokens -> Fate code env
%% -------------------------------------------------------------------

to_bytecode([{function,_line, 'FUNCTION'}|Rest], Address, Env, Code, Opts) ->
    Env2 = insert_fun(Address, Code, Env),
    {Fun, Rest2} = to_fun_def(Rest),
    to_bytecode(Rest2, Fun, Env2, [], Opts);
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
to_bytecode([{string,_line, String}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_string(String)}|Code],
                Opts);
to_bytecode([{object,_line, {address, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_address(Value)}|Code],
                Opts);
to_bytecode([{object,_line, {contract, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_contract(Value)}|Code],
                Opts);
to_bytecode([{object,_line, {oracle, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_oracle(Value)}|Code],
                Opts);
to_bytecode([{object,_line, {name, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_name(Value)}|Code],
                Opts);
to_bytecode([{object,_line, {channel, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_contract(Value)}|Code],
                Opts);
to_bytecode([{hash,_line, Value}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_hash(Value)}|Code],
                Opts);
to_bytecode([{signature,_line, {signature, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_signature(Value)}|Code],
                Opts);
to_bytecode([{id,_line, ID}|Rest], Address, Env, Code, Opts) ->
    {Env2, Id} = insert_symbol(list_to_binary(ID), Env),
    to_bytecode(Rest, Address, Env2, [{immediate, Id}|Code], Opts);
to_bytecode([{'{',_line}|Rest], Address, Env, Code, Opts) ->
    {Map, Rest2} = parse_map(Rest),
    to_bytecode(Rest2, Address, Env, [{immediate, Map}|Code], Opts);
to_bytecode([{'[',_line}|Rest], Address, Env, Code, Opts) ->
    {List, Rest2} = parse_list(Rest),
    to_bytecode(Rest2, Address, Env, [{immediate, List}|Code], Opts);
to_bytecode([{'(',_line}|Rest], Address, Env, Code, Opts) ->
    {Elements, Rest2} = parse_tuple(Rest),
    Tuple = aeb_fate_data:make_tuple(list_to_tuple(Elements)),
    to_bytecode(Rest2, Address, Env, [{immediate, Tuple}|Code], Opts);
to_bytecode([{start_variant,_line}|_] = Tokens, Address, Env, Code, Opts) ->
    {Arities, Tag, Values, Rest} = parse_variant(Tokens),
    Variant = aeb_fate_data:make_variant(Arities, Tag, Values),
    to_bytecode(Rest, Address, Env, [{immediate, Variant}|Code], Opts);
to_bytecode([{bits,_line, Bits}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_bits(Bits)}|Code], Opts);

to_bytecode([{comment, Line, Comment}|Rest], Address, Env, Code, Opts) ->
    Env2 = insert_annotation(comment, Line, Comment, Env),
    to_bytecode(Rest, Address, Env2, Code, Opts);

to_bytecode([], Address, Env, Code,_Opts) ->
    insert_fun(Address, Code, Env).

parse_map([{'}',_line}|Rest]) ->
    {#{}, Rest};
parse_map(Tokens) ->
    {Key, [{arrow, _} | Rest]} = parse_value(Tokens),
    {Value, Rest2} = parse_value(Rest),
    case Rest2 of
        [{',',_} | Rest3] ->
            {Map, Rest4} = parse_map(Rest3),
            {Map#{Key => Value}, Rest4};
        [{'}',_} | Rest3] ->
            {#{Key => Value}, Rest3}
    end.

parse_list([{']',_line}|Rest]) ->
    {[], Rest};
parse_list(Tokens) ->
    {Head , Rest} = parse_value(Tokens),
    case Rest of
        [{',',_} | Rest2] ->
            {Tail, Rest3} = parse_list(Rest2),
            {[Head | Tail], Rest3};
        [{']',_} | Rest3] ->
            {[Head], Rest3}
    end.

parse_tuple([{')',_line}|Rest]) ->
    {[], Rest};
parse_tuple(Tokens) ->
    {Head , Rest} = parse_value(Tokens),
    case Rest of
        [{',',_} | Rest2] ->
            {Tail, Rest3} = parse_tuple(Rest2),
            {[Head | Tail], Rest3};
        [{')',_} | Rest3] ->
            {[Head], Rest3}
    end.


parse_variant([{start_variant,_line}
              , {'[', _line}
              | Rest]) ->
    {Arities, Rest2} = parse_list(Rest),
    %% Make sure Arities is a list of bytes.
    Arities = [A || A <- Arities,
                    is_integer(A), A < 256],

    [{'|',_}
    , {int,_line, Tag}
    , {'|',_}
    , {'(',_} | Rest3] = Rest2,
    {Elements , [{end_variant, _} | Rest4]} = parse_tuple(Rest3),
    Size = length(Arities),
    if 0 =< Tag, Tag < Size ->
            Arity = lists:nth(Tag+1, Arities),
            if length(Elements) =:= Arity ->
                    {Arities, Tag, list_to_tuple(Elements), Rest4}
            end
    end.


parse_value([{int,_line, Int} | Rest]) -> {Int, Rest};
parse_value([{boolean,_line, Bool} | Rest]) -> {Bool, Rest};
parse_value([{'{',_line} | Rest]) -> parse_map(Rest);
parse_value([{'[',_line} | Rest]) -> parse_list(Rest);
parse_value([{'(',_line} | Rest]) ->
    {T, Rest2} = parse_tuple(Rest),
    {aeb_fate_data:make_tuple(list_to_tuple(T)), Rest2};
parse_value([{bits,_line, Bits} | Rest]) ->
    {aeb_fate_data:make_bits(Bits), Rest};
parse_value([{start_variant,_line}|_] = Tokens) ->
    {Arities, Tag, Values, Rest} = parse_variant(Tokens),
    Variant = aeb_fate_data:make_variant(Arities, Tag, Values),
    {Variant, Rest};
parse_value([{string,_line, String} | Rest]) ->
    {aeb_fate_data:make_string(String), Rest};
parse_value([{object,_line, {address, Address}} | Rest]) ->
    {aeb_fate_data:make_address(Address), Rest};
parse_value([{object,_line, {contract, Address}} | Rest]) ->
    {aeb_fate_data:make_contract(Address), Rest};
parse_value([{object,_line, {oracle, Address}} | Rest]) ->
    {aeb_fate_data:make_oracle(Address), Rest};
parse_value([{object,_line, {name, Address}} | Rest]) ->
    {aeb_fate_data:make_name(Address), Rest};
parse_value([{object,_line, {channel, Address}} | Rest]) ->
    {aeb_fate_data:make_channel(Address), Rest};
parse_value([{hash,_line, Hash} | Rest]) ->
    {aeb_fate_data:make_hash(Hash), Rest};
parse_value([{signature,_line, Hash} | Rest]) ->
    {aeb_fate_data:make_signature(Hash), Rest}.

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


%% Type handling

to_type([{id, _, "integer"}   | Rest]) -> {integer, Rest};
to_type([{id, _, "boolean"}   | Rest]) -> {boolean, Rest};
to_type([{id, _, "string"}    | Rest]) -> {string, Rest};
to_type([{id, _, "address"}   | Rest]) -> {address, Rest};
to_type([{id, _, "contract"}  | Rest]) -> {contract, Rest};
to_type([{id, _, "oracle"}    | Rest]) -> {oracle, Rest};
to_type([{id, _, "name"}      | Rest]) -> {name, Rest};
to_type([{id, _, "channel"}   | Rest]) -> {channel, Rest};
to_type([{id, _, "hash"}      | Rest]) -> {hash, Rest};
to_type([{id, _, "signature"} | Rest]) -> {signature, Rest};
to_type([{id, _, "bits"}      | Rest]) -> {bits, Rest};
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
    {{map, KeyType, ValueType}, Rest3};
to_type([{'{', _}
        , {id, _, "variant"}
        , {',', _}
        , {'[', _}
        | Rest]) ->
    {ElementTypes, [{'}', _}| Rest2]} = to_list_of_types(Rest),
    {{variant, ElementTypes}, Rest2}.


to_list_of_types([{']', _} | Rest]) -> {[], Rest};
to_list_of_types(Tokens) ->
    case to_type(Tokens) of
        {Type, [{',', _} |  Rest]} ->
            {MoreTypes, Rest2} = to_list_of_types(Rest),
            {[Type|MoreTypes], Rest2};
        {Type, [{']', _} |  Rest]} ->
            {[Type], Rest}
    end.


%% -------------------------------------------------------------------
%% Helper functions
%% -------------------------------------------------------------------

%% State handling

insert_fun(none, [], Env) -> Env;
insert_fun({NameString, ArgType, RetType}, Code, #{ fate_code := FateCode
                                                  , functions := Funs} = Env) ->
    Name = list_to_binary(NameString),
    {FateCode1, Id} = aeb_fate_code:insert_symbol(Name, FateCode),
    BodyByteCode = aeb_fate_code:serialize_code(lists:reverse(Code)),
    SigByteCode = aeb_fate_code:serialize_signature({ArgType, RetType}),
    FunByteCode = [?FUNCTION, Id, SigByteCode, BodyByteCode],
    Env#{ functions => Funs#{ Id => FunByteCode }
        , fate_code => FateCode1}.

insert_symbol(Name, #{ fate_code := FateCode } = Env) ->
    {FateCode1, Id} = aeb_fate_code:insert_symbol(Name, FateCode),
    { Env#{ fate_code => FateCode1 }
    , Id}.

insert_annotation(comment, Line, Comment, #{ fate_code := FateCode } = Env) ->
    FateCode1 = aeb_fate_code:insert_annotation(comment, Line, Comment, FateCode),
    Env#{ fate_code => FateCode1}.

mk_hash(Id) ->
    %% Use first 4 bytes of blake hash
    {ok, <<A:8, B:8, C:8, D:8,_/binary>> } = eblake2:blake2b(?HASH_BYTES, list_to_binary(Id)),
    <<A,B,C,D>>.

%% Handle annotations

get_comments(Annotations) ->
    [ {Line, Comment} ||
        {?FATE_TUPLE({?FATE_STRING_VALUE("comment"), Line}),
         ?FATE_STRING_VALUE(Comment)} <- maps:to_list(Annotations)].

%% Symbol table handling

lookup(Name, Symbols) ->
    maps:get(Name, Symbols, Name).
