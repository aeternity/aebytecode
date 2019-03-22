%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Assembler for Fate machine code.
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
%%%      2a. addresses, a base58 encoded string prefixed with @
%%%          @foo
%%%      2b. contract address: ct_{base58char}+
%%%      2c. oracle addresse: ok_{base58char}+
%%%      2d. name addresse: nm_{base58char}+
%%%      2e. channel addresse: ch_{base58char}+
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
%%%       9. Variants: (| Size | Tag | ( Elements ) |)
%%%          (| 42 | 12 | ( "foo", 12) |)
%%%      10. Hashes: #{base64char}+
%%%          #AQIDCioLFQ==
%%%      11. Signatures: ${base64char}+
%%%          $AQIDCioLFQ==
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
        , strip/1
        , to_asm/1
        , to_hexstring/1
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


to_asm(#{ functions := Functions
        , symbols := Symbols
        , annotations := Annotations} = _FateCode) ->
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

    Env = to_bytecode(Tokens, none, #{ functions => #{}
                                     , symbols => #{}
                                     , annotations => #{}
                                     }, [], Options),

    ByteList = serialize(Env),
    Signatures = serialize_sigs(Env),
    SymbolTable = serialize_symbol_table(Env),
    Annotatations = serialize_annotations(Env),
    ByteCode = <<  (aeser_rlp:encode(list_to_binary(ByteList)))/binary,
                   (aeser_rlp:encode(list_to_binary(Signatures)))/binary,
                   (aeser_rlp:encode(SymbolTable))/binary,
                   (aeser_rlp:encode(Annotatations))/binary
               >>,

    case proplists:lookup(pp_hex_string, Options) of
        {pp_hex_string, true} ->
            io:format("Code: ~s~n",[to_hexstring(ByteList)]);
        none ->
            ok
    end,

    {Env, ByteCode}.

strip(ByteCode) ->
    {Code, _Rest} = aeser_rlp:decode_one(ByteCode),
    Code.

bytecode_to_fate_code(Bytes, _Options) ->
    {ByteCode, Rest1} = aeser_rlp:decode_one(Bytes),
    {Signatures, Rest2} = aeser_rlp:decode_one(Rest1),
    {SymbolTable, Rest3} = aeser_rlp:decode_one(Rest2),
    {Annotations, <<>>} = aeser_rlp:decode_one(Rest3),

    Env1 = deserialize(ByteCode, #{ function => none
                                  , bb => 0
                                  , current_bb_code => []
                                  , functions => #{}
                                  , code => #{}
                                  }),
    Env2 = deserialize_signatures(Signatures, Env1),
    Env3 = deserialize_symbols(SymbolTable, Env2),
    Env4 = deserialize_annotations(Annotations, Env3),
    Env4.


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

deserialize_op(?SWITCH_VN, Rest, Code) ->
    <<ArgType:8, Rest2/binary>> = Rest,
    {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
    case aeb_fate_encoding:deserialize_one(Rest3) of
    {L, Rest4} when is_list(L) ->
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            immediate = bits_to_modifier((ArgType bsr 2) band 2#11),
            {Rest4, [{aeb_fate_opcodes:mnemonic(?SWITCH_VN)
                     , {Modifier0, Arg0}
                     , {immediate, L}
                     }
                     | Code]};
        _ -> exit(bad_argument_to_switch_vn)
    end;
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
                     | Code]};
        5 ->
            <<ArgType:8, ArgType2:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            {Arg2, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
            {Arg3, Rest6} = aeb_fate_encoding:deserialize_one(Rest5),
            {Arg4, Rest7} = aeb_fate_encoding:deserialize_one(Rest6),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
            Modifier3 = bits_to_modifier((ArgType bsr 6) band 2#11),
            Modifier4 = bits_to_modifier(ArgType2 band 2#11),
            {Rest7, [{ OpName
                     , {Modifier0, Arg0}
                     , {Modifier1, Arg1}
                     , {Modifier2, Arg2}
                     , {Modifier3, Arg3}
                     , {Modifier4, Arg4}
                     }
                     | Code]};
        6 ->
            <<ArgType:8, ArgType2:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            {Arg2, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
            {Arg3, Rest6} = aeb_fate_encoding:deserialize_one(Rest5),
            {Arg4, Rest7} = aeb_fate_encoding:deserialize_one(Rest6),
            {Arg5, Rest8} = aeb_fate_encoding:deserialize_one(Rest7),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
            Modifier3 = bits_to_modifier((ArgType bsr 6) band 2#11),
            Modifier4 = bits_to_modifier(ArgType2 band 2#11),
            Modifier5 = bits_to_modifier((ArgType2 bsr 2) band 2#11),
            {Rest8, [{ OpName
                     , {Modifier0, Arg0}
                     , {Modifier1, Arg1}
                     , {Modifier2, Arg2}
                     , {Modifier3, Arg3}
                     , {Modifier4, Arg4}
                     , {Modifier5, Arg5}
                     }
                     | Code]};
        7 ->
            <<ArgType:8, ArgType2:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            {Arg2, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
            {Arg3, Rest6} = aeb_fate_encoding:deserialize_one(Rest5),
            {Arg4, Rest7} = aeb_fate_encoding:deserialize_one(Rest6),
            {Arg5, Rest8} = aeb_fate_encoding:deserialize_one(Rest7),
            {Arg6, Rest9} = aeb_fate_encoding:deserialize_one(Rest8),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
            Modifier3 = bits_to_modifier((ArgType bsr 6) band 2#11),
            Modifier4 = bits_to_modifier(ArgType2 band 2#11),
            Modifier5 = bits_to_modifier((ArgType2 bsr 2) band 2#11),
            Modifier6 = bits_to_modifier((ArgType2 bsr 4) band 2#11),
            {Rest9, [{ OpName
                     , {Modifier0, Arg0}
                     , {Modifier1, Arg1}
                     , {Modifier2, Arg2}
                     , {Modifier3, Arg3}
                     , {Modifier4, Arg4}
                     , {Modifier5, Arg5}
                     , {Modifier6, Arg6}
                     }
                     | Code]};
        8 ->
            <<ArgType:8, ArgType2:8, Rest2/binary>> = Rest,
            {Arg0, Rest3} = aeb_fate_encoding:deserialize_one(Rest2),
            {Arg1, Rest4} = aeb_fate_encoding:deserialize_one(Rest3),
            {Arg2, Rest5} = aeb_fate_encoding:deserialize_one(Rest4),
            {Arg3, Rest6} = aeb_fate_encoding:deserialize_one(Rest5),
            {Arg4, Rest7} = aeb_fate_encoding:deserialize_one(Rest6),
            {Arg5, Rest8} = aeb_fate_encoding:deserialize_one(Rest7),
            {Arg6, Rest9} = aeb_fate_encoding:deserialize_one(Rest8),
            {Arg7, Rest10} = aeb_fate_encoding:deserialize_one(Rest9),
            Modifier0 = bits_to_modifier(ArgType band 2#11),
            Modifier1 = bits_to_modifier((ArgType bsr 2) band 2#11),
            Modifier2 = bits_to_modifier((ArgType bsr 4) band 2#11),
            Modifier3 = bits_to_modifier((ArgType bsr 6) band 2#11),
            Modifier4 = bits_to_modifier(ArgType2 band 2#11),
            Modifier5 = bits_to_modifier((ArgType2 bsr 2) band 2#11),
            Modifier6 = bits_to_modifier((ArgType2 bsr 4) band 2#11),
            Modifier7 = bits_to_modifier((ArgType2 bsr 6) band 2#11),
            {Rest10, [{ OpName
                      , {Modifier0, Arg0}
                      , {Modifier1, Arg1}
                      , {Modifier2, Arg2}
                      , {Modifier3, Arg3}
                      , {Modifier4, Arg4}
                      , {Modifier5, Arg5}
                      , {Modifier6, Arg6}
                      , {Modifier7, Arg7}
                      }
                      | Code]}
    end.




deserialize_signatures(_Signatures, Env) -> Env.

deserialize_symbols(Table, Env) ->
    ?FATE_MAP_VALUE(SymbolTable) = aeb_fate_encoding:deserialize(Table),
    Env#{symbols => SymbolTable}.

deserialize_annotations(AnnotationsBin, Env) ->
    ?FATE_MAP_VALUE(Annotations) = aeb_fate_encoding:deserialize(AnnotationsBin),
    Env#{annotations => Annotations}.



serialize_sigs(_Env) -> [].

serialize_symbol_table(#{ symbols := Symbols }) ->
    aeb_fate_encoding:serialize(aeb_fate_data:make_map(Symbols)).

serialize_annotations(#{ annotations := Annotations}) ->
    aeb_fate_encoding:serialize(aeb_fate_data:make_map(Annotations)).





serialize(#{functions := Functions} =_Env) ->
    %% Sort the functions oon name to get a canonical serialisation.
    Code = [[?FUNCTION, Name, serialize_signature(Sig), C]  ||
               {Name, {Sig, C}} <- lists:sort(maps:to_list(Functions))],
    serialize_code(lists:flatten(Code)).


%% Argument encoding
%% Agument Specification Byte
%% bitpos:  6    4    2    0
%%         xx   xx   xx   xx
%%       Arg3 Arg2 Arg1 Arg0
%% For 5-8 args another Argument Spec Byte is used
%% Bit pattern
%% 00 : stack/unused (depending on instruction)
%% 01 : argN
%% 10 : varN
%% 11 : immediate
serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
               , {Arg2Type, Arg2}
               , {Arg3Type, Arg3}
               , {Arg4Type, Arg4}
               , {Arg5Type, Arg5}
               , {Arg6Type, Arg6}
               , {Arg7Type, Arg7}
                 | Rest]) ->
    ArgSpec1 =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2) bor
        (modifier_bits(Arg2Type) bsl 4) bor
        (modifier_bits(Arg3Type) bsl 6),
    ArgSpec2 =
        modifier_bits(Arg4Type) bor
        (modifier_bits(Arg5Type) bsl 2) bor
        (modifier_bits(Arg6Type) bsl 4) bor
        (modifier_bits(Arg7Type) bsl 6),
    [ ArgSpec1
    , ArgSpec2
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
    , serialize_data(Arg2Type, Arg2)
    , serialize_data(Arg3Type, Arg3)
    , serialize_data(Arg4Type, Arg4)
    , serialize_data(Arg5Type, Arg5)
    , serialize_data(Arg6Type, Arg6)
    , serialize_data(Arg7Type, Arg7)
      | serialize_code(Rest)];
serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
               , {Arg2Type, Arg2}
               , {Arg3Type, Arg3}
               , {Arg4Type, Arg4}
               , {Arg5Type, Arg5}
               , {Arg6Type, Arg6}
                 | Rest]) ->
    ArgSpec1 =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2) bor
        (modifier_bits(Arg2Type) bsl 4) bor
        (modifier_bits(Arg3Type) bsl 6),
    ArgSpec2 =
        modifier_bits(Arg4Type) bor
        (modifier_bits(Arg5Type) bsl 2) bor
        (modifier_bits(Arg6Type) bsl 4),
    [ ArgSpec1
    , ArgSpec2
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
    , serialize_data(Arg2Type, Arg2)
    , serialize_data(Arg3Type, Arg3)
    , serialize_data(Arg4Type, Arg4)
    , serialize_data(Arg5Type, Arg5)
    , serialize_data(Arg6Type, Arg6)
      | serialize_code(Rest)];
serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
               , {Arg2Type, Arg2}
               , {Arg3Type, Arg3}
               , {Arg4Type, Arg4}
               , {Arg5Type, Arg5}
                 | Rest]) ->
    ArgSpec1 =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2) bor
        (modifier_bits(Arg2Type) bsl 4) bor
        (modifier_bits(Arg3Type) bsl 6),
    ArgSpec2 =
        modifier_bits(Arg4Type) bor
        (modifier_bits(Arg5Type) bsl 2),
    [ ArgSpec1
    , ArgSpec2
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
    , serialize_data(Arg2Type, Arg2)
    , serialize_data(Arg3Type, Arg3)
    , serialize_data(Arg4Type, Arg4)
    , serialize_data(Arg5Type, Arg5)
      | serialize_code(Rest)];
serialize_code([ {Arg0Type, Arg0}
               , {Arg1Type, Arg1}
               , {Arg2Type, Arg2}
               , {Arg3Type, Arg3}
               , {Arg4Type, Arg4}
                 | Rest]) ->
    ArgSpec1 =
        modifier_bits(Arg0Type) bor
        (modifier_bits(Arg1Type) bsl 2) bor
        (modifier_bits(Arg2Type) bsl 4) bor
        (modifier_bits(Arg3Type) bsl 6),
    ArgSpec2 =
        modifier_bits(Arg4Type),
    [ ArgSpec1
    , ArgSpec2
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(Arg1Type, Arg1)
    , serialize_data(Arg2Type, Arg2)
    , serialize_data(Arg3Type, Arg3)
    , serialize_data(Arg4Type, Arg4)
      | serialize_code(Rest)];

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
serialize_code([ ?SWITCH_VN
               , {Arg0Type, Arg0}
               , {immediate, L}
               | Rest]) ->
    ArgSpec =
        modifier_bits(Arg0Type) bor
        (modifier_bits(immediate) bsl 2),
    [?SWITCH_VN
    , ArgSpec
    , serialize_data(Arg0Type, Arg0)
    , serialize_data(immediate, L)] ++ serialize_code(Rest);
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
    [aeb_fate_encoding:serialize_type({tuple, Args}) |
     aeb_fate_encoding:serialize_type(RetType)].



deserialize_signature(Binary) ->
    {{tuple, Args}, Rest}  = aeb_fate_encoding:deserialize_type(Binary),
    {RetType, Rest2} = aeb_fate_encoding:deserialize_type(Rest),
    {{Args, RetType}, Rest2}.



to_hexstring(ByteList) ->
    "0x" ++ lists:flatten(
              [io_lib:format("~2.16.0b", [X])
               || X <- ByteList]).



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
to_bytecode([{address,_line, {address, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_address(Value)}|Code],
                Opts);
to_bytecode([{address,_line, {contract, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_contract(Value)}|Code],
                Opts);
to_bytecode([{address,_line, {oracle, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_oracle(Value)}|Code],
                Opts);
to_bytecode([{address,_line, {name, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_name(Value)}|Code],
                Opts);
to_bytecode([{address,_line, {channel, Value}}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_contract(Value)}|Code],
                Opts);
to_bytecode([{hash,_line, Value}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_hash(Value)}|Code],
                Opts);
to_bytecode([{signature,_line, Value}|Rest],
            Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_signature(Value)}|Code],
                Opts);
to_bytecode([{id,_line, ID}|Rest], Address, Env, Code, Opts) ->
    {Hash, Env2} = insert_symbol(ID, Env),
    to_bytecode(Rest, Address, Env2, [{immediate, Hash}|Code], Opts);
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
    {Size, Tag, Values, Rest} = parse_variant(Tokens),
    Variant = aeb_fate_data:make_variant(Size, Tag, Values),
    to_bytecode(Rest, Address, Env, [{immediate, Variant}|Code], Opts);
to_bytecode([{bits,_line, Bits}|Rest], Address, Env, Code, Opts) ->
    to_bytecode(Rest, Address, Env,
                [{immediate, aeb_fate_data:make_bits(Bits)}|Code], Opts);

to_bytecode([{comment, Line, Comment}|Rest], Address, Env, Code, Opts) ->
    Env2 = insert_annotation(comment, Line, Comment, Env),
    to_bytecode(Rest, Address, Env2, Code, Opts);

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
              , {int,_line, Size}
              , {'|',_}
              , {int,_line, Tag}
              , {'|',_}
              , {'(',_}
              | Rest]) when (Size > 0), (Tag < Size) ->
    {Elements , [{end_variant, _} | Rest2]} = parse_tuple(Rest),
    {Size, Tag, list_to_tuple(Elements), Rest2}.


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
    {Size, Tag, Values, Rest} = parse_variant(Tokens),
    Variant = aeb_fate_data:make_variant(Size, Tag, Values),
    {Variant, Rest};
parse_value([{string,_line, String} | Rest]) ->
    {aeb_fate_data:make_string(String), Rest};
parse_value([{address,_line, {address, Address}} | Rest]) ->
    {aeb_fate_data:make_address(Address), Rest};
parse_value([{address,_line, {contract, Address}} | Rest]) ->
    {aeb_fate_data:make_contract(Address), Rest};
parse_value([{address,_line, {oracle, Address}} | Rest]) ->
    {aeb_fate_data:make_oracle(Address), Rest};
parse_value([{address,_line, {name, Address}} | Rest]) ->
    {aeb_fate_data:make_name(Address), Rest};
parse_value([{address,_line, {channel, Address}} | Rest]) ->
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
    %% TODO: Error handling
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
insert_fun({Name, Type, RetType}, Code, #{functions := Functions} = Env) ->
    {Hash, Env2} = insert_symbol(Name, Env),
    Env2#{
      functions => Functions#{Hash => {{Type, RetType}, lists:reverse(Code)}}
     }.

mk_hash(Id) ->
    %% Use first 4 bytes of blake hash
    {ok, <<A:8, B:8, C:8, D:8,_/binary>> } = eblake2:blake2b(?HASH_BYTES, list_to_binary(Id)),
    <<A,B,C,D>>.

%% Handle annotations

insert_annotation(comment, Line, Comment, #{annotations := A} = Env) ->
    Key = aeb_fate_data:make_tuple({aeb_fate_data:make_string("comment"), Line}),
    Value = aeb_fate_data:make_string(Comment),
    Env#{annotations => A#{ Key => Value}}.

get_comments(Annotations) ->
    [ {Line, Comment} ||
        {?FATE_TUPLE({?FATE_STRING_VALUE("comment"), Line}),
         ?FATE_STRING_VALUE(Comment)} <- maps:to_list(Annotations)].

%% Symbols handling

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

%% Symbol table handling

lookup(Name, Symbols) ->
    maps:get(Name, Symbols, Name).
