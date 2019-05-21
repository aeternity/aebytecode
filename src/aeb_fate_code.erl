%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     ADT for fate byte code/fate code
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(aeb_fate_code).

-export([ annotations/1
        , deserialize/1
        , functions/1
        , insert_annotation/4
        , insert_fun/4
        , insert_symbol/2
        , new/0
        , serialize/1
        , serialize/2
        , serialize/3
        , serialize_code/1
        , serialize_signature/1
        , symbol_identifier/1
        , symbols/1
        ]).

-include("../include/aeb_fate_opcodes.hrl").
-include("../include/aeb_fate_data.hrl").


-record(fcode, { functions   = #{} :: map()
               , symbols     = #{} :: map()
               , annotations = #{} :: map()
               }).

-define(HASH_BYTES, 32).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    #fcode{}.

annotations(#fcode{ annotations = As }) ->
    As.

functions(#fcode{ functions = Fs }) ->
    Fs.

symbols(#fcode{ symbols = Ss}) ->
    Ss.

symbol_identifier(Bin) ->
    %% First 4 bytes of blake hash
    {ok, <<X:4/binary,_/binary>> } = eblake2:blake2b(?HASH_BYTES, Bin),
    X.

insert_fun(Name, {ArgType, RetType}, #{} = BBs, #fcode{ functions = Funs } = F) ->
    {F1, ID} = insert_symbol(Name, F),
    F1#fcode{ functions = Funs#{ ID => {{ArgType, RetType}, BBs}} }.

insert_symbol(Name, #fcode{ symbols = Syms } = F) ->
    ID = symbol_identifier(Name),
    case maps:find(ID, Syms) of
        {ok, Name} ->
            {F, ID};
        {ok, X} ->
            error({two_symbols_with_same_hash, Name, X});
        error ->
            {F#fcode{symbols = Syms#{ ID => Name}}, ID}
    end.

insert_annotation(comment =_Type, Line, Comment, #fcode{ annotations = Anns} = F) ->
    Key   = aeb_fate_data:make_tuple({aeb_fate_data:make_string("comment"), Line}),
    Value = aeb_fate_data:make_string(Comment),
    F#fcode{ annotations = Anns#{ Key => Value}}.

%%%===================================================================
%%% Serialization
%%%===================================================================

serialize(#fcode{} = F) ->
    serialize(F, []).

serialize(#fcode{} = F, Options) ->
    serialize(F, iolist_to_binary(serialize_functions(F)), Options).

serialize(#fcode{} = F, Functions, Options) ->
    SymbolTable = serialize_symbol_table(F),
    Annotatations = serialize_annotations(F),
    ByteCode = <<  (aeser_rlp:encode(Functions))/binary,
                   (aeser_rlp:encode(SymbolTable))/binary,
                   (aeser_rlp:encode(Annotatations))/binary
               >>,

    case proplists:lookup(pp_hex_string, Options) of
        {pp_hex_string, true} ->
            io:format("Code: ~s~n",[to_hexstring(Functions)]);
        none ->
            ok
    end,
    ByteCode.

to_hexstring(ByteList) ->
    "0x" ++ lists:flatten(
              [io_lib:format("~2.16.0b", [X])
               || X <- ByteList]).


serialize_functions(#fcode{ functions = Functions }) ->
    %% Sort the functions on name to get a canonical serialisation.
    Code = [[?FUNCTION, Name, serialize_signature(Sig), serialize_bbs(C)]  ||
               {Name, {Sig, C}} <- lists:sort(maps:to_list(Functions))],
    lists:flatten(Code).

serialize_signature({Args, RetType}) ->
    [aeb_fate_encoding:serialize_type({tuple, Args}) |
     aeb_fate_encoding:serialize_type(RetType)].

serialize_symbol_table(#fcode{ symbols = Symbols }) ->
    aeb_fate_encoding:serialize(aeb_fate_data:make_map(Symbols)).

serialize_annotations(#fcode{ annotations = Annotations}) ->
    aeb_fate_encoding:serialize(aeb_fate_data:make_map(Annotations)).

serialize_bbs(#{} = BBs) ->
    serialize_bbs(BBs, 0, []).

serialize_bbs(BBs, N, Acc) ->
    case maps:get(N, BBs, none) of
        none ->
            %% Assert that the BBs were contiguous
            Size = maps:size(BBs),
            case Size =:= N of
                true  ->
                    lists:reverse(Acc);
                false ->
                    error({not_contiguous_labels, lists:sort(maps:keys(BBs))})
            end;
        BB ->
            serialize_bbs(BBs, N + 1, [serialize_bb(BB, [])|Acc])
    end.

serialize_bb([Op|Rest], Acc) ->
    serialize_bb(Rest, [serialize_op(Op)|Acc]);
serialize_bb([], Acc) ->
    lists:reverse(Acc).

serialize_op(Op) when is_tuple(Op) ->
    [Opcode|Args] = tuple_to_list(Op),
    [aeb_fate_opcodes:m_to_op(Opcode)|serialize_code(Args)];
serialize_op(Opcode) ->
    [aeb_fate_opcodes:m_to_op(Opcode)].

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
serialize_code([B|Rest]) ->
    [B | serialize_code(Rest)];
serialize_code([]) -> [].

serialize_data(_, Data) ->
    aeb_fate_encoding:serialize(Data).

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

%%%===================================================================
%%% Deserialization
%%%===================================================================

deserialize(Bytes) ->
    {ByteCode, Rest1}    = aeser_rlp:decode_one(Bytes),
    {SymbolTable, Rest2} = aeser_rlp:decode_one(Rest1),
    {Annotations, <<>>}  = aeser_rlp:decode_one(Rest2),

    Env = #{ function => none
           , bb => 0
           , current_bb_code => []
           , functions => #{}
           , code => #{}
           },
    #fcode{ functions = deserialize_functions(ByteCode, Env)
          , annotations = deserialize_annotations(Annotations)
          , symbols = deserialize_symbols(SymbolTable)
          }.


deserialize_functions(<<?FUNCTION:8, A, B, C, D, Rest/binary>>,
            #{ function := none
             , bb := 0
             , current_bb_code := []
             } = Env) ->
    {Sig, Rest2} = deserialize_signature(Rest),
    Env2 = Env#{function => {<<A,B,C,D>>, Sig}},
    deserialize_functions(Rest2, Env2);
deserialize_functions(<<?FUNCTION:8, A, B, C, D, Rest/binary>>,
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
            deserialize_functions(Rest2, Env2);
        _ ->
            Env2 = Env#{ bb => 0
                       , current_bb_code => []
                       , function => {<<A,B,C,D>>, NewSig}
                       , code => #{}
                       , functions =>
                             Funs#{F => {Sig,
                                         Program#{ BB => lists:reverse(Code)}}}},
            deserialize_functions(Rest2, Env2)
    end;
deserialize_functions(<<Op:8, Rest/binary>>,
            #{ bb := BB
             , current_bb_code := Code
             , code := Program} = Env) ->
    {Rest2, OpCode} = deserialize_op(Op, Rest, Code),
    case aeb_fate_opcodes:end_bb(Op) of
        true ->
            deserialize_functions(Rest2, Env#{ bb => BB+1
                                             , current_bb_code => []
                                             , code => Program#{BB =>
                                                                    lists:reverse(OpCode)}});
        false ->
            deserialize_functions(Rest2, Env#{ current_bb_code => OpCode})
    end;
deserialize_functions(<<>>, #{ function := {F, Sig}
                             , bb := BB
                             , current_bb_code := Code
                             , code := Program
                             , functions := Funs}) ->
    FunctionCode =
        case Code of
            [] -> Program;
            _ -> Program#{ BB => lists:reverse(Code)}
        end,
    Funs#{F => {Sig, FunctionCode}}.

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



deserialize_signature(Binary) ->
    {{tuple, Args}, Rest}  = aeb_fate_encoding:deserialize_type(Binary),
    {RetType, Rest2} = aeb_fate_encoding:deserialize_type(Rest),
    {{Args, RetType}, Rest2}.

deserialize_symbols(Table) ->
    ?FATE_MAP_VALUE(SymbolTable) = aeb_fate_encoding:deserialize(Table),
    SymbolTable.

deserialize_annotations(AnnotationsBin) ->
    ?FATE_MAP_VALUE(Annotations) = aeb_fate_encoding:deserialize(AnnotationsBin),
    Annotations.
