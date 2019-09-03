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
        , insert_fun/5
        , insert_symbol/2
        , new/0
        , serialize/1
        , serialize/2
        , serialize/3
        , serialize_code/1
        , serialize_signature/1
        , strip_init_function/1
        , symbol_identifier/1
        , symbols/1
        ]).

-include("../include/aeb_fate_opcodes.hrl").
-include("../include/aeb_fate_data.hrl").

-ifdef(EQC).
-export([ update_annotations/2
        , update_functions/2
        , update_symbols/2]).
-endif.

-record(fcode, { functions   = #{} :: map()
               , symbols     = #{} :: map()
               , annotations = #{} :: map()
               }).

-define(HASH_BYTES, 32).

-type fcode() :: #fcode{}.
-export_type([fcode/0]).

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

update_annotations(#fcode{ annotations = As } = FCode, Anns) ->
    FCode#fcode{ annotations = maps:merge(As, Anns) }.

update_functions(#fcode{ functions = Fs } = FCode, Funs) ->
    FCode#fcode{ functions = maps:merge(Fs, Funs) }.

update_symbols(#fcode{ symbols = Ss } = FCode, Symbs) ->
    FCode#fcode{ symbols = maps:merge(Ss, Symbs) }.

symbol_identifier(Bin) ->
    %% First 4 bytes of blake hash
    {ok, <<X:4/binary,_/binary>> } = eblake2:blake2b(?HASH_BYTES, Bin),
    X.

insert_fun(Name, Attrs, {ArgType, RetType}, #{} = BBs, FCode) ->
    {F1, ID} = insert_symbol(Name, FCode),
    update_functions(F1, #{ID => {Attrs, {ArgType, RetType}, BBs}}).

insert_symbol(Name, #fcode{ symbols = Syms } = F) ->
    ID = symbol_identifier(Name),
    case maps:find(ID, Syms) of
        {ok, Name} ->
            {F, ID};
        {ok, X} ->
            error({two_symbols_with_same_hash, Name, X});
        error ->
            {update_symbols(F, #{ID => Name}), ID}
    end.

insert_annotation(comment =_Type, Line, Comment, FCode) ->
    Key   = aeb_fate_data:make_tuple({aeb_fate_data:make_string("comment"), Line}),
    Value = aeb_fate_data:make_string(Comment),
    update_annotations(FCode, #{ Key => Value }).

strip_init_function(#fcode{ functions = Funs,
                            symbols   = Syms } = FCode) ->
    Id    = symbol_identifier(<<"init">>),
    Funs1 = maps:remove(Id, Funs),
    Syms1 = maps:remove(Id, Syms),
    FCode#fcode{ functions = Funs1, symbols = Syms1 }.

%%%===================================================================
%%% Serialization
%%%===================================================================

serialize(#fcode{} = F) ->
    serialize(F, []).

serialize(#fcode{} = F, Options) ->
    sanity_check(F),
    serialize(F, serialize_functions(F), Options).

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
    iolist_to_binary(
      lists:foldr(fun({Id, {Attrs, Sig, C}}, Acc) ->
                          [[?FUNCTION, Id, serialize_attributes(Attrs), serialize_signature(Sig), serialize_bbs(C)] | Acc]
                  end, [], lists:sort(maps:to_list(Functions)))).

serialize_attributes(Attrs) ->
    AttrVal = lists:sum([ attr_value(Attr) || Attr <- Attrs ]),
    aeb_fate_encoding:serialize(?MAKE_FATE_INTEGER(AttrVal)).

attr_value(private) -> 1;
attr_value(payable) -> 2.

serialize_signature({Args, RetType}) ->
    [aeb_fate_encoding:serialize_type({tuple, Args}) |
     aeb_fate_encoding:serialize_type(RetType)].

serialize_symbol_table(#fcode{ symbols = Symbols }) ->
    aeb_fate_encoding:serialize(aeb_fate_data:make_map(Symbols)).

serialize_annotations(#fcode{ annotations = Annotations }) ->
    aeb_fate_encoding:serialize(aeb_fate_data:make_map(Annotations)).

serialize_bbs(#{} = BBs) ->
    serialize_bbs(BBs, 0, []).

serialize_bbs(BBs, N, Acc) ->
    case maps:get(N, BBs, none) of
        none -> lists:reverse(Acc);
        BB   -> serialize_bbs(BBs, N + 1, [serialize_bb(BB, [])|Acc])
    end.

serialize_bb([Op], Acc) ->
    lists:reverse([serialize_op(Op)|Acc]);
serialize_bb([Op|Rest], Acc) ->
    serialize_bb(Rest, [serialize_op(Op)|Acc]).

serialize_op(Op) ->
    [Mnemonic|Args] =
        case is_tuple(Op) of
            true  -> tuple_to_list(Op);
            false -> [Op]
        end,
    [aeb_fate_opcodes:m_to_op(Mnemonic) | serialize_code(Args)].

sanity_check(#fcode{ functions = Funs }) ->
    _ = [ case Def of
              {_, _, BBs} when byte_size(Id) == 4 -> sanity_check_bbs(BBs);
              _ -> error({illegal_function_id, Id})
          end || {Id, Def} <- maps:to_list(Funs) ],
    ok.

sanity_check_bbs(#{} = BBs) ->
    sanity_check_bbs(BBs, 0).

sanity_check_bbs(BBs, N) ->
    case maps:get(N, BBs, none) of
        none ->
            %% Assert that the BBs were contiguous
            case maps:size(BBs) =:= N of
                true  -> ok;
                false -> error({not_contiguous_labels, lists:sort(maps:keys(BBs))})
            end;
        [] ->
            error({empty_code_block, N});
        BB ->
            sanity_check_bb(BB),
            sanity_check_bbs(BBs, N + 1)
    end.

sanity_check_bb([Op]) ->
    sanity_check_op(true, Op);
sanity_check_bb([Op|Rest]) ->
    sanity_check_op(false, Op),
    sanity_check_bb(Rest).

sanity_check_op(IsLast, Op) ->
    [Mnemonic|Args] =
        case is_tuple(Op) of
            true  -> tuple_to_list(Op);
            false -> [Op]
        end,
    safe_sanity_check(IsLast, aeb_fate_opcodes:m_to_op(Mnemonic), Args).

safe_sanity_check(IsLast, Op, Args) ->
   case length(Args) == aeb_fate_opcodes:args(Op) of
       true ->
           case IsLast == aeb_fate_opcodes:end_bb(Op) of
               true  -> ok;
               false -> error({wrong_opcode_in_bb, Op})
           end;
       false -> error({wrong_nr_args_opcode, Op})
   end.


%% Argument encoding
%% Argument Specification Byte
%% bitpos:  6    4    2    0
%%         xx   xx   xx   xx
%%       Arg3 Arg2 Arg1 Arg0
%% For 5-8 args another Argument Spec Byte is used
%% bitpos:  6    4    2    0  |  6    4    2    0
%%         xx   xx   xx   xx  | xx   xx   xx   xx
%%       Arg7 Arg6 Arg5 Arg4  | Arg3 Arg2 Arg1 Arg0
%% Bit pattern
%% 00 : stack/unused (depending on instruction)
%% 01 : argN
%% 10 : varN
%% 11 : immediate

serialize_code([{_,_}|_] = List ) ->
    %% Take out the full argument list.
    {Args, Rest} = lists:splitwith(fun({_, _}) -> true; (_) -> false end, List),
    %% Create the appropriate number of modifier bytes.
    Mods = << <<(modifier_bits(Type, X)):2>> || {Type, X} <- pad_args(lists:reverse(Args)) >>,
    case Mods of
        <<M1:8, M2:8>> ->
            [M1, M2 | [serialize_data(Type, Arg) || {Type, Arg} <- Args, Type =/= stack]] ++
                serialize_code(Rest);
        <<M1:8>> ->
            [M1 | [serialize_data(Type, Arg) || {Type, Arg} <- Args, Type =/= stack]] ++
                serialize_code(Rest)
    end;
serialize_code([Op|Rest]) ->
    [Op|serialize_code(Rest)];
serialize_code([]) ->
    [].

pad_args(List) ->
    case length(List) of
        0 -> List;
        N when N =< 4 ->
            lists:duplicate(4 - N, {stack, 0}) ++ List;
        N when N =< 8 ->
            lists:duplicate(8 - N, {stack, 0}) ++ List
    end.

serialize_data(_, Data) ->
    aeb_fate_encoding:serialize(Data).

%% 00 : stack/unused (depending on instruction)
%% 01 : argN
%% 10 : varN
%% 11 : immediate
modifier_bits(immediate, _) -> 2#11;
modifier_bits(var, _)       -> 2#10;
modifier_bits(arg, _)       -> 2#01;
modifier_bits(stack, 0)     -> 2#00;
modifier_bits(Type, X)      -> error({illegal_argument, Type, X}).

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
    Fcode =
        #fcode{ functions = deserialize_functions(ByteCode, Env)
              , annotations = deserialize_annotations(Annotations)
              , symbols = deserialize_symbols(SymbolTable)
              },
    sanity_check(Fcode),
    Fcode.


deserialize_functions(<<?FUNCTION:8, A, B, C, D, Rest/binary>>,
            #{ function := none
             , bb := 0
             , current_bb_code := []
             } = Env) ->
    {Attrs, Rest2} = deserialize_attributes(Rest),
    {Sig,   Rest3} = deserialize_signature(Rest2),
    Env2 = Env#{function => {<<A,B,C,D>>, Attrs, Sig}},
    deserialize_functions(Rest3, Env2);
deserialize_functions(<<?FUNCTION:8, A, B, C, D, Rest/binary>>,
            #{ function := {F, Attrs, Sig}
             , bb := BB
             , current_bb_code := Code
             , code := Program
             , functions := Funs} = Env) ->
    {NewAttrs, Rest2} = deserialize_attributes(Rest),
    {NewSig,   Rest3} = deserialize_signature(Rest2),
    case Code of
        [] ->
            Env2 = Env#{ bb => 0
                       , current_bb_code => []
                       , function => {<<A,B,C,D>>, NewAttrs, NewSig}
                       , code => #{}
                       , functions => Funs#{F => {Attrs, Sig, Program}}},
            deserialize_functions(Rest3, Env2);
        _ ->
            Env2 = Env#{ bb => 0
                       , current_bb_code => []
                       , function => {<<A,B,C,D>>, NewAttrs, NewSig}
                       , code => #{}
                       , functions =>
                             Funs#{F => {Attrs, Sig,
                                         Program#{ BB => lists:reverse(Code)}}}},
            deserialize_functions(Rest3, Env2)
    end;
deserialize_functions(<<_Op:8, _Rest/binary>>,
            #{ function := none }) ->
    error({code_without_function});
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
deserialize_functions(<<>>, #{ function := none
                             , functions := Funs}) ->
    Funs;
deserialize_functions(<<>>, #{ function := {F, Attrs, Sig}
                             , bb := BB
                             , current_bb_code := Code
                             , code := Program
                             , functions := Funs}) ->
    FunctionCode =
        case Code of
            [] -> Program;
            _ -> Program#{ BB => lists:reverse(Code)}
        end,
    Funs#{F => {Attrs, Sig, FunctionCode}}.

deserialize_op(Op, Rest, Code) ->
    OpName = aeb_fate_opcodes:mnemonic(Op),
    case aeb_fate_opcodes:args(Op) of
        0 ->
            {Rest, [OpName | Code]};
        N ->
            {Args, Rest1} = deserialize_n_args(N, Rest),
            {Rest1, [list_to_tuple([OpName|Args])|Code]}
    end.

deserialize_n_args(N, <<M3:2, M2:2, M1:2, M0:2, Rest/binary>>) when N =< 4 ->
    {ArgMods, Zeros} = lists:split(N, [M0, M1, M2, M3]),
    assert_zero(Zeros),
    lists:mapfoldl(fun(M, Acc) ->
                           case bits_to_modifier(M) of
                               stack ->
                                   {{stack, 0}, Acc};
                               Modifier ->
                                   {Arg, Acc2} = aeb_fate_encoding:deserialize_one(Acc),
                                   {{Modifier, Arg}, Acc2}
                           end
                   end, Rest, ArgMods);
deserialize_n_args(N, <<M7:2, M6:2, M5:2, M4:2, M3:2, M2:2, M1:2, M0:2,
                        Rest/binary>>) when N =< 8 ->
    {ArgMods, Zeros} = lists:split(N, [M0, M1, M2, M3, M4, M5, M6, M7]),
    assert_zero(Zeros),
    lists:mapfoldl(fun(M, Acc) ->
                           case bits_to_modifier(M) of
                               stack ->
                                   {{stack, 0}, Acc};
                               Modifier ->
                                   {Arg, Acc2} = aeb_fate_encoding:deserialize_one(Acc),
                                   {{Modifier, Arg}, Acc2}
                           end
                   end, Rest, ArgMods).

deserialize_attributes(Binary) ->
    {AttrVal, Rest} = aeb_fate_encoding:deserialize_one(Binary),
    Attrs = [ attr(AVal) || AVal <- attr_vals(1, AttrVal) ],
    {lists:sort(Attrs), Rest}.

attr_vals(_, 0)                   -> [];
attr_vals(X, N) when N rem 2 == 0 -> attr_vals(X + 1, N div 2);
attr_vals(X, N)                   -> [X | attr_vals(X + 1, N div 2)].

attr(1) -> private;
attr(2) -> payable.

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

assert_zero([]) ->
    true;
assert_zero([0|Rest]) ->
    assert_zero(Rest);
assert_zero([_|_]) ->
    error(argument_defined_outside_range).
