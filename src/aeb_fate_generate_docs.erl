-module(aeb_fate_generate_docs).
-vsn("3.2.1").

-export([generate_documentation/2, generate_documentation/3]).

-export(
   [ gen_protocol_opcodes_flags_and_gas/1
   , gen_protocol_description_of_operations/1
   , gen_protocol_opcodes/1
   ]).

-define(LIMA_PROTOCOL_VSN, 4).
-define(IRIS_PROTOCOL_VSN, 5).

generate_documentation(Filename, Fields) ->
    generate_documentation(Filename, Fields, fun(_) -> true end).
generate_documentation(Filename, Fields, Filter) when is_function(Filter, 1) ->
    {ok, File} = file:open(Filename, [write, {encoding, utf8}]),
    Header =
        lists:flatten(
          "|" ++ [" " ++ header_name(F) ++ " |" || F <- Fields] ++ "\n"
         ),
    Separator =
        lists:flatten(
          "|" ++ [" " ++ ["-" || _ <- header_name(F)] ++ " |" || F <- Fields] ++ "\n"
         ),
    Instructions =
        lists:flatten(
          [gen_doc_for_op(Op, Fields)
           ++ "\n" || Op <- aeb_fate_generate_ops:get_ops(), Filter(Op)]),
    io:format(File, "~ts~ts~ts\n", [Header, Separator, Instructions]),
    file:close(File).

header_name(opname) ->
    "Name";
header_name(opcode) ->
    "Opcode";
header_name(arity) ->
    "Arity";
header_name(end_bb) ->
    "Ends basic block";
header_name(in_auth) ->
    "Allowed in auth";
header_name(offchain) ->
    "Allowed offchain";
header_name(format) ->
    "Args";
header_name(doc) ->
    "Description";
header_name(gas) ->
    "Gas cost";
header_name(arg_types) ->
    "Arg types";
header_name(res_type) ->
    "Res type".

gen_doc_for_op(#{ opname            := OpName
                , opcode            := OpCode
                , arity             := Arity
                , end_bb            := EndBB
                , in_auth           := InAuth
                , offchain          := AllowedOffchain
                , format            := FateFormat
                , doc               := Doc
                , gas               := Gas
                , arg_types         := ArgTypes
                , res_type          := ResType
                }, Fields) ->
    "| " ++
        string:join(
          [ case Field of
                opname   -> io_lib:format("`~s`", [OpName]);
                opcode   -> io_lib:format("0x~.16b", [OpCode]);
                arity    -> io_lib:format("~p", [Arity]);
                end_bb   -> io_lib:format("~p", [EndBB]);
                in_auth  -> io_lib:format("~p", [InAuth]);
                offchain -> io_lib:format("~p", [AllowedOffchain]);
                format ->
                    case FateFormat of
                        [] -> "";
                        _ ->  lists:join(
                                " ",
                                [format_arg_doc(A) ||
                                    A <-
                                        lists:zip(FateFormat,
                                                  lists:seq(0,length(FateFormat)-1))])
                    end;
                doc -> Doc;
                          gas when is_integer(Gas) -> io_lib:format("~p", [Gas]);
                gas when is_list(Gas) ->
                    lists:flatten(
                      string:join(
                        [ io_lib:format(
                            "~p (~s)",
                            [GasVal, protocol_name(Prot)]
                                     )
                          || {Prot, GasVal} <- Gas
                        ], ", "));
                arg_types -> io_lib:format("~p", [ArgTypes]);
                res_type -> io_lib:format("~p", [ResType])
            end
            || Field <- Fields
          ],
          " | ") ++ " |".

protocol_name(?LIMA_PROTOCOL_VSN) ->
    "lima";
protocol_name(?IRIS_PROTOCOL_VSN) ->
    "iris".

format_arg_doc({a, N}) -> io_lib:format("Arg~w", [N]);
format_arg_doc({is,_N}) -> "Identifier";
format_arg_doc({ii,_N}) -> "Integer";
format_arg_doc({li,_N}) -> "[Integers]";
format_arg_doc({t,_N}) -> "Type".


%% --- protocol documentation ---

gen_protocol_description_of_operations(Filename) ->
    generate_documentation(
      Filename, [opname, format, doc, arg_types, res_type]
     ).

gen_protocol_opcodes_flags_and_gas(Filename) ->
    generate_documentation(
      Filename, [opcode, opname, end_bb, in_auth, offchain, gas]
     ).

gen_protocol_opcodes(Filename) ->
    generate_documentation(
      Filename, [opcode, opname]
     ).
