%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%     Encode and decode data and function calls according to
%%%     Sophia-FATE-ABI
%%% @end
%%% Created : 11 Jun 2019
%%%
%%%-------------------------------------------------------------------
-module(aeb_fate_abi).

-export([ create_calldata/2
        , function_name_from_function_hash/2
        , get_function_hash_from_calldata/1 ]).

-include("../include/aeb_fate_data.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec create_calldata(list(), [term()]) -> {ok, binary()}.
create_calldata(FunName, Args) ->
    FunctionId = aeb_fate_code:symbol_identifier(list_to_binary(FunName)),
    {ok, aeb_fate_encoding:serialize(
           aeb_fate_data:make_tuple({FunctionId,
                                     aeb_fate_data:make_tuple(list_to_tuple(Args))}))}.

-spec function_name_from_function_hash(any(), aeb_fate_code:fcode()) ->
    {ok, term()} | {error, term()}.
function_name_from_function_hash(<<SymbolHash:4/binary, _:28/binary>>, FateCode) ->
    function_name_from_function_hash(SymbolHash, FateCode);
function_name_from_function_hash(SymbolHash = <<_:4/binary>>, FateCode) ->
    Symbols = aeb_fate_code:symbols(FateCode),
    case maps:get(SymbolHash, Symbols, undefined) of
        undefined -> {error, no_function_matching_function_hash};
        Function  -> {ok, Function}
    end.

-spec get_function_hash_from_calldata(binary()) ->
    {ok, binary()} | {error, term()}.
get_function_hash_from_calldata(CallData) ->
    try ?FATE_TUPLE_ELEMENTS(aeb_fate_encoding:deserialize(CallData)) of
        [FunHash, _Args] -> {ok, FunHash};
        _                -> {error, bad_calldata}
    catch _:_ ->
        {error, bad_calldata}
    end.
