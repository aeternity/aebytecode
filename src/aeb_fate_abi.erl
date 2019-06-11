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

-export([ create_calldata/2 ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create_calldata(list(), [term()]) -> {ok, binary()}.
create_calldata(FunName, Args) ->
    FunctionId = aeb_fate_code:symbol_identifier(list_to_binary(FunName)),
    {ok, aeb_fate_encoding:serialize(
           aeb_fate_data:make_tuple({FunctionId,
                                     aeb_fate_data:make_tuple(list_to_tuple(Args))}))}.
