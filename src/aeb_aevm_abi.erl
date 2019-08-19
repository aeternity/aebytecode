%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Encode and decode data and function calls according to
%%%     Sophia-AEVM-ABI
%%% @end
%%% Created : 25 Jan 2018
%%%
%%%-------------------------------------------------------------------
-module(aeb_aevm_abi).
-define(HASH_SIZE, 32).

-export([ create_calldata/4
        , check_calldata/3
        , function_type_info/4
        , function_type_hash/3
        , arg_typerep_from_function/2
        , type_hash_from_function_name/2
        , typereps_from_type_hash/2
        , function_name_from_type_hash/2
        , get_function_hash_from_calldata/1
        , is_payable/2
        , abi_version/0
        ]).

-type hash() :: <<_:256>>. %% 256 = ?HASH_SIZE * 8.
-type function_name() :: binary(). %% String
-type typerep() :: aeb_aevm_data:type().
-type function_type_info() :: { FunctionHash :: hash()
                              , FunctionName :: function_name()
                              , Payable      :: boolean()
                              , ArgType      :: binary() %% binary typerep
                              , OutType      :: binary() %% binary typerep
                              }.
-type type_info() :: [function_type_info()].

%%%===================================================================
%%% API
%%%===================================================================

%% Shall match ?ABI_AEVM_SOPHIA_1
-spec abi_version() -> integer().
abi_version() ->
    1.

%%%===================================================================
%%% Handle calldata

create_calldata(FunName, Args, ArgTypes0, RetType) ->
    ArgTypes = {tuple, ArgTypes0},
    <<TypeHashInt:?HASH_SIZE/unit:8>> =
        function_type_hash(list_to_binary(FunName), ArgTypes, RetType),
    Data = aeb_heap:to_binary({TypeHashInt, list_to_tuple(Args)}),
    {ok, Data}.

-spec check_calldata(binary(), type_info(), boolean()) ->
                        {'ok', typerep(), typerep()} | {'error', atom()}.
check_calldata(CallData, TypeInfo, CheckPayable) ->
    %% The first element of the CallData should be the function name
    case get_function_hash_from_calldata(CallData) of
        {ok, Hash} ->
            check_calldata(Hash, CallData, TypeInfo, CheckPayable);
        {error, _What} ->
            {error, bad_call_data}
    end.

check_calldata(Hash, CallData, TypeInfo, true) ->
    case is_payable(Hash, TypeInfo) of
        {ok, true}       -> check_calldata(Hash, CallData, TypeInfo, false);
        {ok, false}      -> {error, function_is_not_payable};
        Err = {error, _} -> Err
    end;
check_calldata(Hash, CallData, TypeInfo, false) ->
    case typereps_from_type_hash(Hash, TypeInfo) of
        {ok, ArgType, OutType} ->
            try aeb_heap:from_binary({tuple, [word, ArgType]}, CallData) of
                {ok, _Something} ->
                    {ok, {tuple, [word, ArgType]}, OutType};
                {error, _} ->
                    {error, bad_call_data}
            catch
                _T:_E ->
                    {error, bad_call_data}
            end;
        {error, _} ->
            {error, unknown_function}
    end.


-spec get_function_hash_from_calldata(CallData::binary()) ->
                                             {ok, binary()} | {error, term()}.
get_function_hash_from_calldata(CallData) ->
    case aeb_heap:from_binary({tuple, [word]}, CallData) of
        {ok, {HashInt}} -> {ok, <<HashInt:?HASH_SIZE/unit:8>>};
        {error, _} = Error -> Error
    end.

%%%===================================================================
%%% Handle type info from contract meta data

-spec function_type_info(function_name(), boolean(), [typerep()], typerep()) ->
                            function_type_info().
function_type_info(Name, Payable, ArgTypes, OutType) ->
    ArgType = {tuple, ArgTypes},
    { function_type_hash(Name, ArgType, OutType)
    , Name
    , Payable
    , aeb_heap:to_binary(ArgType)
    , aeb_heap:to_binary(OutType)
    }.

-spec function_type_hash(function_name(), typerep(), typerep()) -> hash().
function_type_hash(Name, ArgType, OutType) when is_binary(Name) ->
    Bin =  iolist_to_binary([ Name
                            , aeb_heap:to_binary(ArgType)
                            , aeb_heap:to_binary(OutType)
                            ]),
    %% Calculate a 256 bit digest BLAKE2b hash value of a binary
    {ok, Hash} = eblake2:blake2b(?HASH_SIZE, Bin),
    Hash.

-spec arg_typerep_from_function(function_name(), type_info()) ->
           {'ok', typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
arg_typerep_from_function(Function, TypeInfo) ->
    case lists:keyfind(Function, 2, TypeInfo) of
        {_TypeHash, Function, ArgTypeBin, _OutTypeBin} ->
            arg_typerep_from_type_binary(ArgTypeBin);
        {_TypeHash, Function, _Payable, ArgTypeBin, _OutTypeBin} ->
            arg_typerep_from_type_binary(ArgTypeBin);
        false ->
            {error, unknown_function}
    end.

arg_typerep_from_type_binary(ArgTBin) ->
    case aeb_heap:from_binary(typerep, ArgTBin) of
        {ok, ArgT} -> {ok, ArgT};
        {error,_}  -> {error, bad_type_data}
    end.

-spec typereps_from_type_hash(hash(), type_info()) ->
           {'ok', typerep(), typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
typereps_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash, _Function, ArgTypeBin, OutTypeBin} ->
            typereps_from_type_binaries(ArgTypeBin, OutTypeBin);
        {TypeHash, _Function, _Payable, ArgTypeBin, OutTypeBin} ->
            typereps_from_type_binaries(ArgTypeBin, OutTypeBin);
        false ->
            {error, unknown_function}
    end.

typereps_from_type_binaries(ArgTBin, OutTBin) ->
    case {aeb_heap:from_binary(typerep, ArgTBin), aeb_heap:from_binary(typerep, OutTBin)} of
        {{ok, ArgT}, {ok, OutT}} -> {ok, ArgT, OutT};
        {_, _}                   -> {error, bad_type_data}
    end.

-spec function_name_from_type_hash(hash(), type_info()) ->
                                          {'ok', function_name()}
                                        | {'error', 'unknown_function'}.
function_name_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash, Function, _ArgTypeBin, _OutTypeBin} ->
            {ok, Function};
        {TypeHash, Function, _Payable, _ArgTypeBin, _OutTypeBin} ->
            {ok, Function};
        false ->
            {error, unknown_function}
    end.

-spec type_hash_from_function_name(function_name(), type_info()) ->
                                          {'ok', hash()}
                                        | {'error', 'unknown_function'}.
type_hash_from_function_name(Name, TypeInfo) ->
    case lists:keyfind(Name, 2, TypeInfo) of
        {TypeHash, Name, _ArgTypeBin, _OutTypeBin} ->
            {ok, TypeHash};
        {TypeHash, Name, _Payable, _ArgTypeBin, _OutTypeBin} ->
            {ok, TypeHash};
        false ->
            {error, unknown_function}
    end.

-spec is_payable(hash(), type_info()) -> {ok, boolean()} | {error, 'unknown_function'}.
is_payable(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash, _Function, _ArgTypeBin, _OutTypeBin} ->
            {ok, true};
        {TypeHash, _Function, Payable, _ArgTypeBin, _OutTypeBin} ->
            {ok, Payable};
        false ->
            {error, unknown_function}
    end.

