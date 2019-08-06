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
        , check_calldata/2
        , function_type_info/3
        , function_type_hash/3
        , arg_typerep_from_function/2
        , type_hash_from_function_name/2
        , typereps_from_type_hash/2
        , function_name_from_type_hash/2
        , get_function_hash_from_calldata/1
        , abi_version/0
        ]).

-type hash() :: <<_:256>>. %% 256 = ?HASH_SIZE * 8.
-type function_name() :: binary(). %% String
-type typerep() :: aeb_aevm_data:type().
-type function_type_info() :: { FunctionHash :: hash()
                              , FunctionName :: function_name()
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

-spec check_calldata(binary(), type_info()) ->
                        {'ok', typerep(), typerep()} | {'error', atom()}.
check_calldata(CallData, TypeInfo) ->
    %% The first element of the CallData should be the function name
    case get_function_hash_from_calldata(CallData) of
        {ok, Hash} ->
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
            end;
        {error, _What} ->
            {error, bad_call_data}
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

-spec function_type_info(function_name(), [typerep()], typerep()) ->
                            function_type_info().
function_type_info(Name, ArgTypes, OutType) ->
    ArgType = {tuple, ArgTypes},
    { function_type_hash(Name, ArgType, OutType)
    , Name
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
        {_TypeHash, Function, ArgTypeBin,_OutTypeBin} ->
            case aeb_heap:from_binary(typerep, ArgTypeBin) of
                {ok, ArgType} -> {ok, ArgType};
                {error,_} -> {error, bad_type_data}
            end;
        false ->
            {error, unknown_function}
    end.

-spec typereps_from_type_hash(hash(), type_info()) ->
           {'ok', typerep(), typerep()} | {'error', 'bad_type_data' | 'unknown_function'}.
typereps_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash,_Function, ArgTypeBin, OutTypeBin} ->
            case {aeb_heap:from_binary(typerep, ArgTypeBin),
                  aeb_heap:from_binary(typerep, OutTypeBin)} of
                {{ok, ArgType}, {ok, OutType}} -> {ok, ArgType, OutType};
                {_, _} -> {error, bad_type_data}
            end;
        false ->
            {error, unknown_function}
    end.

-spec function_name_from_type_hash(hash(), type_info()) ->
                                          {'ok', function_name()}
                                        | {'error', 'unknown_function'}.
function_name_from_type_hash(TypeHash, TypeInfo) ->
    case lists:keyfind(TypeHash, 1, TypeInfo) of
        {TypeHash, Function,_ArgTypeBin,_OutTypeBin} ->
            {ok, Function};
        false ->
            {error, unknown_function}
    end.

-spec type_hash_from_function_name(function_name(), type_info()) ->
                                          {'ok', hash()}
                                        | {'error', 'unknown_function'}.
type_hash_from_function_name(Name, TypeInfo) ->
    case lists:keyfind(Name, 2, TypeInfo) of
        {TypeHash, Name,_ArgTypeBin,_OutTypeBin} ->
            {ok, TypeHash};
        false ->
            {error, unknown_function}
    end.
