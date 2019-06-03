%%% @author Thomas Arts
%%% @doc Use `rebar3 as eqc shell` to run properties in the shell
%%%
%%% We need to be able to generate data that serializes with ?LONG_LIST, ?LONG_TUPLE etc.
%%% In other words make some rather broad terms as well as some deep terms
%%%
%%% @end
%%% Created : 13 Dec 2018 by Thomas Arts <thomas@SpaceGrey.lan>

-module(aefate_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile([export_all, nowarn_export_all]).

prop_roundtrip() ->
    ?FORALL(FateData, fate_data(),
            measure(bytes, size(term_to_binary(FateData)),
                    begin
                        Serialized = aeb_fate_encoding:serialize(FateData),
                        ?WHENFAIL(eqc:format("Serialized ~p to ~p~n", [FateData, Serialized]),
                                  equals(aeb_fate_encoding:deserialize(Serialized), FateData))
                    end)).

prop_format_scan() ->
    ?FORALL(FateData, fate_data(),
            ?WHENFAIL(eqc:format("Trying to format ~p failed~n", [FateData]),
                      begin
                          String = aeb_fate_data:format(FateData),
                          {ok, _Scanned, _} = aeb_fate_asm_scan:scan(unicode:characters_to_list(String)),
                          true
                      end)).

prop_serializes() ->
    ?FORALL(FateDatas, non_empty(?SIZED(Size, resize(Size div 2, list(fate_data())))),
            ?WHENFAIL(eqc:format("Trying to serialize/deserialize ~p failed~n", [FateDatas]),
                      begin
                          {T1, Binary} =
                              timer:tc( fun() ->
                                                << begin B = aeb_fate_encoding:serialize(Data),
                                                         <<B/binary>> end || Data <- FateDatas >>
                                        end),
                          {T2, {FateData, _}} =
                              timer:tc(fun() -> aeb_fate_encoding:deserialize_one(Binary) end),
                          measure(binary_size, size(Binary),
                          measure(encode, T1,
                          measure(decode, T2,
                                  conjunction([{equal, equals(hd(FateDatas), FateData)},
                                               {size, size(Binary) < 500000}]))))
                      end)).

prop_fuzz() ->
    in_parallel(
    ?FORALL(Binary, ?LET(FateData, ?SIZED(Size, resize(Size div 4, fate_data())), aeb_fate_encoding:serialize(FateData)),
            ?FORALL(InjectedBin, injection(Binary),
                    try Org = aeb_fate_encoding:deserialize(InjectedBin),
                         NewBin = aeb_fate_encoding:serialize(Org),
                         NewOrg = aeb_fate_encoding:deserialize(NewBin),
                         measure(success, 1,
                         ?WHENFAIL(eqc:format("Deserialize ~p gives\n~p\nSerializes to ~p\n", [InjectedBin, Org, NewOrg]),
                                   equals(NewBin, InjectedBin)))
                    catch _:_ ->
                            true
                    end))).


prop_order() ->
    ?FORALL(Items, vector(3, fate_data()),
            begin
                %% Use lt to take minimum
                Min = lt_min(Items),
                Max = lt_max(Items),
                conjunction([ {minimum, is_empty([ {Min, '>', I} || I<-Items, aeb_fate_data:lt(I, Min)])},
                              {maximum, is_empty([ {Max, '<', I} || I<-Items, aeb_fate_data:lt(Max, I)])}])
            end).

lt_min([X, Y | Rest]) ->
    case aeb_fate_data:lt(X, Y) of
        true -> lt_min([X | Rest]);
        false -> lt_min([Y| Rest])
    end;
lt_min([X]) -> X.

lt_max([X, Y | Rest]) ->
    case aeb_fate_data:lt(X, Y) of
        true -> lt_max([Y | Rest]);
        false -> lt_max([X| Rest])
    end;
lt_max([X]) -> X.

prop_idempotent() ->
    ?FORALL(Items, list({fate_data_key(), fate_data()}),
            equals(aeb_fate_encoding:sort(Items),
                   aeb_fate_encoding:sort(aeb_fate_encoding:sort(Items)))).


fate_data() ->
    ?SIZED(Size, ?LET(Data, fate_data(Size, [map, variant]), eqc_symbolic:eval(Data))).

fate_data_key() ->
    ?SIZED(Size, ?LET(Data, fate_data(Size div 4, []), eqc_symbolic:eval(Data))).

fate_data(0, _Options) ->
    ?LAZY(
       oneof([fate_integer(),
              fate_boolean(),
              fate_nil(),
              fate_unit(),
              fate_string(),
              fate_address(),
              fate_hash(),
              fate_signature(),
              fate_contract(),
              fate_oracle(),
              fate_name(),
              fate_bits(),
              fate_channel()]));
fate_data(Size, Options) ->
    oneof([?LAZY(fate_data(Size - 1, Options)),
           ?LAZY(fate_list( fate_data(Size div 5, Options) )),
           ?LAZY(fate_tuple( list(fate_data(Size div 5, Options)) ))] ++
              [?LAZY(fate_variant( list(fate_data(Size div 5, Options))))
               || lists:member(variant, Options)
              ] ++
              [
               ?LAZY(fate_map( fate_data(Size div 8, Options -- [map, variant]),
                               fate_data(Size div 5, Options)))
              || lists:member(map, Options)
              ]).


fate_integer()    -> {call, aeb_fate_data, make_integer, [oneof([int(), largeint()])]}.
fate_bits()       -> {call, aeb_fate_data, make_bits, [oneof([int(), largeint()])]}.
fate_boolean()    -> {call, aeb_fate_data, make_boolean, [elements([true, false])]}.
fate_nil()        -> {call, aeb_fate_data, make_list, [[]]}.
fate_unit()       -> {call, aeb_fate_data, make_unit, []}.
fate_string()     -> {call, aeb_fate_data, make_string,
                      [frequency([{10, non_quote_string()}, {2, list(non_quote_string())},
                                  {1, ?LET(N, choose(64-3, 64+3), vector(N, $a))}])]}.
fate_address()    -> {call, aeb_fate_data, make_address, [non_zero_binary(256 div 8)]}.
fate_hash()       -> {call, aeb_fate_data, make_hash, [non_zero_binary(32)]}.
fate_signature()  -> {call, aeb_fate_data, make_signature, [non_zero_binary(64)]}.
fate_contract()   -> {call, aeb_fate_data, make_contract, [non_zero_binary(256 div 8)]}.
fate_oracle()     -> {call, aeb_fate_data, make_oracle, [non_zero_binary(256 div 8)]}.
fate_name()       -> {call, aeb_fate_data, make_name, [non_zero_binary(256 div 8)]}.
fate_channel()    -> {call, aeb_fate_data, make_channel, [non_zero_binary(256 div 8)]}.

%% May shrink to fate_unit
fate_tuple(ListGen) ->
    {call, aeb_fate_data, make_tuple, [?LET(Elements, ListGen, list_to_tuple(Elements))]}.

fate_variant(ListGen) ->
    ?LET({L1, L2, TupleAsList}, {list(choose(0, 255)), list(choose(0,255)), ListGen},
         {call, aeb_fate_data, make_variant,
          [L1 ++ [length(TupleAsList)] ++ L2, length(L1), list_to_tuple(TupleAsList)]}).

fate_list(Gen) ->
    {call, aeb_fate_data, make_list, [frequency([{20, list(Gen)}, {1, ?LET(N, choose(64-3, 64+3), vector(N, Gen))}])]}.

fate_map(KeyGen, ValGen) ->
    {call, aeb_fate_data, make_map, [map(KeyGen, ValGen)]}.


non_zero_binary(N) ->
    Bits = N*8,
    ?SUCHTHAT(Bin, binary(N), begin <<V:Bits>> = Bin, V =/= 0 end).

non_quote_string() ->
    ?SUCHTHAT(S, utf8(), [ quote || <<34>> <= S ] == []).

char() ->
    choose(1, 255).

injection(Binary) ->
    ?LET({N, Inj}, {choose(0, byte_size(Binary) - 1), choose(0,255)},
         begin
             M = N * 8,
             <<X:M, _:8, Z/binary>> = Binary,
             <<X:M, Inj:8, Z/binary>>
         end).

is_empty(L) ->
    ?WHENFAIL(eqc:format("~p\n", [L]), L == []).
