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
    ?FORALL({Data, Garbage}, {fate_data(), binary()},
            ?WHENFAIL(eqc:format("Trying to serialize/deserialize ~p failed~n", [Data]),
                      begin
                          Binary = <<(aeb_fate_encoding:serialize(Data))/binary, Garbage/binary>>,
                          {FateData, Rest} = aeb_fate_encoding:deserialize_one(Binary),
                          measure(binary_size, size(Binary),
                                  conjunction([{equal, equals(Data,    FateData)},
                                               {rest,  equals(Garbage, Rest)},
                                               {size, size(Binary) < 500000}]))
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
    ?SIZED(Size, ?LET(Data, fate_data(Size div 4, [variant]), eqc_symbolic:eval(Data))).

fate_data(0, _Options) ->
    ?LAZY(
       frequency(
         [{5, oneof([fate_integer(), fate_boolean(), fate_nil(), fate_unit()])},
          {1, oneof([fate_string(), fate_address(), fate_bytes(), fate_contract(),
                     fate_oracle(), fate_oracle_q(), fate_name(), fate_bits(), fate_channel()])}]));
fate_data(Size, Options) ->
    ?LAZY(
    oneof([fate_data(0, Options),
           fate_list(Size, Options),
           fate_tuple(Size, Options)] ++
          [fate_variant(Size, Options)
            || lists:member(variant, Options)] ++
          [fate_map(Size, Options)
            || lists:member(map, Options)])).


fate_integer()    -> ?LET(X, oneof([int(), largeint()]), return(aeb_fate_data:make_integer(X))).
fate_bits()       -> ?LET(X, oneof([int(), largeint()]), return(aeb_fate_data:make_bits(X))).
fate_boolean()    -> ?LET(X, elements([true, false]), return(aeb_fate_data:make_boolean(X))).
fate_nil()        -> aeb_fate_data:make_list([]).
fate_unit()       -> aeb_fate_data:make_unit().
fate_string()     -> ?LET(X, frequency([{10, non_quote_string()}, {2, list(non_quote_string())},
                                  {1, ?LET(N, choose(64-3, 64+3), vector(N, $a))}]),
                          return(aeb_fate_data:make_string(X))).
fate_address()    -> ?LET(X, binary(256 div 8), return(aeb_fate_data:make_address(X))).
fate_bytes()      -> ?LET(X, non_empty(binary()), return(aeb_fate_data:make_bytes(X))).
fate_contract()   -> ?LET(X, binary(256 div 8), return(aeb_fate_data:make_contract(X))).
fate_oracle()     -> ?LET(X, binary(256 div 8), return(aeb_fate_data:make_oracle(X))).
fate_oracle_q()   -> ?LET(X, binary(256 div 8), return(aeb_fate_data:make_oracle_query(X))).
fate_name()       -> ?LET(X, binary(256 div 8), return(aeb_fate_data:make_name(X))).
fate_channel()    -> ?LET(X, binary(256 div 8), return(aeb_fate_data:make_channel(X))).

fate_values(Size, N, Options) ->
    eqc_gen:list(N, fate_data(Size div max(1, N), Options)).

%% May shrink to fate_unit
fate_tuple(Size, Options) ->
    ?LET(N, choose(0, 6),
    ?LETSHRINK(Elements, fate_values(Size, N, Options),
    return(aeb_fate_data:make_tuple(list_to_tuple(Elements))))).

fate_variant(Size, Options) ->
    ?LET({L1, L2, {tuple, Args}}, {list(choose(0, 255)), list(choose(0,255)), fate_tuple(Size, Options)},
         return(aeb_fate_data:make_variant(L1 ++ [tuple_size(Args)] ++ L2,
                                           length(L1), Args))).

fate_list(Size, Options) ->
    ?LET(N, frequency([{20, choose(0, 6)}, {1, choose(64 - 3, 64 + 3)}]),
    ?LETSHRINK(Vs, fate_values(Size, N, Options),
    return(aeb_fate_data:make_list(Vs)))).

fate_map(Size, Options) ->
    ?LET(N, choose(0, 6),
    ?LETSHRINK(Values, fate_values(Size, N, Options),
    ?LET(Keys, vector(length(Values), fate_data(Size div max(1, N * 2), Options -- [map])),
    return(aeb_fate_data:make_map(maps:from_list(lists:zip(Keys, Values))))))).

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
