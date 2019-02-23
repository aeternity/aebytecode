%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc Basic tests for Fate serialization
%%%
%%% To run:
%%%  TEST=aeb_serialize_test rebar3 eunit
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(aeb_serialize_test).

-include_lib("eunit/include/eunit.hrl").

serialize_integer_test() ->
    <<0>> = aeb_fate_encoding:serialize(aeb_fate_data:make_integer(0)),
    <<2>> = aeb_fate_encoding:serialize(aeb_fate_data:make_integer(1)),
    <<126>> = aeb_fate_encoding:serialize(aeb_fate_data:make_integer(63)),
    <<111, 0>> = aeb_fate_encoding:serialize(aeb_fate_data:make_integer(64)),
    <<111,130,255,255>> = aeb_fate_encoding:serialize(aeb_fate_data:make_integer(65535 + 64)),
    <<111,184,129,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>> =
        aeb_fate_encoding:serialize(aeb_fate_data:make_integer(1 bsl 1024 + 64)).

serialize_deserialize_test_() ->
    [{lists:flatten(io_lib:format("~p", [X])),
      fun() ->
          ?assertEqual(X,
                       aeb_fate_encoding:deserialize(aeb_fate_encoding:serialize(X)))
      end}
     || X <- sources()].

make_int_list(N) -> [aeb_fate_data:make_integer(I) || I <- lists:seq(1, N)].

sources() ->
    FortyTwo = aeb_fate_data:make_integer(42),
    Unit = aeb_fate_data:make_unit(),
    True = aeb_fate_data:make_boolean(true),
    False = aeb_fate_data:make_boolean(false),
    Nil = aeb_fate_data:make_list([]),
    EmptyString = aeb_fate_data:make_string(""),
    EmptyMap = aeb_fate_data:make_map(#{}),
    [aeb_fate_data:make_integer(0),
     aeb_fate_data:make_integer(1),
     True, False, Unit, Nil, EmptyString, EmptyMap,
     aeb_fate_data:make_list([True]),
     aeb_fate_data:make_address(
       <<0,1,2,3,4,5,6,7,8,9,
         0,1,2,3,4,5,6,7,8,9,
         0,1,2,3,4,5,6,7,8,9,
         1,2>>),
     aeb_fate_data:make_string(<<"Hello">>),
     aeb_fate_data:make_string(
       <<"0123456789012345678901234567890123456789"
         "0123456789012345678901234567890123456789"
         "0123456789012345678901234567890123456789"
         "0123456789012345678901234567890123456789">>), %% Magic concat 80 char string.
     aeb_fate_data:make_tuple({True, FortyTwo}),
     aeb_fate_data:make_tuple(list_to_tuple(make_int_list(65))),
     aeb_fate_data:make_map(#{ aeb_fate_data:make_integer(1) => True, aeb_fate_data:make_integer(2) => False}),
     aeb_fate_data:make_map(#{ aeb_fate_data:make_string(<<"foo">>) => aeb_fate_data:make_tuple({FortyTwo, True})}),
     aeb_fate_data:make_list(make_int_list(3)),
     aeb_fate_data:make_integer(-65),
     aeb_fate_data:make_integer(65),
     aeb_fate_data:make_integer(-32432847932847928374983),
     aeb_fate_data:make_bits(0),
     aeb_fate_data:make_bits(1),
     aeb_fate_data:make_bits(-1),
     aeb_fate_data:make_list(make_int_list(65)),
     aeb_fate_data:make_variant(2, 0, {FortyTwo}),
     aeb_fate_data:make_variant(2, 1, {}),
     aeb_fate_data:make_list([aeb_fate_data:make_variant(3, 0, {})]),
     aeb_fate_data:make_variant(255, 254, {}),
     aeb_fate_data:make_variant(5, 3, {aeb_fate_data:make_boolean(true),
                                       aeb_fate_data:make_list(make_int_list(3)),
                                       aeb_fate_data:make_string(<<"foo">>)})

    ].
