-module(edioph_tests).
-include_lib("eunit/include/eunit.hrl").

gcd_test() ->
    ?assertEqual(3, edioph:gcd(9, 6)),
    ?assertEqual(1, edioph:gcd(17, 23)),
    ?assertEqual(5, edioph:gcd(0, 5)),
    ?assertEqual(10, edioph:gcd(10, 0)),
    ?assertEqual(0, edioph:gcd(0, 0)).

lcm_test() ->
    ?assertEqual(18, edioph:lcm(6, 9)),
    ?assertEqual(391, edioph:lcm(17, 23)),
    ?assertEqual(0, edioph:lcm(0, 5)),
    ?assertEqual(0, edioph:lcm(10, 0)),
    ?assertEqual(0, edioph:lcm(0, 0)).

egcd_test() ->
    ?assertEqual({3, -1, 2}, edioph:egcd(9, 6)),
    ?assertEqual({1, -5, 4}, edioph:egcd(17, 23)),
    ?assertEqual({5, 1, 0}, edioph:egcd(0, 5)),
    ?assertEqual({10, 1, 0}, edioph:egcd(10, 0)),
    ?assertEqual({0, 0, 0}, edioph:egcd(0, 0)).

fib_test() ->
    ?assertEqual(0, edioph:fib(0)),
    ?assertEqual(1, edioph:fib(1)),
    ?assertEqual(1, edioph:fib(2)),
    ?assertEqual(2, edioph:fib(3)),
    ?assertEqual(3, edioph:fib(4)),
    ?assertEqual(5, edioph:fib(5)).

run_tests() ->
    ?assertEqual(ok, eunit:test(edioph_tests, [verbose])).