-module(anagram_tests).
-include_lib("eunit/include/eunit.hrl").

anagram_test() ->
    ?assertEqual(true, anagram:eq("abc", "cba")),
    ?assertEqual(false, anagram:eq("abc", "def")),
    ?assertEqual(true, anagram:eqp("abc", "cba")),
    ?assertEqual(false, anagram:eqp("abc", "def")).

anagram_test_2() ->
    ?assertEqual(294, anagram:as_sum("abc")),
    ?assertEqual(294, anagram:as_sum("cba")),
    ?assertEqual(6, anagram:as_prod("abc")),
    ?assertEqual(6, anagram:as_prod("cba")),
    ?assertEqual(2, anagram:as_prime_prod("abc")),
    ?assertEqual(2, anagram:as_prime_prod("cba")).

anagram_test_3() ->
    ?assertEqual(true, anagram:eq("abc", "cba", 1, 3)),
    ?assertEqual(false, anagram:eq("abc", "def", 1, 3)),
    ?assertEqual(true, anagram:eqp("abc", "cba", 1)),
    ?assertEqual(false, anagram:eqp("abc", "def", 1)).

anagram_test_4() ->
    ?assertEqual(294, anagram:as_sum("abc", 1, 3)),
    ?assertEqual(294, anagram:as_sum("cba", 1, 3)),
    ?assertEqual(6, anagram:as_prod("abc", 1)),
    ?assertEqual(6, anagram:as_prod("cba", 1)),
    ?assertEqual(2, anagram:as_prime_prod("abc", 1)),
    ?assertEqual(2, anagram:as_prime_prod("cba", 1)).

anagram_test_5() ->
    ?assertEqual(2, anagram:prime(1)),
    ?assertEqual(3, anagram:prime(2)),
    ?assertEqual(5, anagram:prime(3)),
    ?assertEqual(7, anagram:prime(4)).
