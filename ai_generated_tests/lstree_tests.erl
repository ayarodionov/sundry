-module(lstree_tests).
-include_lib("eunit/include/eunit.hrl").

lstree_test() ->
    ?assertEqual({lsnode,0,false,[]}, lstree:from_list([])),
    ?assertEqual({lsnode,0,1,[]}, lstree:from_list(1)),
    ?assertEqual({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]} , lstree:from_list([2,3])),
    ?assertEqual([1], lstree:to_list({lsnode,0,1,[]})).

lstree_test_2() ->
    ?assertEqual(3, lstree:max({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]})),
    ?assertEqual(2, lstree:min({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]})),
    ?assertEqual(3, lstree:max_min({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]})),
    ?assertEqual(2, lstree:min_max({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]})).

lstree_test_3() ->
    ?assertEqual(3, lstree:max_min_p({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]})),
    ?assertEqual(2, lstree:min_max_p({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]})).

lstree_test_4() ->
    ?assertEqual(3, lstree:map({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]} , fun(X) -> X end)),
    ?assertEqual(3, lstree:map({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]} , fun(X) -> X end, fun(X) -> X end)),
    ?assertEqual(3, lstree:map({lsnode,0,false,[{lsnode,1,2,[]},{lsnode,1,3,[]}]} , fun(X) -> X end, fun(X) -> X end, {0,0})).

lstree_test_5() ->
    ?assertEqual(3, lstree:pmax(fun(X, {_,_}) -> X end, {0,0}, [{lsnode,1,2,[]},{lsnode,1,3,[]}])).
    ?assertEqual(2, lstree:pmin(fun(X, {_,_}) -> X end, {0,0}, [{lsnode,1,2,[]},{lsnode,1,3,[]}])).
