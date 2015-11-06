%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 9:02 PM
%%%-------------------------------------------------------------------
-module(boolean_tests).
-author("martinflores").
-include_lib("eunit/include/eunit.hrl").

-import(boolean, [b_not/1, b_and/2]).

b_not_test() ->
  ?assertEqual(true, b_not(false)).

b_and_test() ->
  ?assertEqual(false, b_and(true, false)).
