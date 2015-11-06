%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2015 7:14 PM
%%%-------------------------------------------------------------------
-module(listoperations_tests).
-author("martinflores").

-include_lib("eunit/include/eunit.hrl").

%% API
-import(listoperations, [sum_all_items/1]).


b_not_test() ->
  ?assertEqual(6, sum_all_items([[1,2,3]])).
