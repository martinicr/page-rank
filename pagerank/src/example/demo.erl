%%%-------------------------------------------------------------------
%%% @author Martin Flores
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 7:52 PM
%%%-------------------------------------------------------------------
-module(demo).
-author("martinflores").

%% API
-export([double/1]).
-export([even/1, number/1]).


double(Value) ->
  times(Value, 2).

times(X,Y) ->
  X * Y.



even(Int) when Int rem 2 == 0    -> true;
even(Int) when Int rem 2 == 1    -> false.
number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num)   -> float;
number(_Other)                   -> false.

