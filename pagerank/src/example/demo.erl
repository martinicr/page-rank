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

double(Value) ->
  times(Value, 2).

times(X,Y) ->
  X * Y.

