%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 8:30 PM
%%%-------------------------------------------------------------------
-module(boolean).
-author("martinflores").

%% API
-export([b_not/1, b_and/2]).

% b_not(false) => true
b_not(true) ->
  false;
b_not(false) ->
  true.

% b_and(false, true) => false

b_and(false, true) ->
  false;
b_and(true, false) ->
  false;
b_and(false, false) ->
  false;
b_and(true, true) ->
  true.

% bool:b_and(bool:b_not(bool:b_and(true, false)), true) ⇒ true

