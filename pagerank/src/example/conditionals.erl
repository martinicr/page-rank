%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 9:36 PM
%%%-------------------------------------------------------------------
-module(conditionals).
-author("martinflores").

%% API
-compile(export_all).



index(X,Y) ->
  index({X,Y}).

index(Z) ->
  case Z of
    {0, [X|_]}             -> X;
    {N, [_|Xs]} when N > 0 -> index(N-1, Xs)
  end.


safe(X) ->
  case X of
    one -> Y =12;
    _   -> Y = 196
  end,
  X + Y.
