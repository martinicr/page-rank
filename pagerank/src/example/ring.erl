%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2015 4:35 PM
%%%-------------------------------------------------------------------
-module(ring).
-author("martinflores").

-export([start/1, start_proc/2]).

start(Num) ->
  start_proc(Num, self()).

start_proc(0, Pid) ->
  io:format("Num = 0, Pid = ~w ~n", [Pid]),
  Pid ! ok;

start_proc(Num, Pid) ->
  NPid = spawn(?MODULE, start_proc, [Num-1, Pid]),
  io:format("Num = ~w, NPid = ~w, Pid = ~w ~n", [Num, NPid, Pid]),
  NPid ! ok,
  receive ok -> ok end.