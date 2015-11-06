%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 10:59 PM
%%%-------------------------------------------------------------------
-module(echo).
-author("martinflores").

-export([go/0, loop/0, some/0]).


go() ->
%%   register(echo, spawn(echo, loop, [])),
  Pid = spawn(echo, loop, []),
  io:format("Pid ~w~n",[Pid]),
  Pid ! {self(), hello},
%%   receive
%%     {_Pid, Msg} ->
%%       io:format("go() ~w ~w ~n",[_Pid, Msg])
%%   end,
  io:format("Antes del stop ~n"),
  Pid ! stop.


loop() ->
  receive
    {From, Msg} ->
      io:format("loop() ~w ~w ~n",[From, Msg]),
      From ! {self(), Msg},
      loop();
    stop ->
      io:format("Stop aqui! ~n"),
      true
  end.

dolphin() ->
  receive
    do_a_flip ->
      io:format("How about no?~n");
    fish ->
      io:format("So long and thanks for all the fish!~n");
    _ ->
      io:format("Heh, we're smarter than you humans.~n")
  end.

bump([]) -> [];
bump([Head|Tail]) -> [Head + 1| bump(Tail)].

some() ->
  call(some).

call(Msg) ->
  io:format("Msg ~w~n",[Msg]).

