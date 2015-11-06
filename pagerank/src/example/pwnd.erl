-module(pwnd).

-export([startSay/0, say/2]).
-export([chit/2, chat/0, startChitChat/0]).
-export([vSum/2, vMul/2, vVec/3, vStart/2, vSpawn/1, vReduce/2, vScatter/5, vWorker/1]).

%% -import(lists).

%
% Simple example of communication
%

say(What, Pid) ->
  io:format("I got a call from ~w saying '~w'. I am PID ~w.~n",
    [Pid, What, self()]).

startSay() ->
  PID1 = spawn(pwnd, say, [hello, self()]),
  PID2 = spawn(pwnd, say, [[fun(X)->2*X end, 3.1415, 0], self()]),
  io:format("I started ~w and ~w.~n", [PID1, PID2]).

%
% A litte example of communication
%

chat() ->
  receive
    yawn ->
      io:format("I think everyone yawned...~n", []);
    {What, PID} ->
      io:format("Process PID ~w wanted to say '~w'. I am PID ~w.~n",
        [PID, What, self()]),
      PID ! {proceed, self()},
      chat()
  end.

chit(0, ChatPID) ->
  ChatPID ! yawn,
  io:format("I stop working now!~n", []);
chit(N, ChatPID) ->
  ChatPID ! {[N, times, fun(X)->2*X end, 3.1415, 0], self()},
  chit(N - 1, ChatPID).


startChitChat() ->
  CHAT = spawn(pwnd, chat, []),
  CHIT = spawn(pwnd, chit, [10, CHAT]),
  io:format("Started chit PID ~w and chat PID ~w, and I am ~w.~n",
    [CHIT, CHAT, self()]).

%
% Matrix-Vector multiplication, simple example
%

vSum([], []) ->
  [];
vSum([H1 | T1], [H2 | T2]) ->
  [H1 + H2] ++ vSum(T1, T2).

vMul([], []) ->
  [];
vMul([H1 | T1], [H2 | T2]) ->
  [H1 * H2] ++ vMul(T1, T2).

vVec(N, I, EL)->
  ZERO = lists:map(fun(X)->0*X end, lists:seq(1, N)),
  lists:sublist(ZERO, I - 1) ++ [EL] ++ lists:sublist(ZERO, N - I).

vWorker(PID) ->
  receive
    {V1, V2, N} ->
      P = vVec(length(V1), N, lists:sum(vMul(V1, V2))),
      io:format("[~w/~w] V1=~w * V2=~w == ~w~n", [self(), N, V1, V2, P]),
      PID ! {vector, P}
  end.

vScatter([], V, [], 0, TOT) ->
  [];
vScatter([MH | MT], V, [PID | REST], N, TOT) ->
  PID ! {MH, V, TOT - N + 1},
  vScatter(MT, V, REST, N - 1, TOT).

vReduce(RES, 0) ->
  RES;
vReduce(RES, N) ->
  receive
    {vector, V} ->
      vReduce(vSum(RES, V), N - 1)
  end.

vSpawn([]) ->
  [];
vSpawn([H | T]) ->
  [spawn(pwnd, vWorker, [self()])] ++ vSpawn(T).

vStart(M, V) ->
  ZERO = lists:map(fun(X)->0*X end, lists:seq(1, length(V))),
  PIDS = vSpawn(lists:seq(1, length(V))),
  vScatter(M, V, PIDS, length(PIDS), length(PIDS)),
  vReduce(ZERO, length(V)).
