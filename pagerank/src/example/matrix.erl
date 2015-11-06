%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Oct 2015 11:02 PM
%%%-------------------------------------------------------------------
-module(matrix).
-author("martinflores").

%% API
-compile(export_all).


-define(MATRIX, [[2,3,4],[1,4], [1], [2,3]]).
-define(VECTOR, [1/20,1/20,1/20,1/20]).
-define(BETA, 4/5).

%% main() ->
%%   Main = spawn(matrix, loop, []),
%%   Workers = create_workers(Main),
%%   io:format("Main ~w, Workers ~w~n",[Main, Workers]),
%%   send_work(Workers),
%%   timer:sleep(3000),
%%   Main ! bye.
%%
%%
%% loop() ->
%%   receive
%%     {Pid, Msg} ->
%%       io:format("Parent. Pid ~w, Msg ~w~n", [Pid, Msg]),
%%       loop();
%%     bye ->
%%       io:format("Stopping now "),
%%       bye
%%   end.
%%
%%
%% create_workers(MainPid) ->
%%   SpawnFunc = fun(_) ->
%%        spawn(matrix, worker, [MainPid])
%%     end,
%%   lists:map(SpawnFunc, lists:seq(1, length(?MATRIX))).
%%
%% send_work([H|T]) ->
%%   H ! {work, "Work!!!"},
%%   send_work(T);
%% send_work([]) -> [].
%%
%% worker(ParentPid) ->
%%   receive
%%     {Pid, Msg} ->
%%       io:format("Worker. Pid = ~w, Msg = ~w~n",[Pid, Msg]),
%%       ParentPid ! {self(), "Respuesta de Worker"}
%%   end.




create_workers(Pid) ->
  Fun = fun(_) ->
    spawn(matrix, worker, [Pid])
  end,
  lists:map(Fun, lists:seq(1, length(?MATRIX))).

send_work(Workers) ->
%%   io:format("BETA = ~w~n",[?BETA]),
  SM = split_matrix(?MATRIX),
  M = matrix_with_values(split_matrix(SM)),
  io:format("SM = ~w~n M = ~w~n",[SM, M]),
  M.
%%   do_work(M, Workers),
%%   loop([], length(?VECTOR)).

do_work([H|T], [W|Rest]) ->
%%   io:format("H ~w - T ~w, W ~w, Rest ~w~n",[H,T,W,Rest]),
  W ! {work, H},
  do_work(T, Rest);
do_work([],[]) -> [].

loop(Results, VectorLength) ->
  Len = length(Results) == VectorLength,
%%   io:format("Results = ~w, VecL = ~w~n",[Results, VectorLength]),
  case Len of
    false -> Len;
    true ->
      io:format("CASE VL = ~w, R = ~w ~n", [Results, VectorLength]),
      self() ! stop
  end,
  receive
    {From, Result} ->
      io:format("From ~w, Result ~w~n",[From, Result]),
      From ! ack,
      loop(Results ++ [Result], VectorLength);
    stop ->
      Results
  end.

worker(Pid) ->
  receive
    {work, R} ->
%%       io:format("Pid ~w~n",[Pid]),
%%       io:format("Entro work normal ~n"),
      Pid ! {self(), calc_row(R)};
    done ->
      io:format("Entro done ~n"),
      Pid ! stop;
    ack ->
      roger
  end.

calc_row(L) ->
  TaxM = apply_taxation(L),
  io:format("TaxM = ~w~n",[TaxM]),
  Row_Results = multiply_row_vector(TaxM, ?VECTOR),
%%   lists:sum(Row_Results).
%%   io:format("L = ~w~n",[lists:map(fun(X) -> X * ?BETA end, L)]),
  lists:sum(Row_Results).

multiply_row_vector([],[]) -> [];
multiply_row_vector([H|T], [V1|V2]) ->
  io:format("H ~w, V1 ~w, T ~w, V2 ~w ~n",[H,V1,T,V2]),
  [H * V1] ++ multiply_row_vector(T, V2).

apply_taxation(L) ->
  Fun = fun(X) -> X * ?BETA end,
  lists:map(Fun, L).


main() ->
  M = split_matrix(?MATRIX),
  matrix_with_values(M).

split_matrix(Matrix) ->
  split_matrix(1, Matrix).

split_matrix(_,[]) -> [];
split_matrix(Col,[Adj|Rest]) ->
  NewResult = split_row(Col, 1.0 / length(Adj), Adj),
  [NewResult] ++ split_matrix(Col + 1 ,Rest).

split_row(_,_,[]) -> [];
split_row(Col,Val,[Row|Rest]) ->
  [{Row, Col, Val}] ++ split_row(Col, Val, Rest).

matrix_with_values([H|T]) ->
  [row_values(H)] ++ matrix_with_values(T);
matrix_with_values([]) -> [].

row_values([H|T]) ->
  [extract_value(H)] ++ row_values(T);
row_values([]) -> [].


extract_value({_, _, V}) -> V.




%%%%%%%%%%%%

splitMatrix() -> splitMatrix(1, ?MATRIX,[]).

splitMatrix(_,[],L) -> lists:reverse(L);
splitMatrix(Col,[Adj|Rest],Result) ->
  NewResult = splitRow(Col,1.0/length(Adj),Adj,Result),
  splitMatrix(Col+1,Rest,NewResult).

splitRow(_,_,[],Result) -> Result;
splitRow(Col,Val,[Row|Rest],Result) -> splitRow(Col,Val,Rest,[{Row,Col,Val}|Result]).

%% calc() ->
%%   Adj = splitMatrix(),
%%   convert_to_row(Adj).

%% convert_to_row(N, [H|T]) ->
%%   Func  = fun(I) ->
%%       case I == N of
%%        true ->
%%     end.



%% ext(Inx) ->
%%   fun(E) ->
%%     io:format("idx = ~w~n",[Idx]),
%%     [X || X <- [1,2,a,3,4,b,5,6], X == 3].
%%
%% %%     I = element(1, E),
%% %%     case I == Idx of
%% %%       true  -> E;
%% %%       false -> 0
%% %%     end
%%   end.

rows() ->
  Pred = indexPred(),
  M = splitMatrix(),
  rows(M, [1,2,3,4], Pred, []).
%%   transition_matrix(Rows, [1,2,3,4], Pred).

rows(_, [], _, Result)      -> lists:reverse(Result);
rows(M, [H|T], Pred, Result) ->
  rows(M, T, Pred, [single_row(M, H, Pred)|Result]).

single_row(M, Idx, Pred) ->
  [X || X <- M, Pred(Idx,X)].

indexPred() ->
  fun(Idx, E) ->
    element(1, E) == Idx
  end.

columnPred() ->
  fun(Idx, E) ->
    io:format("Idx = ~w, E ~w~n",[Idx,E]),
    element(2, E) == Idx
  end.




%% transition_matrix(_, [], _, Result) -> Result;
%% transition_matrix(Rows, V, Pred, Result) ->
%%   Line = lists:nth(H, Rows),
%%   check(Line, ).


%%   io:format("Rows ~w, H ~w, Line ~w,  Results ~w ~n", [Rows, H, Line, Result]),
%%   C = case has_column(Line, H, Pred) of
%%         true ->  1;
%%         false -> 0
%%       end,

%%
%%   transition_matrix(Rows, V, Pred, [C|Result]).


check(_, [], Result) -> Result;
check(Line, [VH|VT], Result) ->
  R = case lists:member(VH, Line) of
    true -> lists:nth(VH, Line);
    false -> 0
  end,
  io:format("Line ~w, VH ~w, VT ~w, R ~w ~n",[Line, VH, VT, R]),
  check(Line, VT, [R|Result]).

%% check_2([], _, R) -> R;
%% check_2([H|T], I, R) ->
check_2([_], _, true, Acc) -> Acc;
check_2([_|T], I, false, Acc) ->
  io:format("2 || I ~w, Acc ~w ~n",[T, Acc]),
  check_2(T, I, Acc + 1).

check_2([], _, _) -> 0;
check_2([H|T], I, Acc) ->
  Result = H == I,
  io:format("T ~w, I ~w, Result ~w, Acc ~w ~n",[T, I, Result, Acc]),
  check_2(T, I, Result, Acc).