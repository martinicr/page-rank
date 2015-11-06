%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2015 6:29 PM
%%%-------------------------------------------------------------------
-module(pagerank_example).
-author("martinflores").


%% API
-export([main/0, split_matrix/1, split_row/4]).

-define(MATRIX, [[3,7,2], [1,4], [2,5,8], [1,6,8,2], [1,2],[5],[8,1,4],[2]]).

main() ->
  split_matrix(?MATRIX).

%split_matrix
split_matrix(Matrix) -> split_matrix(1, Matrix,[]).

split_matrix(_,[],L) -> lists:reverse(L);
split_matrix(Col, [Adj|Rest], Result) ->
  NewResult = split_row(Col, 1.0/length(Adj), Adj, Result),
  split_matrix(Col + 1, Rest, NewResult).

% split_row
split_row(_,_,[], Result) -> Result;
split_row(Col,Val,[Row|Rest],Result) -> split_row(Col,Val,Rest,[{Row,Col,Val}|Result]).


% Procs = pagerank:spawn_map([{16,node()}],2)
spawn_map(NodeList, N_K) ->
  list_to_tuple(spawn_map(NodeList, 0, N_K, N_K * N_K)).

spawn_map(_,I,_,K2) when I >= K2 -> [];
spawn_map([{0,_}|Rest], I, N_K, N_K2) ->
  spawn_map(Rest, I, N_K, N_K2);
spawn_map([{N,Node}|Rest],I,K,K2) ->
  [spawn(Node, pagerank_example, map_task, [self(),1,0]) | spawn_map([{N-1, Node}|Rest], I=1,K,K2)].


map_task(Receiver, Matrix) ->
  receive
    {entrara, Entrada} ->
      map_task(Receiver, [Entrada|Matrix]);
    bye -> io:format("time to go... ~n", []),
            io:format("matriz = ~w~n", [Matrix])
  end.


% pagerank_example:distribuya(Mat, Procs, 2).
% element(2, Procs) ! bye. => obtiene el proceso y le envia el mensaje bye
distribuya([],_,_) -> ok;
distribuya([{Row,Col,Val}|Resto], Procs, N, K) ->
  N_K = N div k,
  NumProc = ((Row - 1) div K) * N_K + (Col -1) div K,
  element(NumProc, Procs) ! {entrada, {Row, Col, Val}},
  distribuya(Resto, Procs, K).

