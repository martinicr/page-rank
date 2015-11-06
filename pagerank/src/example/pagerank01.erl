-module(pagerank01).

-compile(export_all).

-define(MATRIX,[[3,7,2],[1,4],[2,5,8],[1,6,8,2],[1,2],[5],[8,1,4],[2]]).

splitMatrix(Matrix) -> splitMatrix(1, Matrix,[]).

splitMatrix(_,[],L) -> lists:reverse(L);
splitMatrix(Col,[Adj|Rest],Result) ->
	NewResult = splitRow(Col,1.0/length(Adj),Adj,Result),
	splitMatrix(Col+1,Rest,NewResult).

splitRow(_,_,[],Result) -> Result;
splitRow(Col,Val,[Row|Rest],Result) -> splitRow(Col,Val,Rest,[{Row,Col,Val}|Result]).

spawnMap(NodeList,K) ->
	list_to_tuple(spawnMap(NodeList,0,K,K*K)).

spawnMap(_,I,_,K2) when I >= K2 -> [];
spawnMap([{0,_}|Rest],I,K,K2) ->
	spawnMap(Rest,I,K,K2);
spawnMap([{N,Node}|Rest],I,K,K2) ->
  io:format("N ~w, Node ~w, I ~w, K ~w, K2 ~w ~n",[N, Node, I, K, K2]),
	[spawn(Node,pagerank01,mapTask,[self(),[]]) | spawnMap([{N-1,Node}|Rest],I+1,K,K2)].


mapTask(Receiver,Matrix) ->
	receive
		{entrada, Entrada} ->
			  mapTask(Receiver,[Entrada|Matrix]);
		bye -> io:format("time to go...~n",[]),
		       io:format("matriz = ~w~n", [Matrix])
	end.

%% distribuya(Tuplas,Procs,K)
distribuya([],_,_) -> ok;
distribuya([{Row,Col,Val}|Resto],Procs,K) ->
     NumProc = 1 + ((Row - 1) div K) * K + (Col - 1) div K,
     element(NumProc,Procs) ! {entrada, {Row,Col,Val}},
     distribuya(Resto,Procs,K).

main() -> splitMatrix(?MATRIX).

