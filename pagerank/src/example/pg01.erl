-module(pg01).

-compile(export_all).

-define(MATRIX,[[3,7,2],[1,4],[2,5,8],[1,6,8,2],[1,2],[5],[8,1,4],[2]]).

splitMatrix(Matrix) ->splitMatrix(1,Matrix,[]).

splitMatrix(_,[],L) -> lists:reverse(L);
splitMatrix(Col,[Adj|Rest],Result) ->
	NewResult = splitRow(Col,1.0/length(Adj),Adj,Result),
	splitMatrix(Col+1,Rest,NewResult).

splitRow(_,_,[],Result) -> Result;
splitRow(Col,Val,[Row|Rest],Result) -> splitRow(Col,Val,Rest,[{Row,Col,Val}|Result]).

spawnMap(NodeList,N,K) ->
        N_K = N div K,
	list_to_tuple(spawnMap2(NodeList,0,N_K*N_K)).

spawnMap2(_,I,K2) when I >= K2 -> [];
spawnMap2([{0,_}|Rest],I,N_K2) ->
	spawnMap2(Rest,I,N_K2);
spawnMap2([{N,Node}|Rest],I,N_K2) ->
	[spawn_link(Node,?MODULE,mapTask,[self(),[]]) | spawnMap2([{N-1,Node}|Rest],I+1,N_K2)].

mapTask(Receiver,Matrix) ->
	receive
		{entrada, Entrada} ->
			  mapTask(Receiver,[Entrada|Matrix]);
		{vector, ListaVector, I} ->
			Vector = list_to_tuple(ListaVector),
			Receiver ! {mult,lists:map(
				    fun({F,C,Val}) -> {F,element(C-I+1,Vector)*Val} end, Matrix)
				   };

		bye -> io:format("time to go...~n",[]),
		       io:format("matriz = ~w~n", [Matrix])
	end.

%% distribuya(Tuplas,Procs,N,K)
distribute([],_,_,_) -> ok;
distribute([{Row,Col,Val}|Resto],Procs,N,K) ->
     N_K = N div K,
     NumProc = 1 + ((Col - 1) div K) * N_K + (Row - 1) div K,
     element(NumProc,Procs) ! {entrada, {Row,Col,Val}},
     distribute(Resto,Procs,N,K).

main() -> 
       M = splitMatrix(?MATRIX),
       Procs = spawnMap([{16,node()}],8,2),
       distribute(M,Procs,8,2),
       multiply(Procs,[1,1,1,1,1,1,1,1],8,2),
       Result = lists:map(fun(_) -> receive X -> X end end, range(1,16)),
       {M,Procs,Result}.

%% multiply(M,Vec,N,K)
first(0,V) -> {[],V};
first(N,[A|Rest]) ->
    {First,Last} = first(N-1,Rest),
    {[A|First],Last}.

range(A,B) when A > B -> [];
range(A,B) -> [A|range(A+1,B)].

multiply(M,V,N,K) ->
    multiply(1,tuple_to_list(M),V,N,K).

reduce(N) -> reduce(N,[]).
reduce(0,M) -> M;
reduce(N,SoFar) ->
    receive
	{mult,_,List} -> reduce(N-1,List++SoFar)
    end.

multiply(_,[],[],_,_) -> done;
multiply(I,Procs,V,N,K) ->
    {P,PRest}  = first(N div K,Procs),
    {V1,VRest} = first(K,V),
    lists:map(fun(Proc) -> Proc ! {vector,V1,I} end,P),
    multiply(I+K,PRest,VRest,N,K).
