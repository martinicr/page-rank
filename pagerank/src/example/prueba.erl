-module(prueba).

-compile(export_all).

fact(0) -> 1;
fact(N) when N > 0 -> N * fact(N-1).

fib(N) ->
       if
	  N == 0 -> 0;
	  N == 1 -> 1;
	  N > 1  -> fib(N-1) + fib(N-2);
	  true -> io:format("por aqui nunca ~w entrar~n",["debe"])
       end.

proc() ->
       receive
		{hola, S} -> S ! hola, proc();
		{bye, S}  -> S ! hasta_luego 
       end.

primo(N) when N < 2 -> false;
primo(2) -> true;
primo(N) -> primoAux(N,2,math:sqrt(N)).

primoAux(_,I,Sqrt) when I > Sqrt -> true;
primoAux(N,I,_) when (N rem I) == 0 -> false;
primoAux(N,I,Sqrt) -> primoAux(N,I+1,Sqrt).

rango(I,N) when I > N -> [];
rango(I,N) -> [I|rango(I+1,N)].

calcPrimos(N) -> 
	      spawnPrimos(lists:reverse(rango(2,N))),
	      receivePrimos([],N-1).

hola () ->
     io:format("hola~n").

spawnPrimos([]) -> ok;
spawnPrimos([N|Resto]) ->
	spawn(prueba,calcPrimo,[N,self()]),
	spawnPrimos(Resto).

calcPrimo(N,Master) -> Master ! {N, primo(N)}.

receivePrimos(L,0) -> L;
receivePrimos(L,N) ->
        receive
		{P, true}  -> receivePrimos([P|L], N-1);
		{_, false} -> receivePrimos(L, N-1)		
	end.












