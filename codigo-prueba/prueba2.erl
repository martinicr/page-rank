-module(prueba2).

-compile(export_all).

fact(0) -> 1;
fact(N) when N > 0 -> N * fact(N-1).

concat([],L) -> L;
concat([H|T],L) -> [H|concat(T,L)].



