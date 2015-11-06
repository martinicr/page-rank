Page Rank 
=========
Esta es una implementación básica del algoritmo de Page Rank en lenguaje Erlang de acuerdo al capítulo 6 del libro _Mining of Massive Datasets_ de Anan Rajaraman y Jeff Ullman.

## Ejecutar programa _PageRank_

### Local con modulos de `erlang`

#### Nodo "dos" (Mappers)
```
> erl -sname dos -setcookie pr
> c(pagerank).
```

#### Nodo "tres" (Reducer)
```
> erl -sname tres -setcookie pr
> c(pagerank).
```

#### Nodo "uno" (principal)
```
> erl -sname uno -setcookie pr
> c(pagerank).
> V = pagerank:prob().
> Vi = pagerank:prob(V).
```

### Ejecución parametrizada
> Nota: iniciar nodos "dos" y "tres" de la misma forma

```
> erl -sname uno -setcookie pr
> c(pagerank).
> Red_Proc = pagerank_file_based_exec('tres@mf-bbcom').
> Mappers = pagerank_file_based_processes(['dos@mf-bbcom']).
> V = [0.25,0.25,0.25,0.25].
> Adj_List = [[2,3,4],[1,4], [1], [2,3]].
> Vi = pagerank:rank(V, Adj_List, 4, 2, Red_Proc, Mappers).
```

### Nodos distribuidos

```
> vagrant up
------------------------------------
> vagrant ssh node2
> cd page-rank/pagerank/src
> git checkout multi-nodo
> erl -sname node2 -setcookie pr
> c(pagerank).
------------------------------------
> vagrant ssh node1
> cd page-rank/pagerank/src
> git checkout multi-nodo
> erl -sname node1 -setcookie pr
> c(pagerank).
------------------------------------
> vagrant ssh reduce
> cd page-rank/pagerank/src
> git checkout multi-nodo
> erl -sname reduce -setcookie pr
> c(pagerank).
------------------------------------
> vagrant ssh main
> cd page-rank/pagerank/src
> git checkout multi-nodo
> erl -sname reduce -setcookie pr
> c(pagerank).
> Red_Proc = file_based_exec('reducer@reducer-pr').
> Mappers = file_based_processes(?BETA, ['node1@node1-pr','node2@node2-pr'] ).
> V = [0.25,0.25,0.25,0.25].
> Adj_List = [[2,3,4],[1,4], [3], [2,3]].
> Vi = pagerank:rank(V, Adj_List, 4, 2, Red_Proc, Mappers).
```

