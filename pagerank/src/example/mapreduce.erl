-module(mapreduce).
-export([reduce_task/2,
  map_task/2,
  test_map_reduce/0,
  repeat_exec/2]).

%%% Execute the function N times,
%%%   and put the result into a list
repeat_exec(N,Func) ->
  lists:map(Func, lists:seq(0, N-1)).


%%% Identify the reducer process by
%%%   using the hashcode of the key
find_reducer(Processes, Key) ->
  Index = erlang:phash(Key, length(Processes)),
  lists:nth(Index, Processes).

%%% Identify the mapper process by random
find_mapper(Processes) ->
  case random:uniform(length(Processes)) of
    0 ->
      find_mapper(Processes);
    N ->
      lists:nth(N, Processes)
  end.

%%% Collect result synchronously from
%%%   a reducer process
collect(Reduce_proc) ->
  Reduce_proc ! {collect, self()},
  receive
    {result, Result} ->
      Result
  end.


%%% The reducer process
reduce_task(Acc0, ReduceFun) ->
  receive
    {reduce, {K, V}} ->
      io:format("GET = ~w, K = ~w, V = ~w ~n",[get(K), K,V]),
      Acc = case get(K) of
              undefined ->
                io:format("Undefined Acc0 = ~w, K = ~w, V = ~w ~n",[Acc0, K, V]),
                Acc0;
              Current_acc -> Current_acc
            end,
      io:format("2. K = ~w, V = ~w, Acc = ~w~n",[K,V,Acc]),
      put(K, ReduceFun(V, Acc)),
      reduce_task(Acc0, ReduceFun);
    {collect, PPid} ->
      PPid ! {result, get()},
      reduce_task(Acc0, ReduceFun)
  end.

%%% The mapper process
map_task(Reduce_processes, MapFun) ->
  receive
    {map, Data} ->
      IntermediateResults = MapFun(Data),
      io:format("Map function produce: ~w~n", [IntermediateResults ]),
      lists:foreach(
        fun({K, V}) ->
          Reducer_proc = find_reducer(Reduce_processes, K),
          Reducer_proc ! {reduce, {K, V}}
        end, IntermediateResults),

      map_task(Reduce_processes, MapFun)
  end.


%%% The entry point of the map/reduce framework
map_reduce(M, R, Map_func,
    Reduce_func, Acc0, List) ->

  %% Start all the reducer processes
  Reduce_processes =
    repeat_exec(R,
      fun(_) ->
        spawn(mapreduce, reduce_task,
          [Acc0, Reduce_func])
      end),

  io:format("Reduce processes ~w are started~n",
    [Reduce_processes]),

  %% Start all mapper processes
  Map_processes =
    repeat_exec(M,
      fun(_) ->
        spawn(mapreduce, map_task,
          [Reduce_processes, Map_func])
      end),

  io:format("Map processes ~w are started~n",
    [Map_processes]),

  %% Send the data to the mapper processes
  Extract_func =
    fun(N) ->
      Extracted_line = lists:nth(N+1, List),
      Map_proc = find_mapper(Map_processes),
      io:format("Send ~w to map process ~w~n",
        [Extracted_line, Map_proc]),
      Map_proc ! {map, Extracted_line}
    end,

  repeat_exec(length(List), Extract_func),

  timer:sleep(2000),

  %% Collect the result from all reducer processes
  io:format("Collect all data from reduce processes~n"),
  All_results =
    repeat_exec(length(Reduce_processes),
      fun(N) ->
        collect(lists:nth(N+1, Reduce_processes))
      end),
  lists:flatten(All_results).

%%% Testing of Map reduce using word count
test_map_reduce() ->
  M_func = fun(Line) ->
    lists:map(
      fun(Word) ->
        {Word, 1}
      end, Line)
  end,

  R_func = fun(V1, Acc) ->
    Acc + V1
  end,

  map_reduce(1, 5, M_func, R_func, 0,
    [[this, is, a, boy, boy],
      [this, is, a, girl],
      [this, is, lovely, boy],
      [hola, a, todos, los, presentes],
      [esta, es, una, prueba, de, texto]]).