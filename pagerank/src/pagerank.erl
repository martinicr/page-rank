%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2015 11:58 PM
%%%-------------------------------------------------------------------
-module(pagerank).
-author("martinflores").

-compile([export_all]).
%% -define (TIMEOUT, 3000).
%%
%%
%% worker() ->
%%   receive
%%     do_work ->
%%       io:format("I (worker ~p) will work now~n", [self()]),
%%       worker()
%%
%%   after ?TIMEOUT ->
%%     io:format("I (worker ~p) have no work to do~n", [self()]),
%%     io:format("I (worker ~p) will die now ...~n", [self()]),
%%     exit(no_activity)
%%   end.
%%
%%
%%
%% parent() ->
%%
%%   Pid = spawn(pagerank, worker, []),
%%   register(worker, Pid),
%%   Reference = erlang:monitor(process, Pid),
%%
%%   io:format("I (parent) have a new worker~p~n", [Pid]),
%%   ?MODULE ! {new_worker, Pid},
%%   receive
%%     {'DOWN', Reference, process, Pid, Reason} ->
%%       io:format("I (parent) My worker ~p died (~p)~n", [Pid, Reason]),
%%       parent()
%%   end.
%%
%%
%% loop() ->
%%   receive
%%     {new_worker, WorkerPid} ->
%%       io:format("Inside Loop() ~n"),
%% %%       timer:sleep(?TIMEOUT-2000),
%%       WorkerPid ! do_work,
%%       loop()
%%   end.
%%
%%
%% start() ->
%%   Pid = spawn(pagerank, loop, []),
%%   io:format("Pid = ~w ~n",[Pid]),
%%   register(?MODULE, Pid),
%%
%%   ParentPid = spawn(pagerank, parent, []),
%%   io:format("ParentPid = ~w ~n",[ParentPid]),
%%   register(parent, ParentPid),
%%
%%
%%   Ref = erlang:monitor(process, Pid),
%%   erlang:demonitor(Ref),
%%
%%   timer:sleep(round(?TIMEOUT*1.5)),
%%   exit(whereis(worker), finished),
%%   exit(whereis(parent), finished),
%%   exit(whereis(?MODULE), finished),
%%   ok.

%% -define(MAPPERS, 1).
%% -define(REDUCERS, 1).

%% start() ->
%%   Pid = spawn(pagerank, loop, []),
%%   Pid ! {self(), "Mundo Loco"},
%%   timer:sleep(2000),
%%   Pid ! exit,
%%   ok.

%%   Map_func = fun(Line) ->
%%     lists:map(
%%       fun(Item) ->
%%         {item, Item}
%%       end, Line)
%%     end,

%%   Map_func = fun(Line) ->
%%     lists:map(
%%       fun(Word) ->
%%         {Word, 1}
%%       end, Line)
%%   end,

%%   Red_func = fun(V1, Acc) ->
%%     Acc + V1
%%   end,
%%
%%   Reduce_processes = create_reducers(?REDUCERS, Red_func),
%%   Map_processes = create_mappers(?MAPPERS, Map_func, Reduce_processes),
%%   send_work(?LIST, Map_processes),
%%   io:format("Collect all data from reduce processes~n"),
%%   All_results =
%%     repeat_exec(length(Reduce_processes),
%%       fun(N) ->
%%         collect(lists:nth(N+1, Reduce_processes))
%%       end),
%%   lists:flatten(All_results).
%%
%% loop() ->
%%   io:format("En Loop() ~n"),
%%   receive
%%     {Pid, Msg} ->
%%       io:format("Pid ~w, Msg ~w ~n",[Pid, Msg]),
%%       loop();
%%     exit ->
%%       exit
%%   end.
%%
%% create_mappers(N, Map_func, Reduce_processes) ->
%% %%   io:format("Mappers N = ~w ~n",[N]),
%% %%   io:format("R.P. = ~w~n",[Reduce_processes]),
%%   Fun = fun(_) -> spawn(pagerank, map_task, [Map_func, Reduce_processes]) end,
%%   repeat_exec(N, Fun).
%%
%% create_reducers(N, Red_func) ->
%% %%   io:format("Reducers N = ~w ~n", [N]),
%%   Fun = fun(_) -> spawn(pagerank, reduce_task, [0, Red_func]) end,
%%   repeat_exec(N, Fun).
%%
%% send_work(List, Map_processes) ->
%%
%%   Extract_func =
%%     fun(N) ->
%%       Extracted_line = lists:nth(N+1, List),
%%       Map_proc = find_mapper(Map_processes),
%% %%       io:format("Send ~w to map process ~w~n", [Extracted_line, Map_proc]),
%%       Map_proc ! {map, Extracted_line}
%%     end,
%%
%%     repeat_exec(length(List), Extract_func).
%%
%% map_task(Map_func, Reduce_processes) ->
%%   receive
%%     {map, Data} ->
%%       IntermediateResults = Map_func(Data),
%% %%       io:format("Map function produce: ~w~n", [IntermediateResults ]),
%%       lists:foreach(
%%         fun({K, V}) ->
%%           Reducer_proc = find_reducer(Reduce_processes, K),
%% %%           io:format("Reducer proc = ~w~n",[Reducer_proc]),
%%           Reducer_proc ! {reduce, {K, V}}
%%         end, IntermediateResults),
%%
%%       map_task(Map_func, Reduce_processes);
%%     stop ->
%%       stop
%%   end.
%%
%% reduce_task(Acc0, Red_func) ->
%%   receive
%%     {reduce, {K, V}} ->
%%       io:format("GET = ~w, K = ~w, V = ~w ~n",[get(K), K,V]),
%%       Acc = case get(K) of
%%               undefined ->
%%                 io:format("Undefined Acc0 = ~w, K = ~w, V = ~w ~n",[Acc0, K, V]),
%%                 Acc0;
%%               Current_acc ->
%%                 io:format("K = ~w, V = ~w CU.ACC = ~w~n",[K, V, Current_acc]),
%%                 Current_acc
%%             end,
%%       io:format("2. K = ~w, V = ~w, Acc = ~w~n ------- ~n",[K,V,Acc]),
%%       put(K, Red_func(V, Acc)),
%%       reduce_task(Acc0, Red_func);
%%     {collect, PPid} ->
%%       PPid ! {result, get()},
%%       reduce_task(Acc0, Red_func)
%%   end.
%%
%%
%% repeat_exec(N, Func) ->
%%   lists:map(Func, lists:seq(0, N-1)).
%%
%% find_mapper(Processes) ->
%%   case random:uniform(length(Processes)) of
%%     0 -> find_mapper(Processes);
%%     N -> lists:nth(N, Processes)
%%   end.
%%
%% find_reducer(Processes, Key) ->
%%   Index = erlang:phash(Key, length(Processes)),
%%   lists:nth(Index, Processes).
%%
%%
%% collect(Reduce_proc) ->
%%   Reduce_proc ! {collect, self()},
%%   receive
%%     {result, Result} ->
%%       Result
%%   end.
-define(ADJ_LIST, [[2,3,4],[1,4], [1], [2,3]]).
-define(K, 2).
-define(N, 4).
-define(BETA, 0).

matrix_cols(M) ->
  lists:seq(1, length(M)).

col_prob(Col_idx, Col, Prob_Func, Dist_Func, Procs) ->
  Col_length = length(Col),
  col_prob(Col_idx, Col, Col_length, Prob_Func, Dist_Func, Procs).

col_prob(Col_idx, [R|T], Col_length, Prob_Func, Dist_Func, Procs) ->
%%   io:format("R ~w, Col_idx = ~w ~n",[R,Col_idx]),
  Proc_number = dist({R, Col_idx, Col_length}, Dist_Func),
  Map_proc = lists:nth(Proc_number, Procs),
  Map_proc ! {self(), {R, Col_idx, Col_length}},
  [{R, Col_idx, Prob_Func(Col_length)} | col_prob(Col_idx, T, Col_length, Prob_Func, Dist_Func, Procs)];
col_prob(_, [], _, _, _, _) -> [].


transition_matrix() ->
  Proc_number = get_number_of_processes(length(?ADJ_LIST), ?K),
  Procs = create_processes(self(), Proc_number),
  Func = calc_matrix_prob(?ADJ_LIST, ?K, Procs),
%%   LoopId = spawn(pagerank, loop, []),
%%   io:format("LoopId ~w~n",[LoopId]),
  transition_matrix(matrix_cols(?ADJ_LIST), Func, self()),
  loop(dict:new()).


%%   Dict_Results = loop(dict:new()),
%%   Dict_Keys = lists:sort(dict:fetch_keys(Dict_Results)),
%%   Fun = fun(I) ->
%%       L = dict:fetch(I, Dict_Results),
%%       lists:sum(L)
%%     end,
%%   lists:map(Fun, lists:seq(1, length(Dict_Keys))).

transition_matrix(Cols, Func, ParentId) ->
  lists:map(Func, Cols).



loop(Dict) ->
  io:format("Dict Keys ~w~n",[dict:fetch_keys(Dict)]),
  receive
    {MapId, {R, C, Prob}} ->
      io:format("Receiving From ~w| R ~w, C ~w, PROB = ~w~n", [MapId, R, C, Prob]),
      New_Dict = dict:append(R, Prob, Dict),
      io:format("Val ~w~n",[dict:fetch(R, New_Dict)]),
      loop(New_Dict);
    stop ->
      io:format("Bye! ~n"),
      Dict_Proc = spawn(pagerank, reduce, []),
      Dict_Proc ! {reduce, self(), Dict},
      loop(Dict);
    {result, R} ->
        io:format("Result!! ~w~n",[R]),
        R
  end.

reduce() ->
  receive
    {reduce, From, Dict} ->
      io:format("Reduce | From ~w ~n", [From]),
      Dict_Keys = lists:sort(dict:fetch_keys(Dict)),
      Fun = fun(I) ->
        L = dict:fetch(I, Dict),
%%         io:format("Reduce | L ~w ~n", [L]),
        {I, lists:sum(L)}
      end,
      R = lists:map(Fun, lists:seq(1, length(Dict_Keys))),
      io:format("R = ~w~n",[R]),
      From ! {result, R}
  end.

calc_matrix_prob(M, K, Procs) ->
  fun(Idx) ->
    Row = lists:nth(Idx, M),
    Prob_Func = prob(),
    Dist_Func = dist_func(K),
    col_prob(Idx, Row, Prob_Func, Dist_Func, Procs)
  end.

prob() ->
  fun(N) ->
    1.0 / N
  end.

dist_func(K) ->
  fun(Row, Col, Col_length) ->
    N_K = Col_length div K,
    1 + ((Col - 1) div K) * N_K + (Row - 1) div K
  end.

map_task_spawn() ->
  fun(_) ->
    spawn(pagerank, map_task,[self()])
  end.


dist({Row, Col, Col_length}, Dist_func) ->
%%   io:format("Row ~w, Col ~w, Col_Length ~w, Dist_func ~w ~n",
%%     [Row, Col, Col_length, Dist_func(Row, Col, Col_length)]),
  Dist_func(Row, Col, Col_length).



create_processes(ParentProc, N) ->
  Fun = fun(_) ->
    spawn(pagerank, map_task,[ParentProc])
  end,
  lists:map(Fun, lists:seq(1,N)).

%% map_task(ParentProc) ->
%%   receive
%%     {_From, {R, C, C_length}} ->
%%       Prob_Func = prob(),
%%       Prob = Prob_Func(C_length),
%%       ParentProc ! {self(), {R, C, Prob}},
%%       map_task(ParentProc)
%%   end.


get_number_of_processes(N, K) ->
  (N div K) * (N div K).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% distribute() ->
%%   T = [{2,1,0.3},{3,1,0.3},{4,1,0.5},{4,3,0.5}],
%%   distribute(T, 4, 2).
%%

splitMatrix() ->
  M = splitMatrix(1, ?ADJ_LIST, []),
  Proc_number = get_number_of_processes(length(?ADJ_LIST), ?K),
  Procs = create_processes(self(), Proc_number),
  distribute(M, Procs, 8, 2).

splitMatrix(_,[],L) -> lists:reverse(L);
splitMatrix(Col,[Adj|Rest],Result) ->
  NewResult = splitRow(Col,1.0/length(Adj),Adj,Result),
  splitMatrix(Col+1,Rest,NewResult).

splitRow(_,_,[],Result) -> Result;
splitRow(Col,Val,[Row|Rest],Result) -> splitRow(Col,Val,Rest,[{Row,Col,Val}|Result]).


distribute([],_,_,_) -> ok;
distribute([{Row,Col,Val}|Resto],Procs,N,K) ->
  N_K = N div K,
  NumProc = 1 + ((Col - 1) div K) * N_K + (Row - 1) div K,
  element(NumProc,Procs) ! {entrada, {Row,Col,Val}},
  distribute(Resto,Procs,N,K).


princ() ->
  Vector = vector(?N, ?BETA),
  Proc_number = get_number_of_processes(length(?ADJ_LIST), ?K),
  REDUCE = spawn(pagerank, reduce_proc, [self()]),
  LOOP = spawn(pagerank, my_loop, [self(), dict:new()]),

  ReduceFileProc = spawn(pagerank, reduce_row_files,[]),
  SaveFileProc = spawn(pagerank, save_to_file, [[], ReduceFileProc]),

  Procs = create_processes_2(LOOP, Proc_number, SaveFileProc),
  io:format("Procs ~w~n",[Procs]),
  M = splitMatrix(1, ?ADJ_LIST, []),
  PP = spawn(pagerank, second, [self(), M, Procs, Vector]),

  receive
    {_Pid, stop} ->
      io:format("STOP! ~n")
  end,

  PP ! bye,
  LOOP ! stop,
  SaveFileProc ! stop,

  receive
    {result, Dict} ->
      REDUCE ! {reduce, Dict};
    {final, Final} ->
      Final
  end.


second(Parent, [], _, _) ->
  Parent ! {self(), stop};

second(Parent, [H|T], Procs, Vector) ->
  self() ! {process, H},
  receive
    {process, {Col, Row, Val}} ->
       N_K = ?N div ?K,
       Proc_Num = 1 + ((Col - 1) div ?K) * N_K + (Row - 1) div ?K,
       Proc = lists:nth(Proc_Num, Procs),
       Vj = lists:nth(Col, Vector),
       Proc ! {self(), {Col, Row, Val}, Vj},
%%        io:format("H ~w, PN ~w - ~w ~n", [H, Proc_Num, Proc]),
       second(Parent, T, Procs, Vector);
    bye ->
      io:format("bye ~n")
  end.


create_processes_2(ParentProc, N, SaveFileProc) ->
  Fun = fun(_) ->
%%     Map_idx = integer_to_list(Idx),
%%     Filename = string:join(["map-",Map_idx,".dat"],""),
%%     file:write_file(Filename,[]),
    spawn(pagerank, map_task,[ParentProc, SaveFileProc])
  end,
  lists:map(Fun, lists:seq(1,N)).

map_task(ParentProc, SaveFileProc) ->
  receive
    {_From, {R, C, Val}, Vj} ->
      Prob = Val * Vj,

      %% Writing to a file
%%       Row_idx = integer_to_list(R),
%%       Filename = string:join(["row-",Row_idx,".dat"],""),
%%       io:format("Me ~w, Filename ~w, Prob ~w~n",[self(), Filename, Prob]),


      ParentProc ! {self(), {R, C, Prob}},
      SaveFileProc ! {save, {R, Prob}},
      map_task(ParentProc, SaveFileProc)
  end.

save_to_file(Files, ReduceFileProc) ->
  receive
    {save, {R, Prob}} ->
      Row_idx = integer_to_list(R),
      Filename = string:join(["row-",Row_idx,".dat"],""),
      {ok, S} = file:open(Filename, [append]),
      io:format(S, "~w~n",[Prob]),
      file:close(S),

      case lists:member(R, Files) of
        true -> save_to_file(Files, ReduceFileProc);
        false -> save_to_file(lists:append([R], Files), ReduceFileProc)
      end;

%%       save_to_file(Files);
    stop ->
%%       io:format("stop save_to_file ~w ~n", [Files])
      ReduceFileProc ! {reduce, Files}
  end.

reduce_row_files() ->
  receive
    {reduce, Rows} ->
      SL = lists:sort(Rows),
      Fun = fun(Idx) ->
        Row_idx = integer_to_list(Idx),
        Filename = string:join(["row-",Row_idx,".dat"],""),
        {ok, Binary} = file:read_file(Filename),
        S = string:tokens(binary_to_list(Binary), "\r\n\t "),
        Fun2 = fun(X) ->
          list_to_float(X)
        end,
        FL = lists:map(Fun2, S),
        io:format("Idx ~w, Total ~w ~n", [Idx, lists:sum(FL)]),


        file:close(S)
      end,
      lists:map(Fun, SL)
  end.


my_loop(ParentId, Dict) ->
  receive
    {MapId, {R, C, Prob}} ->
%%       io:format("Receiving From ~w| R ~w, C ~w, Prob = ~w~n", [MapId, R, C, Prob]),
      New_Dict = dict:append(R, Prob, Dict),
%%       io:format("New Dict = ~w~n",[New_Dict]),
      my_loop(ParentId, New_Dict);
    stop ->
%%       io:format("LOOP Bye ~n"),
%%       io:format("D ~w~n", [Dict])
      ParentId ! {result, Dict}
  end.


vector(N, Beta) ->
  Fun = fun(_) ->
    (1 - Beta) / N
  end,
  lists:map(Fun, lists:seq(1, N)).


reduce_proc(From) ->
  receive
    {reduce, Dict} ->
      io:format("Reduce ~n"),
      Dict_Keys = lists:sort(dict:fetch_keys(Dict)),
      Fun = fun(I) ->
        L = dict:fetch(I, Dict),
        {I, lists:sum(L)}
      end,
      R = lists:map(Fun, lists:seq(1, length(Dict_Keys))),
      io:format("R = ~w~n",[R]),
      From ! {final, R}
  end.
