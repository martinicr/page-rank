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

-define(ADJ_LIST, [[2,3,4],[1,4], [1], [2,3]]).
-define(K, 2).
-define(N, 4).
-define(BETA, 0).

get_number_of_processes(N, K) ->
  (N div K) * (N div K).


splitMatrix(_,[],L) -> lists:reverse(L);
splitMatrix(Col,[Adj|Rest],Result) ->
  NewResult = splitRow(Col,1.0/length(Adj),Adj,Result),
  splitMatrix(Col+1,Rest,NewResult).

splitRow(_,_,[],Result) -> Result;
splitRow(Col,Val,[Row|Rest],Result) -> splitRow(Col,Val,Rest,[{Row,Col,Val}|Result]).


princ() ->
  Vector = vector(?N, ?BETA),
  Proc_number = get_number_of_processes(length(?ADJ_LIST), ?K),

  REDUCE = spawn(pagerank, reduce_proc, [self()]),
  LOOP = spawn(pagerank, my_loop, [self(), dict:new(), REDUCE]),

  ReduceFileProc = spawn(pagerank, reduce_row_files,[]),
  SaveFileProc = spawn(pagerank, save_to_file, [[], ReduceFileProc]),

  Procs = create_processes_2(LOOP, Proc_number, SaveFileProc),

%%   io:format("Procs ~w~n",[Procs]),
  M = splitMatrix(1, ?ADJ_LIST, []),
  PP = spawn(pagerank, second, [self(), M, Procs, Vector]),

  receive
    {_Pid, stop} ->
      io:format("STOP! ~n")
  end,

  PP ! bye,
  LOOP ! stop,
  SaveFileProc ! stop.

%%   receive
%%     {result, Dict} ->
%%       REDUCE ! {reduce, Dict};
%%     {final, Final} ->
%%       Final
%%   end.


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
    spawn(pagerank, map_task,[ParentProc, SaveFileProc])
  end,
  lists:map(Fun, lists:seq(1,N)).

map_task(ParentProc, SaveFileProc) ->
  receive
    {_From, {R, C, Val}, Vj} ->
      Prob = Val * Vj,

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
        io:format("[~w] ~w ~n", [Idx, lists:sum(FL)]),


        file:close(S)
      end,
      lists:map(Fun, SL)
  end.


my_loop(ParentId, Dict, ReduceProc) ->
  receive
    {MapId, {R, C, Prob}} ->
%%        io:format("Receiving From ~w| R ~w, C ~w, Prob = ~w~n", [MapId, R, C, Prob]),
      New_Dict = dict:append(R, Prob, Dict),
%%       io:format("New Dict = ~w~n",[New_Dict]),
      my_loop(ParentId, New_Dict, ReduceProc);
    stop ->
%%       io:format("LOOP Bye ~n"),
%%       io:format("D ~w~n", [Dict])

%%       ParentId ! {result, Dict}

      ReduceProc ! {reduce, Dict}

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
      io:format("R = ~w~n",[R])
%%       From ! {final, R}
  end.

%%% ---------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IN MEMORY STRATEGY             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_memory_reducer() ->
    receive
      {reduce, Dict} ->
%%       io:format("Reduce ~n"),
      Dict_Keys = lists:sort(dict:fetch_keys(Dict)),
      Fun = fun(I) ->
        L = dict:fetch(I, Dict),
        {I, lists:sum(L)}
        end,
      R = lists:map(Fun, lists:seq(1, length(Dict_Keys))),
      io:format("R = ~w~n",[R])
    end.


in_memory_collector(Dict, ReduceProc) ->
    receive
      {_MapId, {R, C, Prob}} ->
%%        io:format("Receiving From ~w| R ~w, C ~w, Prob = ~w~n", [MapId, R, C, Prob]),
        New_Dict = dict:append(R, Prob, Dict),
        in_memory_collector(New_Dict, ReduceProc);
      stop ->
        ReduceProc ! {reduce, Dict}
    end.


in_memory_exec() ->
  fun() ->
    Reduce_Proc = spawn(pagerank, in_memory_reducer, []),
    spawn(pagerank, in_memory_collector, [dict:new(), Reduce_Proc])
  end.

in_memory_processes() ->
  fun(N, Reduce_Proc) ->
    Fun = fun(_) ->
      spawn(pagerank, in_memory_map_task,[Reduce_Proc])
    end,
    lists:map(Fun, lists:seq(1,N))
  end.

in_memory_map_task(Reduce_Proc) ->
  receive
    {_From, {R, C, Val}, Vj} ->
      Prob = Val * Vj,
      Reduce_Proc ! {self(), {R, C, Prob}},
      in_memory_map_task(Reduce_Proc)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FILE-BASED STRATEGY            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_file_name(Row_Idx) ->
  string:join([
    "row-",
    integer_to_list(Row_Idx),
    ".dat"
  ], "").

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro/1000).

text_file_reducer() ->
  receive
    {reduce, Rows, ParentPid} ->
      SL = lists:sort(dict:fetch_keys(Rows)),
      io:format("SL = ~w~n",[SL]),
      Fun = fun(Idx) ->

%%         Row_idx = integer_to_list(Idx),
%%         Filename = string:join(["row-",Row_idx,".dat"],""),
        Filename = dict:fetch(Idx, Rows),
        {ok, Binary} = file:read_file(Filename),
        S = string:tokens(binary_to_list(Binary), "\r\n\t "),
        Fun2 = fun(X) ->
          list_to_float(X)
        end,
        FL = lists:map(Fun2, S),
        io:format("[~w] ~w ~n", [Idx, lists:sum(FL)]),
        file:close(S),

        Row_idx = integer_to_list(Idx),
        Timestamp = integer_to_list(get_timestamp()),
        NewName = string:join(["row-", Row_idx, "-", Timestamp,".log"],""),

        file:rename(Filename, NewName),
%%         file:delete(Filename),


        lists:sum(FL)
      end,
      R = lists:map(Fun, SL),
      ParentPid ! {result, R}
  end.

text_file_collector(File_ids, ReduceProc) ->
  receive
    {save, {R, Prob}} ->
%%       Row_idx = integer_to_list(R),
%%       Filename = string:join(["row-",Row_idx,".dat"],""),

      Filename = gen_file_name(R),

      {ok, S} = file:open(Filename, [append]),
      io:format(S, "~w~n",[Prob]),
      file:close(S),

%%       case lists:member(R, File_ids) of
      case dict:is_key(R, File_ids) of
        true -> text_file_collector(File_ids, ReduceProc);
%%         false -> text_file_collector(lists:append([R], File_ids), ReduceProc)
        false -> text_file_collector(dict:append(R, Filename, File_ids), ReduceProc)
      end;

    {stop, ParentPid} ->
%%       io:format("stop save_to_file ~w ~n", [File_ids]),
      ReduceProc ! {reduce, File_ids, ParentPid}
  end.


file_based_exec() ->
  fun() ->
%%     Reduce_Proc = spawn(pagerank, text_file_reducer, []),
%%     spawn(pagerank, text_file_collector, [[], Reduce_Proc])
    Reduce_Proc = spawn('tres@mf-bbcom', fun() -> text_file_reducer() end),
    spawn('tres@mf-bbcom', fun() -> text_file_collector(dict:new(), Reduce_Proc) end)
  end.



file_based_processes(Nodes) ->
  fun(N, Reduce_Proc) ->
    Fun = fun(_) ->
%%       spawn(pagerank,file_based_map_task, [Reduce_Proc])
      Node = lists:nth(random:uniform(length(Nodes)), Nodes),
      io:format("Node ~w~n",[Node]),
      start_map_process(Node, Reduce_Proc)
    end,
    lists:map(Fun, lists:seq(1,N))
  end.

start_map_process(Node, Reduce_Proc) ->
  spawn(Node, fun() -> file_based_map_task(Reduce_Proc) end).

file_based_map_task(Reduce_Proc) ->
  receive
    {_From, {R, C, Val}, Vj} ->
      Prob = Val * Vj,
      Reduce_Proc ! {save, {R, Prob}},
      file_based_map_task(Reduce_Proc)
  end.

text(Vector) ->
  TxtRed = file_based_exec(),
  Mappers = file_based_processes(['dos@mf-bbcom']),
  tran_matrix(TxtRed, Mappers, Vector).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mapper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

msg(Pid) ->
  Pid ! {map, self(), "Hello"}.

kill_mapper(Pid) ->
  Pid ! stop.



%% loop() ->
%%   receive
%%     {map, Pid, Msg} ->
%%       io:format("Pid ~w, Msg ~w ~n",[Pid, Msg]);
%%     stop ->
%%       io:format("Stopping mapper! ~n")
%%   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tran_matrix(Reduce_Func, Proc_Func, Vector) ->
%%   Vector = vector(?N, ?BETA),
  Proc_number = get_number_of_processes(length(?ADJ_LIST), ?K),
  M = splitMatrix(1, ?ADJ_LIST, []),

  Reduce = Reduce_Func(),
  Procs = Proc_Func(Proc_number, Reduce),
%%   io:format("Procs = ~w~n",[Procs]),
  Dist = spawn(pagerank, distribute, [self(), M, Procs, Vector]),
  timer:sleep(1000),
  receive
    {_Pid, stop} ->
%%       io:format("STOP! ~n")
      done
  end,

  Dist ! bye,
  Reduce ! {stop, self()},
  receive
    {result, R} ->
      R
  end.


distribute(Parent, [], _, _) ->
  Parent ! {self(), stop};

distribute(Parent, [H|T], Procs, Vector) ->
  self() ! {process, H},
  receive
    {process, {Col, Row, Val}} ->
      N_K = ?N div ?K,
      Proc_Num = 1 + ((Col - 1) div ?K) * N_K + (Row - 1) div ?K,
      Proc = lists:nth(Proc_Num, Procs),
      Vj = lists:nth(Row, Vector),
      Proc ! {self(), {Col, Row, Val}, Vj},
%%         io:format("H ~w, PN ~w - ~w ~n", [H, Proc_Num, Proc]),
      distribute(Parent, T, Procs, Vector);
    bye ->
      io:format("bye ~n")
  end.


inmemory() ->
  MemRed = in_memory_exec(),
  Mappers = in_memory_processes(),
  Vector = vector(?N, ?BETA),
  tran_matrix(MemRed, Mappers, Vector).

