-module(taxation).
-author("Martin Flores").

%% API
-compile([export_all]).


-define(ADJ_LIST, [[2,3,4],[1,4], [3], [2,3]]).
-define(K, 2).
-define(N, 4).
-define(BETA, 0.8).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REDUCE PROCESS: BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
text_file_reducer() ->
  receive
    {reduce, Rows, ParentPid} ->
      SL = lists:sort(dict:fetch_keys(Rows)),
      Fun = fun(Idx) ->
        Filename = dict:fetch(Idx, Rows),
        {ok, Binary} = file:read_file(Filename),
        S = string:tokens(binary_to_list(Binary), "\r\n\t "),
        Fun2 = fun(X) ->
          list_to_float(X)
        end,
        FL = lists:map(Fun2, S),
        file:close(S),

        Row_idx = integer_to_list(Idx),
        Timestamp = integer_to_list(get_timestamp()),
        NewName = string:join(["row-", Row_idx, "-", Timestamp,".log"],""),
        file:rename(Filename, NewName),


        Ones = gen_one_vector(length(SL) - length(FL)),
%%         io:format("FL ~w, Ones ~w ~n",[FL, Ones]),

        lists:sum(lists:append(FL, Ones))
      end,
      R = lists:map(Fun, SL),
      ParentPid ! {result, R}
  end.



text_file_collector(File_ids, ReduceProc) ->
  receive
    {save, {R, Prob}} ->

      Filename = gen_file_name(R),
      {ok, S} = file:open(Filename, [append]),
      io:format(S, "~w~n",[Prob]),
      file:close(S),

      case dict:is_key(R, File_ids) of
        true -> text_file_collector(File_ids, ReduceProc);
        false -> text_file_collector(dict:append(R, Filename, File_ids), ReduceProc)
      end;

    {stop, ParentPid} ->
      ReduceProc ! {reduce, File_ids, ParentPid}
  end.

file_based_exec(Node) ->
  fun() ->
    Reduce_Proc = spawn(Node, fun() -> text_file_reducer() end),
    spawn(Node, fun() -> text_file_collector(dict:new(), Reduce_Proc) end)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REDUCE PROCESS: END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAPPER PROCESSES: BEGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file_based_processes(Nodes) ->
  fun(N, Reduce_Proc) ->
    Fun = fun(_) ->
      Node = lists:nth(random:uniform(length(Nodes)), Nodes),
      io:format("Node ~w, Reduce_Proc ~w~n",[Node, Reduce_Proc]),
      start_map_process(Node, Reduce_Proc)
    end,
    lists:map(Fun, lists:seq(1,N))
  end.

start_map_process(Node, Reduce_Proc) ->
  spawn(Node, fun() -> file_based_map_task(Reduce_Proc) end).

file_based_map_task(Reduce_Proc) ->
  receive
    {_From, {R, _C, Val}, Beta, N, Vj} ->
%%       Prob = ((Beta * Val) + vector_prima(N, Beta)) * Vj,
      Prob = calc_prob(Beta, N, Vj, Val),
      Reduce_Proc ! {save, {R, Prob}},
      file_based_map_task(Reduce_Proc)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAPPER PROCESSES: END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILITIES: BEGIN
%% split_matrix/3
%% split_row/4
%% number_of_processes/2
%% get_process_number/4
%% get_timestamp/0
%% get_file_name/1
%% get_one_vector/1
%% calc_prob/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_matrix(_,[],L) -> lists:reverse(L);
split_matrix(Col,[Adj|Rest],Result) ->
  NewResult = split_row(Col,1.0/length(Adj),Adj,Result),
  split_matrix(Col+1,Rest,NewResult).

split_row(_,_,[],Result) -> Result;
split_row(Col,Val,[Row|Rest],Result) -> split_row(Col,Val,Rest,[{Row,Col,Val}|Result]).


number_of_processes(N, K) ->
  (N div K) * (N div K).

get_process_number(N, K, Col, Row) ->
  N_K = N div ?K,
  1 + ((Col - 1) div ?K) * N_K + (Row - 1) div K.

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro/1000).

gen_file_name(Row_Idx) ->
  string:join([
    "row-",
    integer_to_list(Row_Idx),
    ".dat"
  ], "").

gen_one_vector(N) ->
  Fun = fun(_) ->
    1 * vector_prima(4, 0.8) * (1/4)
  end,
  lists:map(Fun, lists:seq(1,N)).

vector_prima(N, Beta) ->
  (1 - Beta) / N.

calc_prob(Beta, N, Vj, Val) ->
  ((Beta * Val) + vector_prima(N, Beta)) * Vj.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILITIES: END
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tax() ->
  Red_Proc = file_based_exec('tres@mf-bbcom'),
  Mappers = file_based_processes(['dos@mf-bbcom']),
  tax([0.25,0.25,0.25,0.25], ?BETA, ?ADJ_LIST, ?N, ?K, Red_Proc, Mappers).

tax(Vector) ->
  Red_Proc = file_based_exec('tres@mf-bbcom'),
  Mappers = file_based_processes(['dos@mf-bbcom']),
  tax(Vector, ?BETA, ?ADJ_LIST, ?N, ?K, Red_Proc, Mappers).

tax(Vector, Beta, Adj_List, N, K, Red_Proc, Map_Proc) ->
  Proc_number = number_of_processes(N, K),
  TM = split_matrix(1, Adj_List, []),

  Reduce = Red_Proc(),
  Processes = Map_Proc(Proc_number, Reduce),

  Distribute = spawn(taxation, distribute, [self(), TM, Processes, Vector, Beta, N, K]),
  timer:sleep(1000),
  receive
    {_Pid, stop} ->
      done
  end,

  Distribute ! bye,
  Reduce ! {stop, self()},
  receive
    {result, R} ->
      R
  end.



distribute(Parent, [], _, _, _, _, _) ->
  Parent ! {self(), stop};

distribute(Parent, [H|T], Procs, Vector, Beta, N, K) ->
  self() ! {process, H},
  receive
    {process, {Col, Row, Val}} ->
      Proc_Num = get_process_number(N, K, Col, Row),
      Proc = lists:nth(Proc_Num, Procs),
      Vj = lists:nth(Row, Vector),
      Proc ! {self(), {Col, Row, Val}, Beta, N, Vj},
%%         io:format("H ~w, PN ~w - ~w ~n", [H, Proc_Num, Proc]),
      distribute(Parent, T, Procs, Vector, Beta, N, K);
    bye ->
      io:format("bye ~n")
  end.






