%%%-------------------------------------------------------------------
%%% @author martinflores
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2015 7:14 PM
%%%-------------------------------------------------------------------
-module(listoperations).
-author("martinflores").



%% API
-export([
  start/0,
  init/0,
  stop/0, allocate/1, deallocate/1,
  main/0,
  split_list/1,
  sum_items/1,
  generate_workers/1,
  single_worker/0
]).

-define(LIST, [[1,2,3], [4,5,6]]).


%% main(K) ->
%%   spawn()


start() ->
  register(listcount, spawn(listoperations, init, [])).

init() ->
  Workers = generate_workers(2),
  Workers_List = {Workers, []},
  loop(Workers_List).

loop(Workers) ->
  receive
    {request, Pid, {allocate, H}} ->
      io:format("WS ~w ~n", [Workers]),
      {WorkersInProgress, Reply} = allocate(Workers, Pid),
      io:format("WiP ~w ~n", [WorkersInProgress]),

      loop(WorkersInProgress);
    {request, Pid , {deallocate, Proc}} ->
      AvailableWorkers = deallocate(Workers, Proc),
      io:format("AW ~w ~n",[AvailableWorkers]),
      loop(AvailableWorkers);
    {request, Pid, stop} ->
      stop
  end.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_worker}};
allocate({[Proc|Free], Allocated}, Pid) ->
  io:format("Proc ~w Pid ~w ~n", [Proc, Pid]),
  Proc ! {Proc, "Algo"},
  {{Free, [{Proc, Pid}|Allocated]}, {ok, Proc}}.

deallocate({Free, Allocated}, Proc) ->
  NewAllocated = lists:keydelete(Proc, 1, Allocated),
  {[Proc |Free],  NewAllocated}.


stop()           -> call(stop).
allocate(H)       -> call({allocate, H}).
deallocate(Proc) -> call({deallocate, Proc}).

call(Message) ->
  listcount ! {request, self(), Message}.
%%   receive
%%     {reply, Reply} -> Reply
%%   end.




%% sum_loop([H|T], Results) ->

%%   case length(T) of
%%     0 -> ok
%%   end,
%%   receive
%%     more ->
%%       sum_loop([T], Results);
%%     no_more ->
%%       stop
%%   end.






%% send_create_msg([]) -> ok;
%% send_create_msg([H|T]) ->
%%   H ! create,
%%   send_create_msg(T).

main() ->
  split_list(?LIST).


split_list([])    -> [];
split_list([H|T]) -> [ allocate(H) | split_list(T)].

sum_items(List) ->
  lists:sum(List).


%% calc_sum([T], Workers) ->
%%   receive
%%     {From, Msg} ->
%%       io:format("Pid ~w Msg ~w~n",[From, Msg]);
%%     {From, done} ->
%%       io:format("Pid ~w is done", [From])
%%   end.



%% generate_workers(0) -> io:format("Worker 0 created! ~n");
%% generate_workers(N) ->

%%    io:format("Worker ~w created!~n", [N]).
%%   Pid = spawn(listoperations, single_worker, []),
%%   create_workers(N - 1 | create_workers()).

generate_workers(0) -> [];
generate_workers(N) ->
  Pid = spawn(listoperations, single_worker, []),
  io:format("Pid ~w created!~n", [Pid]),
  [Pid | generate_workers(N-1)].


single_worker() ->
  receive
    {From, H} ->
      io:format("From ~w H ~w ~n", [From, H]),
%%       io:format("From ~w, Msg: ~w ~n", [From, Msg]),
      timer:sleep(5000),

      deallocate(From),
      single_worker();
    create ->
      io:format("Fui creado! ~n");
    stop ->
      stopped
  end.


