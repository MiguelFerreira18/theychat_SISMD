-module(test_spawn_link).
-export([init/0,start/1]).

start(Teste) -> register(Teste, spawn(fun init/0)).

init() -> 
    process_flag(trap_exit,true),
    Pid1 = spawn_link(fun() -> loop1({},[]) end),
    Pid2 = spawn_link(fun() -> loop2(Pid1,[]) end),
    Pid1 ! {add_new_pid,Pid2},
       receive
       {_,ok} -> exit(normal)
       end,
    io:format("Pid1: ~p, Pid2: ~p~n", [Pid1,Pid2]).


    
loop1(Pid2,List) ->
    receive
    {add_new_pid,Pid} -> 
        io:format("Adding ~p~n", [Pid]),
        Pid ! {self(),ok},
        loop1(Pid,List);
    {send_msg,Service,Msg} ->
        Service ! {self(), Msg},
        loop1(Pid2,List);
    {'EXIT',SomePid,Reason} ->
        io:format("SomePid: ~p, Reason: ~p~n", [SomePid,Reason]), 
        loop2(Pid2, List);
    {stop} -> exit(normal)
    end.

loop2(Pid1,List) ->
    process_flag(trap_exit, true),
    io:format("List: ~p~n", [List]),
    receive
    {send_msg,Service,Msg} ->
        Service ! {self(), Msg},
        loop2(Pid1,List);
    {'EXIT',SomePid,Reason} ->
        io:format("SomePid: ~p, Reason: ~p~n", [SomePid,Reason]), 
        loop1(Pid1,List);
    {stop} -> exit(normal)
    end.