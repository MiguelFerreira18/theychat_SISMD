-module(server).

-export([start/1,join_router/3]).

start(Server) ->
    Pid1 = spawn(fun() -> loop1(Server,{},{}, []) end),
    Pid2 = spawn(fun() -> loop2(Server,{},{}, []) end),
    Pid1 ! {add_new_pid, Pid2},
    Pid2 ! {add_new_pid, Pid1},
    register(Server, Pid1).

join_router(Server, Router, Remote) -> 
    Server ! {send_server_to_router, Router, Remote}.

% !MONITOR
loop1(Server, Router ,Process, Clients) ->
process_flag(trap_exit, true),
receive
        {register_router_pid, Router_Pid} -> 
            io:format("Adding link to second process ~p~n", [Router_Pid]),
            Router_Pid ! {self(),Server},
            loop1(Server, Router_Pid,Process, Clients);
        {add_new_pid, Pid} ->
            io:format("Adding link to second process ~p~n", [Pid]),
            link(Pid),
            loop1(Server, Router,Pid, Clients);
        {send_server_to_router, Router, Remote} ->
            {Router, Remote} ! {add_server, Server, self()},
            loop1(Server, Router,Process, Clients);
        {leave_server, Client} ->
            io:format("Received leave_server from ~p~n", [Client]),
            Clients = remove_client(Client, Clients),
            loop1(Server, Router ,Process, Clients);
        {join_server, Client} ->
            io:format("Received join_server from ~p~n", [Client]),
            Process ! {join_server, Client},
            loop1(Server, Router ,Process, [Client | Clients]);
        {send_msg_users,From, Msg} ->
            io:format("Received ~p: ~p~n", [From, Msg]),
            Process ! {From, Msg},
            loop1(Server, Router ,Process, Clients);
        {'EXIT', SomePid, Reason} ->
            io:format("SomePid: ~p, Reason: ~p~n", [SomePid, Reason]),
            New_Pid = spawn(fun() -> loop2(Server,{} ,{}, Clients) end),
            New_Pid ! {add_new_pid, self()},
            loop1(Server, Router ,New_Pid, Clients);
        {From, stop} ->
            io:format("Received from ~p message to stop!~n", [From]),
            From ! {self(), server_disconnect},
            exit(normal)
    end.


% !SERVER CHAT
loop2(Server, Router , Process, Clients) ->
    process_flag(trap_exit, true),
    receive
        {add_new_pid, Pid} ->
            io:format("Adding link to second process ~p~n", [Pid]),
            link(Pid),
            loop2(Server,Router,Pid, Clients);
        {leave_server, Client} ->
            io:format("Received leave_server from ~p~n", [Client]),
            Clients = remove_client(Client, Clients),
            loop2(Server,Router ,Process, Clients);
        {join_server, Client} ->
            io:format("Received join_server from ~p~n", [Client]),
            Client ! {receive_server,self(), ok},
            loop2(Server,Router ,Process, [Client | Clients]);
        {From, Msg} ->
            io:format("Received ~p: ~p~n", [From, Msg]),
            io:format("Sending reply...~n"),
            From ! {self(), happy_to_receive_your_message},
            send_message(Clients, Msg),
            loop2(Server,Router ,Process, Clients);
        {'EXIT', SomePid, Reason} ->
            io:format("SomePid: ~p, Reason: ~p~n", [SomePid, Reason]),
            New_Pid = spawn(fun() -> loop2(Server, Router,{}, Clients) end),
            New_Pid ! {add_new_pid, self()},
            loop2(Server,Router ,New_Pid, Clients)
    end.

remove_client(_, []) ->
    [];
remove_client(Client, [Client | T]) ->
    T;
remove_client(Client, [H | T]) ->
    [H] ++ remove_client(Client, T).

send_message([], _) ->
    io:format("No more clients to send message to~n");
send_message([H | T], Message) ->
    H ! {self(), Message},
    send_message(T, Message).
