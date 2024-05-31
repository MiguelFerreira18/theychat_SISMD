-module(router).

-export([start/1, add_server/3, print_servers/1, remove_server/2, print_router/1]).

start(Router) ->
    Pid1 = spawn(fun() -> loop1(Router, {}, []) end),
    Pid2 = spawn(fun() -> loop2(Router, {}, []) end),
    Pid1 ! {add_new_pid, Pid2},
    Pid2 ! {add_new_pid, Pid1},
    register(Router, Pid1).

add_server(Router, Server, Remote) ->
    Router ! {add_server, Server, Remote}.

print_servers(Router) ->
    Router ! {print_servers}.

remove_server(Router, Server) ->
    Router ! {remove_server, Server}.

print_router(Router) ->
    Router ! {print_router}.

% !ROUTER
loop1(Router, Process, Servers) ->
    process_flag(trap_exit, true),
    receive
        {switch_server_Pid,Server,New_Pid} ->
            io:format("Switching server ~p to ~p~n", [Server, New_Pid]),
            erlang:monitor(process,New_Pid),
            Process ! {remove_server_by_id,Server},
            loop1(Router, Process, [{Server, New_Pid} | Servers]);
        {add_new_pid, Pid} ->
            io:format("Adding link to second process ~p~n", [Pid]),
            link(Pid),
            loop1(Router, Pid, Servers);
        {join_server, Client, Server} ->
            io:format("Joining server ~p~n", [Server]),
            Server_Pid = find_server(Server,Servers),
            io:format("Server_Pid: ~p~n", [Server_Pid]),
            Server_Pid ! {join_server, Client},
            io:format("client joined server~n"),
            loop1(Router, Process, Servers);
        {add_server, Server, Server_Pid} -> %% STORE A MONITOR SERVER
            io:format("Adding server ~p with pid ~p~n", [Server, Server_Pid]),
            Server_Pid ! {add_router, self()},
            Process ! {add_server, Server, Server_Pid},
            erlang:monitor(process,Server_Pid),
            loop1(Router, Process, [{Server, Server_Pid} | Servers]);
        {print_servers} ->
            io:format("Servers: ~p~n", [Servers]),
            loop1(Router, Process, Servers);
        {print_servers, Client} ->
            io:format("Servers: ~p~n", [Servers]),
            Client ! {self(), Servers},
            loop1(Router, Process, Servers);
        {remove_server, Server} ->
            Process ! {remove_server,Server},
            io:format("Removing server ~p~n", [Server]),
            New_servers = delete(Server, Servers),
            io:format("New servers: ~p~n", [New_servers]),
            loop1(Router, Process, New_servers);
        {'EXIT', SomePid, Reason} ->
            io:format("Loop1 - SomePid: ~p, Reason: ~p~n", [SomePid, Reason]),
            New_Pid = spawn(fun() -> loop2(Router, {}, Servers) end),
            io:format("New_Pid: ~p~n", [New_Pid]),
            New_Pid ! {add_new_pid, self()},
            loop1(Router, New_Pid, Servers);
        {'DOWN',_ , process,Pid, Reason} ->
            io:format("DOWN - Pid: ~p, Reason: ~p~n", [Pid, Reason]),
            New_Servers = remove_monitored_server_by_pid(Pid, Servers),
            io:format("New Servers: ~p~n", [New_Servers]),
            Process ! {remove_server_by_id, New_Servers},
            loop1(Router, Process,New_Servers);
        {stop} ->
            exit(normal)
    end.

% !MONITOR
loop2(Router, Process, Servers) ->
    process_flag(trap_exit, true),
    receive
        {remove_server_by_id, New_Servers} ->
            loop2(Router, Process, New_Servers);
        {add_new_pid, Pid} ->
            io:format("Adding link to first process ~p~n", [Pid]),
            link(Pid),
            loop2(Router, Pid, Servers);
        {add_server, Server, Server_Pid} ->
            loop2(Router, Process, [ {Server, Server_Pid}| Servers]);
        {remove_server,Server} ->
            loop2(Router, Process, delete(Server, Servers));
        {send_msg, Service, Msg} ->
            Service ! {self(), Msg},
            loop2(Router, Process, Servers);
        {'EXIT', SomePid, Reason} ->
            io:format("Loop2 - SomePid: ~p, Reason: ~p~n", [SomePid, Reason]),
            case whereis(Router) == SomePid of
                false ->
                    ok;
                true ->
                    unregister(Router)
            end,
            io:format("Unregistered ~p~n", [Router]),
            New_Pid = spawn(fun() -> loop1(Router, {}, Servers) end),
            register(Router, New_Pid),
            io:format("New_Pid: ~p~n", [New_Pid]),
            New_Pid ! {add_new_pid, self()},
            io:format("New_Pid: ~p~n", [New_Pid]),
            loop2(Router, New_Pid, Servers);
        {stop} ->
            exit(normal)
    end.


% remove_and_add_new_server(Router, Server, Server_Pid) ->
%     Router ! {remove_server, Server},
%     Router ! {add_server, Server, Server_Pid}.

% Retorna o pid do server
find_server(_, []) -> {};
find_server(Server, [{Server, Server_Pid} | _]) -> Server_Pid;
find_server(Server, [_ | T]) -> find_server(Server, T).


% Retorna uma nova lista de servers
delete(_,[]) -> [];
delete(Server, [{Server, _} | T]) -> T;
delete(Server, [H | T]) -> [H | remove_server(Server, T)].


remove_monitored_server_by_pid(_, []) -> [];
remove_monitored_server_by_pid(Pid, [{_, Pid} | T]) -> T;
remove_monitored_server_by_pid(Pid, [H | T]) -> [H | remove_monitored_server_by_pid(Pid, T)].