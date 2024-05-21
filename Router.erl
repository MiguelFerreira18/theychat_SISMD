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
        {add_new_pid, Pid} ->
            io:format("Adding link to second process ~p~n", [Pid]),
            link(Pid),
            loop1(Router, Pid, Servers);
        {join_server, Client, Server} ->
            case lists:keyfind(Server, 1, Servers) of
                {Server, Remote} ->
                    {Client, Remote} ! {self(), Client, add_client},
                    receive
                        {_, Reply} ->
                            io:format("Received from server: ~p~n", [Reply])
                    end,
                    Client ! {self(), ok};
                false ->
                    Client ! {self(), not_found}
            end,
            loop1(Router, Process, Servers);
        {add_server, Server, Server_Pid} -> %% STORE A MONITOR SERVER
            Process ! {add_server, Server, Server_Pid},
            link(Server_Pid),
            loop1(Router, Process, [Server | {Server, Server_Pid}]);
        {print_servers} ->
            io:format("Servers: ~p~n", [Servers]),
            loop1(Router, Process, Servers);
        {print_servers, Client} ->
            Client ! {self(), Servers},
            loop1(Router, Process, Servers);
        {remove_server, Server} ->
            loop1(Router, Process, lists:delete(Server, Servers));
        {'EXIT', SomePid, Reason} ->
            io:format("Loop1 - SomePid: ~p, Reason: ~p~n", [SomePid, Reason]),
            New_Pid = spawn(fun() -> router:loop2(Router, {}, Servers) end),
            io:format("New_Pid: ~p~n", [New_Pid]),
            New_Pid ! {add_new_pid, self()},
            loop1(Router, New_Pid, Servers);
        {stop} ->
            exit(normal)
    end.

% !MONITOR
loop2(Router, Process, Servers) ->
    process_flag(trap_exit, true),
    receive
        {add_new_pid, Pid} ->
            io:format("Adding link to first process ~p~n", [Pid]),
            link(Pid),
            loop2(Router, Pid, Servers);
        {add_server, Server, Server_Pid} ->
            Process ! {add_server, Server, Server_Pid},
            link(Server_Pid),
            loop2(Router, Process, [Server | {Server, Server_Pid}]);
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
