-module(router).

-export([start/1,on_exit/2, keep_alive/2, add_server/2, get_server/2, print_servers/1, remove_server/2,
         print_router/1]).

start(Router) ->
    register(Router, spawn(fun() -> loop([]) end)).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun() -> keep_alive(Name, Fun) end).
    
on_exit(Pid, Fun) ->
    spawn(fun() ->
                process_flag(trap_exit, true),
                link(Pid),
                receive {'EXIT', Pid, Why} -> Fun(Why) end
            end).
            

monitor_nodes(Router,NewName) ->
    Router ! {monitor_nodes,NewName}.

add_server(Router, Server) ->
    Router ! {add_server, Server}.

get_server(Router, Server) ->
    Router ! {get_server, Server}.

print_servers(Router) ->
    Router ! {print_servers}.

remove_server(Router, Server) ->
    Router ! {remove_server, Server}.

% Send the Servers to the router monitor to also save them

print_router(Router) -> Router ! {print_router}.


loop(Servers) ->
    receive
        {monitor_nodes,NewName} ->
            keep_alive(NewName, fun() -> loop(Servers) end),
            loop(Servers);
        {add_server, Server} ->
            case net_adm:ping(Server) of
                pong ->
                    loop([Server | Servers]);
                pang ->
                    io:format("Server ~p is not reachable~n", [Server]),
                    loop(Servers)
            end;
        {get_server, Server} ->
            case lists:member(Server, Servers) of
                true ->
                    Server ! {self(), ok};
                false ->
                    Server ! {self(), not_found}
            end,
            loop(Servers);
        {print_servers} ->
            io:format("Servers: ~p~n", [Servers]),
            loop(Servers);
        {Client, print_servers} ->
            Client ! {self(), Servers},
            loop(Servers);
        {remove_server, Server} ->
            loop(lists:delete(Server, Servers))

    end.
