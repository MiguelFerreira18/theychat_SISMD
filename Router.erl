-module(router).
-export([start/1, add_server/3, print_servers/1, remove_server/2,
         print_router/1]).

start(Router) ->
    register(Router, spawn(fun() -> loop([]) end)).

add_server(Router, Server, Remote) ->
    Router ! {add_server, Server,Remote}.

print_servers(Router) ->
    Router ! {print_servers}.

remove_server(Router, Server) ->
    Router ! {remove_server, Server}.



print_router(Router) -> Router ! {print_router}.


loop(Servers) ->
    receive
        {join_server,Client,Server} -> 
            case lists:keyfind(Server, 1, Servers) of
                {Server,Remote} -> 
                    {Client,Remote} ! {self(),Client,add_client},
                    receive
                        {_, Reply} -> io:format("Received from server: ~p~n", [Reply])
                    end,
                    Client ! {self(),ok};
                false -> 
                    Client ! {self(),not_found}
            end,
            loop(Servers);
        {add_server, Server, Remote} ->
            case net_adm:ping(Server) of
                pong ->
                    loop([Server | {Server,Remote}]);
                pang ->
                    io:format("Server ~p is not reachable~n", [Server]),
                    loop(Servers)
            end;
        {get_server,Client ,Server} ->
            case lists:keyfind(Server, 1, Servers) of
                {Server,Remote} ->
                    Client ! {self(), ok, Server,Remote};
                false ->
                    Server ! {self(), not_found}
            end,
            loop(Servers);
        {print_servers} ->
            io:format("Servers: ~p~n", [Servers]),
            loop(Servers);
        {print_servers,Client} ->
            Client ! {self(), Servers},
            loop(Servers);
        {remove_server, Server} ->
            loop(lists:delete(Server, Servers))

    end.
