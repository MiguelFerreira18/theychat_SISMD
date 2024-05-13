-module(routerM).
-export([start/1, add_server/2, get_server/2, remove_server/2,add_router_servers/3]).

start(RouterM) -> register(RouterM, spawn(fun() -> loop([],[]) end)).

add_server(RouterM, Server) -> RouterM ! {add_server, Server}.
get_server(RouterM, Server) -> RouterM ! {get_server, Server}.
remove_server(Router, Server) -> Router ! {remove_server, Server}.
% Send the Servers to the router to also save them
add_router_servers(RouterM,Servers,Router) -> RouterM ! {add_router_servers, Servers, Router}.

loop(Servers,RouterPID) ->
    receive
        {add_servers_from_router,Router_PID,Router_servers} -> 
            Router_PID ! {ok, self()},
            loop(Servers ++ Router_servers,Router_PID);
        {add_server, Server} ->
            case net_adm:ping(Server) of
                pong -> loop([Server | Servers],RouterPID);
                pang -> io:format("Server ~p is not reachable~n", [Server]),
                        loop(Servers,RouterPID)
            end;
        {get_server, Server} ->
            case lists:member(Server, Servers) of
                true -> Server ! {self(), ok};
                false -> Server ! {self(), not_found}
            end,
            loop(Servers,RouterPID);
        {remove_server, Server} ->
            loop(lists:delete(Server, Servers),RouterPID)
    end.
