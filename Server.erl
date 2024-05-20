-module(server).
-export([start/1]).
start(Server) -> register(Server, spawn(fun() -> loop([]) end)).
loop(Clients) ->
    receive
        {leave_server, Client} -> 
            io:format("Received leave_server from ~p~n", [Client]),
            Clients = remove_client(Client, Clients),
            loop(Clients);
        {join_server, Client} -> 
            io:format("Received join_server from ~p~n", [Client]),
            Client ! {self(), ok},
            loop([Client | Clients]);
        {From, Msg} ->
            io:format("Received ~p: ~p~n", [From, Msg]),
            io:format("Sending reply...~n"),
            From ! {self(), happy_to_receive_your_message},
            send_message(Clients, Msg),
            loop(Clients);
        {From, stop} ->
            io:format("Received from ~p message to stop!~n", [From]),
            From ! {self(), server_disconnect}
    end.

remove_client(_, [])-> [];
remove_client(Client, [Client | T]) -> T;
remove_client(Client, [H | T]) -> [H] ++ remove_client(Client, T).


send_message([], _)-> io:format("No more clients to send message to~n");
send_message([H | T ], Message)-> H ! {self(), Message}, send_message(T, Message).


    

