-module(client).
-export([start/1, add_remote/1, send_msg/4, stop_client/1,get_servers/3]).

start(Client) -> register(Client, spawn(fun() -> loop() end)).

add_remote(RemoteMachine) -> net_adm:ping(RemoteMachine).
send_msg(Client, Server, RemoteMachine, Message) -> Client ! {send, Server, RemoteMachine, Message}.
get_servers(Client,Router,RemoteMachine) -> Client ! {Router,RemoteMachine,print_servers}.
stop_client(Client) -> Client ! {stop_client}.
% leave server


loop() ->
    receive
        {send, Server, RemoteMachine, Message} ->
            {Server, RemoteMachine} ! {self(), Message},
            receive
                {_, Reply} -> io:format("Received from server: ~p~n", [Reply])
            end,
            loop();
        {Router,RemoteMachine,print_servers} ->
            {Router,RemoteMachine} ! {self(),print_servers},
            receive
                {_, Reply} -> io:format("Received from router: ~p~n", [Reply])
            end,
            loop();
        {stop_client} ->
            io:format("Cliente exiting...")
    end.
