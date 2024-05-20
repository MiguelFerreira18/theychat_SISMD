-module(client).
-export([start/1, add_remote/1, send_msg/2, stop_client/1,print_servers/3,join_server/4,get_server/4]).

start(Client) -> register(Client, spawn(fun() -> loop({}) end)).

add_remote(RemoteMachine) -> net_adm:ping(RemoteMachine).
send_msg(Client, Message) -> Client ! {send, Message}.
print_servers(Client,Router,RemoteMachine) -> Client ! {print_servers,Router,RemoteMachine}.
stop_client(Client) -> Client ! {stop_client}.
% get server
get_server(Client,Router,RemoteMachine,Server) -> Client ! {get_server,Router,RemoteMachine,Server}.
% Leave server
leave_server(Client) -> Client ! {leave_server}.
% Join server
join_server(Client,Router,RemoteMachine,Server) -> Client ! {join_server,Router,RemoteMachine,Server}.

loop(Server_Info) ->
    receive
        {join_server,Router,RemoteMachine,Server} ->
            {Router,RemoteMachine} ! {join_server,self(),Server},
            receive
                {_, Reply} -> 
                    case Reply of
                        ok -> io:format("Server joined ~p ~n", [Server]);
                        not_found -> io:format("Server ~p not found~n", [Server])
                    end
            end,
            loop(Server_Info);
        {alternate_join} -> 
            case Server_Info of
                {Server,Remote} -> 
                    {Server,Remote} ! {join_server,self()},
                    receive
                        {_, _} -> io:format("Server joined ~p ~n", [Server])
                    end
            end,
            loop(Server_Info);
        {leave_server} ->
            io:format("Leaving server~n"),
            case Server_Info of
                {Server,Remote} -> 
                    {Server,Remote} ! {leave_server,self()},
                    receive
                        {_, _} -> io:format("You left the chat")
                    end
            end,
            loop({});
        {get_server,Router,RemoteMachine,Server} ->
            {Router,RemoteMachine} ! {get_server,Server},
            receive
                {_, Reply,Server,Remote} -> 
                    case Reply of
                        ok -> io:format("Server ~p found~n", [Server]),
                                loop({Server,Remote});
                        not_found -> io:format("Server ~p not found~n", [Server]),
                                loop(Server_Info)
                    end
            end;
        {send, Message} ->
            case Server_Info of
                {Server,Remote} -> 
                    {Server,Remote} ! {self(), Message},
                    receive
                        {_, happy_to_receive_your_message} -> io:format("Received from server: ~p~n", [happy_to_receive_your_message])
                    end
            end,
            loop(Server_Info);
        {print_servers,Router,RemoteMachine} ->
            {Router,RemoteMachine} ! {print_servers,self()},
            receive
                {_, Reply} -> io:format("Received from router: ~p~n", [Reply])
            end,
            loop(Server_Info);
        {stop_client} ->
            io:format("Cliente exiting...")
    end.
