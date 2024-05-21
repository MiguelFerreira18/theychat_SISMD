-module(client).
-export([start/1, add_remote/1, send_msg/2, stop_client/1,print_servers/3,join_server/4]).

start(Client) -> register(Client, spawn(fun() -> loop({}) end)).

add_remote(RemoteMachine) -> net_adm:ping(RemoteMachine).
send_msg(Client, Message) -> Client ! {send, Message}.
print_servers(Client,Router,RemoteMachine) -> Client ! {print_servers,Router,RemoteMachine}.
stop_client(Client) -> Client ! {stop_client}.
% get server
% Leave server
leave_server(Client) -> Client ! {leave_server}.
% Join server
join_server(Client,Router,RemoteMachine,Server) -> Client ! {join_server,Router,RemoteMachine,Server}.

loop(Server_Info) ->
    receive
        {join_server,Router,RemoteMachine,Server} ->
            {Router,RemoteMachine} ! {join_server,self(),Server},
            receive
                {_, Reply, Server_Pid} -> 
                    case Reply of
                        ok -> loop(Server_Pid);
                        not_found -> io:format("Server ~p not found~n", [Server])
                    end
            end,
            loop(Server_Pid);
        {leave_server} ->
            io:format("Leaving server~n"),
            % Call leave server with the Server id in the Server_Info
            case Server_Info of
                {Server_Pid} -> Server_Pid ! {leave_server,self()}
            end,
            loop({});
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
            io:format("Client exiting...")
    end.
