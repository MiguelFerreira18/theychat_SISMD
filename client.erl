-module(client).
-export([start/1, add_remote/1, send_msg/2, stop_client/1,print_servers/3,join_server/4,leave_server/1,print_self/1]).

start(Client) -> register(Client, spawn(fun() -> loop({}) end)).

add_remote(RemoteMachine) -> net_adm:ping(RemoteMachine).
send_msg(Client, Message) -> Client ! {send, Message}.
print_servers(Client,Router,RemoteMachine) -> Client ! {print_servers,Router,RemoteMachine}.
stop_client(Client) -> Client ! {stop_client}.

print_self(Client) -> Client ! {print_self}.
% get server

% Leave server
leave_server(Client) -> Client ! {leave_server}.
% Join server
join_server(Client,Router,RemoteMachine,Server) -> Client ! {join_server,Router,RemoteMachine,Server}.

loop(Server_Info) ->
    receive
        {switch_pid,New_Pid} ->
            loop(New_Pid);
        {join_server,Router,RemoteMachine,Server} ->
            {Router,RemoteMachine} ! {join_server,self(),Server},
            receive
                {_, _, Server_Pid} -> 
                io:format("Server_Pid: ~p~n", [Server_Pid]),    
                loop(Server_Pid)
            end,
            io:format("Joining server~n"),
            erlang:monitor(process,Server_Pid),
            loop(Server_Pid);
        {leave_server} ->
            io:format("Leaving server~n"),
            % Call leave server with the Server id in the Server_Info
            Server_Info ! {leave_server,self()},
            loop({});
        {print_self} ->
            io:format("Client: ~p~n", [Server_Info]),
            loop(Server_Info);
        {send, Message} ->
            case Server_Info of
                _ -> 
                    Server_Info ! {send_msg_users,self(), Message},
                    receive
                        {_, happy_to_receive_your_message} -> io:format("Received from server: ~p~n", [happy_to_receive_your_message])
                    end
            end,
            loop(Server_Info);
        {receive_message, Message} ->
            io:format("Received message: ~p~n", [Message]),
            loop(Server_Info);
        {print_servers,Router,RemoteMachine} ->
            {Router,RemoteMachine} ! {print_servers,self()},
            receive
                {_, Reply} -> io:format("Received from router: ~p~n", [Reply])
            end,
            loop(Server_Info);
        {'DOWN',_,_,_,_} ->
            io:format("Lost connection to server"),
            loop({});
        {stop_client} ->
            io:format("Client exiting...")
    end.
