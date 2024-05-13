-module(tst).
-export([start/1, on_exit/2, keep_alive/2]).

start(Test) ->
    register(Test, spawn(fun() -> loop() end)).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun() -> keep_alive(Name, Fun) end).


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



on_exit(Pid, Fun) ->
    spawn(fun() ->
             process_flag(trap_exit, true),
             link(Pid),
             receive {'EXIT', Pid, Why} -> Fun(Why) end
          end).
