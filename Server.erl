-module(server).
-export([start/1]).
start(Server) -> register(Server, spawn(fun() -> loop() end)).
loop() ->
    receive
        {From, stop} ->
            io:format("Received from ~p message to stop!~n", [From]),
            From ! {self(), server_disconnect};
        {From, Msg} ->
            io:format("Received ~p: ~p~n", [From, Msg]),
            io:format("Sending reply...~n"),
            From ! {self(), happy_to_receive_your_message},
            loop()
    end.
