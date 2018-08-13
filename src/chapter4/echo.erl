-module(echo).

-export([start/0, print/1, stop/0, loop/0]).

start() ->
    Pid = erlang:spawn(echo, loop, []),
    %Pid = erlang:spawn(fun echo:loop/0),
    register(echo, Pid),
    ok.

stop() ->
    echo ! stop,
    ok.

print(Msg) ->
    echo ! {self(),Msg},
    ok.

loop() ->
    receive
        { _ , Msg} ->
            io:format("~p~n",[Msg]),
            loop();
    stop ->
        true
    end.
