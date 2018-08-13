-module(ex_db_2).

%load ex_db module to set this up

-export([start/0,stop/0,write/2,delete/1,read/1,match/1]).
-export([loop/1]).

start() ->
    register(?MODULE, erlang:spawn(?MODULE, loop, [[]])),
    ok.

stop() ->
    ?MODULE ! stop,
    ok.

write(Key, Element) ->
    ?MODULE ! {write,Key,Element},
    ok.

delete(Key) ->
    ?MODULE ! {delete,Key},
    ok.

read(Key) ->
    ?MODULE ! {read,Key,self()},
    receive {reply, Reply} -> Reply end.

match(Element) ->
    ?MODULE ! {match,Element,self()},
    receive {reply, Reply} -> Reply end.

loop(Data) ->
    receive
        {write,Key,Element} ->
            io:format("write key - ~p Element - ~p~n",[Key,Element]),
            NewData = ex_db:write(Key,Element,Data),
            loop(NewData);
        {delete,Key} ->
            io:format("delete key - ~p~n",[Key]),
            NewData = ex_db:delete(Key,Data),
            loop(NewData);
        {read,Key,Pid} ->
            io:format("read key - ~p from - ~p~n",[Key,Pid]),
            reply(Pid,ex_db:read(Key,Data)),
            loop(Data);
        {match,Element,Pid} ->
            io:format("match Element - ~p from - ~p~n",[Element,Pid]),
            reply(Pid,ex_db:match(Element,Data)),
            loop(Data);
        stop ->
            true
    end.

reply(To, Msg) ->
    To ! {reply, Msg}.
