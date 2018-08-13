-module(myring).

-export([create_ring/1, destroy_ring/0,send_message/2,create_ring_help/2]).

create_ring(Num) ->
    create_ring_help(Num,1).
destroy_ring() -> ring_entry_point ! {cmd,"Terminate"}.

send_message(0, _) -> ok;
send_message(Count, Msg) ->
    ring_entry_point ! {cmd,"Send",Msg},
    send_message(Count - 1, Msg).

create_ring_help(0,_) ->
    io:format("last proc created - ~p~n",[self()]),
    loop_proc(whereis(ring_entry_point));
create_ring_help(Num,1) -> %first enter
    NPid = spawn(?MODULE, create_ring_help,[Num - 1,0]),
    register(ring_entry_point,NPid),
    {ok,"entry point registered"};
create_ring_help(Num,_) ->
    io:format("proc created - ~p~n",[self()]),
    NPid = spawn(?MODULE, create_ring_help,[Num - 1,0]),
    loop_proc(NPid).

loop_proc(undefined) -> {error,"Can`t find entry point"};
loop_proc(Pid) ->
    EntryPoint = self() == whereis(ring_entry_point),
    receive
        {cmd,"Send",Msg} -> loop_proc_send(Pid,Msg);
        {cmd,"Terminate"} -> loop_proc_send(Pid,stop);
        stop when EntryPoint ->
            io:format("stop proc~p~n",[self()]);
        stop ->
            io:format("stop proc~p~n",[self()]),
            loop_proc_send(Pid,stop);
        Msg when EntryPoint ->
            io:format("transfer of the message - ~p - end~n",[Msg]),
            loop_proc(Pid);
        Msg ->
            %io:format("receive in ~p - ~p~n",[self(),Msg]),
            loop_proc_send(Pid,Msg)
    end.

loop_proc_send(Pid,Msg) ->
    Pid ! Msg,
    %io:format("send from ~p to ~p~n",[self(),Pid]),
    loop_proc(Pid).
