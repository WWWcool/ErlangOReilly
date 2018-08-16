-module(freq).

-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and %% initialize the server.

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The client Functions
stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message %% protocol in a functional interface.
call(Message) ->
    ?MODULE ! {request, self(), Message},
    receive {reply, Reply} -> Reply end.

%% The Main Loop
loop(Frequencies) ->
    {_, Allocated} = Frequencies,
    ListLen = length(Allocated),
    receive
        {request, Pid, allocate} ->
            Count = check(Frequencies, Pid, 0),
            if Count < 3 ->
                    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
                    reply(Pid, Reply),
                    loop(NewFrequencies);
                true ->
                    reply(Pid, max_number_of_allocations),
                    loop(Frequencies)
            end;
        {request, Pid, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {'EXIT', Pid, _Reason} ->
            NewFrequencies = exited(Frequencies, Pid),
            loop(NewFrequencies);
        {request, Pid, stop} when ListLen == 0 ->
            reply(Pid, ok);
        {request, Pid, _} ->
            reply(Pid, not_all_freq_deallocated),
            loop(Frequencies)
    end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and %% deallocate frequencies.
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated},{error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),
    {{Free, [{Freq, Pid}|Allocated]},{ok, Freq}}.

check({_, []}, _, Count) -> Count;
check({Free, [{_, LocPid} | Allocated]}, Pid, Count) when LocPid == Pid ->
    check({Free, Allocated}, Pid, Count + 1);
check({Free, [_| Allocated]}, Pid, Count) ->
    check({Free, Allocated}, Pid, Count).

deallocate({Free, Allocated}, Freq, Pid) ->
    {NewFree, NewAllocated} = deallocate_help(Free, [], Allocated, Freq, Pid),
    if length(Allocated) == length(NewAllocated) ->
            {{Free, Allocated}, {error, bad_arg}};
        true ->
            case lists:keysearch(Pid,2,NewAllocated) of
                {value,{Freq,Pid}} ->
                    ok;
                false ->
                    unlink(Pid)
            end,
            {{NewFree, NewAllocated}, {ok, done}}
    end.

deallocate_help(Free, Allocated, [], _, _) -> {Free, Allocated};
deallocate_help(Free, Allocated, [{Freq, Pid}| OldAllocated], Freq, Pid) ->
    {[Freq | Free], Allocated ++ OldAllocated};
deallocate_help(Free, Allocated, [H | OldAllocated], Freq, Pid) ->
    deallocate_help(Free, [H | Allocated], OldAllocated, Freq, Pid).

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid,2,Allocated) of
        {value,{Freq,Pid}} ->
            NewAllocated = lists:keydelete(Freq,1,Allocated),
            exited({[Freq|Free],NewAllocated}, Pid);
        false ->
            {Free,Allocated}
    end.




