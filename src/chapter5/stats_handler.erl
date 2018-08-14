-module(stats_handler).

-export([init/1, terminate/1, handle_event/2]).

init(_) -> [].

terminate(Data) -> Data.

handle_event(Event, Data) ->
    stat_check(Event, Data).

stat_check({Type, Id, Description}, []) ->
    [{Type, Id, stat_add(Description, [])}];
stat_check({Type, Id, Description}, [{Type, Id, Data}| Rest]) ->
    [{Type, Id, stat_add(Description, Data)} | Rest];
stat_check(Event,[Head | Rest]) ->
    [Head | stat_check(Event, Rest)].

stat_add(Description, []) -> [{Description, 1}];
stat_add(Description, [{Description, Count} | Rest]) ->
    [{Description, Count + 1} | Rest];
stat_add(Description,[Head | Rest]) ->
    [Head | stat_add(Description, Rest)].
