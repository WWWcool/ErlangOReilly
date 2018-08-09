-module(ex_db).

-export([new/0,destroy/0,write/3,delete/2,read/2,match/2,
        lib_write/3,lib_delete/2,lib_read/2,lib_match/2]).

new() -> [].

destroy() -> ok.

write(Key, Element, Db) -> Db ++ [{Key,Element}].

delete( _ ,[]) -> [];
delete(Key,[{Key, _ } | T]) -> delete(Key,T);
delete(Key,[{_Key,_Element} | T]) -> [{_Key,_Element}] ++ delete(Key,T).

read( _ ,[]) -> {error,instance};
read(Key,[{Key,Element} | _]) -> {ok,Element};
read(Key,[ _ | T]) -> read(Key,T).

match( _ , []) -> [];
match(Element, [{Key,Element} | T]) -> [Key] ++ match(Element,T);
match(Element, [ _ | T]) -> match(Element,T).

lib_write(Key, Element, Db) -> lists:append(Db,[{Key,Element}]).

lib_delete(Key,List) -> lists:delete(lists:keyfind(Key,1,List),List).

lib_read(Key,List) ->
    Tuple = lists:keyfind(Key,1,List),
    case is_tuple(Tuple) of
        true ->
            {Key,Element} = Tuple,
            {ok,Element};
        false -> {error,instance}
    end.
lib_match(Element, List) ->
    lib_get_keys(lists:filter(fun ({_, El}) -> El == Element end,List)).

lib_get_keys([]) -> [];
lib_get_keys([{Key, _ } | T]) -> [Key | lib_get_keys(T)];
lib_get_keys([ _ | T]) -> lib_get_keys(T).




