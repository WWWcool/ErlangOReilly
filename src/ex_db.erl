-module(ex_db).

-export([new/0,destroy/0,write/3,delete/2,read/2,match/2]).

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
