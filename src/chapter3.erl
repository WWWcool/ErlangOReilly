-module(chapter3).

-export([index/2,g/1,even/1,merge/2,ex_sum/1,ex_sum_ext/2,ex_list/1,ex_rvlist/1,
            ex_print_all/1,ex_filter/2,ex_reverse/1,ex_concatenate/1,ex_flatten/1]).

%Chapter examples
index(0,[X|_]) -> X;
index(N,[_|Xs]) when N>0 -> index(N-1,Xs).

g([0|Xs]) -> g(Xs);
g([Y|Xs]) -> Y+g(Xs);
g([]) -> 0.

even(Int) when Int rem 2 == 0 -> true;
even(Int) when Int rem 2 == 1 -> false;
even(_) -> false.

merge(Xs,Ys) -> lists:reverse(mergeL(Xs,Ys,[])).

mergeL([X|Xs],Ys,Zs) -> mergeR(Xs,Ys,[X|Zs]);
mergeL([],[],Zs) -> Zs.

mergeR(Xs,[Y|Ys],Zs) -> mergeL(Xs,Ys,[Y|Zs]);
mergeR([],[],Zs) -> Zs.

% Ex.3-1
ex_sum(Int) when Int == 1 -> 1;
ex_sum(Int) -> Int + ex_sum(Int - 1).

ex_sum_ext(Int1,Int2) when Int2 - Int1 == 0 -> Int2;
ex_sum_ext(Int1,Int2) -> Int1 + ex_sum_ext(Int1 + 1,Int2).

% Ex.3-2
ex_list(Int) when Int == 0 -> [];
ex_list(Int) -> lists:append(ex_list(Int - 1), [Int]).

ex_rvlist(Int) when Int == 0 -> [];
ex_rvlist(Int) -> lists:append([Int], ex_rvlist(Int - 1)).

% Ex.3-3
ex_print_all(0) -> ok;
ex_print_all(Int) when Int rem 2 == 1 ->
    ex_print_single(Int),
    ex_print_all(Int - 1);
ex_print_all(Int) -> ex_print_all(Int - 1).
ex_print_single(Int) when is_integer(Int) -> io:format("Number:~p~n",[Int]).


% Ex.3-5
ex_filter([], _ ) -> [];
ex_filter([H | T], Int ) when H =< Int -> [H | ex_filter(T,Int)];
ex_filter([_ | T], Int ) -> ex_filter(T,Int).

ex_reverse([]) -> [];
ex_reverse([H | T]) -> ex_reverse(T) ++ [H].

ex_concatenate(Array) -> ex_concatenate_add(Array,[]).
%ex_concatenate([H | T]) -> [H | ex_concatenate(T)].

ex_concatenate_add([],List) -> List;
ex_concatenate_add([[H2 | T2]|T],List) ->
    ex_concatenate_add(T,[ex_concatenate_add(T2,[H2 | List]) | List]);
ex_concatenate_add([H|T],List) -> ex_concatenate_add(T,[H | List]).

ex_flatten([]) -> [];
ex_flatten([[H2 | T2] | T]) ->
    ex_flatten([ex_concatenate([H2 | T2]) | T]);
ex_flatten([H | T]) -> [H | ex_flatten(T)].



