-module(chapter3).

-export([index/2,g/1,even/1,merge/2,ex_sum/1,ex_sum_ext/2,ex_list/1,ex_rvlist/1,
            ex_print_all/1,ex_filter/2,ex_reverse/1,ex_concatenate/1,ex_flatten/1,
            ex_quicksort/1,ex_merge_sort/1]).

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
    %io:format("Find array of array H = ~p T = ~p~n",[H2,T2]),
    List_add = ex_concatenate_add([H2 | T2],[]),
    %io:format("create list = ~p~n",[List_add]),
    ex_concatenate_add(T, List ++ List_add);
ex_concatenate_add([[]|T],List) ->
    ex_concatenate_add(T,List);
ex_concatenate_add([H|T],List) ->
    %io:format("Find array H = ~p T = ~p~n",[H,T]),
    ex_concatenate_add(T,List ++ [H]).

ex_flatten([]) -> [];
ex_flatten(Array) -> ex_concatenate(Array).

% Ex.3-6
ex_quicksort([]) -> [];
ex_quicksort([H | T]) -> ex_quicksort_help(T,H,[],[]).

ex_quicksort_help([],Pivot,Smaller,Others) when Smaller == [] -> Smaller ++ [Pivot] ++ Others;
ex_quicksort_help([],Pivot,Smaller,Others) ->
    Slist = ex_quicksort(Smaller),
    Olist = ex_quicksort(Others),
    Slist ++ [Pivot] ++ Olist;
ex_quicksort_help([H | T],Pivot,Smaller,Others) when H < Pivot ->
    ex_quicksort_help(T,Pivot,[H | Smaller],Others);
ex_quicksort_help([H | T],Pivot,Smaller,Others) ->
    ex_quicksort_help(T,Pivot,Smaller,[H | Others]).

ex_merge_sort([]) -> [];
ex_merge_sort(Array) when length(Array) > 1 ->
    ArrayLen = length(Array),
    ArrayHalfLen = ArrayLen div 2 + ArrayLen rem 2,
    FirstPart = lists:sublist(Array,1,ArrayHalfLen),
    SecondPart = lists:sublist(Array,1 + ArrayHalfLen,ArrayLen - ArrayHalfLen),
    FirstPartSorted = ex_merge_sort(FirstPart),
    SecondPartSorted = ex_merge_sort(SecondPart),
    %io:format("First = ~p Second = ~p~n",[FirstPart,SecondPart]),
    %io:format("First sorted = ~p Second sorted = ~p~n",[FirstPartSorted,SecondPartSorted]),
    Result = ex_merge_sort_help(FirstPartSorted,SecondPartSorted,[]),
    %io:format("Sort result = ~p~n",[Result]),
    Result;
ex_merge_sort(Array) -> Array.

ex_merge_sort_help([],[],List) -> List;
ex_merge_sort_help([],Array,List) -> lists:reverse(List) ++ Array;
ex_merge_sort_help(Array,[],List) -> lists:reverse(List) ++ Array;
ex_merge_sort_help([H1 | T1],[H2 | T2], List)  when H2 < H1 ->
    %io:format("add small -  ~p vs ~p~n",[H2,H1]),
    ex_merge_sort_help([H1 | T1],T2,[H2 | List]);
ex_merge_sort_help([H1 | T1],[H2 | T2], List) ->
    %io:format("add other -  ~p vs ~p~n",[H1,H2]),
    ex_merge_sort_help(T1,[H2 | T2],[H1 | List]).





