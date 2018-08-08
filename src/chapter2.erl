-module(chapter2).

-export([hw/0,b_not/1,b_and/2,b_or/2,b_nand/2]).

-spec hw() -> any().
%-spec b_not() -> bool().
%-spec b_and() -> bool().
%-spec b_or() -> bool().
%-spec b_nand() -> bool().

hw() ->
    io:format("Hello world!~n"),
    1 + 2.


b_not(true) -> false;
b_not(false) -> true.

b_and(true,true) -> true;
b_and(_X,_Y) -> false.

b_or(false,false) -> false;
b_or(_X,_Y) -> true.

b_nand(X,Y) ->
    b_not(b_and(X,Y)).



