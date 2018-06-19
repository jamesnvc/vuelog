:- module(util, [ts_day/2,
                listof//2]).

ts_day(Ts, Day) :-
    number(Ts), !,
    format_time(string(Day), "%Y-%m-%d", Ts).
ts_day(Ts, Day) :-
    string(Day), !,
    parse_time(Day, iso_8601, Ts).


:- meta_predicate listof(3, +, ?, ?).
%! listof(DCG//1, Elements) generates a list of elements from a DCG
%! (presumably html//1) to make them work with Quench Vue.
listof(DCG, Elements) -->
    listof(DCG, Elements, true).
listof(_, [], _) --> [].
listof(DCG, [E|Rest], true) -->
    call(DCG, E), listof(DCG, Rest, false).
listof(DCG, [E|Rest], false) -->
    ["<!-- <q> -->"],
    call(DCG, E),
    ["<!-- </q> -->"],
    listof(DCG, Rest, false).
