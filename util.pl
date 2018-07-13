/** <module> Utilities
* Helper predicates.
*/
:- module(util, [ts_day/2,
                 listof//2,
                 numlist_desc/3,
                 shuffled/2]).

%! ts_day(+Ts:timestamp, -Day:string) is det.
%! ts_day(-Ts:timestamp, +Day:string) is semidet.
%
%  True when =Day= is a string representation of the day that =Ts= is
%  on, in the format YYYY-MM-DD.
ts_day(Ts, Day) :-
    number(Ts), !,
    format_time(string(Day), "%Y-%m-%d", Ts).
ts_day(Ts, Day) :-
    string(Day), !,
    parse_time(Day, iso_8601, Ts).


:- meta_predicate listof(3, +, ?, ?).
%! listof(:DCG, +Elements:list)//
%  Generates a list of elements from =DCG= (which presumably uses
%  =html//1=) to make them work with Quench Vue.
%
%  @arg DCG A DCG taking one argument, which will be a member or
%            =Elements=.
%  @arg Elements the list of elements to apply =DCG= to.
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

%! numlist_desc(+A:int, +B:int, -C:list) is semidet.
%  Like numlist/3 but makes a descending list.
numlist_desc(A, B, C) :-
    must_be(integer, A),
    must_be(integer, B),
    A >= B,
    numlist_desc_(A, B, C).
numlist_desc_(A, A, [A]) :- !.
numlist_desc_(A, C, [A|D]) :-
    B is A - 1,
    numlist_desc_(B, C, D).

%! shuffled(+L, -Shuffled) is det.
shuffled([], []) :- !.
shuffled(L, [X|ShufRest]) :-
    random_select(X, L, Rest),
    shuffled(Rest, ShufRest).
