:- module(util, [ts_day/2]).

ts_day(Ts, Day) :-
    number(Ts), !,
    format_time(string(Day), "%Y-%m-%d", Ts).
ts_day(Ts, Day) :-
    string(Day), !,
    parse_time(Day, iso_8601, Ts).
