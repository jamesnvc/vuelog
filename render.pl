:- module(render, [meal_plan_page//1]).

:- use_module(library(http/html_write), [html//1, html_post//2]).
:- use_module(library(css_write), [css//1, write_css/2]).
:- use_module(library(list_util), [replicate/3]).

include_css(CssDcg) -->
    { write_css(CssDcg, CssTxt) },
    html_post(css, style([], CssTxt)).

meal_plan_page(State) -->
    { ts_day(State.start_date, StartDay),
      ts_day(State.end_date, EndDay) },
    html([div(id(main),
              [div(class('parameters'),
                   % TODO: figure out how to make these vars work for "reactive"
                   % maybe have pengines re-evaluate the DCG?
                   [label(["Start Date",
                           input([type(date), value(StartDay)], [])]),
                    label(["End Date",
                           input([type(date), value(EndDay)], [])]),
                    label(["Meals per day",
                           input([type(number), value(State.meals_per_day)], [])])
                   ]),
               div(class(meals), \meals(State)),
               div(class('free-time'), \availability(State)),
               div(class(schedule), [h2("Schedule"), \calendar(State)])])]).

meals(State) -->
    html([h2("Menu Options"),
          ul(\meal_items(State.meals)) ]).

meal_items([]) --> [].
meal_items([Meal|Rest]) -->
    html(li(class(meal), Meal.name)), meal_items(Rest).

availability(_State) -->
    html([h2("Availability"),
          div([])]).

calendar_css -->
    css(['.calendar'(
             [display(flex),
              'flex-direction'(row)],
             '.day'([margin('0.5em')],
                    '.meal-slot'([width('2em'),
                                  height('2em'),
                                  margin('0.5em'),
                                  'background-color'(green)])))]).

calendar(State) -->
    html([\include_css(calendar_css),
          div(class(calendar),
              \calendar_items(State.meals_per_day,
                              State.start_date,
                              State.end_date))]).

ts_day(Ts, Day) :-
    number(Ts), !,
    format_time(string(Day), "%Y-%m-%d", Ts).
ts_day(Ts, Day) :-
    string(Day), !,
    parse_time(Day, "%Y-%m-%d", Ts).

calendar_items(_, D, D) --> [].
calendar_items(NSlots, S, E) -->
    { Next is S + 3600*24,
      replicate(NSlots, div(class('meal-slot'), []), Slots),
      ts_day(S, Day) },
    html(div(class(day),
             [span(Day)|Slots])),
    calendar_items(NSlots, Next, E).
