:- module(server, [go/1]).

:- use_module(library(http/thread_httpd), [http_server/2]).
:- use_module(library(http/http_dispatch), [http_redirect/3,
                                            http_dispatch/1,
                                            http_handler/3]).
:- use_module(library(http/http_session), [http_session_asserta/1,
                                           http_session_retractall/1,
                                           http_session_data/1,
                                           http_set_session_options/1]).
:- use_module(library(http/html_write), [html_post//2, html_receive//1, reply_html_page/2,
                                         html//1]).
:- use_module(library(http/http_parameters), [http_parameters/3]).
:- use_module(library(css_write), [css//1, write_css/2]).
:- use_module(library(list_util), [replicate/3]).

% State

init_state(State) :-
    get_time(NowTs),
    Today is integer(NowTs) div (3600 * 24),
    End is Today + 7,
    % get meals for user
    State = _{start_date: Today,
              end_date: End,
              meals_per_day: 2,
              meals: [_{name: "Pasta"},
                      _{name: "Caldo Verde"}] }.

go(Port) :-
    http_set_session_options([]),
    http_server(http_dispatch, [port(Port)]).

% Routes
:- http_handler(/, meal_plan_handler, []).

% main handler
meal_plan_handler(Request) :-
    memberchk(method(get), Request),
    init_state(State) ,
    reply_html_page(
        [title('Eating Plan'),
         \html_receive(css)],
        \meal_plan_page(State)
    ).

include_css(CssDcg) -->
    { write_css(CssDcg, CssTxt) },
    html_post(css, style([], CssTxt)).

meal_plan_page(State) -->
    html([div(id(main),
              [div(class('parameters'),
                   % TODO: figure out how to make these vars work for "reactive"
                   % maybe have pengines re-evaluate the DCG?
                   [label(["Start Date",
                           input([type(date), value(State.start_date)], [])]),
                    label(["End Date",
                           input([type(date), value(State.end_date)], [])]),
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

availability(State) -->
    html([h2("Availability"),
          div([])
            ]).

calendar(State) -->
    html([\include_css(
              css(['.calendar'([display(flex),
                                'flex-direction'(row)],
                               '.day'([margin('0.5em')],
                                   '.meal-slot'([width('2em'),
                                                 height('2em'),
                                                 margin('0.5em'),
                                                 'background-color'(green)])))])),
          div(class(calendar),
              \calendar_items(State.meals_per_day, State.start_date, State.end_date))]).

calendar_items(_, D, D) --> [].
calendar_items(NSlots, S, E) -->
    { Next is S + 1,
      replicate(NSlots, div(class('meal-slot'), []), Slots) },
    html(div(class(day),
             % TODO: show actual date
             [span(S)|Slots])),
    calendar_items(NSlots, Next, E).

% make schedule

suggest_schedule(State).
