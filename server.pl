:- module(server, [go/1]).

:- use_module(library(http/thread_httpd), [http_server/2]).
:- use_module(library(http/http_dispatch), [http_redirect/3,
                                            http_dispatch/1,
                                            http_handler/3]).
:- use_module(library(http/http_session), [http_session_asserta/1,
                                           http_session_retractall/1,
                                           http_session_data/1,
                                           http_set_session_options/1]).
:- use_module(library(http/html_write), [html_receive//1,
                                         reply_html_page/2]).
:- use_module(library(http/http_parameters), [http_parameters/3]).

:- use_module(render, [meal_plan_page//1]).

% main start
:- use_module(api).
:- use_module(library(pengines)).
:- pengine_application(meals_app).
:- use_module(meals_app:api).

go(Port) :-
    http_set_session_options([]),
    http_server(http_dispatch, [port(Port)]).

% State

init_state(State) :-
    get_time(Start),
    End is Start + 7*3600*24,
    % get meals for user
    State = _{start_date: Start,
              end_date: End,
              meals_per_day: 2,
              meals: [_{name: "Spaghetti d'olio",
                        tags: [pasta]},
                      _{name: "Caldo Verde",
                        tags: [soup]}]}.

% Routes
:- http_handler(/, meal_plan_handler, []).

% main handler
meal_plan_handler(Request) :-
    memberchk(method(get), Request),
    init_state(State) ,
    reply_html_page(
        [title('Eating Plan'),
         \html_receive(css)],
        [\meal_plan_page(State),
         script(src('/pengine/pengines.js'), [])]).

% new plan: hook up pengines, just write javascript for now to send
% query when changing stuff. That query can then update the state
% (which...will be stored for the user? or query will send state
% along, I guess), then send the updated HTML and js can just
% innerHTML it in

% make schedule
suggest_schedule(_State).
