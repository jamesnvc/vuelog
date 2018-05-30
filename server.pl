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
:- use_module(library(http/html_head), [html_resource/2, html_requires//1]).
:- use_module(library(http/http_files), [http_reply_from_files/3]).
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

% Routes
:- multifile http:location/3.
:- dynamic http:location/3.
http:location(js, '/js', []).
user:file_search_path(js, './js').

:- html_resource(app_script, [virtual(true),
                              requires(['/pengine/pengines.js', js('app.js')]),
                              ordered(true),
                              mime_type(text/javascript)]).

:- http_handler(js(.), http_reply_from_files('js/', []),
                [priority(1000), prefix]).

:- http_handler(/, meal_plan_handler, []).

% main handler
meal_plan_handler(Request) :-
    memberchk(method(get), Request),
    init_state(State) ,
    reply_html_page(
        [title('Eating Plan'),
         \html_receive(css)],
        [\meal_plan_page(State),
         \html_requires(app_script)]).

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

% make schedule
suggest_schedule(_State).
