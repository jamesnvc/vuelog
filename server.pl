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
                                         html//1,
                                         reply_html_page/3]).
:- use_module(library(http/html_head), [html_resource/2, html_requires//1]).
:- use_module(library(http/http_files), [http_reply_from_files/3]).
:- use_module(library(http/http_parameters), [http_parameters/3]).

:- use_module(util, [ts_day/2]).
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

:- http_handler(/, meal_plan_handler, []).

:- multifile user:head//2.
user:head(app, Head) -->
    html(head([Head, \html_receive(css)])).

:- multifile user:body//2.
user:body(app, Body) -->
    html(body([Body,
               script(src('https://cdn.jsdelivr.net/npm/vue/dist/vue.js'), []),
               script(src('https://unpkg.com/quench-vue/umd/quench-vue.min.js'), []),
               script(src('/pengine/pengines.js'), []),
               \html_receive(js)])).

% main handler
meal_plan_handler(Request) :-
    memberchk(method(get), Request),
    init_state(State) ,
    reply_html_page(app,
        title('Eating Plan'),
        \meal_plan_page(State)).

% State

init_state(State) :-
    get_time(Start),
    End is Start + 7*3600*24,
    ts_day(Start, StartDay),
    ts_day(End, EndDay),
    % get meals for user
    State = _{start_day: StartDay,
              end_day: EndDay,
              meals_per_day: 2,
              meals: [_{name: "Spaghetti d'olio",
                        id: 1,
                        tags: [pasta]},
                      _{name: "Caldo Verde",
                        id: 2,
                        tags: [soup]}]}.

% make schedule
suggest_schedule(_State).
