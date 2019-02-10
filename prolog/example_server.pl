/** <module> Example server
* Demo of the Vue/Pengine integration
*/
:- module(example_server, [go/1]).

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

:- use_module(example_render, [meal_plan_page//1]).

% main start
:- use_module(example_api).
:- use_module(library(pengines)).
:- pengine_application(meals_app).
:- use_module(meals_app:example_api).

%! go(+Port) is det.
%  Main entry point to start the server.
%
%  @arg Port Integer port number to start the server on.
go(Port) :-
    http_set_session_options([]),
    http_server(http_dispatch, [port(Port)]).

% Routes

:- http_handler(root(.), meal_plan_handler, []).

:- multifile user:head//2.
user:head(app, Head) -->
    html(head([Head, \html_receive(css)])).

:- multifile user:body//2.
user:body(app, Body) -->
    html(body([Body,
               script(src('https://cdn.jsdelivr.net/npm/vue/dist/vue.js'), []),
               script(src('https://cdn.jsdelivr.net/npm/jquery@3.3.1/dist/jquery.min.js'), []),
               script(src('/pengine/pengines.js'), []),
               \html_receive(js)])).

%! meal_plan_handler(+Request) is semidet.
%  Handler for main meal plain page.
meal_plan_handler(Request) :-
    memberchk(method(get), Request),
    example_api:init_state(State),
    reply_html_page(app,
        title('Eating Plan'),
        \meal_plan_page(State)).
