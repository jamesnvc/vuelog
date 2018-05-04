:- module(server, [go/1]).

:- use_module(library(http/thread_httpd), [http_server/2]).
:- use_module(library(http/http_dispatch), [http_redirect/3,
                                            http_dispatch/1,
                                            http_handler/3]).
:- use_module(library(http/http_session), [http_session_asserta/1,
                                           http_session_retractall/1,
                                           http_session_data/1,
                                           http_set_session_options/1]).
:- use_module(library(http/html_write), [reply_html_page/2,
                                         html//1]).
:- use_module(library(http/http_parameters), [http_parameters/3]).

% TODO:
authenticate_user(_, _, _).
register_user(_, _, _).

go(Port) :-
    http_set_session_options([]),
    http_server(http_dispatch, [port(Port)]).

% Routes
:- http_handler(/, logged_in_page(home_page), []).
:- http_handler('/login', login_handler, []).
:- http_handler('/logout', logout_handler, []).

% Login/out/register handlers
logged_in(User) :-
    http_session_data(logged_in(User)).

logged_in_page(Page, Request) :-
    ( logged_in(_)
    -> call(Page, Request)
    ;  reply_html_page(
           [title('Login or Register')],
           \login_body)).

login_body -->
    html(div(id(body),
             [div(id(login),
                  [h1('Log In'),
                   form([action('/login'), method('POST')],
                        [label(['User Name:',
                                input([type(text), name(uname)], [])]),
                         label(['Password:',
                                input([type(password), name(pw)])]),
                         input([type(submit), value(submit)])]) ]),
              div(id(register),
                  [h1('Register'),
                   form([action('/register'), method('POST')],
                        [label('User Name:',
                               input([type(text), name(uname)], [])),
                         label(['Password',
                                input([type(password), name(pw)])]),
                         label(['Confirm Password',
                                input([type(password), name(pw_conf)])]),
                         input([type(submit), value(submit)])])])])).


login_handler(Request) :-
    memberchk(method(post), Request),
    http_parameters(Request, [], [form_data(Data)]),
    memberchk(uname=Uname, Data),
    memberchk(pw=Passwd, Data),
    authenticate_user(Uname, Passwd, UserId),
    http_session_asserta(logged_in(UserId)),
    !, http_redirect(moved, '/', Request).
login_handler(_Request) :-
    % TODO: "flash" in session
    reply_html_page([title('Login or Register')],
                   \login_body).

logout_handler(Request) :-
    memberchk(method(post), Request),
    http_session_retractall(logged_in(_)),
    !, http_redirect(moved, '/', Request).

register_handler(Request) :-
    memberchk(method(post), Request),
    http_parameters(Request, [], [form_data(Data)]),
    memberchk(uname=Uname, Data),
    memberchk(pw=Passwd, Data),
    memberchk(pw_conf=PasswdConf, Data),
    Passwd = PasswdConf,
    register_user(Uname, Passwd, UserId),
    http_session_asserta(logged_in(UserId)),
    !, http_redirect(moved, '/', Request).
