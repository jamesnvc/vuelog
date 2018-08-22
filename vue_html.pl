:- module(vue_html, [vue_html//1,
                     vue_context//2,
                     qvue_html/2,
                     op(400, xfx, in)
                    ]).

:- use_module(library(http/html_write), [html//1]).
:- use_module(library(http/js_write), [javascript/4,
                                       js_expression//1]).
:- use_module(library(http/json), [json_write_dict/3]).

:- module_transparent(vue_html//1).


vue_context(State, Stuff) -->
    html(script(type('text/javascript'),
                {|javascript(State)||
                  var appEl = document.getElementById('???');
                var template = quenchVue.createAppTemplate(appEl);
                  var app = new Vue(
                                {el: appEl,
                                 data: State,
                                 template: template,
                                 // TODO
                                });
                |})),
    vue_html(Stuff).

vue_html(Spec) -->
    { qvue_html(Spec, Vued), ! },
    html(Vued).

qvue_html(Var, _) :-
    var(Var), !, instantiation_error(Var).
qvue_html([], []) :- !.
qvue_html([H|T], [RH|RT]) :- !,
    vue_html_expand(H, RH),
    qvue_html(T, RT).
qvue_html(Spec, ExSpec) :-
    vue_html_expand(Spec, ExSpec).

vue_html_expand(Tok, Tok) :- atomic(Tok).
vue_html_expand(\Term, \Term).
vue_html_expand(vue_form(submit(Event), Elements),
                form(['@submit'('handleEvent(' + Event + ')')],
                     ExElements)) :-

    qvue_html(Elements, ExElements).
vue_html_expand(vue_input(Attrs),
                input(['v-model'(Prop)|OutAttrs], [])) :-
    selectchk(model(Prop), Attrs, OutAttrs).
vue_html_expand(vue_list(in(Key, Vals),
                         Container),
                ListTemplateElt) :-
    Container =.. [ContainerElt, Props_],
    format(atom(BindingAtom), "~w in ~w", [Key, Vals]),
    Props = ['v-for'(BindingAtom)|Props_],
    qvue_html(Container, TemplateBody),
    ListTemplateElt =.. [ContainerElt, Props, TemplateBody].
vue_html_expand($(Var), TemplateVar) :-
    format(atom(TemplateVar), "{{ ~w }}", [Var]).
