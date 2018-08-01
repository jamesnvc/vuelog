:- module(vue_html, [vue_html//1]).

:- use_module(library(http/html_write), [html//1]).
:- use_module(library(http/json), [json_write_dict/3]).

:- module_transparent(vue_html//1).

vue_html(Spec) -->
    { qvue_html(Spec, Vued), ! },
    html(Vued).

qvue_html(Var, _) :-
    var(Var), !, instantiation_error(Var).
qvue_html([], []).
qvue_html([H|T], [RH|RT]) :-
    vue_html_expand(H, RH),
    qvue_html(T, RT).
qvue_html(Spec, ExSpec) :-
    vue_html_expand(Spec, ExSpec).

vue_html_expand(Tok, Tok) :- atomic(Tok).
vue_html_expand(\Term, \Term).
vue_html_expand(vue_state(State), StateJs) :-
    with_output_to(string(StateJSON),
                   json_write_dict(current_output, State, [])),
    StateJs = script([], ['window.vue_state =', StateJSON, ';']).
vue_html_expand(input(Attrs), input(ExAttrs)) :-
    vue_html_expand_attrs(Attrs, ExAttrs).
vue_html_expand(Term, ExTerm) :-
    ( Term =.. [Env, Attrs, Content]
    ; ( Term =.. [Env, Content], Attrs = []) ),
    Content = vue(Prop),
    ExContent = [],
    vue_html_expand_attrs(Attrs, ExAttrs_),
    ExAttrs = ['data-content'(Prop)|ExAttrs_],
    ExTerm =.. [Env, ExAttrs, ExContent].
vue_html_expand(Term, ExTerm) :-
    ( Term =.. [Env, Attrs, Content]
    ; ( Term =.. [Env, Content], Attrs = []) ),
    vue_html_expand_attrs(Attrs, ExAttrs),
    qvue_html(Content, ExContent),
    ExTerm =.. [Env, ExAttrs, ExContent].
vue_html_expand(Term, Term).

vue_html_expand_attrs([], []).
vue_html_expand_attrs([H|T], [ExH|ExT]) :-
    vue_html_expand_attr(H, ExH),
    vue_html_expand_attrs(T, ExT).
vue_html_expand_attrs(Attr, [ExAttr]) :-
    vue_html_expand_attr(Attr, ExAttr).

vue_html_expand_attr(vue_item(Item), 'data-item-template'(Item)).
vue_html_expand_attr(vue_foreach(Items), 'data-foreach'(Items)).
vue_html_expand_attr(vue_sort_by(Prop), 'data-sort-by'(Prop)).
vue_html_expand_attr(onclick_query(Q), 'data-onclick'(Q)).
vue_html_expand_attr(onclick_application(App), 'data-application'(App)).
vue_html_expand_attr(checked(vue(V)), 'data-checked'(V)).
vue_html_expand_attr(value(vue(Prop)), 'data-value'(Prop)).
vue_html_expand_attr(Attr, Attr).
