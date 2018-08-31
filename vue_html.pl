:- module(vue_html, [vue_html//1,
                     vue_context//3,
                     qvue_html/2,
                     op(400, xfx, in)
                    ]).

:- use_module(library(http/html_write), [html//1, html_post//2]).
:- use_module(library(http/js_write), [javascript/4,
                                       js_expression//1]).
:- use_module(library(http/json), [json_write_dict/3]).

include_js(JsTxt) -->
    html_post(js, JsTxt).

:- meta_predicate vue_context(+, +, :, -, +).
vue_context(State, Elt, Stuff) -->
    include_js(script(type('text/javascript'),
                {|javascript(State, Elt)||
                 var appEl = document.getElementById(Elt);
                 var _updating = false;
                 var conf = {application: "vuelog_app",
                             onsuccess: function() {
                               const newState = this.data[0].S;
                               for (let k in newState) {
                                 app[k] = newState[k];
                               }
                               if (this.data.more) {
                                 this.stop();
                               }
                               pengine = new Pengine(conf);
                               app.$nextTick(() => _updating = false);
                             },
                             onerror: function() {
                               console.error("Pengine error", this);
                               app.$nextTick(() => _updating = false);
                               pengine = new Pengine(conf);
                             }
                            };
                 var pengine = new Pengine(conf);
                 const updateFn = (event) => {
                   if (_updating) return;
                   _updating = true;
                   let state = Object.keys(app.$data)
                       .reduce((o, k) => { o[k] = app[k]; return o; },
                               {});
                   let stateJson = Pengine.stringify(state);
                   pengine.ask(`handle_event(${stateJson}, ${event}, S)`);
                 };
                 var app = new Vue(
                   {el: appEl,
                    data: State,
                    beforeUpdate: function() {
                      updateFn('update');
                    },
                    methods: {
                      handleEvent: function(eventName, event) {
                        updateFn(eventName);
                      }
                    }
                   });
|})),
    vue_html(Stuff).

:- meta_predicate vue_html(:, -, +).
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
    Container =.. [ContainerElt, Props_, Children],
    format(atom(BindingAtom), "~w in ~w", [Key, Vals]),
    Props = ['v-for'(BindingAtom)|Props_],
    qvue_html(Children, TemplateBody),
    ListTemplateElt =.. [ContainerElt, Props, TemplateBody].
vue_html_expand($(Var), TemplateVar) :-
    format(atom(TemplateVar), "{{ ~w }}", [Var]).
vue_html_expand(ElAttrsChildren, ExElt) :-
    ElAttrsChildren =.. [El, Attrs, Children],
    qvue_html(Children, ExChildren),
    ExElt =.. [El, Attrs, ExChildren].
vue_html_expand(ElChildren, ExElt) :-
    ElChildren =.. [El, Children],
    qvue_html(Children, ExChildren),
    ExElt =.. [El, ExChildren].
vue_html_expand(A, A).
