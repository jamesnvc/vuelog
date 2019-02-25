:- module(vuelog, [vue_html//1,
                   vue_context//2,
                   qvue_html/2,
                   op(400, xfx, in)
                  ]).

:- use_module(library(http/html_write), [html//1, html_post//2, op(_, _, html_meta)]).
:- use_module(library(http/js_write), [javascript/4,
                                       js_expression//1]).
:- use_module(library(http/json), [json_write_dict/3]).

include_js(JsTxt) -->
    html_post(js, JsTxt).

:- html_meta vue_context(+, html, ?, ?).
vue_context(CtxDict, Stuff) -->
    { _{initial_state: State,
        pengine_app_name: PengineName,
        root_element_sel: Elt} :< CtxDict },
    include_js(script(type('text/javascript'),
                {|javascript(State, PengineName, Elt)||
                 let appEl = document.querySelector(Elt);
                 let _updating = false;
                 let conf = {application: PengineName,
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
                 let pengine = new Pengine(conf);
                 const updateFn = (event) => {
                   if (_updating) return;
                   _updating = true;
                   const state = Object.keys(app.$data)
                         .reduce((o, k) => { o[k] = app[k]; return o; },
                                 {});
                   const stateJson = Pengine.stringify(state);
                   pengine.ask(`handle_event(${stateJson}, ${event}, S)`);
                 };
                 let app = new Vue(
                   {el: appEl,
                    data: State,
                    beforeUpdate: function() {
                      updateFn('update');
                      // console.log("Update");
                    },
                    methods: {
                      handleFormEvent: function(eventName, event) {
                        let formArgs = {};
                        for (const k of event.target.elements) {
                          if (k.type !== 'submit') {
                            formArgs[k.name] = k.value;
                            k.value = null;
                          }
                        }
                        const eventArgs = Pengine.stringify(formArgs);
                        updateFn(`${eventName}(${eventArgs})`);
                      },
                      handleEvent: function(eventName) {
                        updateFn(eventName);
                      }
                    }
                   });
|})),
    vue_html(Stuff).

:- html_meta vue_html(html, ?, ?).
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
                form(['@submit.prevent'('handleFormEvent("' + Event + '", $event)')],
                     ExElements)) :-
    qvue_html(Elements, ExElements).
vue_html_expand(vue_list(in(Key, Vals),
                         Container),
                ListTemplateElt) :-
    Container =.. [ContainerElt, Props_, Children],
    format(string(BindingAtom), "~w in ~w", [Key, Vals]),
    vue_html_expand_attrs(Props_, ExProps_),
    Props = ['v-for'(BindingAtom)|ExProps_],
    qvue_html(Children, TemplateBody),
    ListTemplateElt =.. [ContainerElt, Props, TemplateBody].
vue_html_expand($(Var), TemplateVar) :-
    format(string(TemplateVar), "{{ ~w }}", [Var]).
vue_html_expand(ElAttrsChildren, ExElt) :-
    ElAttrsChildren =.. [El, Attrs, Children],
    qvue_html(Children, ExChildren),
    vue_html_expand_attrs(Attrs, ExAttrs),
    ExElt =.. [El, ExAttrs, ExChildren].
vue_html_expand(ElChildren, ExElt) :-
    ElChildren =.. [El, Children],
    qvue_html(Children, ExChildren),
    ExElt =.. [El, ExChildren].
vue_html_expand(A, A).

vue_html_expand_attrs(Attrs, ExAttrs) :-
    is_list(Attrs), !,
    debug(xxx, "Expanding attrs ~w", [Attrs]),
    maplist(vue_html_expand_attr, Attrs, ExAttrs).
vue_html_expand_attrs(Attr, ExAttr) :-
    vue_html_expand_attr(Attr, ExAttr).

vue_html_expand_attr(model(Model), 'v-model'(Model)).
vue_html_expand_attr(click(Event), '@click'('handleEvent("' + Event + '")')).
vue_html_expand_attr(@(AttrProps), VBindAttrProps) :-
    debug(xxx, "Expanding ~w", [AttrProps]),
    AttrProps =.. [Attr, PropOrProps],
    format(atom(VBindAtom), "v-bind:~w", [Attr]),
    ensure_list(PropOrProps, Props),
    maplist([K=V, KV]>>(format(atom(KV), "'~w': ~w", [K, V])),
            Props, PropBindings),
    atomic_list_concat(PropBindings, ', ', PropBinding),
    atomic_list_concat(['{', PropBinding, '}'], Bindings),
    VBindAttrProps =.. [VBindAtom, Bindings].
vue_html_expand_attr(A, A).


ensure_list(Xs, Xs) :- is_list(Xs), !.
ensure_list(X, [X]).
