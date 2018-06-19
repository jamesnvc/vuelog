:- module(render, [meal_plan_page//1]).

:- use_module(library(http/html_write), [html//1, html_post//2]).
:- use_module(library(http/js_write), [javascript/4, js_expression//1]).
:- use_module(library(http/json), [atom_json_term/3]).
:- use_module(library(css_write), [css//1, write_css/2]).
:- use_module(library(list_util), [replicate/3]).

:- use_module(util, [ts_day/2]).

% Helper predicates

include_css(CssDcg) -->
    { write_css(CssDcg, CssTxt) },
    html_post(css, style([], CssTxt)).

include_js(JsTxt) -->
    html_post(js, JsTxt).

:- meta_predicate listof(//, +).
%% listof(DCG//1, Elements) generates a list of elements from a DCG
%% (presumably html//1) to make them work with Quench Vue.
listof(DCG, Elements) -->
    listof(DCG, Elements, true).
listof(_, [], _) --> [].
listof(DCG, [E|Rest], true) -->
    call(DCG, E), listof(DCG, Rest, false).
listof(DCG, [E|Rest], false) -->
    ["<!-- <q> -->"],
    call(DCG, E),
    ["<!-- </q> -->"],
    listof(DCG, Rest, false).

% Rendering pages

main_js(State) -->
    include_js(
        script(type('text/javascript'),
               {|javascript(State)||
                var appEl = document.getElementById('app');
                var template = quenchVue.createAppTemplate(appEl);
                var _updating = false;
                var pengine = new Pengine({application: "meals_app",
                                           onsuccess: function() {
                                             const newState = this.data[0].S;
                                             for (let k in newState) {
                                               app[k] = newState[k];
                                             }
                                             app.$nextTick(() => _updating = false);
                                           },
                                           onerror: function() {
                                             console.error("Pengine error", this);
                                             app.$nextTick(() => _updating = false);
                                           }
                                          });
                var app = new Vue(
                  {el: appEl,
                   data: State,
                   template: template,
                   beforeUpdate: function() {
                     if (_updating) return;
                     _updating = true;
                     let state = Object.keys(app.$data)
                         .reduce((o, k) => { o[k] = app[k]; return o; },
                                 {});
                     let stateJson = Pengine.stringify(state);
                     pengine.ask(`handle_event(${stateJson}, update, S)`);
                   },
                   methods: {
                     addMeal: function(event) {
                       let name = event.target.elements["name"].value;
                       app.meals.push({name: name});
                       event.target.elements["name"].value = "";
                     }}});
     |})).

meal_plan_page(State) -->
    html([div([id(app)],
              [div(class('parameters'),
                   [label(["Start Date",
                           input([type(date), 'v-model'(start_day),
                                  value(State.start_day)], [])]),
                    label(["End Date",
                           input([type(date), 'v-model'(end_day),
                                 value(State.end_day)], [])]),
                    label(["Meals per day",
                           input([type(number), 'v-model.number'(meals_per_day),
                                  value(State.meals_per_day)], [])])]),
               div(class(meals), \meals(State)),
               div(class(schedule), [h2("Schedule"), \calendar(State)])]),
          \main_js(State)]).

% XXX: right now, the places that use `listof' rely on the author to
% name the arguments to the `v-for' bit correctly (e.g. for the list
% of meal items in State.meals, the v-for needs to be looping over
% "meals")
% Idea: make the listof thing take the State and key & let it add the
% appropriate v-for?
% Issue: Nested loops, which instead rely on knowing the name the
% thing in the outer loop was bound to (e.g. calendar_slot).

meals(State) -->
    html([h2("Menu Options"),
          ul(\listof(meal_item, State.meals)),
          \add_meal]).

meal_item(Meal) -->
    html(li([class(meal), 'v-for'("meal in meals"),
             'v-text'("meal.name")],
            Meal.name)).

add_meal -->
    html(form(['@submit.prevent'("addMeal")],
              [input([type(text), name(name), placeholder('Food name')]),
              input([type(submit), value('Add')])])).

calendar_css -->
    css(['.calendar'(
             [display(flex),
              'flex-direction'(row)],
             '.day'([margin('0.5em')],
                    '.meal-slot'([width('2em'),
                                  height('2em'),
                                  margin('0.5em'),
                                  'background-color'(green)])))]).

calendar(State) -->
    html([\include_css(calendar_css),
          div(class(calendar),
              \listof(calendar_item, State.slots))]).

calendar_item(Slot) -->
    { Day = "foo" },
    html(div([class(day), 'v-for'("slot in slots")],
             [span(Day),
              \listof(calendar_slot, Slot)])).

calendar_slot(S) -->
    html(div([class('meal-slot'), 'v-for'("x in slot"), 'v-text'(x)],
             [S])).
