/** <module> Rendering
* Predicates for generating HTML, CSS, and Javascript.
*/
:- module(render, [meal_plan_page//1]).

:- use_module(library(http/html_write), [html//1, html_post//2]).
:- use_module(library(http/js_write), [javascript/4, js_expression//1]).
:- use_module(library(css_write), [css//1, write_css/2]).

:- use_module(util, [listof//2]).

% Helper predicates

include_css(CssDcg) -->
    { write_css(CssDcg, CssTxt) },
    html_post(css, style([], CssTxt)).

include_js(JsTxt) -->
    html_post(js, JsTxt).

% Rendering pages

main_js(State) -->
    include_js(
        script(type('text/javascript'),
               {|javascript(State)||
                var appEl = document.getElementById('app');
                var template = quenchVue.createAppTemplate(appEl);
                var _updating = false;
                var conf = {application: "meals_app",
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
                       const name = event.target.elements["name"].value;
                       const tags = event.target.elements["tags"].value.split(/,\s*/);
                       const days = parseInt(event.target.elements["days"].value, 10);
                       app.meals.push({name: name, tags: tags, days: days});
                       for (let attr in ["name", "tags", "days"]) {
                         event.target.elements[attr].value = "";
                       }
                     },
                     regenSchedule: function(_event) {
                       if (_updating) return;
                       let state = Object.keys(app.$data)
                           .reduce((o, k) => { o[k] = app[k]; return o; },
                                   {});
                       let stateJson = Pengine.stringify(state);
                       _updating = true;
                       pengine.ask(`handle_event(${stateJson}, rerun, S)`);
                     }
                   }});
     |})).


%! meal_plan_page(+State:dict)//
%  @arg State A dict representing the app state.
%  @see api:init_state/1.
%
%  DCG for the main page to display meal plans.
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
               div(class(schedule), [h2("Schedule"),
                                     button('@click.prevent'(regenSchedule),
                                            "New Schedule"),
                                     \calendar(State)])]),
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
    html(li([class(meal), 'v-for'("meal in meals")],
            [span('v-text'('meal.name'), Meal.name),
             br([]),
             span('Makes a meal for '), span('v-text'('meal.days'), Meal.days),
             span(' days'),
             br([]),
             \listof(meal_tag, Meal.tags)])).

meal_tag(Tag) -->
    html(span('v-for'("tag in meal.tags"),
              [span('v-text'(tag), Tag),
               &(nbsp)])).

add_meal -->
    html(form(['@submit.prevent'("addMeal")],
              [input([type(text), name(name), required(true),
                      placeholder('Food name')]),
               input([type(text), name(tags), required(true),
                      placeholder('Comma-separated tags')]),
               input([type(number), name(days), required(true), min(1),
                      placeholder('How many days will this last?')]),
               input([type(submit), value('Add')])])).

calendar_css -->
    css(['.calendar'(
             [display(flex), 'flex-direction'(row)],
             '.day'([margin('0.5em')],
                    '.meal-slot'(['min-height'('2em'),
                                  margin('0.5em'),
                                  'text-align'(center),
                                  'background-color'(darkgreen),
                                  color(white)])))]).

calendar(State) -->
    html([\include_css(calendar_css),
          div(class(calendar),
              \listof(calendar_item, State.slots))]).

calendar_item(Slot) -->
    html(div([class(day), 'v-for'("slot in slots")],
             [span(['v-text'('slot.day')], Slot.day),
              \listof(calendar_slot, Slot.entries)])).

calendar_slot(E) -->
    html(div([class('meal-slot'), 'v-for'("entry in slot.entries"), 'v-text'('entry.name')],
             [E.name])).

     /*******************************
     *               C    *
     *******************************/

/*
<script src="https://unpkg.com/vue"></script>


<ul id="example-1">
  <li v-for="item in items">
    <p>{{ item.message }}</p>
  </li>
</ul>


var example1 = new Vue({
  el: '#example-1',
  data: {
    items: [

    ]
  }
})

*/

my_little_list(State) -->
    html(ul(id('example-1'),
            v_for( [loop_var(item), loop_over(State.items)],
                li(
                    curlies('item.message')
                )
            )
           )
        ).

my_little_list(State) -->
    vue_ul_list(State, items,
            p(curlies($message))
           ).

my_little_list(State) -->
    vue_context(State,
                \mll).

mll -->
        vue_list(ul, items,
            p(['my message is ', $message])
           ).



my_little_list(State) -->
    html(ul(id('example-1'),
                li( 'v-for'("item in items"),
                    vue('item.message')
                )
           )
        ).
