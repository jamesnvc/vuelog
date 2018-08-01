/** <module> Rendering
* Predicates for generating HTML, CSS, and Javascript.
*/
:- module(render, [meal_plan_page//1]).

:- use_module(vue_html, [vue_context//2, vue_html//1]).
:- use_module(library(css_write), [css//1, write_css/2]).

meal_plan_page(State) -->
    vue_context(State,
                div([id(app)],
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
                     div(class(meals), \meals),
                     div(class(schedule), [h2("Schedule"),
                                           button('@click.prevent'(regenSchedule),
                                                  "New Schedule"),
                                           \calendar(State)])])).

meals -->
    vue_html([h2("Menu Options"),
              vue_list(ul, [meals],
                       li([class(meal)]),
                       [$name,
                        br([]),
                        p(['Makes a meal for ', $days, ' days']),
                        br([]),
                        vue_list(div, $tags,
                                 span,
                                 $)]),
              \add_meal]).


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
