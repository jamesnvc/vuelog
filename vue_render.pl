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
                                 vue_input([type(date), 'model'(start_day)], [])]),
                          label(["End Date",
                                 vue_input([type(date), model(end_day)], [])]),
                          label(["Meals per day",
                                 vue_input([type(number), 'model.number'(meals_per_day)], [])])]),
                     div(class(meals), \meals),
                     div(class(schedule),
                         [h2("Schedule"),
                          vue_button(click(regenSchedule), "New Schedule"),
                          \calendar(State)])])).

meals -->
    vue_html([h2("Menu Options"),
              vue_list(ul, [meal in meals],
                       li([class(meal)]),
                       [$meal.name,
                        br([]),
                        p(['Makes a meal for ', $meal.days, ' days']),
                        br([]),
                        vue_list(div, [tag in $meal.tags],
                                 span,
                                 $tag)]),
              \add_meal]).


add_meal -->
    vue_html(vue_form([submit(addMeal)],
                      [input([type(text), name(name), required(true),
                              placeholder('Food name')]),
                       input([type(text), name(tags), required(true),
                              placeholder('Comma-separated tags')]),
                       input([type(number), name(days), required(true), min(1),
                              placeholder('How many days will this last?')]),
                       input([type(submit), value('Add')])])).
