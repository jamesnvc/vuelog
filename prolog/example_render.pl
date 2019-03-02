/** <module> Rendering
* Predicates for generating HTML, CSS, and Javascript.
*/
:- module(example_render, [meal_plan_page//1]).

:- use_module(library(http/html_write), [html_post//2]).
:- use_module(library(css_write), [css//1, write_css/2]).
:- use_module(vuelog).

include_css(CssDcg) -->
    { write_css(CssDcg, CssTxt) },
    html_post(css, style([], CssTxt)).

page_css -->
    css([body('background-color'(gray)),
         '#app'([margin('1em'),
                 padding('1em'),
                 'font-family'('sans-serif'),
                 'border-radius'('5px'),
                 'background-color'(white)])]).

meal_plan_page(State) -->
    vue_context(_{initial_state: State,
                  pengine_app_name: meals_app,
                  root_element_sel: "#app"},
                div([id(app)],
                    [\include_css(page_css),
                     div(class('parameters'),
                         [label(["Start Date",
                                 input([type(date), model(start_day)], [])]),
                          label(["End Date",
                                 input([type(date), model(end_day)], [])]),
                          label(["Meals per day",
                                 % XXX: some way to indicate that
                                 % meals_per_day should be treated as
                                 % a number?
                                 input([type(number), model(meals_per_day)], [])])]),
                     div(class(meals), \meals),
                     div(class(schedule),
                         [h2("Schedule"),
                          button(click(rerun), "Generate New Schedule"),
                          \calendar])])).

meals_css -->
    css(['.meals-list'(
             [display(flex),
              'flex-direction'(row),
             'flex-wrap'(wrap)],
            ['.meal'(
                 [color(gray),
                  'list-style-type'(none),
                  'padding'('0.5em'),
                  'margin'('0.5em'),
                  'border'('1px solid darkgray'),
                  'border-radius'('5px')
                 ],
                 ['&.active'(color(black)),

                  label(display(block)),

                  'input[type=checkbox]'(display(none))
                 ])
            ])]).

meals -->
    vue_html([\include_css(meals_css),
              h2("Menu Options"),
              ul([class('meals-list')],
                 vue_list(meal in meals,
                          li([class(meal), @(class(active='meal.enabled'))],
                             label([],
                                   [$('meal.name'),
                                    br([]),
                                    p(['Makes a meal for ', $('meal.days'),
                                       ' day', when('meal.days > 1', 's')]),
                                    br([]),
                                    if('meal.enabled',
                                       span([$('meal.name'), " is enabled"]),
                                       span([$('meal.name'), " is disabled"])),
                                    input([type(checkbox), name(toggle),
                                           model('meal.enabled')], []),
                                    br([]),
                                    span("Tags: "),
                                    vue_list(tag in 'meal.tags',
                                             span([], [$(tag), &(nbsp)]))])))),
              \add_meal]).


add_meal -->
    vue_html(vue_form(submit(add_meal),
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

calendar -->
    vue_html([\include_css(calendar_css),
              div(class(calendar),
                  vue_list(slot in slots,
                           div(class(day),
                               [$('slot.day'), \slots('slot.entries')])))
             ]).

slots(Entries) -->
    vue_html(vue_list(entry in Entries,
                      div(class('meal-slot'),
                          $('entry.name')))).
