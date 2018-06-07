:- module(api, [handle_event/3]).

:- use_module(library(http/json), [atom_json_dict/3]).

ensure_number(N, N) :- number(N).
ensure_number(S, N) :-
    string(S), number_string(N, S).

handle_event(State, inc_meals, OutState):-
    debug(pengine, "inc_meals event ~w", [State]),
    ensure_number(State.meals_per_day, MealsPerDay),
    IncMeals is MealsPerDay + 1,
    OutState = State.put(meals_per_day, IncMeals).

handle_event(State, Event, State) :-
    debug(pengine, "Unknown Pengine event ~w ~w", [State, Event]).
