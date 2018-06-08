:- module(api, [init_state/1, handle_event/3]).

:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(util, [ts_day/2]).

init_state(State) :-
    get_time(Start),
    End is Start + 7*3600*24,
    ts_day(Start, StartDay),
    ts_day(End, EndDay),
    % get meals for user
    State = _{start_day: StartDay,
              end_day: EndDay,
              meals_per_day: 2,
              meals: [_{name: "Spaghetti d'olio",
                        id: 1,
                        tags: [pasta]},
                      _{name: "Caldo Verde",
                        id: 2,
                        tags: [soup]}]}.

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
