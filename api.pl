:- module(api, [init_state/1, handle_event/3]).


:- use_module(library(clpfd)).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(util, [ts_day/2]).

% State calculations

state_gen_slots(State0, State1) :-
    _{end_day: EndD, start_day: StartD, meals_per_day: PerDay} :< State0,
    ts_day(EndTs, EndD), ts_day(StartTs, StartD),
    NSlots is round((EndTs - StartTs) / (3600*24)),
    debug(pengine, "Gen slots for ~w days", [NSlots]),
    length(Slots, NSlots),
    Test #= NSlots * PerDay,
    maplist({PerDay,Test}/[X]>>(
                length(X, PerDay),
                maplist(=(Test), X)
            ),
            Slots),
    State1 = State0.put(slots, Slots).

update_state -->
    state_gen_slots.

init_state(State) :-
    get_time(Start),
    End is Start + 7*3600*24,
    ts_day(Start, StartDay),
    ts_day(End, EndDay),
    % get meals for user
    State0 = _{start_day: StartDay,
               end_day: EndDay,
               meals_per_day: 2,
               meals: [_{name: "Spaghetti d'olio",
                         id: 1,
                         tags: [pasta]},
                       _{name: "Caldo Verde",
                         id: 2,
                         tags: [soup]}]},
    update_state(State0, State).

% Events

handle_event(State, inc_meals, OutState):-
    debug(pengine, "inc_meals event ~w", [State]),
    ensure_number(State.meals_per_day, MealsPerDay),
    IncMeals is MealsPerDay + 1,
    update_state(State.put(meals_per_day, IncMeals), OutState).

handle_event(State0, update, State1) :-
    update_state(State0, State1).

handle_event(State, Event, State) :-
    debug(pengine, "Unknown Pengine event ~w ~w", [State, Event]).

% Helpers

ensure_number(N, N) :- number(N).
ensure_number(S, N) :-
    string(S), number_string(N, S).
