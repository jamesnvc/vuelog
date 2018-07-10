:- module(api, [init_state/1, handle_event/3]).


:- use_module(library(clpfd)).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(random), [random_member/2]).
:- use_module(util, [ts_day/2]).

% State calculations

state_check_meals_type, [State1] -->
    [State0],
    { ensure_number(State0.meals_per_day, MPD),
      debug(pengine, "ensure meals per day ~w", [MPD]),
      State1 = State0.put(meals_per_day, MPD) }.

state_gen_slots, [State1] -->
    [State0],
    { _{end_day: EndD, start_day: StartD, meals_per_day: PerDay, meals: Meals} :< State0,
      ts_day(EndTs, EndD), ts_day(StartTs, StartD),
      NSlots is round((EndTs - StartTs) / (3600*24)),
      debug(pengine, "Gen slots for ~w days", [NSlots]),
      numlist(0, NSlots, SlotNums),
      maplist({PerDay,StartTs,Meals}/[N, _{entries: Entries, day: Day}]>>(
                  DayTs is StartTs + 3600*24*N,
                  ts_day(DayTs, Day),
                  length(Entries, PerDay),
                  maplist({Meals}/[E]>>random_member(E, Meals), Entries)
              ), SlotNums, Slots),
      State1 = State0.put(slots, Slots) }.

update_state -->
    { debug(pengine, "update state", []) },
    state_check_meals_type,
    state_gen_slots.

%! init_state(-NewState:dict) is det.
%  NewState is a dictionary representing the fresh app state.
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
                         tags: [pasta]},
                       _{name: "Caldo Verde",
                         tags: [soup, portuguese]}]},
    phrase(update_state, [State0], [State]).

% Events

%! handle_event(+CurrentState:dict, +Event:atom, -NewState:dict) is det.
handle_event(State0, update, State1) :-
    phrase(update_state, [State0], [State1]), !.
handle_event(State0, rerun, State1) :-
    phrase(state_gen_slots, [State0], [State1]), !.
handle_event(State, Event, State) :-
    debug(pengine, "Unknown Pengine event ~w ~w", [State, Event]).

% Helpers

%! ensure_number(+X:any, -N:number) is det.
%  Unify N with X as a number, or zero if X isn't reasonably
%  convertable to a number
ensure_number(N, N) :- number(N), !.
ensure_number(S, N) :-
    string(S), number_string(N, S), !.
ensure_number(_, 0).
