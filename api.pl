:- module(api, [init_state/1, handle_event/3]).


:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(ordsets), [list_to_ord_set/2,
                                 ord_intersection/3,
                                 ord_union/3]).
:- use_module(library(random), [random_member/2]).
:- use_module(util, [ts_day/2]).

% State calculations

state_check_meals_type, [State1] -->
    [State0],
    { ensure_number(State0.meals_per_day, MPD),
      State1 = State0.put(meals_per_day, MPD) }.

state_gen_slots, [State1] -->
    [State0],
    { _{end_day: EndD, start_day: StartD, meals_per_day: PerDay, meals: Meals} :< State0,
      ts_day(EndTs, EndD), ts_day(StartTs, StartD),
      NDays is round((EndTs - StartTs) / (3600*24)),
      numlist(0, NDays, DayNums),
      maplist({PerDay,StartTs,Meals}/[N, _{entries: Entries, day: Day}]>>(
                  DayTs is StartTs + 3600*24*N,
                  ts_day(DayTs, Day),
                  length(Entries, PerDay),
                  maplist({Meals}/[E]>>random_member(E, Meals), Entries)
              ), DayNums, Slots),
      State1 = State0.put(slots, Slots) }.

meal_mealordtags(Meal, NewMeal) :-
    list_to_ord_set(Meal.tags, TagsSet),
    NewMeal = Meal.put(tags, TagsSet).

state_make_tags_sets, [State1] -->
    [State0],
    { maplist(meal_mealordtags, State0.meals, NewMeals),
      State1 = State0.put(meals, NewMeals) }.

update_state -->
    state_check_meals_type,
    state_make_tags_sets,
    state_gen_slots.

%! init_state(-NewState:dict) is det.
%  Create a fresh app state dict.
%
%  @arg NewState Dictionary representing the fresh app state.
init_state(State) :-
    get_time(Start),
    End is Start + 7*3600*24,
    ts_day(Start, StartDay),
    ts_day(End, EndDay),
    % get meals for user
    State0 = _{start_day: StartDay,
               end_day: EndDay,
               meals_per_day: 1,
               meals: [_{name: "Spaghetti d'olio",
                         tags: [pasta, vege, pasta]},
                       _{name: "Caldo Verde",
                         tags: [vege, soup, portuguese]}]},
    phrase(update_state, [State0], [State]).

% Events

%! handle_event(+CurrentState:dict, +Event:atom, -NewState:dict) is det.
%  Given the current state and an event, calculate what the next state
%  will be.
%
%  @arg CurrentState Dictionary representing the current app state.
%  @arg Event An atom representing a state transation.
%  @arg NewState Dictionary representing the state that results from
%                 applying =Event= to =CurrentState=.
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
%
%  @arg X A number or a string.
%  @arg N X coerced to a number.
ensure_number(N, N) :- number(N), !.
ensure_number(S, N) :-
    string(S), number_string(N, S), !.
ensure_number(_, 0).


%! meals_score(+Meal1:dict, +Meal2:dict, -Score:int) is det.
%
%  =Score= is an integer indicating how "similiar" two meals are. 100 is
%  identical, 0 is completely different.
meals_score(M, M, 100) :- !.
meals_score(M1, M2, S) :-
    ord_intersection(M1.tags, M2.tags, I),
    length(I, IL),
    ord_union(M1.tags, M2.tags, U),
    length(U, UL),
    % add a little bit of a fudge factor, so two meals with the same
    % tags aren't the same as exactly the same meal.
    S is integer(IL / (UL * 1.2) * 100).


%! meals_next_score(+Meals, +NextMeal, -Score) is det.
%
%  =Score= is an integer between 0 and 100, indicating how good a fit
%  =NextMeal= would be, after =Meals=. 0 is a perfect fit, 100 is the worst.
meals_next_score([], _, 0).
meals_next_score(Meals, Meal, S) :-
    maplist(meals_score(Meal), Meals, Scores),
    % make the scores drop off for recent
    true.
