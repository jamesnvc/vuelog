/** <module> Public API
* State manipulation for the meal planner. This module is exposed via pengines.
*/
:- module(example_api, [init_state/1, handle_event/3]).


:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(ordsets), [list_to_ord_set/2,
                                 ord_intersection/3,
                                 ord_union/3]).
:- use_module(library(random), [random_member/2]).
:- use_module(library(list_util), [minimum_by/3,
                                   split/3,
                                   replicate/3,
                                   iterate/3]).
:- use_module(library(clpfd), [transpose/2]).
:- use_module(util, [ts_day/2]).

% State calculations

state_check_meals_type, [State1] -->
    [State0],
    { ensure_number(State0.meals_per_day, MPD),
      State1 = State0.put(meals_per_day, MPD) }.

state_gen_slots, [State1] -->
    [State0],
    { State0.meals = [], !,
      State1 = State0.put(slots, []) }.
state_gen_slots, [State1] -->
    [State0],
    { _{end_day: EndD, start_day: StartD, meals_per_day: PerDay, meals: Meals} :< State0,
      ts_day(EndTs, EndD),
      ts_day(StartTs, StartD),
      NDays is round((EndTs - StartTs) / (3600*24)),
      length(TSlots, PerDay),
      maplist(schedule(Meals, NDays), TSlots),
      transpose(TSlots, Slots_),
      iterate({EndTs}/[D, Dn, D]>>(D =< EndTs, Dn is D + 3600*24),
              StartTs, Days_),
      % XXX: need to do this extra `take` because iterate/3 yields a
      % lazy list (by with freeze/2? Not entirely sure how it works)
      list_util:take(NDays, Days_, Days),
      maplist({StartTs}/[DayTs, Entries, _{entries: Entries, day: Day}]>>(
                  ts_day(DayTs, Day)
              ), Days, Slots_, Slots),
      State1 = State0.put(slots, Slots) }.

state_make_tags_sets, [State1] -->
    [State0],
    { maplist(meal_mealordtags, State0.meals, NewMeals),
      State1 = State0.put(meals, NewMeals) }.

update_state -->
    state_check_meals_type,
    state_make_tags_sets,
    state_gen_slots.

add_meal_(NewMeal), [State1] -->
    [State0],
    { Meals = [NewMeal|State0.meals],
      State1 = State0.put(meals, Meals) }.

add_meal(NewMeal) -->
    add_meal_(NewMeal),
    update_state.

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
    State0 = state{start_day: StartDay,
                   end_day: EndDay,
                   meals_per_day: 1,
                   meals: [meal{name: "Spaghetti d'olio",
                                days: 1,
                                tags: [pasta, vege, pasta]},
                           meal{name: "Caldo Verde",
                                days: 3,
                                tags: [vege, soup, portuguese]},
                           meal{name: "Steak",
                                days: 1,
                                tags: [meat]}]},
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
handle_event(State0,
             add_meal(js{days: DaysStr, name: Name, tags: TagsStr}),
             State1) :-
    string_codes(TagsStr, TagsCodes),
    split(TagsCodes, 0',, TagsCodesList),
    maplist(atom_codes, Tags, TagsCodesList),
    number_string(Days, DaysStr),
    NewMeal = meal{name: Name, days: Days, tags: Tags},
    phrase(add_meal(NewMeal), [State0], [State1]), !.
handle_event(State, Event, State) :-
    debug(pengine, "Unknown Pengine event ~w ~w", [State, Event]).

% Helpers

%! meal_mealordtags(+OldMeal:dict, -NewMeal:dict) is det.
%  True when =NewMeal= is equivalent to =OldMeal= but with the =tags=
%  list an ordered set.
%  Pulling this out as a separate predicate because dict access in
%  lambdas seems to have weird issues.
meal_mealordtags(Meal, NewMeal) :-
    list_to_ord_set(Meal.tags, TagsSet),
    NewMeal = Meal.put(tags, TagsSet).

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


%! meals_next_score(+Meals:list, +NextMeal:dict, -Score:int) is det.
%  Calculate how good a fit =NextMeal= would be, given the previous
%  list of meals =Meals=.
%
%  @arg Meals List of meals previously scheduled. First element is the
%              most recent one, last element is the first.
%  @arg NextMeal Candidate meal to come next in the schedule.
%  @arg Score Integer from 0 to 100 indicating how good a fit
%              =NextMeal= would be. 0 is perfect fit, 100 is terrible.
%  @see meals_score/3.
meals_next_score([], _, 0) :- !.
meals_next_score(Meals, Meal, S) :-
    maplist(meals_score(Meal), Meals, Scores),
    length(Meals, NMeals),
    numlist(1, NMeals, NsR),
    reverse(NsR, Ns),
    maplist({NMeals}/[N, Score, AdjScore]>>(
                % sigmoid function
                Factor is (1 / (1 + exp(-6*(N / NMeals)))),
                AdjScore is Factor * Score),
            Ns, Scores, AdjustedScores),
    sumlist(AdjustedScores, ScoreSum),
    S is integer(ScoreSum / NMeals).

% making our own version of list_util:minimum_with/3 that just
% compares the projected values. This implies that if two values
% project to the same thing, the one that comes first will be
% selected; we will take advantage of this by shuffling each time.
minimum_with(Project, List, Minimum) :-
    map_list_to_pairs(Project, List, Pairs),
    list_util:minimum_by([O, T1-_, T2-_]>>compare(O, T1, T2), Pairs, _-Minimum).

best_next_meal(Meals, Schedule, NextMeal) :-
    random_permutation(Meals, RandMeals),
    % Note that minimum_by (which minimum_with calls) waits for the
    % input list to be ground, so the dicts must have an actual tag,
    % otherwise this doesn't unify properly
    minimum_with(meals_next_score(Schedule), RandMeals, NextMeal).

meal_fits(N, Meal) :- Meal.days =< N.

schedule(Meals, N, Schedule) :-
    schedule_(Meals, N, [], RSchedule),
    reverse(RSchedule, Schedule).
schedule_(_, 0, Schedule, Schedule) :- !.
schedule_(Meals, N, CurrentSchedule, Schedule) :-
    N > 0,
    include(meal_fits(N), Meals, FilteredMeals),
    best_next_meal(FilteredMeals, CurrentSchedule, NextMeal),
    Nn is N - NextMeal.days,
    replicate(NextMeal.days, NextMeal, NextMeals),
    append(NextMeals, CurrentSchedule, NextSchedule),
    schedule_(Meals, Nn, NextSchedule, Schedule).
