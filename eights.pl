% This is identical to best1, except a nontrivial definition is given for
% the heuristic predicate.

:- [adts].

%% Top Left
move([0, C1, C2, C3, C4, C5, C6, C7, C8], [C3, C1, C2, 0, C4, C5, C6, C7, C8]).
move([0, C1, C2, C3, C4, C5, C6, C7, C8], [C1, 0, C2, C3, C4, C5, C6, C7, C8]).

%% Top Right
move([C1, C2, 0, C3, C4, C5, C6, C7, C8], [C1, 0, C2, C3, C4, C5, C6, C7, C8]).
move([C1, C2, 0, C3, C4, C5, C6, C7, C8], [C1, C2, C5, C3, C4, 0, C6, C7, C8]).

%% Bottom Left
move([C1, C2, C3, C4, C5, C6, 0, C7, C8], [C1, C2, C3, 0, C5, C6, C4, C7, C8]).
move([C1, C2, C3, C4, C5, C6, 0, C7, C8], [C1, C2, C3, C4, C5, C6, C7, 0, C8]).

%% Bottom Right
move([C1, C2, C3, C4, C5, C6, C7, C8, 0], [C1, C2, C3, C4, C5, 0, C7, C8, C6]).
move([C1, C2, C3, C4, C5, C6, C7, C8, 0], [C1, C2, C3, C4, C5, C6, C7, 0, C8]).

%% Center Tile
move([C1, C2, C3, C4, 0, C5, C6, C7, C8], [C1, 0, C3, C4, C2, C5, C6, C7, C8]).
move([C1, C2, C3, C4, 0, C5, C6, C7, C8], [C1, C2, C3, C4, C7, C5, C6, 0, C8]).
move([C1, C2, C3, C4, 0, C5, C6, C7, C8], [C1, C2, C3, 0, C4, C5, C6, C7, C8]).
move([C1, C2, C3, C4, 0, C5, C6, C7, C8], [C1, C2, C3, C4, C5, 0, C6, C7, C8]).

%% Left Middle
move([C1, C2, C3, 0, C4, C5, C6, C7, C8], [C1, C2, C3, C4, 0, C5, C6, C7, C8]).
move([C1, C2, C3, 0, C4, C5, C6, C7, C8], [C1, C2, C3, C6, C4, C5, 0, C7, C8]).
move([C1, C2, C3, 0, C4, C5, C6, C7, C8], [0, C2, C3, C1, C4, C5, C6, C7, C8]).

%% Right Middle
move([C1, C2, C3, C4, C5, 0, C6, C7, C8], [C1, C2, C3, C4, 0, C5, C6, C7, C8]).
move([C1, C2, C3, C4, C5, 0, C6, C7, C8], [C1, C2, 0, C4, C5, C3, C6, C7, C8]).
move([C1, C2, C3, C4, C5, 0, C6, C7, C8], [C1, C2, C3, C4, C5, C8, C6, C7, 0]).

%% Top Middle 
move([C1, 0, C2, C3, C4, C5, C6, C7, C8], [0, C1, C2, C3, C4, C5, C6, C7, C8]).
move([C1, 0, C2, C3, C4, C5, C6, C7, C8], [C1, C2, 0, C3, C4, C5, C6, C7, C8]).
move([C1, 0, C2, C3, C4, C5, C6, C7, C8], [C1, C4, C2, C3, 0, C5, C6, C7, C8]).

%% Bottom Middle
move([C1, C2, C3, C4, C5, C6, C7, 0, C8], [C1, C2, C3, C4, C5, C6, C7, C8, 0]).
move([C1, C2, C3, C4, C5, C6, C7, 0, C8], [C1, C2, C3, C4, C5, C6, 0, C7, C8]).
move([C1, C2, C3, C4, C5, C6, C7, 0, C8], [C1, C2, C3, C4, 0, C6, C7, C5, C8]).


%unsafe(0).

% In predicate heuristic(State,Goal,Value), Value is to be calculated as the
% estimated distance from the State to the Goal, e.g., in the 8-puzzle this
% might be the number of tiles out of place in State compared with Goal.  

%% If the first argument is some state and the second argument is the goal, 
%% the third argument evaluates to the number of tiles out of place.

heuristic([], [], 0).

heuristic([H1|T1], [H2|T2], V1) :- 
	H1 =:= 0,
	heuristic(T1, T2, V1).

heuristic([H1|T1], [H2|T2], V1) :-
	H1 =:= H2,
	heuristic(T1, T2, V1).

heuristic([H1|T1], [H2|T2], V2) :- 
	H1 =\= H2, 
	heuristic(T1, T2, V1),
	V2 is V1+1.


% The precedes predicate is needed for the priorty-queue code in the atds
% file.  The positions between the [ and ] represent State, Parent (of State),
% Depth (from of State from root), Heuristic (value for State),
% Depth+Heurstic (the value of the evaluation function f(n)=g(n)+h(n)).

precedes([_,_,_,_,F1], [_,_,_,_,F2]) :- F1 =< F2.

%%%%%%% Best first search algorithm%%%%%%%%%

	% go initializes Open and CLosed and calls path	

go(Start, Goal) :- 
	empty_set(Closed_set),
	empty_pq(Open),
	heuristic(Start, Goal, H),
	insert_pq([Start, nil, 0, H, H], Open, Open_pq),
	path(Open_pq, Closed_set, Goal).


	% Path performs a best first search,
	% maintaining Open as a priority queue, and Closed as
	% a set.
	
	% Open is empty; no solution found

path(Open_pq, _, _) :- 
	empty_pq(Open_pq),
	write('graph searched, no solution found').

	% The next record is a goal
	% Print out the list of visited states

path(Open_pq, Closed_set, Goal) :- 
	dequeue_pq([State, Parent, _, _, _], Open_pq, _),
	State = Goal,
	write('Solution path is: '), nl,
	printsolution([State, Parent, _, _, _], Closed_set).
	
	% The next record is not equal to the goal
	% Generate its children, add to open and continue

path(Open_pq, Closed_set, Goal) :- 
	dequeue_pq([State, Parent, D, H, S], Open_pq, Rest_of_open_pq),
        get_children([State, Parent, D, H, S], Rest_of_open_pq, Closed_set,
             Children, Goal),
	insert_list_pq(Children, Rest_of_open_pq, New_open_pq),
	union([[State, Parent, D, H, S]], Closed_set, New_closed_set),
	path(New_open_pq, New_closed_set, Goal),!.

get_children([State, _, D, _, _], Rest_of_open_pq, Closed_set, Children, 
             Goal) :-
        bagof(Child, moves([State, _, D, _, _], Rest_of_open_pq,
             Closed_set, Child, Goal), Children);
        empty_set(Children).

	% moves generates all children of a state that are not already on
	% open or closed.  For each child, it adds 1 to the current depth D
	% calculates the heuristic H for the child, as well as the sum S of
	% these two values (i.e., calculation of f(n)=g(n)+h(n)).

	% Also, unsafe is commented out as we don't need it here.

moves([State, _, Depth, _, _], Rest_of_open_pq, Closed_set, 
             [Next, State, New_D, H, S], Goal) :-
	move(State, Next),
%	not(unsafe(Next)),
	not(member_pq([Next, _, _, _, _], Rest_of_open_pq)),
	not(member_set([Next, _, _, _, _], Closed_set)),
	New_D is Depth + 1,
	heuristic(Next, Goal, H),
	S is New_D + H.

	% Printsolution prints out the solution path by tracing
	% back through the states on closed using parent links.

printsolution([State, nil, _, _, _], _):-  
	write(State), nl.
printsolution([State, Parent, _, _, _], Closed_set) :-
	member_set([Parent, Grandparent, _, _, _], Closed_set),
	printsolution([Parent, Grandparent, _, _, _], Closed_set),
	write(State), nl.

test :-
	go([2, 8, 3, 1, 6, 4, 7, 0, 5], [1, 2, 3, 8, 0, 4, 7, 6, 5]).
