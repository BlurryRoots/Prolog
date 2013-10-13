/*
    Author:        	Sven Freiberg
    EMail:        	info@blurryroots.com
    Web:           	http://www.blurryroots.com
    Copyright (C): 	2013 Sven Freiberg
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>
*/

%declare module
:-  module( 
	deterministic_finite_automaton, 
	[dfa_declare/5,
	 dfa_get_states/1,
	 dfa_get_alphabet/1,
	 dfa_get_accepting_states/1,
	 dfa_get_transitions/1,
	 dfa_get_starting_state/1,
	 dfa_delta/3,
	 dfa_is_accepting_state/1
	] 
). 

/*

In order to use these predicates, you have to define all neccessary information
for a complete DFA. These are the following facts:

at least one character:
	character( Character ).

at least one state:
	state( StateName ).

starting state:
	starting_state( StateName ).

the complete definition for state transitions:
	transition( State, Character, ResultingState ).

any amount of accepting states:
	accepting_state( StateName ).

As an alternative you might want to use the declare predicates.
*/

%predefine predicates, for dynamic binding
:- dynamic( character/1 ).
:- dynamic( state/1 ).
:- dynamic( starting_state/1 ).
:- dynamic( transition/3 ).
:- dynamic( accepting_state/1 ).

%alphabet
declare_alphabet( [] ).
declare_alphabet( [Character|Rest] ) :-
	declare_alphabet( Character ),
	declare_alphabet( Rest ).
declare_alphabet( Character ) :-
	asserta( character( Character ) ).

%states
declare_states( [] ).
declare_states( [State|Rest] ) :-
	declare_states( State ),
	declare_states( Rest ).
declare_states( State ) :-
	asserta( state( State ) ).

%starting state
declare_starting_state( StartingState ) :-
	(is_list( StartingState ) -> 
		throw( 'Only on state can be declared as starting state!' ));
	asserta( starting_state( StartingState ) ).

%starting state
declare_a_transition( [StartState, Character, ResultingState] ) :-
	((is_list( StartState ); is_list( Character ); is_list( ResultingState )) ->
		throw( 'No parameter should be a list!' ));
	asserta( transition( StartState, Character, ResultingState ) ).
declare_transitions( [] ).
declare_transitions( [Transition|Rest] ) :-
	declare_a_transition( Transition ),
	declare_transitions( Rest ).

%acceptiong states
declare_accepting_states( [] ).
declare_accepting_states( [AcceptingState|Rest] ) :-
	declare_accepting_states( AcceptingState ),
	declare_accepting_states( Rest ).
declare_accepting_states( AcceptingState ) :-
	asserta( accepting_state( AcceptingState ) ).

%declare a dfa
dfa_declare( Alphabet, States, Transitions, StartingState, AcceptingStates ) :-
	declare_alphabet( Alphabet ),
	declare_states( States ),
	declare_transitions( Transitions ),
	declare_starting_state( StartingState ),
	declare_accepting_states( AcceptingStates ).


/*begin module predicates*/

%find all declared states and put them into a list.
dfa_get_states( States ) :-
	findall( X, state( X ), States ).

%find all declared chacters and put them into a list.
dfa_get_alphabet( Alphabet ) :-
	findall( X, character( X ), Alphabet ).

%find all declared accepting states and put them into a list.
dfa_get_accepting_states( AcceptingStates ) :-
	findall( X, accepting_state( X ), AcceptingStates ).

%find all declared transitions.
dfa_get_transitions( Transitions ) :-
	findall( [A, T, B], transition( A, T, B ), Transitions ).

%return starting state
dfa_get_starting_state( StartingState ) :-
	starting_state( StartingState ).

%transition from State to ResultingState
dfa_delta( State, [], ResultingState ) :-
	ResultingState = State.
dfa_delta( State, [FirstCharacter|RestWord], ResultingState ) :-
	dfa_delta( State, FirstCharacter, ResultOfFirstCharacter ),
	dfa_delta( ResultOfFirstCharacter, RestWord, ResultingState ).
dfa_delta( State, Character, ResultingState ) :-
	transition( State, Character, ResultingState ).

%checks if given state is a declared acceptiong state.
dfa_is_accepting_state( SomeState ) :-
	dfa_get_accepting_states( AcceptingStates ),
	member( SomeState, AcceptingStates ).

/*end module predicates*/
