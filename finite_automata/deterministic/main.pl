/*
	Author: Sven Freiberg
	EMail: info@blurryroots.com
	Web: http://www.blurryroots.com
	Copyright (C): 2013 Sven Freiberg

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program. If not, see <http://www.gnu.org/licenses/>
*/

% import module to use predicates from deterministic_finite_automaton.pl
:-  use_module( deterministic_finite_automaton ). 

% define predicate to declare an example automaton
% capable of parsing binary numbers ending with 10
load_example_dfa_definition :-
	dfa_declare(
		[0,1],				% declare alphate
		[a,b,c],			% declare states
		[					% declare transitions
			[ a, 0, a ],	
			[ a, 1, b ],
			[ b, 0, c ],
			[ b, 1, b ],
			[ c, 0, a ],
			[ c, 1, b ]
		],
		a,					% declare starting state
		[c] 				% declare accepted states
	).


% predicate for writing declared dfa to standard outstream
print_dfa :-
	dfa_get_alphabet( Alphabet ), 
	write( 'Alphabet: ' ),
	write( Alphabet ), 
	nl,
	dfa_get_states( States ), 
	write( 'States: ' ),
	write( States ),
	nl,
	dfa_get_transitions( Transitions ), 
	write( 'Transitions: ' ),
	write( Transitions ),
	nl,
	dfa_get_starting_state( Start ),
	write( 'Starts at: ' ),
	write( Start ), 
	nl,
	dfa_get_accepting_states( AcceptingStates ),
	write( 'Accepting states: ' ),
	write( AcceptingStates ).

% main predicate
main :-
	load_example_dfa_definition,				% load declaration
	print_dfa, 									% print automaton	
	nl,											
Word = [0,1,0,1,0,1,1,0],						% unify Word and list of given bits
	write( 'Running dfa with word: ' ), 		
	write( Word ), 								 
	nl,											 
	dfa_delta( a, Word, X ),					% call delta function starting at a with Word, expecting resulting state X
	write( 'Endstate is: ' ), 
	write( X ), 
	nl,
	( dfa_is_accepting_state( X ) -> 			
		write( 'Accepted!' );					 
		write( 'Not accepted!' )).				 
	