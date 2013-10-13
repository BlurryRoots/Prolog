:-  use_module( deterministic_finite_automaton ). 

load_example_dfa_definition :-
	dfa_declare(
		[0,1],
		[a,b,c],
		[
			[ a, 0, a ],
			[ a, 1, b ],
			[ b, 0, c ],
			[ b, 1, b ],
			[ c, 0, a ],
			[ c, 1, b ]
		],
		a,
		c
	).



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

main :-
	load_example_dfa_definition,
	print_dfa, 
	nl,
	Word = [0,1,1,0,0,1,0],
	write( 'Running dfa with word: ' ), 
	write( Word ), 
	nl,
	dfa_delta( a, Word, X ),
	write( 'Endstate is: ' ), write( X ), nl,
	( dfa_is_accepting_state( X ) -> 
		write( 'Accepted!' );
		write( 'Not accepted!' )).
	