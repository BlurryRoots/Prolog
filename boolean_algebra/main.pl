:- use_module( boolean_algebra ).

main :-
	A = false, B = false, C = true,
	F = ( (true => B) <=> B*C + A ) ^ C,
	write_canonical( F ), nl,
	( F -> 
		write( 'true' );
		write( 'false' )).
