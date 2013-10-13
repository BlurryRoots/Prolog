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

%import module to use operators defined in boolean_algebra.pl
:- use_module( boolean_algebra ).

%main predicate
main :-
	A = false, B = false, C = true,			% setup variables
	F = ( (true => B) <=> B*C + A ) ^ C,	% assemble logical expression
	write_canonical( F ), nl,				% print F in canonical operator form
	( F -> 									% evaluate expression
		write( 'true' );
		write( 'false' )).
