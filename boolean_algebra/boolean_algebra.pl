:- module( 
	boolean_algebra, 
	[
		(~)/1,
		(*)/2,
		(+)/2,
		(^)/2,
		(=>)/2,
		(<=>)/2
	]).

%not
~( X ) :-
	not( X ).
:- op( 300, fx, user:(~) ).

%and
*( X, Y ) :-
	X, Y.
:- op( 400, xfy, user:(*) ).

%or
+( X, Y ) :-
	X; Y.
:- op( 500, xfy, user:(+) ).

%exclusive or
^( X, Y ) :-
	X * ~Y + ~X * Y.
:- op( 500, xfy, user:(^) ).

%implication
=>( X, Y ) :-
	~X; Y.
:- op( 600, xfx, user:(=>) ).

%equality
<=>( X, Y ) :-
	(X => Y) * (Y => X).
:- op( 700, xfx, user:(<=>) ).
