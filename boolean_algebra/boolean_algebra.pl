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

%export module
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
