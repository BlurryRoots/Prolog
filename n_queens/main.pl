/*
    Author:         Sven Freiberg
    EMail:          info@blurryroots.com
    Web:            http://www.blurryroots.com
    Copyright (C):  2013 Sven Freiberg

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

% ###
% Acknowledgements to John R. Fisher for his
% tutorial, which can be found under
% http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_11.html
% ###

% ---
% The predicate create_diagonal_list/3 creates
% a list where the value is equal its index.
% This is useful in the context of the n queens
% problem, simply because its impossible for a
% solution to be valid if two or more queens are
% in the same row. So we can start of with a list
% where all queens are in a separate row.
% ---
% If the index is zero, simply bypass the input,
% and cut the solution finding.
create_diagonal_list( 0, ListIn, ListIn ) :- !.
% Else count down from N to 0, and create the list,
% by prepending the new index to the given list.
create_diagonal_list( N, ListIn, ListOut ) :-
    NewN is N - 1,
    create_diagonal_list( NewN, [NewN|ListIn], ListOut ).

% ---
% The predicate create_diagonal_list/2 serves as
% shortcut for create_diagonal_list/3.
% ---
% If size is zero, return empty list.
create_diagonal_list( 0, [] ).
% Else create new list based on an empty list.
create_diagonal_list( N, List ) :-
    create_diagonal_list( N, [], List ).

% ---
% The predicate remove is defined,
% to later use it to let prolog 
% find all szenarios from where 
% a given item might haven
% been removed.
% ---
% If item is the head, simply return
% the tail of the list.
remove( Item, [Item|Tail], Tail ). 
% Else check the tail of the list, and
% create the new list from the previous
% head and the newly created tail.
remove( Item, [Head|Tail], [Head|NewTail] ) :-
    remove( Item, Tail, NewTail ).

% ---
% The predicate shuffle ought to 
% rearrange a given list.
% ---
% If we are given an empty list,
% return an empty list.
shuffle( [], [] ).
% Else split the head of the list,
% shuffle its tail, and let prolog
% figure out a way the shuffled tail
% and its tail could be reunited.
shuffle( [Head|Tail], Result ) :-
    shuffle( Tail, Intermediate ),
    remove( Head, Result, Intermediate ).

% ---
% The predicate no_diagonals/4 is used to figure out
% if the given list has no diagonally positioned queens.
% ---
% If theres no items, cut it.
no_diagonals( _, _, _, [] ) :- !.
% Else if there is only on item in the list, check
% its value against the given up and down values.
no_diagonals( UpValue, DownValue, Index, [Item] ) :-
    CurrentUpValue is Index - Item,
    UpValue =\= CurrentUpValue, 
    CurrentDownValue is Index + Item,
    DownValue =\= CurrentDownValue, 
    !.
% Else if there is a list, check its head against the
% given up and down values, and check the rest of the list.
no_diagonals( UpValue, DownValue, Index, [Head|Tail] ) :-
    CurrentUpValue is Index - Head,
    UpValue =\= CurrentUpValue, 
    CurrentDownValue is Index + Head,
    DownValue =\= CurrentDownValue, 
    NewIndex is Index + 1,
    no_diagonals( UpValue, DownValue, NewIndex, Tail ).  

% ---
% The predicate no_diagonals/1 is a shortcut for
% no_diagonals/4, allowing the user to simply pass
% a list to be checked.  
% ---
% If there are no items, there are no diagonals.
no_diagonals( [] ).
% Else create up and down values depending on the first
% item in the list, and check the rest of the list.
no_diagonals( [Head|Tail] ) :-
    Index = 0,
    UpValue is Index - Head,
    DownValue is Index + Head,
    NewIndex is Index + 1,
    % First with the values of the first item.
    no_diagonals( UpValue, DownValue, NewIndex, Tail ),
    % And then with any subsequent item in the list.
    no_diagonals( Tail ).

% ---
% The predicate n_queens/2 searches for a solution,
% to the n queens problem, by taking N amounts of
% queens and returning a list representing the solution.
% If there is no solution, the predicate will fail.
% ---
n_queens( N, Solution ) :-
    create_diagonal_list( N, List ),
    shuffle( List, Solution ),
    no_diagonals( Solution ).
% ---
% The predicate n_queens_list/2 will search for all
% solutions to the given amount of queens, and return
% them in a list. This algorithm is pretty unefficient,
% so trying anything above n = 10, will quickly grow into
% hours and days of processing time.
% ---
n_queens_list( N, SolutionList ) :-
    findall(
        Solution,
        n_queens( N, Solution ),
        SolutionList
    ).

main :-
    writeln( 'Seachring solution for 11 queens.' ),
    writeln( 'The list returned represents the board,' ),
    writeln( 'where the index stands for the column and' ),
    writeln( 'the value represents the row, where the queen is located.' ),
    n_queens_list( 8, S ),
    writeln( S ).
