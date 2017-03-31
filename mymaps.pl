%  file    : mymaps
%  Authors : Peter Schachte
%  Purpose : maintain a set of key-value associations

%				   Abstract
%
%  This code implements a kind of map, also known as a finite function, a
%  dictionary.  A map is a set of *Key* -> *Value* associations.  Operations
%  supplied by this code allow creating a new map, looking up a *Value* given
%  a *Key*, associating a new *Value* with a specified *Key*, and traversing
%  all the associations in a map.

:- module(mymaps, [
	map_empty/1,
	map_store/4,
	map_fetch/3,
	map_member/3,
	map_first/2,
	map_next/5
   ]).

:- use_module(library(avl)).			% use map code from QP library


/*****************************************************************
			       Basic Operations
*****************************************************************/

%  map_empty(-Empty)
%  Checks if Empty is an empty map, or binds Empty to the empty map.

map_empty(Empty) :-
	empty_avl(Empty).


%  map_store(+Key, +Value, +Map0, -Map)
%  Map is the same as Map0, except that in Map, key Key maps to Value.

map_store(Key, Value, Map0, Map) :-
	avl_store(Key, Map0, Value, Map).


%  map_fetch(+Key, +Map, -Value)
%  Value is the value associated with key Key in map Map.

map_fetch(Key, Map, Value) :-
	avl_fetch(Key, Map, Value).


/*****************************************************************

			       Traversing a Map

We support two kinds of traversal:  backtracking and forward.  Backtracking,
implemented by map_member/3 is certainly the simplest.

Forward traversal requires the introduction of a new data structure:  the
cursor.  A map may be traversed by obtaining a cursor from a map, and then
repeatedly supplying a map and a cursor to get a key, and value, and a new
cursor.

Note that the current implementation of these traversal predicates finds the
keys in alphabetical order (actually @< order), but you should not count on
that for future versions.

*****************************************************************/

%  map_member(+Map, *Key, *Value)
%  Like map_fetch, except it is meant to backtrack over all the Key-Value
%  pairs in the map.

map_member(Map, Key, Value) :-
	avl_member(Key, Map, Value).


%  map_first(+Map, -Cursor)
%  Cursor is a cursor that may be used for traversing Map.

map_first(Map, Cursor) :-
	(   avl_min(Map, Key, Val) ->
		Cursor = Key-Val
	;   Cursor = end
	).


%  map_next(+Cursor0, +Map, -Key, -Value, -Cursor)
%  Key and Value are the association found in Map at position Cursor0, and
%  Cursor is the cursor to find the next association.

map_next(Key-Value, Map, Key, Value, Cursor) :-
	% fails if Cursor0 = end
	(   avl_next(Key, Map, Knext, Vnext) ->
		Cursor = Knext-Vnext
	;   Cursor = end
	).


/*****************************************************************
				Printing Maps

For debugging, we provide portray/1 hook code to print maps.  We have two
forms, 'short' and 'full'.  In 'short' mode (the default), we only show the
keys, while 'full' mode shows the associated values as well.

This code shamelessy swiped from the AVL library code and modified to my
purposes.  This is a violation of referential transparency, but this is only a
debugging hack, so I can live with that.

*****************************************************************/


:- multifile user:portray/1.

user:portray(empty) :-
	write('MAP{}').
user:portray(node(K,V,B,L,R)) :-
	write('MAP{'),
	portray_avl(L, 0, X0),
	portray_avl(K, V, B, X0),
	portray_avl(R, 1, _),
	put("}").


portray_avl(empty, X, X).
portray_avl(node(K,V,B,L,R), X0, X) :-
	portray_avl(L, X0, X1),
	portray_avl(K, V, B, X1),
	portray_avl(R, 1, X).


portray_avl(K, V, B, X0) :-
	( X0 =:= 0 -> true ; put(0',) ),
	(   showing_maps(full) ->
		nl,
		print(K),
		(   B < 0 -> write('*->')
		;   B > 0 -> write('->*')
		;   write('->')
		),
		print(V)
	;   print(K)
	).

showmaps(X) :-
	(   showing_maps(X) ->
		true
	;   retractall(showing_maps(_)),
	    assert(showing_maps(X))
	).

:- dynamic showing_maps/1.

showing_maps(short).
