%   Package: logarr
%   Authors: Mostly David H.D. Warren, some changes by Fernando C. N.  Pereira
%   Updated: 09 Jan 1990
%   Purpose: Extendable arrays with logarithmic access time.

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%   Modified 23 Jan 95 by Peter Schachte to use arity 16 terms rather than 4.

/*  LOGARITHMIC ARRAYS.

    An array extends from 0 to (2^Size)-1, where Size is divisible by 4.
    Note that 2^Size = 1<<Size.

External interface:

    new_array(A)
	returns a new empty array A.

    is_array(A)
	checks whether A is an array.

    aref(Index, Array, Element)
	unifies Element with Array[Index], or fails if Array[Index]
	has not been set.

    arefa(Index, Array, Element)
	is as aref/3, except that it unifies Element with a new array if
	Array[Index] is undefined.  This is useful for multidimensional
	arrays implemented as arrays of arrays.

    arefl(Index, Array, Element)
	is as aref/3, except that Element appears as '[]' for
	undefined cells.

    aset(Index, Array, Element, NewArray)
	unifies NewArray with the result of setting Array[Index]
	to Element.

    alist(Array, List)
	returns a list of pairs Index-Element of all the elements
	of Array that have been set.

    BEWARE: the atom '$' is used to indicate an unset element, and the
    functor '$'/16 is used to indicate a subtree.  In general, array
    elements whose principal function symbol is '$' will not work.
*/

:- module(logarr, [
	new_array/1,
	is_array/1,
	aref/3,
	arefa/3,
	arefl/3,
	aset/4
%	alist/2
   ]).
:- use_module(library(types), [
	must_be_nonneg/3
   ]).
:- mode
	aref(+, +, ?),
	arefa(+, +, ?),
	arefl(+, +, ?),
%	alist(+, -),
	aset(+, +, +, -),
	array_item(+, +, +, +, ?),
	is_array(+),
	new_array(-),
	not_undef(+),
	subarray(+, +, ?),
	subarray_to_list(+, +, +, +, ?, ?),
	update_subarray(+, +, ?, ?, -).

sccs_id('"@(#)90/01/09 logarr.pl	36.2"').



new_array(array($($,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$),4)).


is_array(array(_,_)).


% alist(array($(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,AA,AB,AC,AD,AE,AF),Size), L0) :-
% 	N is Size-4,
% 	subarray_to_list(0,  N, 0, A0, L0, L1),
% 	subarray_to_list(1,  N, 0, A1, L1, L2),
% 	subarray_to_list(2,  N, 0, A2, L2, L3),
% 	subarray_to_list(3,  N, 0, A3, L3, L4),
% 	subarray_to_list(4,  N, 0, A4, L4, L5),
% 	subarray_to_list(5,  N, 0, A5, L5, L6),
% 	subarray_to_list(6,  N, 0, A6, L6, L7),
% 	subarray_to_list(7,  N, 0, A7, L7, L8),
% 	subarray_to_list(8,  N, 0, A8, L8, L9),
% 	subarray_to_list(9,  N, 0, A9, L9, LA),
% 	subarray_to_list(10, N, 0, AA, LA, LB),
% 	subarray_to_list(11, N, 0, AB, LB, LC),
% 	subarray_to_list(12, N, 0, AC, LC, LD),
% 	subarray_to_list(13, N, 0, AD, LD, LE),
% 	subarray_to_list(14, N, 0, AE, LE, LF),
% 	subarray_to_list(15, N, 0, AF, LF, []).


subarray_to_list(K, 0, M, Item, [N-Item|L], L) :-
	not_undef(Item), !,
	N is K+M.
subarray_to_list(K, N, M, $(A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,AA,AB,AC,AD,AE,AF),
		L0, L) :-
	N > 0, !,
	N1 is N-4,
	M1 is (K+M) << 4,
	subarray_to_list(0,  N1, M1, A0, L0, L1),
	subarray_to_list(1,  N1, M1, A1, L1, L2),
	subarray_to_list(2,  N1, M1, A2, L2, L3),
	subarray_to_list(3,  N1, M1, A3, L3, L4),
	subarray_to_list(4,  N1, M1, A4, L4, L5),
	subarray_to_list(5,  N1, M1, A5, L5, L6),
	subarray_to_list(6,  N1, M1, A6, L6, L7),
	subarray_to_list(7,  N1, M1, A7, L7, L8),
	subarray_to_list(8,  N1, M1, A8, L8, L9),
	subarray_to_list(9,  N1, M1, A9, L9, LA),
	subarray_to_list(10, N1, M1, AA, LA, LB),
	subarray_to_list(11, N1, M1, AB, LB, LC),
	subarray_to_list(12, N1, M1, AC, LC, LD),
	subarray_to_list(13, N1, M1, AD, LD, LE),
	subarray_to_list(14, N1, M1, AE, LE, LF),
	subarray_to_list(15, N1, M1, AF, LF, L ).
subarray_to_list(_, _, _, _, L, L).



aerr(Index, Array, Goal) :-
	must_be_nonneg(Index, 1, Goal),
	types:should_be(array, Array, 2, Goal).



aref(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	Index < 1<<Size,
	N is Size-4,
	Subindex is Index>>N /\ 15,
	array_item(Subindex, N, Index, Array, Item).
aref(Index, Array, Item) :-
	aerr(Index, Array, aref(Index,Array,Item)).


arefa(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	(   Index < 1<<Size,
	    N is Size-4,
	    Subindex is Index>>N /\ 15,
	    array_item(Subindex, N, Index, Array, Value),
	    !,
	    Item = Value
	;   new_array(Item)
	).
arefa(Index, Array, Item) :-
	aerr(Index, Array, arefa(Index,Array,Item)).


arefl(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	(   Index < 1<<Size,
	    N is Size-4,
	    Subindex is Index>>N /\ 15,
	    array_item(Subindex, N, Index, Array, Value),
	    !,
	    Item = Value
	;   Item = []
	).
arefl(Index, Array, Item) :-
	aerr(Index, Array, arefl(Index,Array,Item)).



aset(Index, array(Array0,Size0), Item, array(Array,Size)) :-
	integer(Size0),
	integer(Index), 0 =< Index,
	!,
	enlarge_array(Index, Size0, Array0, Size, Array1),
	update_array_item(Size, Index, Array1, Item, Array).
arefl(Index, Array0, Item, Array) :-
	aerr(Index, Array0, aset(Index,Array0,Item,Array)).



% Guts

enlarge_array(I, Size0, Array0, Size, Array) :-
    (	I < 1<<Size0 ->
	Size = Size0, Array = Array0
    ;	Size1 is Size0+4,
	enlarge_array(I, Size1, $(Array0,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$), Size,
		      Array)
    ).


array_item(Subindex, N, Index, Array, Value) :-
	Sub1 is Subindex+1,
	arg(Sub1, Array, Elt),
	(   N =:= 0 ->
		Elt \== $,
		Value = Elt
	;   N1 is N-4,
	    Subindex1 is Index >> N1 /\ 15,
	    array_item(Subindex1, N1, Index, Elt, Value)
	).


% array_item(0, 0,_Index, $(Item,_,_,_), Item) :- !,
% 	not_undef(Item).
% array_item(0, N, Index, $(Array,_,_,_), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).
% array_item(1, 0,_Index, $(_,Item,_,_), Item) :- !,
% 	not_undef(Item).
% array_item(1, N, Index, $(_,Array,_,_), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).
% array_item(2, 0,_Index, $(_,_,Item,_), Item) :- !,
% 	not_undef(Item).
% array_item(2, N, Index, $(_,_,Array,_), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).
% array_item(3, 0,_Index, $(_,_,_,Item), Item) :- !,
% 	not_undef(Item).
% array_item(3, N, Index, $(_,_,_,Array), Item) :-
% 	N1 is N-4,
% 	Subindex is Index >> N1 /\ 15,
% 	array_item(Subindex, N1, Index, Array, Item).


not_undef($) :- !,
	fail.
not_undef(_).


update_array_item(0,_Index,_Item, NewItem, NewItem) :- !.
update_array_item(N, Index, Array, NewItem, NewArray) :-
	N1 is N-4,
	Subindex is Index >> N1 /\ 15,
	(   Array == $ ->
		update_subarray(Subindex, $($,$,$,$,$,$,$,$,$,$,$,$,$,$,$,$),
				Array1, NewArray1, NewArray)
	;   update_subarray(Subindex, Array, Array1, NewArray1, NewArray)
	),
	update_array_item(N1, Index, Array1, NewItem, NewArray1).


update_subarray(0,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), A, A1,
		$(A1,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(1,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), B, B1,
		$(A,B1,C,D,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(2,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), C, C1,
		$(A,B,C1,D,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(3,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), D, D1,
		$(A,B,C,D1,E,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(4,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), E, E1,
		$(A,B,C,D,E1,F,G,H,I,J,K,L,M,N,O,P)).
update_subarray(5,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), F, F1,
		$(A,B,C,D,E,F1,G,H,I,J,K,L,M,N,O,P)).
update_subarray(6,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), G, G1,
		$(A,B,C,D,E,F,G1,H,I,J,K,L,M,N,O,P)).
update_subarray(7,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), H, H1,
		$(A,B,C,D,E,F,G,H1,I,J,K,L,M,N,O,P)).
update_subarray(8,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), I, I1,
		$(A,B,C,D,E,F,G,H,I1,J,K,L,M,N,O,P)).
update_subarray(9,  $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), J, J1,
		$(A,B,C,D,E,F,G,H,I,J1,K,L,M,N,O,P)).
update_subarray(10, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), K, K1,
		$(A,B,C,D,E,F,G,H,I,J,K1,L,M,N,O,P)).
update_subarray(11, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), L, L1,
		$(A,B,C,D,E,F,G,H,I,J,K,L1,M,N,O,P)).
update_subarray(12, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), M, M1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M1,N,O,P)).
update_subarray(13, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), N, N1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M,N1,O,P)).
update_subarray(14, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), O, O1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O1,P)).
update_subarray(15, $(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P), P, P1,
		$(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P1)).
