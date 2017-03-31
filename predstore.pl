%  File   : predstore
%  Authors: Peter Schachte
%  Purpose: storage and retrieval of predicate and analysis information
%
%				Abstract
%
%  This module implements an abstract datatype for storing the code of
%  and analysis information for the predicates to be analyzed.

:- module(predstore, [
	empty_predstore/1,
	predstore_first/2,
	predstore_next/4,
	predstore_size/2,
	get_pred_ref/4,
	get_pred_ref/5,
	get_old_pred_ref/3,
	put_pred_alias/4,
	put_pred_code/4,
	get_pred_code/3,
	put_pred_status/4,
	get_pred_status/3,
	property_mask/2,
	add_pred_property/4,
	pred_has_property/3,
	put_pred_travnum/4,
	get_pred_travnum/3,
	put_pred_success/4,
	get_pred_success/3,
	put_pred_call/4,
	get_pred_call/3,
	put_pred_prep/4,
	get_pred_prep/3,
	put_pred_spec/4,
	get_pred_spec/3,
	print_goal/2
   ]).


%  :- use_module(library(logarr)).		% use array code from library
:- use_module(analysis).			% for analysis domain stuff
:- use_module(logarr).				% use my array code
:- use_module(mymaps).				% and my maps code

/****************************************************************
		       Predicate Stores and References

This implementation of a predicate store contains three parts:
++enumerate
    1.  a mapping from predicate spec (Module:Name/Arity term) to predicate
	reference, which is just a number;
    2.  an array of data, one record per predicate; and
    3.  the next predicate reference number to be assigned.
--enumerate
We could just use the predicate spec as a predicate reference, but this
implementation should be more efficient.  It also admits reimplementation
using real arrays and real destructive modification, which should be quite a
bit more efficient.
****************************************************************/

%  empty_predstore(-Empty)
%  Empty is an empty predicate store.

empty_predstore(store(0,Map,Array)) :-
	map_empty(Map),
	new_array(Array).


%  predstore_first(+Store, -Cursor)
%  Cursor is a term that can be used to enumerate all the predicate references
%  in a predicate store.

predstore_first(store(N,_,_), N).


%  predstore_next(+Store, +Cursor0, -Predref, -Cursor)
%  Predref is the predicate reference for a predicate in Store at position
%  Cursor, and Cursor is the position of the following predicate reference.

predstore_next(_, Cursor0, Cursor0, Cursor) :-
	Cursor0 > 0,
	Cursor is Cursor0 - 1.


%  predstore_size(+Store, -Size)
%  Predstore Store contains Size predicates.

predstore_size(store(Size,_,_), Size).


%  get_pred_ref(+Spec, +Store0, -Ref, -Store)
%  get_pred_ref(+Spec, +Defmodule, +Store0, -Ref, -Store)
%  Ref is the predicate ref associated with predicate spec Spec, with default
%  module Defmodule, in Store.  Store is the same as Store0, except that it
%  may have been extended by adding predicate Spec.  If Defmodule is omitted,
%  Spec must be a complete spec of the form Module:Name/Arity.

get_pred_ref(Mod:Spec, _, Store0, Ref, Store) :-
	get_pred_ref(Spec, Mod, Store0, Ref, Store).
get_pred_ref(Name/Arity, Mod, Store0, Ref, Store) :-
	get_pred_ref(Mod:Name/Arity, Store0, Ref, Store).


get_pred_ref(Spec, Store0, N, Store) :-
	(   get_old_pred_ref(Spec, Store0, N) ->
		Store = Store0
	;   Store0 = store(N0,Map0,Array0),
	    Store = store(N,Map,Array),
	    N is N0 + 1,
	    map_store(Spec, N, Map0, Map),
	    anz_bottom(F1),
	    anz_bottom(F2),
	    aset(N, Array0,
		 pred(undefined,0,unprocessed,F1,F2,_,Spec), Array)
	).


%  get_old_pred_ref(+Spec, +Store0, -Ref)
%  get_old_pred_ref(-Spec, +Store0, +Ref)
%  get_old_pred_ref(*Spec, +Store0, *Ref)
%  Ref is the predicate ref associated with predicate spec Spec in Store0.
%  This differs from get_pred_ref/4 in that no new pred number will be
%  allocated.

get_old_pred_ref(Spec, Store, Ref) :-
	Store = store(_,Map,_),
	(   ground(Spec) ->
		map_fetch(Spec, Map, Ref)
	;   integer(Ref) ->
		get_pred_spec(Ref, Store, Spec)
	;   map_member(Map, Spec, Ref)		% backtrack over whole store
	).


%  put_pred_alias(+Oldpred, +Alias, +Store0, -Store)
%  Predicate store Store is the same as Store0, except that it defines Alias,
%  a predicate spec, as an alias for predicate Oldpred.

put_pred_alias(Oldpred, Alias, Store0, Store) :-
	get_old_pred_ref(Oldpred, Store0, Ref),
	\+ get_old_pred_ref(Alias, Store0, _),
	Store0 = store(N,Map0,Array),
	Store = store(N,Map,Array),
	map_store(Alias, Ref, Map0, Map).


/*****************************************************************
		       Accessing Predicate Information

The data we store for each predicate are:
++description
    code	The predicate's code.  The code is stored in a ground
		representation, as described below.
    status	The predicate's status bits.  This contains information about
		whether the predicate is static or dynamic, whether it is
		defined over multiple files, etc.
    travnum	Temporary dfs traversal number used when computing SCCs.
		Either 'unprocessed' or a positive integer.
    success	The predicate's success patterns.
    call	The predicate's call patterns.
    prep	The predicate's analysis preparation.  See 'prep.pl' for a
		description.
    spec	The predicate's Module:Name/Arity specification.
--description
*****************************************************************/


%  put_pred_code(+Ref, +Code, +Store0, -Store)
%  Store is the same as Store0, except that the code of the predicate referred
%  to by Ref is Code.

put_pred_code(Ref, Code, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(_,B,C,D,E,F,G)),
	aset(Ref, Array0, pred(Code,B,C,D,E,F,G), Array).


%  get_pred_code(+Ref, +Store, -Code)
%  Code is the code of the predicate referred to by Ref in Store.

get_pred_code(Ref, store(_,_,Array), Code) :-
	aref(Ref, Array, pred(Code,_,_,_,_,_,_)).


%  put_pred_status(+Ref, +Status, +Store0, -Store)
%  Store is the same as Store0, except that the status of the predicate
%  referred to by Ref is Status.

put_pred_status(Ref, Status, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,_,C,D,E,F,G)),
	aset(Ref, Array0, pred(A,Status,C,D,E,F,G), Array).


%  get_pred_status(+Ref, +Store, -Status)
%  Status is the status of the predicate referred to by Ref in Store.

get_pred_status(Ref, store(_,_,Array), Status) :-
	aref(Ref, Array, pred(_,Status,_,_,_,_,_)).


%  property_mask(+Property, -Mask)
%  property_mask(-Property, +Mask)
%  property_mask(*Property, *Mask)
%  Mask is the bitmask associated with predicate property Property.

property_mask(dynamic, 1).
property_mask(foreign, 2).
property_mask(multifile, 4).
property_mask(discontiguous, 8).


%  add_pred_property(+Ref, +Property, +Store0, -Store)
%  The predicate referred to by Ref in Store has property Property, but
%  otherwise Store is the same as Store0.

add_pred_property(Ref, Property, Store0, Store) :-
	get_pred_status(Ref, Store0, Status0),
	property_mask(Property, Mask),
	Status is Status0 \/ Mask,
	put_pred_status(Ref, Status, Store0, Store).


%  pred_has_property(+Ref, +Property, +Store)
%  The predicate referred to by Ref in Store has property Property.

pred_has_property(Ref, Property, Store) :-
	get_pred_status(Ref, Store, Status),
	property_mask(Property, Mask),
	Mask is Status /\ Mask.


%  put_pred_travnum(+Ref, +Travnum, +Store0, -Store)
%  Store is the same as Store0, except that the lowest-numbered predicate
%  reachable from the predicate referred to by Ref is Travnum.

put_pred_travnum(Ref, Travnum, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,_,D,E,F,G)),
	aset(Ref, Array0, pred(A,B,Travnum,D,E,F,G), Array).


%  get_pred_travnum(+Ref, +Store, -Travnum)
%  Travnum is the lowest-numbered predicate reachable from the predicate
%  referred to by Ref in Store.

get_pred_travnum(Ref, store(_,_,Array), Travnum) :-
	aref(Ref, Array, pred(_,_,Travnum,_,_,_,_)).


%  put_pred_success(+Ref, +Success, +Store0, -Store)
%  Store is the same as Store0, except that the success of the
%  predicate referred to by Ref is Success.

put_pred_success(Ref, Success, store(N,Map,Array0),
		store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,Old,E,F,G)),
	anz_free_if_unshared(Old, Success),
	aset(Ref, Array0, pred(A,B,C,Success,E,F,G), Array).


%  get_pred_success(+Ref, +Store, -Success)
%  Success is the success of the predicate referred to
%  by Ref in Store.

get_pred_success(Ref, store(_,_,Array), Success) :-
	aref(Ref, Array, pred(_,_,_,Success,_,_,_)).


%  put_pred_call(+Ref, +Call, +Store0, -Store)
%  Store is the same as Store0, except that the call of the
%  predicate referred to by Ref is Call.

put_pred_call(Ref, Call, store(N,Map,Array0),
		store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,D,Old,F,G)),
	anz_free_if_unshared(Old, Call),
	aset(Ref, Array0, pred(A,B,C,D,Call,F,G), Array).


%  get_pred_call(+Ref, +Store, -Call)
%  Call is the call of the predicate referred to by Ref
%  in Store.

get_pred_call(Ref, store(_,_,Array), Call) :-
	aref(Ref, Array, pred(_,_,_,_,Call,_,_)).


%  put_pred_prep(+Ref, +Prep, +Store0, -Store)
%  Store is the same as Store0, except that the prep of the
%  predicate referred to by Ref is Prep.

put_pred_prep(Ref, Prep, store(N,Map,Array0),
		store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,D,E,_,G)),
	aset(Ref, Array0, pred(A,B,C,D,E,Prep,G), Array).


%  get_pred_prep(+Ref, +Store, -Prep)
%  Prep is the prep of the predicate referred to by Ref
%  in Store.

get_pred_prep(Ref, store(_,_,Array), Prep) :-
	aref(Ref, Array, pred(_,_,_,_,_,Prep,_)).


%  put_pred_code(+Ref, +Code, +Store0, -Store)
%  Store is the same as Store0, except that the code of the predicate referred
%  to by Ref is Code.

put_pred_spec(Ref, Spec, store(N,Map,Array0), store(N,Map,Array)) :-
	aref(Ref, Array0, pred(A,B,C,D,E,F,_)),
	aset(Ref, Array0, pred(A,B,C,D,E,F,Spec), Array).


%  get_pred_spec(+Ref, +Store, -Spec)
%  Spec is the spec of the predicate referred to by Ref in Store.

get_pred_spec(Ref, store(_,_,Array), Spec) :-
	aref(Ref, Array, pred(_,_,_,_,_,_,Spec)).


/*****************************************************************
				Printing Code

This code pretty-prints the ground representation we use for predicate code.

A goal specification is one of:
++description
    'conj'(*Gs*)
	specifying a conjunction of goals, where *Gs* is a list of goal specs.
    'equal'(*V,T,Vs,A,R*)
	specifying an explicit or implicit unification, where *V* is a
	variable number, *T* is a term description, *Vs* is a list of the
	variable numbers appearing in *T*, *A* is an analysis term containing
	analysis information for the point of the call (before the call), and
	*R* is the smallest variable number that does not appear later in the
	clause;
    'eval'(*V,E,Vs,A,R*)
	 specifying an explicit or implicit numeric calculation, where *V* is
	a variable number, *E* is a term description of the expression to
	evaluate, *Vs* is a list of the variable numbers appearing in *E*, and
	*A* and *R* are as for 'equal'/5;
    'builtin'(*T,A,R*)
	specifying a call to a Prolog builtin, where *T* is a term
	representing the goal and *T*'s arguments are all distinct variable
	numbers, and *A* and *R* are as for 'equal'/5;
    'call'(*P,T,A,R*)
	specifying a call to a user predicate, where *P* is the predicate
	number and *T*, *A*, and *R* are as for 'builtin'/3;
    '!'	(ordinary cut, cuts to outermost disjunction);
    'disj'(*Gs*)
	specifying a disjunction of goals, where *Gs* is a list of goal specs.
    'if_then_else'(*G1,G2,G3*)
	representing a Prolog if->then;else construct, where *G1*, *G2*, and
	*G3* are goal specs.
--description
We store analysis information for unifications, evaluations, and
builtin calls because it may prove useful in compiling these
operations, and it takes no time, only a little space.

A term description is one of:
++enumerate
    1.  'var'(*N*), where *N* is a variable number;
    2.  'atom'(*C*), where *C* is an atom;
    3.  'integer'(*C*), where *C* is an integer;
    4.  'float'(*C*), where *C* is a float;
    5.  'compound'(*F, A, Args*), where *F* is ther term's principal functor,
	*A* is its arity, and *Args* is a list of term descriptions.
--enumerate

*****************************************************************/


%  print_code(+Code, +Indent)
%  Print out Code at indentation level Indent.

print_code(Var, Indent) :-
	var(Var),
	!,
	indent(Indent),
	write('UNBOUND!'),
	nl.
print_code(undefined, Indent) :-
	!,
	indent(Indent),
	write('UNDEFINED!'),
	nl.
print_code(Code, Indent) :-
	indent(Indent),
	print_goal(Code, Indent),
	write('.'),
	nl.


print_body(Var, _) :-
	var(Var),
	!,
	write(Var).
print_body([], _) :-
	write('true').
print_body([G1|Gs], Indent) :-
	print_goal(G1, Indent),
	print_body_tail(Gs, Indent).


print_body_tail(Var, Indent) :-
	var(Var),
	!,
	write(' |'),
	nl,
	indent(Indent),
	write(Var).
print_body_tail([], _).
print_body_tail([G1|Gs], Indent) :-
	write(','),
	nl,
	indent(Indent),
	print_goal(G1, Indent),
	print_body_tail(Gs, Indent).
	


print_goal(Var, _) :-
	var(Var),
	!,
	write(Var).
print_goal(conj(Specs), Indent) :-
	print_body(Specs, Indent).
print_goal(equal(V,T,_,_,R), _) :-
	print_var(V),
	write(' = '),
	print_term(T),
	format(' /* ~w */', [R]).
print_goal(eval(V,E,_,_,R), _) :-
	print_var(V),
	write(' is '),
	print_term(E),
	format('/* ~w */', [R]).
print_goal(builtin(T,_,R), _) :-
	print_simple_goal(T),
	format('/* ~w */', [R]).
print_goal(call(_,T,_,R), _) :-
	print_simple_goal(T),
	format('/* ~w */', [R]).
print_goal(!, _) :-
	write(!).
print_goal(disj([G1|Gs]), Indent) :-
	write('(   '),
	I1 is Indent + 4,
	print_goal(G1, I1),
	print_disjunction(Gs, Indent, I1),
	nl,
	indent(Indent),
	write(')').
print_goal(if_then_else(G1,G2,G3), Indent) :-
	write('(   '),
	I1 is Indent + 4,
	I2 is Indent + 8,
	print_if_then_else(G1, G2, G3, Indent, I1, I2),
	nl,
	indent(Indent),
	write(')').


print_simple_goal(T) :-
	functor(T, Name, Arity),
	write(Name),
	(   Arity > 0 ->
		write('('),
		arg(1, T, V1),
		print_var(V1),
		print_simple_goal_args(2, Arity, T),
		write(')')
	;   true
	).


print_simple_goal_args(N, Arity, T) :-
	(   N =< Arity ->
		arg(N, T, Vn),
		write(', '),
		print_var(Vn),
		N1 is N + 1,
		print_simple_goal_args(N1, Arity, T)
	;   true
	).


print_if_then_else(G1, G2, G3, Indent, I1, I2) :-
	print_goal(G1, I1),
	write(' ->'),
	nl,
	indent(I2),
	print_goal(G2, I2),
	nl,
	indent(Indent),
	write(';   '),
	print_else(G3, Indent, I1, I2).


print_else(Var, _, _, _) :-
	var(Var),
	!,
	write(Var).
print_else([if_then_else(G1,G2,G3)], Indent, I1, I2) :-
	!,
	print_if_then_else(G1, G2, G3, Indent, I1, I2).
print_else(G3, _, I1, _) :-
	print_goal(G3, I1).



print_disjunction(Var, Indent, _) :-
	var(Var),
	!,
	nl,
	indent(Indent),
	write('|   '),
	write(Var).
print_disjunction([], _, _).
print_disjunction([D|Ds], Indent, I1) :-
	nl,
	indent(Indent),
	write(';   '),
	print_goal(D, I1),
	print_disjunction(Ds, Indent, I1).


print_var(N) :-
	write('V'),
	write(N).


print_term(Var) :-
	var(Var),
	!,
	write(Var).
print_term(var(N)) :-
	print_var(N).
print_term(atom(C)) :-
	write(C).
print_term(integer(I)) :-
	write(I).
print_term(float(F)) :-
	write(F).
print_term(compound(Name,0,[])) :-
	!,
	write(Name).
print_term(compound(.,2,[H,T])) :-
	!,
	write('['),
	print_var(H),
	write('|'),
	print_var(T),
	write(']').
print_term(compound(Name,_,[Arg|Args])) :-
	write(Name),
	write('('),
	print_var(Arg),
	print_term_args(Args),
	write(')').

print_term_args([]).
print_term_args([Arg|Args]) :-
	write(','),
	print_var(Arg),
	print_term_args(Args).


indent(N) :-
	(   N =:= 0 ->
		true
	;   N >= 8 ->
		put(0'	),
		N1 is N - 8,
		indent(N1)
	;   put(0' ),
	    N1 is N - 1,
	    indent(N1)
	).


/*****************************************************************
			  Printing Predicate Stores

For debugging, we provide portray/1 hook code to print stores.

*****************************************************************/

print_store(X, Map, Cursor0, Store) :-
	(   map_next(Cursor0, Map, Pred, Predref, Cursor1) ->
		print_store_separator(X),
		print_store_pred(X, Pred, Predref, Store),
		print_store(X, Map, Cursor1, Store)
	;   true
	).


print_store_pred(none, _, _, _) :-
	!.
print_store_pred(X, Pred, Predref, Store) :-
	format('~w=~q', [Predref,Pred]),
	print_store_pred_suffix(X),
	print_store_pred1(X, Predref, Store).


print_store_pred_suffix(none).
print_store_pred_suffix(short).
print_store_pred_suffix(full) :-
	print_store_pred_suffix(code).
print_store_pred_suffix(groundness) :-
	format(':  ', []).
print_store_pred_suffix(code) :-
	format(' :-~n', []).


print_store_prefix(none) :-
	write('...').
print_store_prefix(short).
print_store_prefix(full) :-
	print_store_prefix(code).
print_store_prefix(code) :-
	nl.
print_store_prefix(groundness) :-
	nl.


print_store_separator(none).
print_store_separator(short) :-
	write(', ').
print_store_separator(full) :-
	nl.
print_store_separator(groundness) :-
	nl.
print_store_separator(calls) :-
	nl.

%  unneeded:  print_store_pred1(none, _, _).
print_store_pred1(short, _, _).
print_store_pred1(full, Predref, Store) :-
	print_store_pred1(code, Predref, Store),
	format('    groundness --~n', []),
	print_store_pred1(groundness, Predref, Store).
print_store_pred1(code, Predref, Store) :-
	get_pred_code(Predref, Store, Code),
	print_code(Code, 8).
print_store_pred1(groundness, Predref, Store) :-
	get_pred_call(Predref, Store, Call),
	get_pred_success(Predref, Store, Success),
	write('	'),
	print_boolfn(Call),
	format(' => ', []),
	print_boolfn(Success),
	nl.


print_boolfn(X) :-
	print(X).


:- dynamic showing_stores/1.

showing_stores(short).

user:show_stores(X) :-
	(   showing_stores(X) ->
		true
	;   \+ valid_store_show_state(X) ->
		format('invalid state for showstores/1:  ~q~n', [X]),
		fail
	;   retractall(showing_stores(_)),
	    assert(showing_stores(X))
	).

valid_store_show_state(none).
valid_store_show_state(short).
valid_store_show_state(full).
valid_store_show_state(code).
valid_store_show_state(groundness).

:- multifile user:portray/1.

user:portray(store(N,Map,Array)) :-
	integer(N),
	nonvar(Map),
	nonvar(Array),
	!,
	write('STORE{'),
	map_first(Map, Cursor0),
	showing_stores(X),
	(   map_next(Cursor0, Map, Pred, Predref, Cursor1) ->
		Store = store(N,Map,Array),
		print_store_prefix(X),
		print_store_pred(X, Pred, Predref, Store),
		print_store(X, Map, Cursor1, Store)
	;   true
	),
	write('}').
user:portray(conj(L)) :-
	print_goal(conj(L), 8).
user:portray(disj(L)) :-
	print_goal(disj(L), 8).
user:portray(if_then_else(G1,G2,G3)) :-
	print_goal(if_then_else(G1,G2,G3), 8).
user:portray(equal(A,B,C,D,E)) :-
	print_goal(equal(A,B,C,D,E), 8).
user:portray(eval(A,B,C,D,E)) :-
	print_goal(eval(A,B,C,D,E), 8).
user:portray(builtin(A,B,C)) :-
	print_goal(builtin(A,B,C), 8).
user:portray(call(A,B,C,D)) :-
	print_goal(call(A,B,C,D), 8).



