%  File   : scc
%  Authors: Peter Schachte
%  Purpose: Find Strongly Connected Components in a program call graph
%  Copyright 1998 Peter Schachte
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%				Abstract
%
%  Find strongly-connected components in a program call graph using Tarjan's
%  algorithm, as described in Sedgewick's *Algorithms* book.  This algorithm
%  also topologically sorts the strongly-connected components.


:- module(scc, [
	sccs/3,
	sccs/4
   ]).

:- ensure_loaded(predstore).

/*************************************************************************

		   Condensing Strongly-Connected Components

The choice of the word *condense* to describe this may be a bit confusing,
since the word "condensing" is used to describe Abstract Interpretation
domains.  In traditional program analysis literature, however, "condensing" is
used to refer to creating an acycylic graph from a cyclic one by combining
cyclic subgraphs into single nodes, which is exactly what we're doing here.

*************************************************************************/

/*================================================================

				  Algorithm

We use a declarative adaption of Tarjan's SCC algorithm.  For this, we must
give each predicate a traversal number.  We can't use the predicate
numbers we've already assigned because it's important that a predicate
that's already been visited in the traversal must have a lower number.
The algorithm basically works like this:

to process a pred P:
    If we've already processed P, do nothing more.  P's lowlink is
	just P's traversal number.
    Otherwise, give P the next traversal number, push P on the stack, and
	process all of P's immediate successors, collecting the minimum of
	their lowlinks and P's traversal number, which will be P's lowlink.
	If P's lowlink = P's traversal number, pop nodes off the stack down to
	and including P, marking them as completed by setting their traversal
	numbers to an impossibly large number.  This is a new strong
	component.

================================================================*/

%  sccs(+Preds0, -Preds, -SCCs)
%  sccs(+Preds0, -Preds, +Goal, -SCCs)
%  sccs(+Preds, +Cursor, +Id0, -Id, +Stack0, -Stack, +SCCs0, -SCCs)
%  SCCs is a list of the strongly-connected components on Preds, a predicate
%  store.  Each strong component is represented as a list of predicate
%  references.  If Goal is supplied, then SCCs includes only sccs reachable
%  from Goal, which is a (possibly compound) goal in the form of a clause
%  body.
%
%  Cursor is a cursor for enumerating the predicates in Preds.  Id0 is the
%  traversal number to give to the next predicate we need one for, and Id is
%  the next one available after processing Preds.  Stack0 is a list of
%  predicates being processed, simulating Tarjan's stack.  Stack is the stack
%  after processing Preds.

sccs(Preds0, Preds, Goal, SCCs) :-
	handle_calls(Goal, Preds0, Preds, 0, _, 0, _, [], [], [], SCCs).

sccs(Preds0, Preds, SCCs) :-
	predstore_first(Preds0, Cursor),
	sccs(Preds0, Preds, Cursor, 0, _, [], [], [], SCCs).

sccs(Preds0, Preds, Cursor, Id0, Id, Stack0, Stack, SCCs0, SCCs) :-
	(   predstore_next(Preds0, Cursor, Pred, Cursor1) ->
		handle_pred(Pred, Preds0, Preds1, Id0, Id1, Stack0, Stack1,
			    SCCs1, SCCs, _),
		sccs(Preds1, Preds, Cursor1, Id1, Id, Stack1, Stack, SCCs0,
		     SCCs1)
	;   Id = Id0,
	    Stack = Stack0,
	    SCCs = SCCs0,
	    Preds = Preds0
	).


%  handle_pred(+Pred, +Preds0, -Preds, +Id0, -Id, +Stack0, -Stack, +SCCs0, 
%		-SCCs, -Lowlink)
%  Recursively handle the predicates called by a single predicate, and if this
%  predicate's strongly-connected component isn't already in SCCs, put it
%  there.  Most arguments are as for sccs/9.  Pred is this predicate's
%  reference.  Lowlink is the lowest id number of all the predicates reached
%  (recursively called) from the predicate defined by Predterm.

handle_pred(Pred, Preds0, Preds, Id0, Id, Stack0, Stack, SCCs0, SCCs, Low) :-
	get_pred_code(Pred, Preds0, Code),
	get_pred_travnum(Pred, Preds0, Travnum),
	(   Travnum == unprocessed ->
		put_pred_travnum(Pred, Id0, Preds0, Preds1),
		Id1 is Id0 + 1,
		handle_calls(Code, Preds1, Preds2, Id1, Id, Id0, Low,
			     [Pred|Stack0], Stack1, SCCs1, SCCs),
		(   Id0 == Low ->
			SCCs1 = [SCC|SCCs0],
			pop_stack(Stack1, Stack, Pred, Preds2, Preds, SCC)
		;   SCCs1 = SCCs0,
		    Stack = Stack1,
		    Preds = Preds2
		)
	;   Preds = Preds0,
	    Id = Id0,
	    Stack = Stack0,
	    SCCs = SCCs0,
	    Low = Travnum
	).



%  handle_calls(+Code, +Preds0, -Preds, +Id0, -Id, +Low0, -Low, +Stack0,
%		-Stack, +SCCs0, -SCCs)
%  Code is code of a predicate.  Low is the lowest traversal number (the
%  lowlink) of any predicate called from Bodies, or Low0, whichever is
%  smaller.  Other args are as above.

handle_calls(undefined, Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs,
		SCCs).				% report undefineds elsewhere
handle_calls(foreign(_), Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs,
		SCCs).				% Foreigns call nothing
handle_calls(conj(Gs), Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		SCCs0, SCCs) :-
	handle_calls(Gs, Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		     SCCs0, SCCs).
handle_calls([], Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs, SCCs).
handle_calls([G|Gs], Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack, SCCs0,
		SCCs) :-
	handle_calls(G, Preds0, Preds1, Id0, Id1, Low0, Low1, Stack0, Stack1,
		     SCCs1, SCCs),
	handle_calls(Gs, Preds1, Preds, Id1, Id, Low1, Low, Stack1, Stack,
		     SCCs0, SCCs1).
handle_calls(equal(_,_,_,_,_), Preds, Preds, Id, Id, Low, Low, Stack, Stack,
		SCCs, SCCs).
handle_calls(eval(_,_,_,_,_), Preds, Preds, Id, Id, Low, Low, Stack, Stack,
		SCCs, SCCs).
handle_calls(builtin(_,_,_), Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs,
		SCCs). 
handle_calls(call(Pred,_,_,_), Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		SCCs0, SCCs) :-
	handle_pred(Pred, Preds0, Preds, Id0, Id, Stack0, Stack, SCCs0, SCCs,
		    Low1),
	(   Low0 < Low1 ->			% Low is the minimum of Low0
		Low = Low0			% and Low1
	;   Low = Low1
	).
handle_calls(!, Preds, Preds, Id, Id, Low, Low, Stack, Stack, SCCs, SCCs).
handle_calls(disj(Gs), Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		SCCs0, SCCs) :-
	handle_calls(Gs, Preds0, Preds, Id0, Id, Low0, Low, Stack0, Stack,
		     SCCs0, SCCs).
handle_calls(if_then_else(G1,G2,G3), Preds0, Preds, Id0, Id, Low0, Low,
		Stack0, Stack, SCCs0, SCCs) :-
	handle_calls(G1, Preds0, Preds1, Id0, Id1, Low0, Low1, Stack0, Stack1,
		     SCCs1, SCCs),
	handle_calls(G2, Preds1, Preds2, Id1, Id2, Low1, Low2, Stack1, Stack2,
		     SCCs2, SCCs1),
	handle_calls(G3, Preds2, Preds, Id2, Id, Low2, Low, Stack2, Stack,
		     SCCs0, SCCs2).


%  pop_stack(+Stack0, -Stack, +Pred, +Preds0, -Preds, -SCC)
%  SCC is the strongly-connected component currently constructed on Stack0,
%  and Stack is what is left when the elements of SCC have been popped off
%  Stack0.  Pred is the sentinal predicate which is the last one to pop.

pop_stack([Pred0|Stack0], Stack, Pred, Preds0, Preds, [Pred0|SCC]) :-
	put_pred_travnum(Pred0, 999999, Preds0, Preds1),
	(   Pred0 == Pred ->
		SCC = [],
		Stack = Stack0,
		Preds = Preds1
	;   pop_stack(Stack0, Stack, Pred, Preds1, Preds, SCC)
	).



/*************************************************************************

				 Testing Code

Read a file (defined in 'calls.pl'), computes the strongly-connected
components, and print the results, one component per line.

*************************************************************************/

test(File) :-
	calls:file_contents(File, Preds, _, _),
	sccs(Preds, _, SCCs),
	write_sccs(SCCs, Preds).


write_sccs([], _) :-
	nl.
write_sccs([SCC|SCCs], Preds) :-
	write('    '),
	write_predspecs(SCC, Preds),
	nl,
	write_sccs(SCCs, Preds).


write_predspecs([], _).
write_predspecs([P|Ps], Preds) :-
	write_predspec(P, Preds),
	write_predspecs_rest(Ps, Preds).


write_predspecs_rest([], _).
write_predspecs_rest([P|Ps], Preds) :-
	write(', '),
	write_predspec(P, Preds),
	write_predspecs_rest(Ps, Preds).


write_predspec(Ref, Preds) :-
	get_old_pred_ref(Spec, Preds, Ref),
	write(Spec).
