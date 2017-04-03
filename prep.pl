%  file    : prep.pl
%  Authors : Peter Schachte
%  Purpose : Prepare Prolog code for groundness analysis
%
%				Abstract
%
%  Abstract Interpretation is based on fixpoint iteration.  In order to
%  minimize the time, we do as much of the work as possible in an initial
%  preparation phase, outside the loop.  This code does the fixed part of the
%  analysis, that part that does not require fixpoint iteration.  For
%  bottom-up analysis, this means computing the glb of the analyses of the
%  calls to predicates in other strongly-connected components for each clause,
%  and also computing the lub of these fixed parts for all the clauses that
%  have no calls to the same strongly-connected component.  For top-down
%  analysis, this means computing for each call in each clause, the glb of the
%  analyses of all the inter-scc (non-recursive) calls up to that call.  We do
%  both here.

:- module(prep, [
	prepare_scc/3,
	prepare_code/5
   ]).

:- use_module(predstore).
:- use_module(analysis).
:- use_module(misc).


%  TODO:  use 0 instead of 'none'.  Should work but needs careful testing.
%  This would allow me to use max/3 instead of choose_projection/3,
%  choose_projection_limit/3, and rlimit_max/3.


/*****************************************************************

			       Code Preparation

This code prepares predicate code for analysis.  This means that we
create for each predicate in a given strongly-connected component a
data structure which contains all and only the information we need to
consider in doing the fixpoint computation for both the bottom-up and
top-down analysis.  We call this data structure a prep term, which is
either:
++description
    'fixed'(*A*)	where *A* is the predicate's complete success
			pattern analysis (we get this for
			non-recursive predicates); or
    a compound prep	which we get for recursive or mutually
			recursive predicates.
--description
A compound prep is one of:
++description
    'conj_prep'(*A,V*)	which describes a conjunction, where *A* is
			the non-recursive part of the analysis and *V*
			is the recursive part; or
    'disj_prep'(*A,V,R*)
			which describes a disjjunction, where *A* is
			the non-recursive part of the analysis, *V* is the
			recursive part, and *R* is the projection threshold
			(i.e., the lowest numbered variable not appearing
			later in the code); or
    'call_prep'(*P,T,A,R*)
			which describes an individual goal, where *P*
			is the predicate ref of the predicate being
			called, *T* is the call term, *A* is the
			analysis of the calls up to this one, and *R*
			is the projection threshold.
--description

The *Projection* terms above apply only *after* conjunction with the left
context of the prep term, i.e. the glb of the analyses of the preceding prep
terms.  For example, in the evaluation of a 'disj_prep' term, the lub of *A*
and the analyses of the elements of *V* are first meeted with the the left
context, and then variables numbered greater or equal to *R* are projected
away.

****************************************************************/

%  prepare_scc(+SCC, +Preds0, -Preds)
%  prepare_scc(+SCC, +Preds0, -Preds, +WholeSCC)
%  Preds is a predicate store identical to Preds0 except that all the
%  predicates specified on SCC have been ¨prepared.¨  SCC is a list of
%  predicate references.  WholeSCC is the whole list of predicates in the
%  scc.

prepare_scc(SCC, Preds0, Preds) :-
        prepare_scc(SCC, Preds0, Preds, SCC).

prepare_scc([], Preds, Preds, _).
prepare_scc([Ref|Refs], Preds0, Preds, SCC) :-
	get_old_pred_ref(_:_/Arity, Preds0, Ref),
	(   pred_has_property(Ref, dynamic, Preds0) ->
		% This is correct, but less than ideal.  In many case we could
		% do better if we could find all the possible places where a
		% clause for this predicate could be asserted.  In that case,
		% we could use the predicate's natural prep stand, and then
		% after a top-down analysis, we could see if we could be
		% adding and clauses that would change the analysis.  If so,
		% we would have to repeat the bottom-up analysis starting from
		% this predicate (re-analyzing any predicates that call
		% changed predicates.  Then we would need to repeat the
		% top-down analysis starting with predicates that call the
		% changed predicates.
		anz_top(T),
		Prep = fixed(T)
	;   get_pred_code(Ref, Preds0, Code),
	    prepare_code(Code, Arity, Preds0, SCC, Prep)
	),
	put_pred_prep(Ref, Prep, Preds0, Preds1),
	prep_fixed_part(Prep, Arity, Approx1),
	put_pred_success(Ref, Approx1, Preds1, Preds2),
	prepare_scc(Refs, Preds2, Preds, SCC).


%  prepare_code(+-Code, +Arity, +Preds, +SCC, -Prep)
%  Prep is the prepared version of Code, given that SCC is a list of the
%  references of the predicates in the same SCC as Code, and Arity is the
%  arity of the predicate whose code is Code. Preds is a predstore.

prepare_code(undefined, _, _, _, fixed(T)) :-
	!,
	% This should really be bottom, since an undefined predicate can't
	% succeed.  The trouble with making it bottom is that we wouldn't do a
	% very good job analyzing programs that use builtin predicates the
	% analyzer doesn't know about.
	anz_top(T).				
prepare_code(foreign(Foreignspec), _, _, _, fixed(Fixed)) :-
	!,
	analyze_foreign(Foreignspec, Fixed).
prepare_code(Code, Arity, Preds, SCC, Prep) :-
	anz_top(Top),
	anz_bottom(Bottom),
	% could just call prepare_1_conj, but that will usually get around to
	% making the following call, anyway:
	prepare_1_disj(Code, Preds, SCC, none, _, 999999, _, Arity, Top,
		       Bottom, Fixed, Var, []),
	(   Var == [] ->
		Prep = fixed(Fixed)
	% This won't handle all cases, as anz_bottom isn't meant to test if an
	% analysis is top, only to generate *an instance* of top.  Still, it
	% will often work, and is better than nothing.
 	;   Var = [Single],
 	    anz_bottom(Bottom2),
	    anz_equiv(Bottom2, Fixed) ->
 		Prep = Single
	;   Var = [Elt|_],
	    prep_projection(Elt, Projection),
	    Prep = disj_prep(Fixed,Var,Projection)
	).


%  prepare_conj(+Gs, +Preds, +SCC, +Rlim0, -Rlim, +Rlast0, -Rlast, -Rnext0,
%		+Rnext, +Fixed0, -Fixed, -Var, +Var0)
%  Fixed is the fixed (non-recursive) part of the analysis of the conjunction
%  of the goals on the list Gs, meeted with Fixed0.  Var is a list of the
%  variable (recursive) goals on Gs, followed by Var0.  Preds is the predstore
%  containing all the predicates in the program, and SCC is a list of the
%  predicate references of all the predicates in the same SCC as the predicate
%  being examined.  Var is basically a list of the recursive goals on Gs, and
%  Fixed is the glb of analyses of all the rest.  Rlim is Rlim0 unless Rlim0
%  is 'none', in which case it is the projection threshold for the last goal
%  in Gs before the first recursive call, or 'none' if there are no recursive
%  calls.  Rlast is projection threshold of the last goal in Gs and Rlast0 is
%  the threshold for the last goal before Gs.  Rnext0 is the threshold for the
%  last goal in Gs before the first recursive goal, or Rnext if there are no
%  recursive goals, and Rnext is the threshold to be used for the prep term
%  for the last recursive call in Gs, which should be the last threshold
%  before the first recursive call *following* Gs, or 1 + the arity of the
%  predicate if there are no following recursive calls; this pair is used for
%  backward information flow.

prepare_conj([], _, _, Rlim, Rlim, Rlast, Rlast, Rnext, Rnext, Fixed, Fixed,
		Var, Var).
prepare_conj([G|Gs], Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0, Rnext,
		Fixed0, Fixed, Var, Var0) :-
	prepare_1_conj(G, Preds, SCC, Rlim0, Rlim1, Rlast0, Rlast1, Rnext0,
		       Rnext1, Fixed0, Fixed1, Var, Var1),
	prepare_conj(Gs, Preds, SCC, Rlim1, Rlim, Rlast1, Rlast, Rnext1,
		     Rnext, Fixed1, Fixed, Var1, Var0).


%  prepare_1_conj(+Goal, +Preds, +SCC, +Rlimit0, -Rlimit, +Rlast0, -Rlast,
%		  -Rnext0, +Rnext, +Fixed0, -Fixed, +Var0, -Var)
%  Same as prepare_conj, except that Goal is a single goal.

% prepare_1_conj(undefined, _, _, Rlim, Rlim, Rlast, Rlast, Rnext, Rnext,
% 		Fixed, Fixed, Var, Var).
prepare_1_conj(conj(Goals), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0,
		Rnext, Fixed0, Fixed, Var, Var0) :-
	prepare_conj(Goals, Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0,
		     Rnext, Fixed0, Fixed, Var, Var0).
prepare_1_conj(equal(V,T,Vs,Fixed0,R), _, _, Rlim, Rlim, _, R, Rnext, Rnext,
		Fixed0, Fixed, Var, Var) :-
	choose_projection(Rlim, R, Projection),
	analyze_unif(V, T, Vs, Projection, Fixed0, Fixed).
prepare_1_conj(eval(V,T,Vs,Fixed0,R), _, _, Rlim, Rlim, _, R, Rnext, Rnext,
		Fixed0, Fixed, Var, Var) :-
	choose_projection(Rlim, R, Projection),
	analyze_eval(V, T, Vs, Projection, Fixed0, Fixed).
prepare_1_conj(builtin(T,Fixed0,R), _, _, Rlim, Rlim, _, R, Rnext, Rnext,
		Fixed0, Fixed, Var, Var) :-
	choose_projection(Rlim, R, Projection),
	analyze_builtin(T, Projection, Fixed0, Fixed).
prepare_1_conj(call(P,T,Fixed1,R), Preds, SCC, Rlim0, Rlim, Rlast0, R,
		Rnext0, Rnext, Fixed0, Fixed, Var, Var0) :-
	anz_copy(Fixed0, Fixed1),
	(   member(P, SCC) ->			% it's a recursive call
		Var = [call_prep(P,T,Fixed1,Rnext)|Var0],
		Fixed = Fixed0,
		Rnext0 = Rlast0,
		choose_projection_limit(Rlim0, Rlast0, Rlim)
	;   Rlim = Rlim0,			% non-recursive call
	    Var = Var0,
	    Rnext = Rnext0,
	    choose_projection(Rlim, R, Projection),
	    get_pred_success(P, Preds, Success),
	    analyze_call(Fixed0, Success, T, Projection, Fixed)
	).
prepare_1_conj(!, _, _, Rlim, Rlim, Rlast, Rlast, Rnext, Rnext, Fixed, Fixed,
		Var, Var).
prepare_1_conj(disj(Ds), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext0,
		Rnext, Fixed0, Fixed, Var, Var0) :-
	anz_bottom(Bottom),
	choose_projection(Rlim0, Rlast0, Rnext1),
	prepare_disj(Ds, Preds, SCC, Rlim0, Rlim0, Rlim, Rlast0, 0, Rlast,
		     Rnext1, Fixed0, Bottom, Fixed1, Var1, []),
	(   Var1 == [] ->
		Var = Var0,
		Rnext = Rnext0,
		Fixed = Fixed1
% I don't think this code is right!!!!
% 	;   Var1 = [conj_prep(Fixed2,Var2)] ->
% 		anz_meet(Fixed0, Fixed1, Fixed1a),
% 		anz_meet(Fixed1a, Fixed2, Fixed),
% 		append(Var2, Var0, Var)
	;   Fixed = Fixed0,
	    Rnext0 = Rlast0,
	    Var = [disj_prep(Fixed1,Var1,Rnext)|Var0]
	).
prepare_1_conj(if_then_else(G1,G2,G3), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast,
		Rnext0, Rnext, Fixed0, Fixed, Var, Var0) :- 
	prepare_1_conj(disj([conj([G1,G2]),G3]), Preds, SCC, Rlim0, Rlim,
		       Rlast0, Rlast, Rnext0, Rnext, Fixed0, Fixed, Var,
		       Var0).


%  prepare_disj(+Gs, +Preds, +SCC, +Rlimctxt, +Rlim0, -Rlim, +Rlastctxt,
%  		+Rlast0, -Rlast, +Rnext, +Context, +Fixed0, -Fixed, -Var,
%  		+Var0)
%  Fixed is the fixed (non-recursive) part of the analysis of the disjunction
%  of the goals on the list Gs, joined with Fixed0.  Var is a list of the
%  variable (recursive) goals on Gs, followed by Var0.  Context is the fixed
%  part of the analysis of the goals conjoined to the left of the disjunction
%  Gs.  Preds is the predstore containing all the predicates in the program,
%  and SCC is a list of the predicate references of all the predicates in the
%  same SCC as the predicate being examined.  Var is basically a list of the
%  recursive goals on Gs, and Fixed is the lub of analyses of all the rest.
%  Rlim is the maximum of Rlim0 and the projection thresholds for all the
%  goals in Gs, or 'none' if there are no recursive calls.  Rlimctxt is the
%  projection limit to apply to each disjunct.  Rlast is projection
%  threshold of the last goal in each disjunct in Gs and Rlastctst is the
%  threshold for the last goal before Gs.  The Rlast for most disjuncts will
%  be the same, but a disjunct with only a cut won't get it right, so we take
%  the max of Rlast0 and the Rlasts of the disjuncts.  Rnext is the threshold
%  to be used for the prep term of the last recursive call in each disjunct in
%  Gs, which should be the last threshold before the first recursive call
%  *following* Gs, or the arity of the predicate if there are no following
%  recursive calls.

prepare_disj([], _, _, _, Rlim, Rlim, _, Rlast, Rlast, _, _, Fixed, Fixed,
		Var, Var).
prepare_disj([D|Ds], Preds, SCC, Rlimctxt, Rlim0, Rlim, Rlastctxt, Rlast0,
		Rlast, Rnext, Context, Fixed0, Fixed, Var, Var0) :-
	prepare_1_disj(D, Preds, SCC, Rlimctxt, Rlim1, Rlastctxt, Rlast1,
		       Rnext, Context, Fixed0, Fixed1, Var, Var1),
	rlimit_max(Rlim0, Rlim1, Rlim2),
	max(Rlast0, Rlast1, Rlast2),		% most Rlasts are the same
	prepare_disj(Ds, Preds, SCC, Rlimctxt, Rlim2, Rlim, Rlastctxt, Rlast2,
		     Rlast, Rnext, Context, Fixed1, Fixed, Var1, Var0).


%  prepare_1_disj(+G, +Preds, +SCC, +Rlim0, -Rlim, +Rlast0, -Rlast,
%		       +Rnext, +Context, +Fixed0, -Fixed, -Var, +Var0)

% prepare_1_disj(undefined, _, _, _, Rlim, Rlim, Rlast, Rlast, _, _, Fixed,
% 		Fixed, Var, Var).
prepare_1_disj(conj(Goals), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext,
		Context, Fixed0, Fixed, Var, Var0) :-
	anz_copy(Context, Context1),
	prepare_conj(Goals, Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, _, Rnext,
		     Context1, Fixed1, Var1, []),
	(   Var1 == [] ->
		anz_join(Fixed0, Fixed1, Fixed),
		Var = Var0
% I don't think this code is right!!!!
% 	;   Var1 = [disj_prep(Fixed2,Var2,_)] ->
% 		anz_join(Fixed0, Fixed1, Fixed1a),
% 		anz_join(Fixed1a, Fixed2, Fixed),
% 		append(Var2, Var0, Var)
	;   Fixed = Fixed0,			% has a variable part, so
						% fixed part doesn't
						% contribute to first approx.
	    Var = [conj_prep(Fixed1,Var1)|Var0]
	).
prepare_1_disj(equal(V,T,Vs,Context,R), _, _, Rlim, Rlim, _, R, _,
		Context, Fixed0, Fixed, Var, Var) :-
	choose_projection(Rlim, R, Projection),
	anz_copy(Context, Context1),
	analyze_unif(V, T, Vs, Projection, Context1, Fixed1),
	anz_join(Fixed0, Fixed1, Fixed).
prepare_1_disj(eval(V,T,Vs,Context,R), _, _, Rlim, Rlim, _, R, _,
		Context, Fixed0, Fixed, Var, Var) :-
	choose_projection(Rlim, R, Projection),
	anz_copy(Context, Context1),
	analyze_eval(V, T, Vs, Projection, Context1, Fixed1),
	anz_join(Fixed0, Fixed1, Fixed).
prepare_1_disj(builtin(T,Context,R), _, _, Rlim, Rlim, _, R, _,
		Context, Fixed0, Fixed, Var, Var) :-
	choose_projection(Rlim, R, Projection),
	anz_copy(Context, Context1),
	analyze_builtin(T, Projection, Context1, Fixed1),
	anz_join(Fixed0, Fixed1, Fixed).
prepare_1_disj(call(P,T,Context,R), Preds, SCC, Rlim0, Rlim, Rlast0, R, _,
		Context, Fixed0, Fixed, Var, Var0) :-
	(   member(P, SCC) ->			% it's a recursive call
		Var = [call_prep(P,T,Context,Rlim)|Var0],
		Fixed = Fixed0,
		choose_projection_limit(Rlim0, Rlast0, Rlim)
	;   Rlim = Rlim0,
	    Var = Var0,
	    choose_projection(Rlim, R, Projection),
	    anz_copy(Context, Context1),
	    get_pred_success(P, Preds, Success),
	    analyze_call(Context1, Success, T, Projection, Fixed1),
	    anz_join(Fixed0, Fixed1, Fixed)
	).
prepare_1_disj(!, _, _, Rlim, Rlim, Rlast, Rlast, _, _, Fixed, Fixed, Var,
		Var).
prepare_1_disj(disj(Goals), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast, Rnext,
		Context, Fixed0, Fixed, Var, Var0) :-
	prepare_disj(Goals, Preds, SCC, Rlim0, Rlim0, Rlim, Rlast0, Rlast0,
		     Rlast, Rnext, Context, Fixed0, Fixed, Var, Var0).
prepare_1_disj(if_then_else(G1,G2,G3), Preds, SCC, Rlim0, Rlim, Rlast0, Rlast,
		Rnext, Context, Fixed0, Fixed, Var, Var0) :-
	prepare_1_disj(conj([G1,G2]), Preds, SCC, Rlim0, Rlim1, Rlast0,
		       Rlast1, Rnext, Context, Fixed0, Fixed1, Var, Var1),
	prepare_1_disj(G3, Preds, SCC, Rlim0, Rlim2, Rlast0, Rlast2, Rnext,
		       Context, Fixed1, Fixed, Var1, Var0),
	rlimit_max(Rlim1, Rlim2, Rlim),
	max(Rlast1, Rlast2, Rlast).


%  choose_projection(+Rlimit0, +R, -Projection)

choose_projection(none, Projection, Projection) :-
	!.
choose_projection(Projection, _, Projection).
	

%  choose_projection_limit(+Rlim0, +Rlast, -Rlim)

choose_projection_limit(none, Rlast, Rlast) :-
	!.
choose_projection_limit(Rlim, _, Rlim).


%  rlimit_max(+Limit0, +Limit1, -Limit)
%  Limit is the maximum projection limit of Limit0 and Limit1.  Note that
%  either or both of these may be 'none', which we treat like -infinity.

rlimit_max(none, Limit, Limit) :-
	!.
rlimit_max(Limit0, Limit1, Limit) :-
	(   Limit1 == none ->
		Limit = Limit0
	;   max(Limit0, Limit1, Limit)
	).


%  prep_fixed_part(+Prep, +Projection, -Fixed)
%  Fixed is the fixed part of Prep, a prep term, projected to variables less
%  than Projection.

prep_fixed_part(fixed(Fixed), _, Fixed).
prep_fixed_part(conj_prep(Fixed0,Var), Projection, Fixed) :-
	anz_copy(Fixed0, Fixed1),
	prep_list_fixed_part(Var, Fixed1, Fixed2),
	anz_project(Fixed2, Projection, Fixed).
prep_fixed_part(disj_prep(Fixed,_,_), _, Fixed).
prep_fixed_part(call_prep(_,_,_,_), _, Bottom) :-
	anz_bottom(Bottom).


prep_list_fixed_part([], Fixed, Fixed).
prep_list_fixed_part([Prep|Preps], Fixed0, Fixed) :-
	prep_fixed_part(Prep, 999999, Fixed1),
	anz_meet(Fixed0, Fixed1, Fixed2),
	prep_list_fixed_part(Preps, Fixed2, Fixed).


%  prep_projection(+Prep, -Projection)
%  Projection is the projection threshold for prep term Prep.

prep_projection(conj_prep(_,Var), Projection) :-
	last(Var, Last),
	prep_projection(Last, Projection).
prep_projection(disj_prep(_,_,Projection), Projection).
prep_projection(call_prep(_,_,_,Projection), Projection).


%  last(+List, -Last)
%  List is a non-empty list, and Last is its last element.

last([H|T], Last) :-
	last(T, H, Last).

last([], Last, Last).
last([H|T], _, Last) :-
	last(T, H, Last).
	
