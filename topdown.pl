%  file    : topdown
%  Authors : Peter Schachte
%  Purpose : Top down part of Prolog groundness analysis program
%
%				Abstract
%
%  This is the top down part of a Prolog groundness analyzer.  When this is
%  called, we have already performed a bottom-up analysis, so we have
%  success patterns for all predicates.  This program finds predicate call
%  patterns.


:- module(topdown, [
	analyze_top_down/3,
	analyze_single_goal/3
   ]).


:- use_module(analysis).
:- use_module(predstore).
%  :- use_module(calls).				% for simplify_goal/7
:- use_module(prep).
:- use_module(library(basics)).



/*****************************************************************

			 Top-down Groundness Analysis

While one can think of the bottom-up part of the analysis as
collecting the analysis of lower parts of the call graph to produce
the analysis of the higher parts of the graph, top-down part of the
analysis can be thought of as distributing the analysis back down the
tree.  The conditions on completion of a call to a predicate, relative
to the call conditions determine, in part, the relative completion
conditions, i.e. the success pattern, of the predicates that call it.
Similarly, the call conditions for a predicate, together with the
success patterns of the predicates it calls, determine the call
patterns of the predicates it calls.

The top-down phase of groundness analysis, like the bottom-up phase, works on
one strongly-connected component at a time.  Of course, the SCCs are
processed in the opposite order.  For the top-down analysis, we also have
access to the work performed during the bottom-up analysis.

Top-down analysis of each SCC is divided into two phases.  First, we find the
correct call patterns for the predicates in that SCC.  This is done using the
the prep term resulting from the preparation phase of the bottom-up analysis.
Thus the first phase computes call patters for only the recursive calls.
Once a fixpoint is reached, we have the final call pattern for all the
predicates in that SCC.

The bottom-up preparation has already associated with each goal the glb of
the success analyses of the goals up to (but not including) that goal.  In
the second phase, we use this, along with the final call patterns for the
predicates in the same SCC, to compute the correct call patterns for all the
calls in that SCC.

Note: this code doesn't get the most precise possible results.  We find a
separate call pattern for each call, rather than only for each predicate,
which gives us strong results.  But that isn't enough: we must have a call
pattern for each predicate so that we can determine the call patterns for that
predicate's calls.  What we should do is do this computation separately for
each groundness pattern that predicate is called with.  Instead, we lub
together all the different call patterns to produce a single call pattern for
the predicate, using the call patterns for that predicate's calls.  This is
less than ideal.

In order to fix this, we should really do two things: adopt a representation
for boolean functions for which equality checking is very cheap (e.g., Bryant
graphs).  This would help because what we would need to do for each call is
check to see if the new call pattern is already on the list of call patterns
for that predicate, and only add it if not.  The second, more important thing,
is to compute functional dependencies for the predicates before the top-down
pass.  This would allow us to recognize call patterns that are "essentially"
the same.  For example, a call to functor/3 with all arguments ground is
essentially the same as one with only the first ground, because it can be
compiled as a call with the first argument supplied and the other two
temporaries, which are then unified (equality checked) against the two ground
arguments as supplied.  This also applies to user-supplied predicates.  This
is Somogyi's concept of implied modes.  It's not as simple as that, though;
for example, the first two arguments to append/3 imply the third, but one
wouldn't want to compile a call to append/3 with all arguments ground as a
call with a temporary third argument, which would then have to be
general-unified with the supplied argument.  It seems types as well as
functional dependencies are needed to do this properly.  The alternative is to
really generate different versions of code for each distinct call pattern,
which seems a bit much.

So for now I don't worry about this.

*****************************************************************/


%  analyze_single_goal(+Goal, +Preds0, -Preds)
%  Preds is the same as Preds0, except that it accounts for Goal as a possible
%  call pattern.  This is intended to be used for the top-level goal, before
%  the top-down analysis proper.

analyze_single_goal(Goal, Preds0, Preds) :-
%	simplify_goal(Goal, user, 1, _, Preds0, Preds1, Code),
	prepare_code(Goal, 0, Preds0, [], _),
	anal_top(Top),
	propagate_goal_calls(Goal, Top, _, [], Preds0, Preds).



%  analyze_top_down(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed top-down for call conditions.
%  SCCs is a list of strongly-connected components in the program call graph,
%  topologically sorted in top-down order.  Each SCC is represented as a list
%  of predicate references.

analyze_top_down([], Preds, Preds).
analyze_top_down([SCC|SCCs], Preds0, Preds) :-
	analyze_to_fixpoint(SCC, Preds0, Preds1),
	propagate_call_patterns(SCC, SCC, Preds1, Preds2),
	analyze_top_down(SCCs, Preds2, Preds).



/*======================================================================

			    The Fixpoint Iteration

Here we perform the fixpoint iteration for an SCC.  We do this by meeting the
fixed part of the analysis of each clause and the glb of current best
approximation of the call patterns of the variable parts.  We join this result
for each call with the current approximation for that predicate to determine
its next approximation.  If the previous and next approximations for each
predicate called from the SCC are the same, then we're done, otherwise we must
try again.

  ======================================================================*/


%  analyze_to_fixpoint(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed top-down for call
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.

analyze_to_fixpoint(SCC, Preds0, Preds) :-
	analyze_once(SCC, Preds0, Preds1, false, Changed),
	(   Changed == true ->
		analyze_to_fixpoint(SCC, Preds1, Preds)
	;   Preds = Preds1
	).


%  analyze_once(+SCC, +Preds0, -Preds, +Changed0, -Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCC have been analyzed once top-down for call
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.


analyze_once([], Preds, Preds, Changed, Changed).
analyze_once([Pred|SCC], Preds0, Preds, Changed0, Changed) :-
	get_pred_prep(Pred, Preds0, Prep),
	(   Prep = fixed(_) ->			% nonrecursive:  do no more!
		Changed = Changed0,
		Preds = Preds0
	;   get_pred_call(Pred, Preds0, Callpat),
	    anal_copy(Callpat, Callpat1),
	    analyze_prep(Prep, Callpat1, Callpat2, Preds0, Preds1,
			 Changed0, Changed1),
	    anal_free_if_unshared(Callpat2, Callpat1),
	    analyze_once(SCC, Preds1, Preds, Changed1, Changed)
	).


%  analyze_prep(+Prep, +Analysis0, -Analysis, +Preds0, -Preds, +Changed0,
%		-Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified in Prep, a prep term, have had their call patterns
%  updated corresponding to their call(s) in Prep.  Analysis0 is the (current
%  approximation of) the pattern in which the Prep term would be used, and
%  Analysis is the pattern in which the next goal following Prep would be
%  called.  Changed is true if  Changed0 is, or if one of the predicates
%  specified on Prep gets a new call pattern due to this analysis.

analyze_prep(conj_prep(_,Var), Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	analyze_conj(Var, Anal0, Anal, Preds0, Preds, Ch0, Ch).
analyze_prep(disj_prep(_,Var,R), Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	anal_bottom(Bottom),
	analyze_disj(Var, R, Anal0, Bottom, Anal, Preds0, Preds, Ch0, Ch).
analyze_prep(call_prep(Pred,Call,Anal2,Restriction), Anal0, Anal, Preds0,
		Preds, Ch0, Ch) :-
	anal_copy(Anal0, Anal1),
	anal_meet(Anal1, Anal2, Anal3),
	analyze_call_pattern(Call, Anal3, Cpat),
	update_call_pattern(Pred, Cpat, Preds0, Preds, Ch0, Ch),
	% This is where we rely on our meet operation being commutitive.
	get_pred_success(Pred, Preds, Success),
	analyze_call(Anal0, Success, Call, Restriction, Anal).


%  analyze_conj(+Preps, +Analysis0, -Analysis, +Preds0, -Preds, +Changed0,
%		-Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified in Preps, a list of prep terms, have had their call
%  patterns updated corresponding to their call(s) in Preps.  Analysis0 is the
%  (current approximation of) the pattern in which the Preps term would be
%  used, and Analysis is the pattern in which the next goal following Preps
%  would be called.  Changed is true if Changed0 is, or if one of the
%  predicates specified on Preps gets a new call pattern due to this analysis.

analyze_conj([], Anal, Anal, Preds, Preds, Ch, Ch).
analyze_conj([Prep|Preps], Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	analyze_prep(Prep, Anal0, Anal1, Preds0, Preds1, Ch0, Ch1),
	analyze_conj(Preps, Anal1, Anal, Preds1, Preds, Ch1, Ch).


%  analyze_disj(+Preps, +Restriction, +Context, +Analysis0, -Analysis,
%		+Preds0, -Preds, +Changed0, -Changed)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified in Preps, a list of prep terms, have had their call
%  patterns updated corresponding to their call(s) in Preps.  Context is the
%  (current approximation of) the pattern in which the Preps term would be
%  used.  Analysis is Analysis0 joined with the lub of the analysis of Preps.
%  Changed is true if Changed0 is, or if one of the predicates specified on
%  Preps gets a new call pattern due to this analysis.

analyze_disj([], R, _, Anal0, Anal, Preds, Preds, Ch, Ch) :-
	anal_restrict(Anal0, R, Anal).
analyze_disj([Prep|Preps], R, Context, Anal0, Anal, Preds0, Preds, Ch0, Ch) :-
	anal_copy(Context, Context1),
	analyze_prep(Prep, Context1, Anal1, Preds0, Preds1, Ch0, Ch1),
	(   Preps == [] ->
		Preds = Preds1,
		Ch = Ch1,
		anal_join(Anal0, Anal1, R, Anal)
	;   anal_join(Anal0, Anal1, Anal2),
	    analyze_disj(Preps, R, Context, Anal2, Anal, Preds1, Preds, Ch1,
			 Ch)
	).



%  update_call_pattern(Pred, Callpat, Preds0, Preds, Changed0, Changed)
%  Preds is the same as Preds0, except that it now notes that predicate Pred
%  may be called with call pattern Callpat.  Changed is true Changed0 is, or
%  if Preds is different than Preds0.

update_call_pattern(Pred, Callpat, Preds0, Preds, Changed0, Changed) :-
	get_pred_call(Pred, Preds0, Callpat0),
	anal_copy(Callpat0, Callpat1),
	anal_join(Callpat1, Callpat, Callpat2),
	(   Changed0 == true ->
		Changed = Changed0,
		put_pred_call(Pred, Callpat2, Preds0, Preds)
	;   anal_equiv(Callpat0, Callpat2) ->
		Changed = Changed0,
		Preds = Preds0
	;   Changed = true,
	    put_pred_call(Pred, Callpat2, Preds0, Preds)
	).



/*======================================================================

			     Analysis Propagation

Once we've completed the call pattern analysis of an SCC, we must
propagate the call pattern information to all the predicates called
from the SCC.  We can ignore calls to predicates in the SCC itself,
since we've handled those calls in the fixpoint computation.  This
part is the complement of the fixpoint analysis:  there we were
interested only in recursive calls, here we only handle nonrecursive
calls.  This can be viewed as the dual of the preparation part of the
bottom-up analysis phase.

Here we again make use of the information computed during the
preparation part of the bottom-up analysis phase.  During that part of
the analysis, we computed a partial call pattern for each call, which
excluded the call pattern for the predicate being analyzed and the
success pattern for the recursive calls appearing earlier in the
clause, since that information was not available at that stage.  Now
it is.  Thus all we need to do at this point is to collect the missing
information and combine it with the information already stored to give
the final answer.

This approach isn't as optimal as it may seem, since we still have to
perform a meet for each call.  Still it's a saving, because had we not
stored that partial information, we'd have to perform a meet for each
unification and builtin, too.

  ======================================================================*/

%  propagate_call_patterns(+List, +SCC, +Preds0, -Preds)
%  Preds is the same predstore as Preds0, except that all predicates not on
%  the list SCC and called by predicates whose pred refs are on List have had
%  their call patterns updated to reflect those calls.

propagate_call_patterns([], _, Preds, Preds).
propagate_call_patterns([Pred|List], SCC, Preds0, Preds) :-
	get_pred_call(Pred, Preds0, Callpat),
	get_pred_code(Pred, Preds0, Code),
	anal_copy(Callpat, Callpat1),
	propagate_goal_calls(Code, Callpat1, _, SCC, Preds0, Preds1),
	propagate_call_patterns(List, SCC, Preds1, Preds).


%  propagate_goal_calls(+Code, +Callpat, -Exitpat, +SCC, +Preds0, -Preds)

propagate_goal_calls(undefined, Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(foreign(_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(conj(Goals), Callpat, Exitpat, SCC, Preds0, Preds) :-
	propagate_conj_calls(Goals, Callpat, Exitpat, SCC, Preds0, Preds).
propagate_goal_calls(equal(_,_,_,_,_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(eval(_,_,_,_,_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(builtin(_,_,_), Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(call(P,T,A,R), Callpat, Exitpat, SCC, Preds0, Preds) :-
	(   member(P, SCC) ->
		% recursive call:  add success pattern to call pattern to find
		% the next goal's call pattern
		Preds = Preds0,
		get_pred_success(P, Preds, Success),
		analyze_call(Callpat, Success, T, R, Exitpat)
	;   % non-recursive call:  meet current call pattern with existing
	    % analysis to determine actual call pattern, and update Preds.
	    % Note:  here we rely on meet being commutative.
	    anal_copy(Callpat, Exitpat),
	    anal_meet(Callpat, A, Context),
	    analyze_call_pattern(T, Context, Actualcall),
	    simple_update_call_pattern(P, Actualcall, Preds0, Preds)
	).
propagate_goal_calls(!, Exitpat, Exitpat, _, Preds, Preds).
propagate_goal_calls(disj(Goals), Callpat, Exitpat, SCC, Preds0, Preds) :-
	anal_bottom(Bottom),
	propagate_disj_calls(Goals, Callpat, Bottom, Exitpat, SCC, Preds0,
			     Preds).
propagate_goal_calls(if_then_else(G1,G2,G3), Callpat, Exitpat, SCC, Preds0,
		Preds) :-
	anal_bottom(Bottom),
	propagate_disj_calls([conj([G1,G2]),G3],Callpat, Bottom, Exitpat, SCC,
			     Preds0, Preds).


%  propagate_conj_calls(+Goals, +Callpat, -Exitpat, +SCC, +Preds0, -Preds)

propagate_conj_calls([], Exitpat, Exitpat, _, Preds, Preds).
propagate_conj_calls([Goal|Goals], Callpat, Exitpat, SCC, Preds0, Preds) :-
	propagate_goal_calls(Goal, Callpat, Callpat1, SCC, Preds0, Preds1),
	propagate_conj_calls(Goals, Callpat1, Exitpat, SCC, Preds1, Preds).


%  propagate_disj_calls(+Goals, +Context, +Callpat, -Exitpat, +SCC, +Preds0,
%		-Preds)

propagate_disj_calls([], _, Exitpat, Exitpat, _, Preds, Preds).
propagate_disj_calls([Goal|Goals], Context, Callpat, Exitpat, SCC, Preds0,
		Preds) :-
	anal_copy(Context, Context1),
	propagate_goal_calls(Goal, Context1, Exitpat1, SCC, Preds0, Preds1),
	anal_join(Callpat, Exitpat1, Callpat1),
	propagate_disj_calls(Goals, Context, Callpat1, Exitpat, SCC, Preds1,
			     Preds).



%  simple_update_call_pattern(Pred, Callpat, Preds0, Preds)
%  Preds is the same as Preds0, except that it now notes that predicate Pred
%  may be called with call pattern Callpat.

simple_update_call_pattern(Pred, Callpat, Preds0, Preds) :-
	get_pred_call(Pred, Preds0, Callpat0),
	anal_copy(Callpat0, Callpat1),
	anal_join(Callpat1, Callpat, Callpat2),
	put_pred_call(Pred, Callpat2, Preds0, Preds).
