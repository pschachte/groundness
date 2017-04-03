%  file    : bottomup
%  Authors : Peter Schachte
%  Purpose : The bottom-up phase of the groundness analyzer
%  Copyright 2017 Peter Schachte
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
%  The bottom-up part of our global analyzer.  We analyze bottom-up, one
%  strongly-connected component at a time, so that at the time we analyze
%  each scc, we know all there is to know about all the predicates it calls,
%  except for those in that scc.  We iterate analyzing the predicates in the
%  scc until we reach a fixpoint.


:- module(bottomup, [
	analyze_bottom_up/3
   ]).


:- use_module(prep).
:- use_module(analysis).
:- use_module(predstore).



/*****************************************************************
			   Analyzing SCCs Bottom-Up

The file has been read and the strongly-connected components (SCCs) have been
found; now we perform a bottom-up analysis of the file.

For each strongly-connected component, we first fully analyze all calls to
predicates defined *outside* that SCC, and collect all calls to predicates
defined *in* that SCC.  This is accomplished by topologically sorting the SCCs
(this is done as we collect the SCCs), and then analyzing the SCCs in order.
This allows us to analyze the *fixed* part of a predicate, comprising
everything but recursive (or mutually recursive) calls, outside the fixpoint
iteration.  The preparation of a SCC is done by prepare_scc/3, defined in
prep.pl.  After that, we perform the fixpoint computation on only the
*variable* part of the clause, comprising the recursive calls, yielding the
final (approximate) analysis of the predicates in that SCC.

Our approach, computing the glb of all the fixed parts and the meeting this
with the variable parts, requires that our meet operation be commutative.
And, in fact, out meet must be associative for our bottom-up approach to work
at all, otherwise we would not be able to compute an analysis for a predicate
independent of its call pattern.  We could remove the requirement for a
commutative meet with a fairly small performance penalty, by keeping a
separate glb for each contiguous group of calls in a clause.  So the fixpoint
iteration would often require an extra glb or two for each recursive clause.
There's no way to remove the requirement that meet be associative without
tossing the whole two-phase approach.

*****************************************************************/

%  analyze_bottom_up(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed bottom-up for success
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, topologically sorted in bottom-up order.  Each SCC is
%  represented as a list of predicate references.

analyze_bottom_up([], Preds, Preds).
analyze_bottom_up([SCC|SCCs], Preds0, Preds) :-
	prepare_scc(SCC, Preds0, Preds1),
	analyze_to_fixpoint(SCC, Preds1, Preds2),
	analyze_bottom_up(SCCs, Preds2, Preds).



/*======================================================================

			    The Fixpoint Iteration

Here we perform the fixpoint iteration for an SCC.  We do this by meeting the
fixed part of each clause and the current best approximation of the variable
parts.  We join these results for each clause with a non-empty variable part
along with the previous approximation to compute the next approximation for
that predicate.  If the previous and next approximations for each predicate in
the SCC are the same, then we're done, otherwise we must try again.

  ======================================================================*/


%  analyze_to_fixpoint(+SCCs, +Preds0, -Preds)
%  Preds is a predstore identical to Preds0, except that all predicates in it
%  that are specified on SCCs have been analyzed bottom-up for success
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
%  that are specified on SCCs have been analyzed once bottom-up for success
%  conditions.  SCCs is a list of strongly-connected components in the program
%  call graph, where each SCC is represented as a list of predicate
%  references.  All predicates specified on SCCs have been prepared.


analyze_once([], Preds, Preds, Changed, Changed).
analyze_once([Pred|SCC], Preds0, Preds, Changed0, Changed) :-
	get_pred_prep(Pred, Preds0, Prep),
	(   Prep = fixed(_) ->			% nonrecursive:  do no more!
		Changed = Changed0,
		Preds = Preds0
	;   anz_top(Top),
	    analyze_prep(Prep, Preds0, Top, Anal1),
	    (   Changed0 == true ->
		    put_pred_success(Pred, Anal1, Preds0, Preds1),
		    Changed1 = true		% don't compare graphs if
						% we've already found a change
	    ;   get_pred_success(Pred, Preds0, Anal0),
		anz_equiv(Anal0, Anal1) ->	% are graphs are the same?
		    Preds1 = Preds0,		% yes:  no need to update Preds
		    Changed1 = false,
		    anz_free_if_unshared(Anal1, Anal0)
	    ;	put_pred_success(Pred, Anal1, Preds0, Preds1),
		Changed1 = true
	    ),
	    analyze_once(SCC, Preds1, Preds, Changed1, Changed)
	).


%  analyze_prep(+Prep, +Preds, +Anal0, -Anal)
%  Anal is the glb of Anal0 and an approximate analysis of the predicate whose
%  prep term is Prep.  Preds is a predicate store containing the current
%  approximation of all predicates referred to by Prep.  Note that Prep will
%  not be 'fixed'(_), as we have dealt with that case earlier.

analyze_prep(conj_prep(Fixed,Var), Preds, Anal0, Anal) :-
	anz_meet(Anal0, Fixed, Anal1),
	analyze_conj(Var, Preds, Anal1, Anal).
analyze_prep(disj_prep(Fixed0,Var,Restriction), Preds, Anal0, Anal) :-
	anz_copy(Fixed0, Fixed),
	analyze_disj(Var, Preds, Fixed, Anal1),
	anz_meet(Restriction, Anal0, Anal1, Anal).
analyze_prep(call_prep(Pred,Call,_,Restriction), Preds, Anal0, Anal) :-
	get_pred_success(Pred, Preds, Success),
	analyze_call(Anal0, Success, Call, Restriction, Anal).


%  analyze_conj(+Preps, +Preds, +Anal0, -Anal)
%  Anal is the glb of Anal0 and the analysis of Preps, a list of prep terms.
%  Preds is a predicate store containing the current approximation of all
%  predicates referred to by Preps.

analyze_conj([], _, Anal, Anal).
analyze_conj([Prep|Preps], Preds, Anal0, Anal) :-
	analyze_prep(Prep, Preds, Anal0, Anal1),
	analyze_conj(Preps, Preds, Anal1, Anal).


%  analyze_disj(+Preps, +Preds, +Anal0, -Anal)
%  Anal is the lub of Anal0 and the analyses of Preps, a list of prep terms.
%  Preds is a predicate store containing the current approximation of all
%  predicates referred to by Preps.

analyze_disj([], _, Anal, Anal).
analyze_disj([Prep|Preps], Preds, Anal0, Anal) :-
	anz_top(Top),
	analyze_prep(Prep, Preds, Top, Anal1),
	anz_join(Anal0, Anal1, Anal2),
	analyze_disj(Preps, Preds, Anal2, Anal).

