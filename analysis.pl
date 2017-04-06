%  file    : analysis.pl
%  Authors : Peter Schachte
%  Purpose : Storage and manipulation of predicate analyses
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
%  This code maintains an abstract datatype storing predicate analyses.
%  I.e., this module implements the abstract domain.  It provides all the
%  operations necessary for analysis.  

:- module(analysis, [
	max_variable/1,
	init_rep/0,
	conclude_rep/0,
	anz_free/1,
	anz_free_if_unshared/2,
	anz_copy/2,
	anz_equiv/2,
	anz_top/1,
	anz_bottom/1,
	anz_meet/3,
	anz_meet/4,
	anz_join/3,
	anz_join/4,
	anz_meet_vars/2,
	anz_project/3,
	anz_iffconj/3,
	anz_print/1,
	anz_print_stderr/1,
	analyze_unif/6,
	analyze_eval/6,
	analyze_builtin/4,
	analyze_call/5,
	analyze_call_pattern/3,
	analyze_foreign/2,
        builtin_analysis/2,
        milli_time/1
   ]).


%  max_variable(-Limit)
%  Limit is the /smallest invalid/ variable number.  Valid variables range
%  from 1 to Limit-1.

%  ** implemented in C **


%  init_rep
%  This must be called before performing any operations in this file.


%  conclude_rep
%  Calling this frees all Boolean function representations allocated by
%  predicates in this module so no such data structures may be used after this
%  call.  It also resets the state of the foreign code so that the system is
%  just as if init_rep hadn't yet been called.  After performing this
%  operation, no operations in this file may be
%  performed again until init_rep/0 is called again.

%  ** implemented in C **


%  anz_free(+Analysis)
%  Declares that the Boolean function representation Analysis will never be
%  referred to again, and so should be freed if the current representation
%  goes in for that kind of thing.

anz_free(_).  % no-op for robdds

%  anz_free_if_unshared(+Analysis1, +Analysis2)
%  Free Analysis if it does not share with Analysis2, if we do freeing.

anz_free_if_unshared(_, _).  % no-op for robdds


%  anz_copy(+Analysis0, -Analysis)
%  Analysis is identical to Analysis0, but using destructive operations on
%  Analysis0 will not affect Analysis.  For representations which to not
%  use destructive update, this can be the same as Analysis = Analysis0.

anz_copy(X, X).  % no need to copy robdds


%  anz_equiv(+Analysis1, +Analysis2)
%  Analysis1 is identical to Analysis2.  For strongly canonical
%  representations, this is the same as Analysis1 = Analysis2.

anz_equiv(F, F).  % semantic equality entails pointer equality for robdds


%  anz_top(-Top)
%  Top is the topmost (weakest) element in our analysis domain (lattice).

%  ** implemented in C **


%  anz_bottom(-Bottom)
%  Bottom is the bottommost (strongest) element in out analysis domain
%  (lattice).

%  ** implemented in C **


%  anz_meet(+Analysis1, +Analysis2, -Analysis)
%  anz_meet(+Projection, +Analysis1, +Analysis2, -Analysis)
%  Analysis is the meet (greatest lower bound) of Analysis1 and Analysis2, two
%  predicate analyses, projected to the variables numbered strictly lower
%  than Projection if Projection is supplied.

%  ** implemented in C **

%  anz_meet_vars(+Vars, -Analysis)
%  Analysis is the greatest lower bound of the representations of all the
%  variables on Vars, a list of variable numbers.

%  ** implemented in C **


%  anz_join(+Analysis1, +Analysis2, -Analysis)
%  anz_join(+Analysis1, +Analysis2, +Projection, -Analysis)
%  Analysis is the join (least upper bound) of Analysis1 and Analysis2, two
%  predicate analyses, projected to the variables numbered strictly lower
%  than Projection if Projection is supplied.

%  ** implemented in C **

anz_join(Projection, Analysis1, Analysis2, Analysis) :-
	anz_join(Analysis1, Analysis2, Analysis3),
	project_threshold(Projection, Analysis3, Analysis).


%  anz_project(+Analysis0, +Projection, -Analysis)
%  Analysis is Analysis0 projected to variables numbered strictly lower than
%  Projection.

anz_project(Analysis0, Projection, Analysis) :-
	project_threshold(Projection, Analysis0, Analysis).
	

%  anz_print(+Analysis)
%  Print out Analysis in some suitable format to stdout.

%  ** implemented in C **


%  anz_print_stderr(+Analysis)
%  Print out Analysis in some suitable format to stderr.

%  ** implemented in C **


%  analyze_unif(Var, Term, Vars, Projection, Analysis0, Analysis)
%  Our abstract unification operation.  Analysis is the meet of Analysis0 and
%  the analysis of the unification of variable Var with term Term (which
%  contains all and only the variables on list Vars), projected to the
%  variables numbered strictly lower than Projection.

analyze_unif(Var, _Term, Vars, Projection, Analysis0, Analysis) :-
	abstract_unify_list(Analysis0, Var, Vars, Projection, Analysis).

% 	anz_iffconj(Var, Vars, Analysis1),
% 	bool_meet_projected(Analysis0, Analysis1, Projection, Analysis).

% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_project(Analysis2, Projection, Analysis).


%  analyze_eval(Var, Term, Vars, Projection, Analysis0, Analysis)
%  Analysis is meet of Analysis0 and the analysis of the binding of Var to the
%  evaluation of expression Term, which contains all the vars on the list
%  Vars, all projected to the variables numbered strictly lower than
%  Projection.

analyze_eval(Var, _, Vars, Projection, Analysis0, Analysis) :-
	anz_meet_vars([Var|Vars], Analysis1),
	anz_meet(Projection, Analysis0, Analysis1, Analysis),
	anz_free(Analysis1).
% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_project(Analysis2, Projection, Analysis).


%  analyze_builtin(Call, Projection, Analysis0, Analysis)
%  Analysis is the meet of Analysis0 and the analysis of Call, a call to a
%  builtin predicate, projected to the variables numbered strictly lower than
%  Projection.  Preds is a predicate store containing the analysis of Pred.
%  Essentially this just looks up the analysis of Pred in Preds and renames
%  variables in it to match those in Call, and then does a projected meet.

analyze_builtin(Call, Projection, Analysis0, Analysis) :-
	builtin_analysis(Call, Analysis1),
	anz_meet(Projection, Analysis0, Analysis1, Analysis),
	anz_free(Analysis1).
% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_project(Analysis2, Projection, Analysis).


%  analyze_call(+Analysis0, +Success, +Call, +Projection, -Analysis)
%  Analysis is the meet of Analysis0 and the analysis of Call, a call to
%  predicate Pred, projected to the variables numbered strictly lower than
%  Projection.  Success is the analysis of Pred.  Essentially this just
%  renames variables in Success to match those in Call, and then does a
%  projected meet. 

%  ** implemented in C **


%  analyze_call_pattern(+Call, +Analysis, -Callpat)
%  Callpat is the call pattern for call Call if Analysis is the analysis of
%  the variables at the program point just before the call.

analyze_call_pattern(Call, Analysis, Callpat) :-
	reverse_rename_term(Analysis, Call, Callpat).


milli_time(Time) :-
        Time is integer(cputime * 1000).


%  analyze_foreign(+Foreignspec, -Analysis)
%  Analysis is the success pattern for the foreign predicate with foreign
%  specification Foreignspec.

analyze_foreign(Foreignspec, Analysis) :-
	functor(Foreignspec, _, Arity),
	(   setof(Arg, (between(1, Arity, Arg),
			arg(Arg, Foreignspec, Argspec),
			definite_argspec(Argspec)),
		  Args) ->
		anz_meet_vars(Args, Analysis)
	;   anz_top(Analysis)
	).
	

definite_argspec(+Spec) :-
	definite_argspec1(Spec).
definite_argspec(-Spec) :-
	definite_argspec1(Spec).
definite_argspec([-Spec]) :-
	definite_argspec1(Spec).

definite_argspec1(integer).
definite_argspec1(float).
definite_argspec1(single).
definite_argspec1(double).
definite_argspec1(atom).
definite_argspec1(string).
definite_argspec1(address).
definite_argspec1(address(_)).
%  the only argspec that isn't definite is 'term'.


%  builtin_analysis(+Goal, -Analysis)
%  Analysis is the boolean function describing the groundness of Goal's args on
%  successful exit from a call to the builtin predicate described by Goal.
%  The args of Goal are integers in increasing order.  Note that if new
%  predicaes are added here, they should also be added to builtins:builtin/2.

builtin_analysis(!, T) :- anz_top(T).		% No args
builtin_analysis(true, T) :- anz_top(T).	% No args
builtin_analysis(trace, T) :- anz_top(T).	% No args
builtin_analysis(debug, T) :- anz_top(T).	% No args
builtin_analysis(notrace, T) :- anz_top(T).	% No args
builtin_analysis(nodebug, T) :- anz_top(T).	% No args
builtin_analysis(debuggging, T) :- anz_top(T).	% No args
builtin_analysis(spy(A), X) :-			% A
	variable_rep(A,X).
builtin_analysis(nospy(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(nospyall, T) :- anz_top(T).	% No args
builtin_analysis(otherwise, T) :- anz_top(T).	% No args
builtin_analysis(fail, F) :- anz_bottom(F).	% Can't succeed!
builtin_analysis(false, F) :- anz_bottom(F).	% Can't succeed!
builtin_analysis(abort, F) :- anz_bottom(F).	% Can't succeed!
builtin_analysis(raise_exception(_), F) :-	% Can't succeed!
	anz_bottom(F).
builtin_analysis(halt, F) :- anz_bottom(F).	% Can't succeed!
builtin_analysis(halt(_), F) :- anz_bottom(F).	% Can't succeed!
builtin_analysis(call(_), T) :- anz_top(T).	% No info
builtin_analysis(bagof(_,_,_), T) :-		% No info
	anz_top(T).
builtin_analysis(setof(_,_,_), T) :-		% No info
	anz_top(T).
builtin_analysis(findall(_,_,_), T) :-		% No info
	anz_top(T).
builtin_analysis(unix(A), X):-			% A
	variable_rep(A, X).
builtin_analysis(repeat, T) :- anz_top(T).	% No args
builtin_analysis(compare(A,_,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis((_ @<_ ), T) :- anz_top(T).	% No info
builtin_analysis((_ @=<_ ), T) :- anz_top(T).	% No info
builtin_analysis((_ ==_ ), T) :- anz_top(T).	% No info
builtin_analysis((_ @>=_ ), T) :- anz_top(T).	% No info
builtin_analysis((_ @>_ ), T) :- anz_top(T).	% No info
builtin_analysis((_ \==_ ), T) :- anz_top(T).	% No info
builtin_analysis(\=(_,_), T) :- anz_top(T).	% No info
builtin_analysis(~=(_,_), T) :- anz_top(T).	% No info
builtin_analysis((A < B), X) :-			% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A =< B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A =:= B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A >= B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A > B), X) :-			% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A =\= B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A is B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis((A =.. B), X) :-		% A <-> B
	anz_iffconj(A, [B], X).
builtin_analysis(atom(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(atomic(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(callable(_), T) :-		% No info
	anz_top(T).
builtin_analysis(compound(_), T) :-		% No info
	anz_top(T).
builtin_analysis(db_reference(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(float(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ground(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(integer(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(nonvar(_), T) :- anz_top(T).	% No info
builtin_analysis(number(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(simple(_), T) :- anz_top(T).	% No info
builtin_analysis(var(_), T) :- anz_top(T).	% No info (can't say ~A)
builtin_analysis(functor(_,B,C), X) :-		% B & C
	anz_meet_vars([B,C], X).
builtin_analysis(arg(A,B,C), X) :-		% A & (B -> C)
	variable_rep(B, Vb),
	variable_rep(C, Vc),
	implies(Vb, Vc, Imp),
	anz_free(Vc),
	variable_rep(A, Va),
	anz_meet(Imp, Va, X),
	anz_free(Va).
builtin_analysis(name(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(atom_chars(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(number_chars(A,B), X) :-	% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(numbervars(A,B,C), X) :-	% A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(hash_term(A,B), X) :-		% A & B (error when var(A))
	anz_meet_vars([A,B], X).
builtin_analysis(subsumes_chk(A,B), X) :-	% A -> B
	variable_rep(A, Va),
	variable_rep(B, Vb),
	implies(Va, Vb, X),
	anz_free(Vb).
builtin_analysis(copy_term(_,_), T) :-		% No info (can't say A<->B)
	anz_top(T).
% This is commented out because it's in so many test suites:
% builtin_analysis(append(A,B,C), X) :-		% (A & B) <-> C
%	anz_iffconj(C, [A,B], X).
builtin_analysis(length(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(sort(A,B), X) :-		% A <-> B
	anz_iffconj(A, [B], X).
builtin_analysis(keysort(A,B), X) :-		% A <-> B
	anz_iffconj(A, [B], X).
builtin_analysis('C'(A,B,C), X) :-		% A <-> (B & C)
	anz_iffconj(A, [B,C], X).
builtin_analysis(statistics, T) :- anz_top(T).	% No args
builtin_analysis(statistics(A,B), X) :-
	anz_meet_vars([A,B], X).
builtin_analysis(see(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(seeing(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(seen, T) :- anz_top(T).	% No args
builtin_analysis(tell(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(telling(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(told, T) :- anz_top(T).	% No args
builtin_analysis(open(A,B,C), X) :-		% A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(open(A,B,C,D), X) :-		% A & B & C & D
	anz_meet_vars([A,B,C,D], X).
builtin_analysis(close(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(current_input(A), X) :-	% A
	variable_rep(A, X).
builtin_analysis(current_output(A), X) :-	% A
	variable_rep(A, X).
builtin_analysis(set_input(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(set_output(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(current_stream(A,B,C), X) :-	% A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(stream_code(A,B), X) :-	% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(absolute_file_name(A,B), X) :-	% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(absolute_file_name(A,B,C), X) :- % A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(current_op(A,B,C), X) :-	% A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(op(A,B,C), X) :-		% A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(prompt(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(prompt(A,B,C), X) :-		% A & B & C
	anz_meet_vars([A,B,C], X).
builtin_analysis(read(_), T) :- anz_top(T).	% No info
builtin_analysis(read(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(write(_), T) :- anz_top(T).	% No info
builtin_analysis(write(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(write_canonical(_), T) :-	% No info
	anz_top(T).
builtin_analysis(write_canonical(A,_), X) :-	% A
	variable_rep(A, X).
builtin_analysis(print(_), T) :- anz_top(T).	% No info
builtin_analysis(print(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(writeq(_), T) :- anz_top(T).	% No info
builtin_analysis(writeq(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(display(_), T) :-		% No info
	 anz_top(T).
builtin_analysis(display(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(format(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(format(A,B,_), X) :-		% A
	anz_meet_vars([A,B], X).
builtin_analysis(get(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(get(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(get0(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(get0(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(nl, T) :- anz_top(T).		% No args
builtin_analysis(nl(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(peek_char(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(peek_char(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(put(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(put(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(skip(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(skip(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(skip_line, T) :-		% No args
	anz_top(T).
builtin_analysis(skip_line(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(tab(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(tab(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(ttyget(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyget0(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttynl, T) :- anz_top(T).	% No args
builtin_analysis(ttyput(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyskip(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttytab(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyflush, T) :- anz_top(T).	% No args
builtin_analysis(abolish(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(abolish(A,B), X) :-		% A & B
	anz_meet_vars([A,B], X).
builtin_analysis(assert(_), T) :- anz_top(T).	% No info
builtin_analysis(assert(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(asserta(_), T) :- anz_top(T).	% No info
builtin_analysis(asserta(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(assertz(_), T) :- anz_top(T).	% No info
builtin_analysis(assertz(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(clause(_,_), T) :-		% No info
	anz_top(T).
builtin_analysis(clause(_,_,C), X) :-		% C
	variable_rep(C, X).
builtin_analysis(current_key(A,_), X) :-	% A
	variable_rep(A, X).
builtin_analysis(erase(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(instance(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(recorda(A,_,C), X) :-		% A & C
	anz_meet_vars([A,C], X).
builtin_analysis(recorded(A,_,C), X) :-		% A & C
	anz_meet_vars([A,C], X).
builtin_analysis(recordz(A,_,C), X) :-		% A & C
	anz_meet_vars([A,C], X).
builtin_analysis(retract(_), T) :- anz_top(T).	% No info
builtin_analysis(retractall(_), T) :-		% No info
	anz_top(T).
builtin_analysis(expand_term(_,_), T) :-	% No info
	anz_top(T).

%  metapredicates:   these should be handled more carefully, so that if we can
%  determine at compile time the predicate that will be called, we use that
%  predicate's analysis.  But this will have to be done before we determine
%  the SCCs, otherwise we won't be guaranteed the bottom-up order.
builtin_analysis(phrase(_,_), T) :-		% No info
	anz_top(T).
builtin_analysis(phrase(_,_,_), T) :-		% No info
	anz_top(T).
% Nonstandard (or non-Quintus) builtins added for specific tests
builtin_analysis(inc(A,B), X) :-
	anz_meet_vars([A,B], X).			% A & B
builtin_analysis(dec(A,B), X) :-
	anz_meet_vars([A,B], X).			% A & B
builtin_analysis(real(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(not(_), T) :-			% No info
	anz_top(T).


/*****************************************************************

			      Foreign Interface

Here's the actual foreign interface.  This code refers to 'rep.so', which is a
shared object file.  Using a shared object file allows us to run the analyzer
with different implementations of the boolean functions by just making
symbolic links to different '.so' files.

The analyzer expects the following functions to be supplied by the
representation.  Here `void *' mean a representation of a boolean function,
whatever form that takes.

    void initRep(void);
    void concludeRep(void);
    void free_rep(void *a);
    void free_rep_if_diff(void *a, void *b);
    void *copy(void *a);
    int  equiv(void *a, void *b);
    void *trueVar(void);
    void *falseVar(void);
    void *variableRep(int var);
    void *glb(void *a, void *b);
    void *lub(void *a, void *b);
    void *implies(void *a, void *b);
    void *projectThresh(int c,void *a);
    void *projected_glb(int c, void *f, void *g);
    void printOut(void *f)

These are actually needed by boolfn.c, which handles converting Prolog
term representations into more C-appropriate representations:

    void *renameArray(void *in, int count, int mapping[]);
    void *reverseRenameArray(void *in, int count, int rev_mapping[]);
    void *iff_conj_array(int v0, int n, int arr[]);
    void *abstract_unify(void *f, int v0, int n, int arr[], int thresh);
    void *abstract_exit(void *context, void *f, int n, int mapping[],
                               int thresh);
    void *glb_array(int n, int arr[]);

*****************************************************************/

% Load in C ROBDD support code that defines all these predicates.
:- use_foreign_library(swi_robdd).
