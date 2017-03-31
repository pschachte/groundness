%  file    : analysis.pl
%  Authors : Peter Schachte
%  Purpose : Storage and manipulation of predicate analyses
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
	anal_free/1,
	anal_free_if_unshared/2,
	anal_copy/2,
	anal_equiv/2,
	anal_top/1,
	anal_bottom/1,
	anal_meet/3,
	anal_meet/4,
	anal_join/3,
	anal_join/4,
	anal_meet_vars/2,
	anal_restrict/3,
	anal_iffconj/3,
	anal_print/1,
	analyze_unif/6,
	analyze_eval/6,
	analyze_builtin/4,
	analyze_call/5,
	analyze_call_pattern/3,
	analyze_foreign/2,
	builtin_analysis/2
   ]).


%  :- use_module(predstore).
:- use_module(library(between)).

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


%  anal_free(+Analysis)
%  Declares that the Boolean function representation Analysis will never be
%  referred to again, and so should be freed if the current representation
%  goes in for that kind of thing.

%  ** implemented in C **


%  anal_copy(+Analysis0, -Analysis)
%  Analysis is identical to Analysis0, but using destructive operations on
%  Analysis0 will not affect Analysis.  For representations which to not
%  use destructive update, this can be the same as Analysis = Analysis0.

%  ** implemented in C **


%  anal_equiv(+Analysis1, +Analysis2)
%  Analysis1 is identical to Analysis2.  For strongly canonical
%  representations, this is the same as Analysis1 = Analysis2.

anal_equiv(F, G) :-
	equiv_test(F, G, 1).


%  anal_top(-Top)
%  Top is the topmost (weakest) element in our analysis domain (lattice).

%  ** implemented in C **


%  anal_bottom(-Bottom)
%  Bottom is the bottommost (strongest) element in out analysis domain
%  (lattice).

%  ** implemented in C **


%  anal_meet(+Analysis1, +Analysis2, -Analysis)
%  anal_meet(+Restriction, +Analysis1, +Analysis2, -Analysis)
%  Analysis is the meet (greatest lower bound) of Analysis1 and Analysis2, two
%  predicate analyses, restricted to the variables numbered strictly lower
%  than Restriction if Restriction is supplied.

%  ** implemented in C **

%  anal_meet_vars(+Vars, -Analysis)
%  Analysis is the greatest lower bound of the representations of all the
%  variables on Vars, a list of variable numbers.

%  ** implemented in C **


%  anal_join(+Analysis1, +Analysis2, -Analysis)
%  anal_join(+Analysis1, +Analysis2, +Restriction, -Analysis)
%  Analysis is the join (least upper bound) of Analysis1 and Analysis2, two
%  predicate analyses, restricted to the variables numbered strictly lower
%  than Restriction if Restriction is supplied.

%  ** implemented in C **

anal_join(Analysis1, Analysis2, Restriction, Analysis) :-
	anal_join(Analysis1, Analysis2, Analysis3),
	restrict_threshold(Restriction, Analysis3, Analysis).


%  anal_restrict(+Analysis0, +Restriction, -Analysis)
%  Analysis is Analysis0 restricted to variables numbered strictly lower than
%  Restriction.

anal_restrict(Analysis0, Restriction, Analysis) :-
	restrict_threshold(Restriction, Analysis0, Analysis).
	

%  anal_print(+Analysis)
%  Print out Analysis in some suitable format.

%  ** implemented in C **


%  analyze_unif(Var, Term, Vars, Restriction, Analysis0, Analysis)
%  Our abstract unification operation.  Analysis is the meet of Analysis0 and
%  the analysis of the unification of variable Var with term Term (which
%  contains all and only the variables on list Vars), restricted to the
%  variables numbered strictly lower than Restriction.

analyze_unif(Var, _Term, Vars, Restriction, Analysis0, Analysis) :-
	bool_abstract_unify(Analysis0, Var, Vars, Restriction, Analysis).

% 	anal_iffconj(Var, Vars, Analysis1),
% 	bool_meet_restricted(Analysis0, Analysis1, Restriction, Analysis).

% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_restrict(Analysis2, Restriction, Analysis).


%  analyze_eval(Var, Term, Vars, Restriction, Analysis0, Analysis)
%  Analysis is meet of Analysis0 and the analysis of the binding of Var to the
%  evaluation of expression Term, which contains all the vars on the list
%  Vars, all restricted to the variables numbered strictly lower than
%  Restriction.

analyze_eval(Var, _, Vars, Restriction, Analysis0, Analysis) :-
	anal_meet_vars([Var|Vars], Analysis1),
	anal_meet(Restriction, Analysis0, Analysis1, Analysis),
	anal_free(Analysis1).
% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_restrict(Analysis2, Restriction, Analysis).


%  analyze_builtin(Call, Restriction, Analysis0, Analysis)
%  Analysis is the meet of Analysis0 and the analysis of Call, a call to a
%  builtin predicate, restricted to the variables numbered strictly lower than
%  Restriction.  Preds is a predicate store containing the analysis of Pred.
%  Essentially this just looks up the analysis of Pred in Preds and renames
%  variables in it to match those in Call, and then does a restricted meet.

analyze_builtin(Call, Restriction, Analysis0, Analysis) :-
	builtin_analysis(Call, Analysis1),
	anal_meet(Restriction, Analysis0, Analysis1, Analysis),
	anal_free(Analysis1).
% 	bool_meet(Analysis0, Analysis1, Analysis2),
% 	bool_restrict(Analysis2, Restriction, Analysis).


%  analyze_call(+Analysis0, +Success, +Call, +Restriction, -Analysis)
%  Analysis is the meet of Analysis0 and the analysis of Call, a call to
%  predicate Pred, restricted to the variables numbered strictly lower than
%  Restriction.  Success is the analysis of Pred.  Essentially this just
%  renames variables in Success to match those in Call, and then does a
%  restricted meet. 

%  ** implemented in C **


%  analyze_call_pattern(+Call, +Analysis, -Callpat)
%  Callpat is the call pattern for call Call if Analysis is the analysis of
%  the variables at the program point just before the call.

analyze_call_pattern(Call, Analysis, Callpat) :-
	reverse_rename_term(Analysis, Call, Callpat).


%  analyze_foreign(+Foreignspec, -Analysis)
%  Analysis is the success pattern for the foreign predicate with foreign
%  specification Foreignspec.

analyze_foreign(Foreignspec, Analysis) :-
	functor(Foreignspec, _, Arity),
	(   setof(Arg, (between(1, Arity, Arg),
			arg(Arg, Foreignspec, Argspec),
			definite_argspec(Argspec)),
		  Args) ->
		anal_meet_vars(Args, Analysis)
	;   anal_top(Analysis)
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

builtin_analysis(!, T) :- anal_top(T).		% No args
builtin_analysis(true, T) :- anal_top(T).	% No args
builtin_analysis(trace, T) :- anal_top(T).	% No args
builtin_analysis(debug, T) :- anal_top(T).	% No args
builtin_analysis(notrace, T) :- anal_top(T).	% No args
builtin_analysis(nodebug, T) :- anal_top(T).	% No args
builtin_analysis(debuggging, T) :- anal_top(T).	% No args
builtin_analysis(spy(A), X) :-			% A
	variable_rep(A,X).
builtin_analysis(nospy(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(nospyall, T) :- anal_top(T).	% No args
builtin_analysis(otherwise, T) :- anal_top(T).	% No args
builtin_analysis(fail, F) :- anal_bottom(F).	% Can't succeed!
builtin_analysis(false, F) :- anal_bottom(F).	% Can't succeed!
builtin_analysis(abort, F) :- anal_bottom(F).	% Can't succeed!
builtin_analysis(raise_exception(_), F) :-	% Can't succeed!
	anal_bottom(F).
builtin_analysis(halt, F) :- anal_bottom(F).	% Can't succeed!
builtin_analysis(halt(_), F) :- anal_bottom(F).	% Can't succeed!
builtin_analysis(call(_), T) :- anal_top(T).	% No info
builtin_analysis(bagof(_,_,_), T) :-		% No info
	anal_top(T).
builtin_analysis(setof(_,_,_), T) :-		% No info
	anal_top(T).
builtin_analysis(findall(_,_,_), T) :-		% No info
	anal_top(T).
builtin_analysis(unix(A), X):-			% A
	variable_rep(A, X).
builtin_analysis(repeat, T) :- anal_top(T).	% No args
builtin_analysis(compare(A,_,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis((_ @<_ ), T) :- anal_top(T).	% No info
builtin_analysis((_ @=<_ ), T) :- anal_top(T).	% No info
builtin_analysis((_ ==_ ), T) :- anal_top(T).	% No info
builtin_analysis((_ @>=_ ), T) :- anal_top(T).	% No info
builtin_analysis((_ @>_ ), T) :- anal_top(T).	% No info
builtin_analysis((_ \==_ ), T) :- anal_top(T).	% No info
builtin_analysis(\=(_,_), T) :- anal_top(T).	% No info
builtin_analysis(~=(_,_), T) :- anal_top(T).	% No info
builtin_analysis((A < B), X) :-			% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A =< B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A =:= B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A >= B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A > B), X) :-			% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A =\= B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A is B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis((A =.. B), X) :-		% A <-> B
	anal_iffconj(A, [B], X).
builtin_analysis(atom(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(atomic(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(callable(_), T) :-		% No info
	anal_top(T).
builtin_analysis(compound(_), T) :-		% No info
	anal_top(T).
builtin_analysis(db_reference(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(float(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ground(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(integer(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(nonvar(_), T) :- anal_top(T).	% No info
builtin_analysis(number(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(simple(_), T) :- anal_top(T).	% No info
builtin_analysis(var(_), T) :- anal_top(T).	% No info (can't say ~A)
builtin_analysis(functor(_,B,C), X) :-		% B & C
	anal_meet_vars([B,C], X).
builtin_analysis(arg(A,B,C), X) :-		% A & (B -> C)
	variable_rep(B, Vb),
	variable_rep(C, Vc),
	implies(Vb, Vc, Imp),
	anal_free(Vc),
	variable_rep(A, Va),
	anal_meet(Imp, Va, X),
	anal_free(Va).
builtin_analysis(name(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(atom_chars(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(number_chars(A,B), X) :-	% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(numbervars(A,B,C), X) :-	% A & B & C
	anal_meet_vars([A,B,C], X).
builtin_analysis(hash_term(A,B), X) :-		% A & B (error when var(A))
	anal_meet_vars([A,B], X).
builtin_analysis(subsumes_chk(A,B), X) :-	% A -> B
	variable_rep(A, Va),
	variable_rep(B, Vb),
	implies(Va, Vb, X),
	anal_free(Vb).
builtin_analysis(copy_term(_,_), T) :-		% No info (can't say A<->B)
	anal_top(T).
% This is commented out because it's in so many test suites:
% builtin_analysis(append(A,B,C), X) :-		% (A & B) <-> C
%	anal_iffconj(C, [A,B], X).
builtin_analysis(length(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(sort(A,B), X) :-		% A <-> B
	anal_iffconj(A, [B], X).
builtin_analysis(keysort(A,B), X) :-		% A <-> B
	anal_iffconj(A, [B], X).
builtin_analysis('C'(A,B,C), X) :-		% A <-> (B & C)
	anal_iffconj(A, [B,C], X).
builtin_analysis(statistics, T) :- anal_top(T).	% No args
builtin_analysis(statistics(A,B), X) :-
	anal_meet_vars([A,B], X).
builtin_analysis(see(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(seeing(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(seen, T) :- anal_top(T).	% No args
builtin_analysis(tell(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(telling(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(told, T) :- anal_top(T).	% No args
builtin_analysis(open(A,B,C), X) :-		% A & B & C
	anal_meet_vars([A,B,C], X).
builtin_analysis(open(A,B,C,D), X) :-		% A & B & C & D
	anal_meet_vars([A,B,C,D], X).
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
	anal_meet_vars([A,B,C], X).
builtin_analysis(stream_code(A,B), X) :-	% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(absolute_file_name(A,B), X) :-	% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(absolute_file_name(A,B,C), X) :- % A & B & C
	anal_meet_vars([A,B,C], X).
builtin_analysis(current_op(A,B,C), X) :-	% A & B & C
	anal_meet_vars([A,B,C], X).
builtin_analysis(op(A,B,C), X) :-		% A & B & C
	anal_meet_vars([A,B,C], X).
builtin_analysis(prompt(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(prompt(A,B,C), X) :-		% A & B & C
	anal_meet_vars([A,B,C], X).
builtin_analysis(read(_), T) :- anal_top(T).	% No info
builtin_analysis(read(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(write(_), T) :- anal_top(T).	% No info
builtin_analysis(write(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(write_canonical(_), T) :-	% No info
	anal_top(T).
builtin_analysis(write_canonical(A,_), X) :-	% A
	variable_rep(A, X).
builtin_analysis(print(_), T) :- anal_top(T).	% No info
builtin_analysis(print(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(writeq(_), T) :- anal_top(T).	% No info
builtin_analysis(writeq(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(display(_), T) :-		% No info
	 anal_top(T).
builtin_analysis(display(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(format(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(format(A,B,_), X) :-		% A
	anal_meet_vars([A,B], X).
builtin_analysis(get(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(get(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(get0(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(get0(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(nl, T) :- anal_top(T).		% No args
builtin_analysis(nl(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(peek_char(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(peek_char(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(put(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(put(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(skip(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(skip(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(skip_line, T) :-		% No args
	anal_top(T).
builtin_analysis(skip_line(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(tab(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(tab(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(ttyget(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyget0(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttynl, T) :- anal_top(T).	% No args
builtin_analysis(ttyput(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyskip(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttytab(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(ttyflush, T) :- anal_top(T).	% No args
builtin_analysis(abolish(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(abolish(A,B), X) :-		% A & B
	anal_meet_vars([A,B], X).
builtin_analysis(assert(_), T) :- anal_top(T).	% No info
builtin_analysis(assert(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(asserta(_), T) :- anal_top(T).	% No info
builtin_analysis(asserta(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(assertz(_), T) :- anal_top(T).	% No info
builtin_analysis(assertz(_,B), X) :-		% B
	variable_rep(B, X).
builtin_analysis(clause(_,_), T) :-		% No info
	anal_top(T).
builtin_analysis(clause(_,_,C), X) :-		% C
	variable_rep(C, X).
builtin_analysis(current_key(A,_), X) :-	% A
	variable_rep(A, X).
builtin_analysis(erase(A), X) :-		% A
	variable_rep(A, X).
builtin_analysis(instance(A,_), X) :-		% A
	variable_rep(A, X).
builtin_analysis(recorda(A,_,C), X) :-		% A & C
	anal_meet_vars([A,C], X).
builtin_analysis(recorded(A,_,C), X) :-		% A & C
	anal_meet_vars([A,C], X).
builtin_analysis(recordz(A,_,C), X) :-		% A & C
	anal_meet_vars([A,C], X).
builtin_analysis(retract(_), T) :- anal_top(T).	% No info
builtin_analysis(retractall(_), T) :-		% No info
	anal_top(T).
builtin_analysis(expand_term(_,_), T) :-	% No info
	anal_top(T).

%  metapredicates:   these should be handled more carefully, so that if we can
%  determine at compile time the predicate that will be called, we use that
%  predicate's analysis.  But this will have to be done before we determine
%  the SCCs, otherwise we won't be guaranteed the bottom-up order.
builtin_analysis(phrase(_,_), T) :-		% No info
	anal_top(T).
builtin_analysis(phrase(_,_,_), T) :-		% No info
	anal_top(T).
% Nonstandard (or non-Quintus) builtins added for specific tests
builtin_analysis(inc(A,B), X) :-
	anal_meet_vars([A,B], X).			% A & B
builtin_analysis(dec(A,B), X) :-
	anal_meet_vars([A,B], X).			% A & B
builtin_analysis(real(A), X) :-			% A
	variable_rep(A, X).
builtin_analysis(not(_), T) :-			% No info
	anal_top(T).


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
    void *restrictThresh(int c,void *a);
    void *restricted_glb(int c, void *f, void *g);
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

foreign(max_variable, c, max_variable([-integer])).
foreign(initRep, c, init_rep).
foreign(concludeRep, c, conclude_rep).
foreign(free_rep, c, anal_free(+address)).
foreign(copy, c, anal_copy(+address, [-address])).
foreign(equiv, c, equiv_test(+address, +address, [-integer])).
foreign(trueVar, c, anal_top([-address])).
foreign(falseVar, c, anal_bottom([-address])).
foreign(variableRep, c, variable_rep(+integer, [-address])).
foreign(glb, c, anal_meet(+address, +address, [-address])).
foreign(lub, c, anal_join(+address, +address, [-address])).
foreign(implies, c, implies(+address, +address, [-address])).
foreign(restrictThresh, c, restrict_threshold(+integer, +address, [-address])).
foreign(restricted_glb, c, anal_meet(+integer, +address, +address,
		[-address])).
foreign(printOut, c, anal_print(+address)).

foreign(glb_list, c, anal_meet_vars(+term, [-address])).
foreign(rename_term, c, rename_term(+address, +term, [-address])).
foreign(reverse_rename_term, c,
		reverse_rename_term(+address, +term, [-address])).
foreign(iff_conj_list, c, anal_iffconj(+integer, +term, [-address])).
foreign(abstract_unify_list, c, bool_abstract_unify(+address, +integer, +term,
		+integer, [-address])).
foreign(abstract_exit_term, c, analyze_call(+address, +address, +term,
		+integer, [-address])).
foreign(free_rep_if_diff, c, anal_free_if_unshared(+address, +address)).

foreign_file('boolfn.so', [max_variable, initRep, concludeRep, free_rep,
                copy, equiv, trueVar, falseVar, variableRep, glb,
                lub, implies, restrictThresh, restricted_glb, printOut,
                glb_list, rename_term, reverse_rename_term, iff_conj_list,
		abstract_unify_list, abstract_exit_term, free_rep_if_diff]).

:- load_foreign_executable('boolfn.so').
