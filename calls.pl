%  File   : calls
%  Authors: Peter Schachte
%  Purpose: Load a Prolog source file to be analyzed
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
%  This program reads a Prolog source file returning a list of (the Clark
%  completions of) the predicates in the file, and all the files it loads.
%  This program also pays some attention to modules, although at the moment it
%  is not complete.

:- module(calls, [
	file_contents/4,
	simplify_goal/7,
	check_var_limit/2
   ]).

:- ensure_loaded(builtins).
:- ensure_loaded(analysis).	% for max_variable/1
:- ensure_loaded(misc).		% for max/3
:- ensure_loaded(map).		% for map_empty/1, map_fetch/3, map_store/3

/************************************************************************

			       Loading a File

This code is responsible for reading a file and all the files it
loads, returning a collection of all the predicates in all these
files, with the code in a form suitable for analysis.  This code is
also responsible for handling other file-related issues in the code,
including modules and multi-file and discontiguous predicates.

The collection of predicates returned by this code is represented as a
mapping from *Module:Name/Arity* to a predicate descriptor.  A predicate
descriptor is one of
++enumerate
    'import'(*Number,Predspec*), where *Predspec* names another predicate,
		whose predicate number is *Number*, which this predicate is an
		alias for; or
    'pred'(*Number, Num2, Props, Code*), where the arguments are 
++description
   *Number*	is the predicate's number
   *Num2*	is a temporary number used in computing strongly-connected
		components
   *Props*	is a bitset specifying the predicate's properties
   *Code*	is a goal specification containing all the code for
		the predicate.
--description
--enumerate
*Code* represents the Clark completion of that predicate (without
inequality axioms).  All predicate arguments are variables
(non-variable arguments are replaced by variables, with explicit
unifications added to the body).  All variables are numbered, with the
head arguments in particular numbered from 1 through *Arity*; this
means that the "head" of the Clark completion can be ignored
altogether.

*Code* is a goal specification, which is one of:
++description
    'conj'(*Gs*)
	specifying a conjunction of goals, where *Gs* is a list of goal specs.
    'equal'(*V,T,Vs,A,R*)
	specifying an explicit or implicit unification, where *V* is a
	variable number, *T* is a term description, *Vs* is a list of the
	variable numbers appearing in *T*, *A* is an analysis term containing
	analysis information for the point of the call (before the call), and
	*R* is the largest variable number appearing later in the clause;
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
operations.

A term description is one of:
++enumerate
    1.  'var'(*N*), where *N* is a variable number;
    2.  'atom'(*C*), where *C* is an atom;
    3.  'integer'(*C*), where *C* is an integer;
    4.  'float'(*C*), where *C* is a float;
    5.  'compound'(*F, A, Args*), where *F* is ther term's principal functor,
	*A* is its arity, and *Args* is a list of term descriptions.
--enumerate

This is a ground representation, with variables represented as
numbers.  Note that the *R* argument in each of these terms specifies
a threshold; variables numbered at or above that threshold do not
appear later in the predicate body, nor in the head.  In order for
this to be possible, we must number first the predicate arguments, and
then the body arguments, beginning with the *last* goal and working
toward the beginning.  Variable numbers appearing in a disjunction but not
outside it may be shared among the disjuncts, though they do not
represent the same variable.

TODO: Note that at present, we don't use the 'eval'/4 goal specification
properly.  The idea here is to simplify arithmetic operations by removing
unification from 'is'/2 (generating an explicit unification after the
evaluation when argument 1 of 'is' is not a first occurrance of a variable)
and by removing computation from the arithmetic comparisons (by generating one
or two explicit computations first when either argument of an arithmetic
comparison is not a variable).

************************************************************************/

/************************************************************************

		   Thoughts on Incremental Analysis

Since predicate numbers are included in the representation of calls to
user defined predicates, we don't want to have to renumber predicates
due to the addition, removal, or modification of predicates.  This
means that we can't use a predicate's topological sort order as a
predicate number.  Therefore we might as well just assign a predicate
number when we see the first clause for or call to the predicate.

************************************************************************/


:- ensure_loaded(predstore).


%  file_contents(+File, -Preds, -Maxvars, -Totvars)
%  file_contents(+File, +Relfile, -Absfiles, +Files0, -Files, +Mods0, -Mods,
%		+Preds0, -Preds, +Maxvars0, -Maxvars, +Totvars0, -Totvars)
%  Preds is a predicate store containing all the code from File and all the
%  files it depends upon, plus whatever Preds0 contained.  Files0 is a mapping
%  from file names to either 'loading', indicating that that file is currently
%  being processed, or a list of the modules defined in that file.  Files is a
%  similar mapping after File has been processed.  Mods is a mapping from
%  module name to the list of that module's exported predicates; Mods0 is the
%  same mapping before processing File.  Relfile is a directory or file name
%  relative to which relative file names will be taken.  File is the name of
%  the file or files to be loaded, possibly relative file names; Absfiles is a
%  list of the absolute file name for (the files on list) File.

file_contents(File, Preds, Maxvars, Totvars) :-
	map_empty(Mods0),
	map_empty(Files0),
	empty_predstore(Preds0),
	file_contents(File, '.', _, Files0, _, Mods0, _, Preds0, Preds1,
		0, Maxvars, 0, Totvars),
	report_undefined_preds(Preds1, Preds).

file_contents([], _, [], Files, Files, Mods, Mods, Preds, Preds, MV, MV,
		TV, TV) :-
	!.
file_contents([F|Fs], Rel, List, Files0, Files, Mods0, Mods, Preds0, Preds,
		MV0, MV, TV0, TV) :- 
	!,
	file_contents(F, Rel, L0, Files0, Files1, Mods0, Mods1, Preds0, Preds1,
		      MV0, MV1, TV0, TV1),
	append(L0, L1, List),
	file_contents(Fs, Rel, L1, Files1, Files, Mods1, Mods, Preds1, Preds,
		      MV1, MV, TV1, TV).
file_contents(F, Rel, [File], Files0, Files, Mods0, Mods, Preds0, Preds,
		MV0, MV, TV0, TV) :-
	source_file_name(F, Rel, File),
	(   map_fetch(File, Files0, FMods) ->	% already process{ed,ing} File
		Mods = Mods0,
		Preds = Preds0,
		MV = MV0,
		TV = TV0,
		seen_file_contents(FMods, File, Files0, Files)
	;   open(File, read, Stream),
            map_store(File, loading, Files0, Files1),
	    process_stream(File, Stream, user, Files1, Files2, Mods0, Mods,
                           Preds0, Preds, MV0, MV, TV0, TV, none, _, FileMods),
            close(Stream),
	    map_store(File, FileMods, Files2, Files)
	).


%  seen_file_contents(+FMods, +File, +Files0, -Files)
%  FMods is the atom 'loading' or a list of the modules defined in file File
%  (not including files loaded by File).  Other args are as above.

seen_file_contents(loading, File, _Files0, _Files) :-
	!,
	% TODO:  We don't handle circular dependencies yet.  We'll have to
	% open the file and just scan it for the modules it defines, and then
	% make File map to that list in Files.  For now we do the wrong thing.
	raise_exception('! circular file dependency !'(File)).
seen_file_contents(_, _, Files, Files).


%  process_stream(+File, +Stream, +Defmod, +Files0, -Files, +Mods0, -Mods,
%		+Preds0, -Preds, +Maxvars0, -Maxvars, +Totvars0, -Totvars,
%		+Prevpred, -Clauses, -FileMods)
%  Defmod is the current default module.  File is the file we are currently
%  reading from stream Stream.  Other arguments are as above.

process_stream(File, Stream, Defmod, Files0, Files, Mods0, Mods, Preds0, Preds,
		MV0, MV, TV0, TV, Prevpred, Clauses, Fmods) :-
	read(Stream, Clause0),
	expand_term(Clause0, Clause),
	(   var(Clause) ->
		write('Warning:  unbound clause in '),
		write(File),
		nl,
		process_stream(File, Stream, Defmod, Files0, Files, Mods0, Mods,
			     Preds0, Preds, MV0, MV, TV0, TV, Prevpred,
			     Clauses, Fmods)
	;   process_term(Clause, File, Stream, Defmod, Files0, Files, Mods0,
			 Mods, Preds0, Preds, MV0, MV, TV0, TV, Prevpred,
			 Clauses, Fmods)
	).


%  process_term(+Clause, +File, +Stream, +Defmod, +Files0, -Files, +Mods0,
%		  -Mods, +Preds0, -Preds, +Maxvars0, -Maxvars,
%		  +Totvars0, -Totvars, +Prevpred, -Clauses, -FileMods)
%  Same as process_stream, except that Clause is the clause that has just been
%  read.

process_term(end_of_file, _, _, _, Files0, Files, Mods0, Mods, Preds0, Preds,
		MV0, MV, TV0, TV, _, Clauses, Fmods) :-
	!,
	Files = Files0,
	Mods = Mods0,
	Preds = Preds0,
	MV = MV0,
	TV = TV0,
	Clauses = [],
	Fmods = [].
process_term((:- Directive), File, Stream, Defmod, Files0, Files, Mods0, Mods,
             Preds0, Preds, MV0, MV, TV0, TV, Prevpred, Clauses, Fmods) :-
	!,
	process_directive(Directive, File, Defmod, Defmod1, Files0, Files1,
			  Mods0, Mods1, Preds0, Preds1, MV0, MV1, TV0, TV1,
			  Fmods, Fmods1),
	process_stream(File, Stream, Defmod1, Files1, Files, Mods1, Mods,
                       Preds1, Preds, MV1, MV, TV1, TV, Prevpred, Clauses,
                       Fmods1).
process_term(foreign(_,_,Spec), File, Stream, Defmod, Files0, Files, Mods0, Mods,
		Preds0, Preds, MV0, MV, TV0, TV, Prevpred, Clauses, Fmods) :-
	!,
	process_foreign_decl(Spec, Defmod, Preds0, Preds1),
	process_stream(File, Stream, Defmod, Files0, Files, Mods0, Mods, Preds1,
		       Preds, MV0, MV, TV0, TV, Prevpred, Clauses, Fmods).
process_term(foreign(_,Spec), File, Stream, Defmod, Files0, Files, Mods0, Mods,
		Preds0, Preds, MV0, MV, TV0, TV, Prevpred, Clauses, Fmods) :-
	!,
	process_foreign_decl(Spec, Defmod, Preds0, Preds1),
	process_stream(File, Stream, Defmod, Files0, Files, Mods0, Mods, Preds1,
		       Preds, MV0, MV, TV0, TV, Prevpred, Clauses, Fmods).
process_term(Clause, File, Stream, Defmod, Files0, Files, Mods0, Mods, Preds0, Preds,
		MV0, MV, TV0, TV, Prevpred, PrevClauses, Fmods):-
	simplify_clause(Clause, Defmod, Preds0, Preds1, Predspec, V, Simple),
	check_var_limit(V, Predspec),
	max(MV0, V, MV1),
	TV1 is TV0 + V,
	(   Prevpred == Predspec ->
		PrevClauses = [Simple|Simples],
		Preds3 = Preds1
	;   get_pred_ref(Predspec, Preds1, Predref, Preds2) ->
		get_pred_code(Predref, Preds2, Code),
		PrevClauses = [],
		(   Code == undefined ->
			Clauses0 = []
		;   Code = disj(Clauses0)
		    % continued pred:  must be discontiguous or multifile
		    % TODO:  we really should check!
		),
		append(Clauses0, [Simple|Simples], Clauses),
						% add new clauses after old
		put_pred_code(Predref, disj(Clauses), Preds2, Preds3)
	),
	process_stream(File, Stream, Defmod, Files0, Files, Mods0, Mods, Preds3,
		       Preds, MV1, MV, TV1, TV, Predspec, Simples, Fmods).


%  process_directive(+Directive, +Relfile, +Defmod0, -Defmod, +Files0, -Files,
%		     +Mods0, -Mods, +Preds0, -Preds, +Maxvars0, -Maxvars,
%		     +Totvars0, -Totvars, -Fmods, +Fmods0)
%  After handling directive :- Directive, Defmod is the default module;
%  Defmod0 was the default module before processing Directive.  Fmods is a
%  list of the modules defined by the file loaded by this directive, if any,
%  followed by Fmods0.  Relfile is the file that we are currently loading.
%  Other args are as above.

process_directive(module(Mod,Exports), _, _, Mod, Files, Files, Mods0,
		Mods, Preds, Preds, MV, MV, TV, TV, [Mod|Fmods], Fmods) :-
	!,
	handle_exports(Exports, Mod, Mods0, Mods).
process_directive(dynamic(Predspec), _, Defmod, Defmod, Files, Files, Mods,
		Mods, Preds0, Preds, MV, MV, TV, TV, Fmods, Fmods) :-
	!,
	set_property(Predspec, Defmod, _, dynamic, Preds0, Preds).
process_directive(discontiguous(Predspec), _, Defmod, Defmod, Files, Files,
		Mods, Mods, Preds0, Preds, MV, MV, TV, TV, Fmods, Fmods) :-
	!,
	set_property(Predspec, Defmod, _, discontiguous, Preds0, Preds).
process_directive(multifile(Predspec), _, Defmod, Defmod, Files, Files,
		Mods, Mods, Preds0, Preds, MV, MV, TV, TV, Fmods, Fmods) :-
	!,
	set_property(Predspec, Defmod, _, multifile, Preds0, Preds).
process_directive(consult(File), R, Defmod, Defmod, Files0, Files, Mods0,
		Mods, Preds0, Preds, MV0, MV, TV0, TV, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, MV0, MV, TV0, TV, Defmod).
process_directive(compile(File), R, Defmod, Defmod, Files0, Files, Mods0,
		Mods, Preds0, Preds, MV0, MV, TV0, TV, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, MV0, MV, TV0, TV, Defmod).
process_directive(ensure_loaded(File), R, Defmod, Defmod, Files0, Files,
		Mods0, Mods, Preds0, Preds, MV0, MV, TV0, TV, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, MV0, MV, TV0, TV, Defmod).
process_directive(use_module(File), R, Defmod, Defmod, Files0, Files, Mods0,
		Mods, Preds0, Preds, MV0, MV, TV0, TV, Fmods, Fmods) :-
	!,
	import_file_contents(File, R, Files0, Files, Mods0, Mods, Preds0,
			     Preds, MV0, MV, TV0, TV, Defmod).
process_directive(use_module(File,Imports), R, Defmod, Defmod, Files0, Files,
		Mods0, Mods, Preds0, Preds, MV0, MV, TV0, TV, Fmods, Fmods) :-
	!,
	file_contents(File, R, List, Files0, Files, Mods0, Mods, Preds0,
		      Preds1, MV0, MV, TV0, TV),
	collect_file_exports(List, Files, Exp),
 	handle_imports(Imports, Defmod, Exp, Mods, Preds1, Preds).
process_directive((X,Y), R, Defmod0, Defmod, Files0, Files, Mods0, Mods,
		Preds0, Preds, MV0, MV, TV0, TV, Fmods, Fmods0) :-
	!,
	process_directive(X, R, Defmod0, Defmod1, Files0, Files1, Mods0, Mods1,
			  Preds0, Preds1, MV0, MV1, TV0, TV1, Fmods1, Fmods0),
	process_directive(Y, R, Defmod1, Defmod, Files1, Files, Mods1, Mods,
			  Preds1, Preds, MV1, MV, TV1, TV, Fmods, Fmods1).
process_directive(op(P,T,O), _, Defmod, Defmod, Files, Files, Mods, Mods,
		Preds, Preds, MV, MV, TV, TV, Fmods, Fmods) :-
	!,
	op(P, T, O).
process_directive(_, _, Defmod, Defmod, Files, Files, Mods, Mods, Preds,
		Preds, MV, MV, TV, TV, Fmods, Fmods).
						% couldn't be too important


%  handle_exports(+Exports, +Module, +Mods0, -Mods)
%  Export predicates on the list Exports from Module.  Mods0 and Mods
%  are as described above.

handle_exports(Exports0, Module, Mods0, Mods) :-
	(   map_fetch(Module, Mods0, OldExports) ->
		append(OldExports, Exports0, Exports1),
		sort(Exports1, Exports)		% remove duplicates
	;   Exports = Exports0
	),
	map_store(Module, Exports, Mods0, Mods).



%  import_file_contents(+File, +Relfile, +Files0, -Files, +Mods0, -Mods,
%		+Preds0, -Preds, +Maxvars0, -Maxvars, +Totvars0, -Totvars,
%		+Importmod)
%  Load File, whose name is taken relative to Relfile, and import all its
%  exported preds into module Importmod.  Other args are as above.

import_file_contents(File, Relfile, Files0, Files, Mods0, Mods, Preds0, Preds,
		MV0, MV, TV0, TV, Defmod) :-
	file_contents(File, Relfile, List, Files0, Files, Mods0, Mods, Preds0,
		      Preds1, MV0, MV, TV0, TV),
	collect_file_exports(List, Files, Exp),
 	handle_imports(all, Defmod, Exp, Mods, Preds1, Preds).
	

%  collect_file_exports(+List, +Files, -Exp)
%  Exp is a list of the modules defined by all the files on List.

collect_file_exports([], _, []).
collect_file_exports([F|Fs], Files, Exp) :-
	map_fetch(F, Files, Exp0),
	append(Exp0, Exp1, Exp),
	collect_file_exports(Fs, Files, Exp1).

%  handle_imports(+Imports, +ToMod, +Exp, +Mods, +Preds0, -Preds)
%  Preds is Preds0 extended to include import/1 terms as described above for
%  each of the predicates listed on Imports, importing them into ToMod.  Exp
%  is a list of the modules available from which to import these preds, and
%  Mods is a mapping from module name to the list of that module's exports.
%  As a (common) special case, if Imports is the atom 'all', then all
%  predicates exported from all the modules on Exps are imported.

handle_imports(all, ToMod, Exp, Mods, Preds0, Preds) :-
	collect_exports(Exp, Mods, Imports),
	handle_imports(Imports, ToMod, Exp, Mods, Preds0, Preds).
handle_imports([], _, _, _, Preds, Preds).
handle_imports([Spec|Specs], ToMod, Exp, Mods, Preds0, Preds) :-
	handle_1_import(Spec, ToMod, Exp, Mods, Preds0, Preds1),
	handle_imports(Specs, ToMod, Exp, Mods, Preds1, Preds).

handle_1_import(Spec, ToMod, Exp, Mods, Preds0, Preds) :-
	(   ( Spec = Mod:Name/Arity ; Spec = Name/Arity ) ->
		(   member(Mod, Exp),
		    map_fetch(Mod, Mods, Exports),
		    member(Name/Arity, Exports),
		    Fromspec = Mod:Name/Arity,
		    put_pred_alias(Fromspec, ToMod:Name/Arity, Preds0,
				   Preds) ->
			true
		;   format('! Can''t import predicate ~q~n', [Spec]),
		    % TODO:  should handle the case where we've already found
		    % calls to the imported pred and so assigned it a ref.  In
		    % this case we should make the code part of the new ref
		    % some sort of reference to the Fromspec pred.
		    Preds = Preds0
		)
	;   format('Invalid import spec:  ~q~n', [Spec]),
	    Preds = Preds0
	).


%  collect_exports(+Expmods, +Mods, -Exports)
%  Exports is a list of all the predicates exported by all the modules on the
%  list Expmods.  Mods is a mapping from module name to the list of the
%  predicates it exports.

collect_exports([], _, []).
collect_exports([M|Ms], Mods, Exports) :-
	map_fetch(M, Mods, Exports0),
	append(Exports0, Exports1, Exports),
	collect_exports(Ms, Mods, Exports1).



%  set_property(+Predspec, +Defmod, -Pred, +Property, +Preds0, -Preds).

set_property(Predspec, Defmod, Pred, Property, Preds0, Preds) :-
	get_pred_ref(Predspec, Defmod, Preds0, Pred, Preds1),
	add_pred_property(Pred, Property, Preds1, Preds).


%  source_file_name(+File0, +Relfile, -File)
%  File is the full and proper name of the Prolog source file File0.  Relfile
%  is a directory or file name relative to which relative files will be taken.
%  The current implementation ignores Relfile, because Quintus'
%  absolute_file_name/3 doesn't support file specs relative to another file.

source_file_name(File0, _Relfile, File) :-
	% This is very Quintus-specific
	absolute_file_name(File0,
			   [ignore_underscores(true),
			    file_type(prolog),
			    ignore_version(true),
			    access(read)],
			   File).



%  report_undefined_preds(+Preds0, -Preds)
%  print a report of all undefined predicates in predicate store Preds0.
%  Preds is the same as Preds0, except that perhaps some undefined preds have
%  been resolved.

report_undefined_preds(Preds0, Preds) :-
	predstore_first(Preds0, Cursor),
	report_undefined_preds_1(Cursor, Preds0, Preds).


report_undefined_preds_1(Cursor0, Preds0, Preds) :-
	(   predstore_next(Preds0, Cursor0, Predref, Cursor1) ->
		(   get_pred_code(Predref, Preds0, undefined),
		    \+ pred_has_property(Predref, dynamic, Preds0) ->	
			get_old_pred_ref(Spec, Preds0, Predref),
			Preds3 = Preds0,
			format('! Undefined predicate:  ~q~n', [Spec])
		;   Preds3 = Preds0
		),
		report_undefined_preds_1(Cursor1, Preds3, Preds)
	;   Preds = Preds0
	).
		


/****************************************************************

			     Clause Normalization

This code handles normalizing clauses.  Our representation of a predicate's
code is described above.  Note that it is a ground representation, with
variables represented by integers.

We form the Clark completion of each predicate one clause at a time by
moving head unification into the body of each clause, and numbering the
argument variables from 1 through the predicate's arity.

****************************************************************/

%  simplify_clause(+Clause, +Defmod, +Preds0, -Preds, -Predspec, -Numvars,
%		-Simples)
%  Simples is a simplified version of clause Clause, with its head removed,
%  its arguments all turned into '$ VAR $'/1 terms numbered as described
%  above, and all its other variables numbered from Arity + 1 up.  Defmod is
%  the default module in which Clause appears, and Predspec is a predicate
%  specification of the predicate defined by Clause.  Numvars is the number of
%  distinct variables in the (normalized) clause.  Other args are as above.

simplify_clause(Module:Clause, _, Preds0, Preds, Predspec, V, Simples) :- 
	!,
	simplify_clause(Clause, Module, Preds0, Preds, Predspec, V, Simples).  
simplify_clause((Head:-Body), Defmod, Preds0, Preds, Predspec, V, Simples) :- 
	!,
	simplify_compound_clause(Head, Body, Defmod, Defmod, Preds0, Preds,
				 Predspec, V, Simples).
simplify_clause(Unit, Module, Preds, Preds, Module:Name/Arity, V,
		conj(Simples)) :-
	functor(Unit, Name, Arity),
	simplify_head_args(Arity, Unit, Arity, V, Simples, []).


simplify_compound_clause(Mod:Head, Body, _, Bodmod, Preds0, Preds, Predspec,
		V, Simples) :-
	!,
	simplify_compound_clause(Head, Body, Mod, Bodmod, Preds0, Preds,
				 Predspec, V, Simples).
simplify_compound_clause(Head, Body, Mod, Bodmod, Preds0, Preds,
		Mod:Name/Arity, V, conj(Simples)) :-
	functor(Head, Name, Arity),
	prenumber_head_args(Arity, Head),
	simplify_goal_1(Body, Bodmod, Arity, Arity, V1, Preds0, Preds,
			Simples0, []),
	% It's a nuissance, but we have to do this last, to get the variable
	% numbering right:
	simplify_head_args(Arity, Head, V1, V, Simples, Simples0).


%  prenumber_head_args(+N, +-Head)
%  Not terribly declarative.  Binds all unbound argument *i* (1 =< i =< N) of
%  Head to a '$ VAR $' term for variable *i*.  This avoids having these
%  variables assigned other variable numbers.

prenumber_head_args(N, Head) :-
	(   N =< 0 ->
		true
	;   arg(N, Head, Arg),
	    (	var(Arg) ->
		    object_var(Arg, N)
	    ;	true
	    ),
	    N1 is N - 1,
	    prenumber_head_args(N1, Head)
	).


%  simplify_head_args(+N, +-Head, +V0, -V, -Simples, +Simples0)
%  Simples is a list of goals unifying arguments *i* from 1 through N of Head
%  with '$ VAR $'(*i*) terms, followed by Simples0.  Where an argument *i* is a
%  variable, it is simply unified with '$ VAR $'(*i*).  Variables within head
%  arguments are numbered from V0 through V-1.

simplify_head_args(N, Head, V0, V, Simples, Simples0) :-
	(   N < 1 ->
		Simples = Simples0,
		V = V0
	;   arg(N, Head, Arg),
	    (   object_var(Arg, N) ->
		    Simples1 = Simples0,
		    V1 = V0
	    ;   Simples1 = [equal(N,Descr,Vars,_,V0)|Simples0],
		term_descr(Arg, V0, V1, Descr, Vars)
	    ),
	    N1 is N - 1,
	    simplify_head_args(N1, Head, V1, V, Simples, Simples1)
	).


%  simplify_goal(+Goal, +Mod, +V0, -V, +Preds0, -Preds, -Simple)
%  simplify_goal(+Goal, +Mod, +R, +V0, -V, +Preds0, -Preds, -Simple)

simplify_goal(Goal, Mod, V0, V, Preds0, Preds, Simple) :-
	simplify_goal(Goal, Mod, V0, V0, V, Preds0, Preds, Simple).

simplify_goal(Goal, Mod, R, V0, V, Preds0, Preds, Simple) :-
	simplify_goal_1(Goal, Mod, R, V0, V, Preds0, Preds, Simples, []),
	(   Simples = [Simple] ->
		true
	;   Simple = conj(Simples)
	).


%  simplify_goal_1(+Goal, +Mod, +R, +V0, -V, +Preds0, -Preds, -Simples,
%		   +Simples0)

simplify_goal_1(Var, Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	( var(Var) ; object_var(Var, _) ),
	!,
	simplify_goal_1(call(Var), Mod, R, V0, V, Preds0, Preds, Simples,
			Simples0).
simplify_goal_1((G1,G2), Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	!,
	% Note that we number G2 before G1, but the simplified version of G1
	% is put before that of G2 in Simples.  This is because we need
	% variables last appearing later in the clause to be numbered lower
	% than variables last appearing earlier.
	simplify_goal_1(G2, Mod, R, V0, V1, Preds0, Preds1, Simples1,
			Simples0),
	simplify_goal_1(G1, Mod, V1, V1, V, Preds1, Preds, Simples, Simples1). 
simplify_goal_1((G1;G2), Mod, R, V0, V, Preds0, Preds, [Simple|Simples0],
		Simples0) :-
	!,
	simplify_disjunction(G1, G2, Mod, R, V0, V, Preds0, Preds, Simple).
simplify_goal_1((\+G), Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	!,
	simplify_goal_1((G->fail;true), Mod, R, V0, V, Preds0, Preds,
		      Simples, Simples0).
simplify_goal_1((G1->G2), Mod, R, V0, V, Preds0, Preds, Simples,
		Simples0) :-
	!,
	simplify_goal_1((G1->G2;fail), Mod, R, V0, V, Preds0, Preds,
		      Simples, Simples0).
simplify_goal_1((X=Y), _, R, V0, V, Preds, Preds, Simples, Simples0) :-
	!,
	simplify_unification(X, Y, R, V0, V, Simples, Simples0).
simplify_goal_1(is(X,Y), _, R, V0, V, Preds, Preds, Simples, Simples0) :-
	!,
	simplify_eval(X, Y, R, V0, V, Simples, Simples0).
simplify_goal_1(!, _, _, V, V, Preds, Preds, [!|Simples], Simples) :-
	!.
simplify_goal_1(Mod:Goal, _, R, V0, V, Preds0, Preds, Simples,
		Simples0) :-
	!,
	simplify_goal_1(Goal, Mod, R, V0, V, Preds0, Preds, Simples,
		      Simples0).
simplify_goal_1(Goal, Mod, R, V0, V, Preds0, Preds, Simples, Simples0) :-
	functor(Goal, Name, Arity),
	functor(Gterm, Name, Arity),
	simplify_goal_args(1, Arity, Goal, Gterm, V, V0, V, [], Simples,
			   Simples1),
	(   builtin(Name, Arity) ->
		Simples1 = [builtin(Gterm,_,R)|Simples0],
		Preds = Preds0
	;   Simples1 = [call(Pnum,Gterm,_,R)|Simples0],
	    get_pred_ref(Mod:Name/Arity, Preds0, Pnum, Preds)
	).


%  simplify_disjunction(+G1, +G2, +Mod, +R, +V0, -V, +Preds0, -Preds,
%		-Num, -Simple)
%  This is a bit tricky.  When simplifying a conjunction X,Y we simplify Y
%  first, and then X, so that earlier goals will get higher variable numbers.
%  For a disjunction X;Y it doesn't matter in what order we number them, because
%  we project each disjunct to the same threshold:  namely, the variable
%  number at the beginning of the whole disjunction.  In order for this to
%  work, though, we must be sure to number goals conjoined both before and
%  after the disjunction before numbering the disjunction itself.  This is
%  taken care of by simplify_goal/10.

simplify_disjunction((If->Then), Else, Mod, R, V0, V, Preds0, Preds,
		if_then_else(Sif,Sthen,Selse)) :-
	!,
	simplify_goal(Then, Mod, R, V0, V1, Preds0, Preds1, Sthen),
	simplify_goal(If, Mod, V1, V1, V2, Preds1, Preds2, Sif),
	(   Else = (G1;G2) ->
		simplify_disjunction(G1, G2, Mod, R, V2, V, Preds2, Preds,
				     Selse)
	;   simplify_goal(Else, Mod, R, V2, V, Preds2, Preds, Selse)
	).
simplify_disjunction(G1, G2, Mod, R, V0, V, Preds0, Preds, disj(Sgs)) :-
	!,
	simplify_disjunct(G1, Mod, R, V0, V1, Preds0, Preds1, Sgs, Sgs1),
	simplify_disjunct(G2, Mod, R, V1, V, Preds1, Preds, Sgs1, []).


%  simplify_disjunct(+G2, +Mod, +R, +V0, -V, +Preds0, -Preds, -Sgs, +Sgs0)

simplify_disjunct((G1;G2), Mod, R, V0, V, Preds0, Preds, Sgs, Sgs0) :-
	!,
	simplify_disjunct(G1, Mod, R, V0, V1, Preds0, Preds1, Sgs, Sgs1),
	simplify_disjunct(G2, Mod, R, V1, V, Preds1, Preds, Sgs1, Sgs0).
simplify_disjunct(G, Mod, R, V0, V, Preds0, Preds, [Simp|Sgs], Sgs) :-
	simplify_goal(G, Mod, R, V0, V, Preds0, Preds, Simp).


%  simplify_goal_args(+N, +Arity, +Goal, -Gterm, +R, +V0, -V, +Vs, -Simps,
%		      +Simps0)

simplify_goal_args(N, Arity, Goal, Gterm, R, V0, V, Vs, Simples, Simples0) :-
	(   N > Arity ->
		V = V0,
		Simples = Simples0
	;   arg(N, Goal, Garg),
	    arg(N, Gterm, Targ),
	    simplify_1_goal_arg(Garg, Targ, R, V0, V1, Vs, Simples, Simples1),
	    N1 is N + 1,
	    simplify_goal_args(N1, Arity, Goal, Gterm, R, V1, V, [Targ|Vs],
			       Simples1, Simples0)
	).


simplify_1_goal_arg(Garg, Targ, R, V0, V, Vs, Simples, Simples0) :-
	(   var(Garg) ->
		Targ = V,
		Simples = Simples0,
		numbered_object_var(Garg, V0, V)
	;   object_var(Garg, N) ->
		(   member(N, Vs) ->
			% this variable has already been used as an arg in this
			% goal, so we'll have to create a new variable for it
			Targ = V1,
			numbered_object_var(Newvar, V0, V1),
			simplify_unification(Newvar, Garg, R, V1, V, Simples,
					     Simples0)
		;   Targ = N,
		    V = V0,
		    Simples = Simples0
		)
	;   Targ = V1,
	    numbered_object_var(Newvar, V0, V1),
	    simplify_unification(Newvar, Garg, R, V1, V, Simples, Simples0)
	).


%  simplify_unification(+X, +Y, +R, +V0, -V, -Simples, +Simples0)

simplify_unification(X, Y, R, V0, V, Simples, Simples0) :-
	(   var(X) ->
		Simples = [equal(V1,T,Vs,_,R)|Simples0],
		numbered_object_var(X, V0, V1),
		term_descr(Y, V1, V, T, Vs)
	;   object_var(X, N) ->
		Simples = [equal(N,T,Vs,_,R)|Simples0],
		term_descr(Y, V0, V, T, Vs)
	;   var(Y) ->
		Simples = [equal(V1,T,Vs,_,R)|Simples0],
		numbered_object_var(Y, V0, V1),
		term_descr(X, V1, V, T, Vs)
	;   object_var(Y, _N) ->
		Simples = [equal(V,T,Vs,_,R)|Simples0],
		term_descr(X, V0, V, T, Vs)
	;   functor(X, Name, Arity),
	    functor(Y, Name, Arity) ->
		simplify_unifications(1, Arity, X, Y, R, V0, V, Simples,
				      Simples0)
	;   Simples = [builtin(fail,_,V0)],
	    write('! the unification '),
	    write(X=Y),
	    write(' is sure to fail'),
	    nl,
	    V = V0
	).


simplify_unifications(N, Arity, X, Y, R, V0, V, Simples, Simples0) :-
	(   N > Arity ->
		V = V0,
		Simples = Simples0
	;   arg(N, X, Xn),
	    arg(N, Y, Yn),
	    simplify_unification(Xn, Yn, R, V0, V1, Simples, Simples1),
	    N1 is N + 1,
	    simplify_unifications(N1, Arity, X, Y, V1, V1, V,
				  Simples1, Simples0)
	).
	    


%  simplify_eval(+X, +Y, +V0, -V, -Simples, +Simples0)

simplify_eval(X, Y, R, V0, V, Simples, Simples0) :-
	(   var(X) ->
		Simples = [eval(V1,T,Vs,_,R)|Simples0],
		numbered_object_var(X, V0, V1),
		term_descr(Y, V1, V, T, Vs)
	;   object_var(X, N) ->
		Simples = [eval(N,T,Vs,_,R)|Simples0],
		term_descr(Y, V0, V, T, Vs)
	;   simplify_unification(Newvar, X, V1, V0, V1, Simples, Simples1),
	    simplify_eval(Newvar, Y, R, V1, V, Simples1, Simples0)
	).
	    


%  term_descr(+Term, +V0, -V, -Descr, -Vars)
%  term_descr(+Term, +V0, -V, -Descr, +Vars0, -Vars)
%  Descr is the term description, as described above, corresponding to Term,
%  and Vars is a sorted list of all the variables appearing in Term.

term_descr(Term, V0, V, Descr, Vars) :-
	term_descr(Term, V0, V, Descr, [], Vars1),
	sort(Vars1, Vars).			% sort and remove duplicates

term_descr(Term, V0, V, Descr, Vars0, Vars) :-
	(   var(Term) ->
		Descr = var(V),
		numbered_object_var(Term, V0, V),
		Vars = [V|Vars0]
	;   object_var(Term, N) ->
		Descr = var(N),
		V = V0,
		Vars = [N|Vars0]
	;   atom(Term) ->
		Descr = atom(Term),
		V = V0,
		Vars = Vars0
	;   integer(Term) ->
		Descr = integer(Term),
		V = V0,
		Vars = Vars0
	;   float(Term) ->
		Descr = float(Term),
		V = V0,
		Vars = Vars0
	;   Descr = compound(F,A,Args),
	    functor(Term, F, A),
	    arg_descrs(Term, 1, A, V0, V, Args, Vars0, Vars)
	).

%  arg_descrs(+Term, +N, +A, +V0, -V, -Args, +Vars0, -Vars)
%  Args is a list of the argument descriptions corresponding to arguments N
%  through A of Term, and Vars is a list of the variables appearing in those
%  args, appended to Vars0.

arg_descrs(Term, N, A, V0, V, Args, Vars0, Vars) :-
	(   N > A ->
		V = V0,
		Args = [],
		Vars = Vars0
	;   Args = [Descr|Args1],
	    arg(N, Term, Arg),
	    term_descr(Arg, V0, V1, Descr, Vars0, Vars1),
	    N1 is N + 1,
	    arg_descrs(Term, N1, A, V1, V, Args1, Vars1, Vars)
	).
	


object_var(Var, V) :-
	functor(Var, '$ VAR $', 1),		% Don't just write '$ VAR $'/1
	arg(1, Var, V).				% term since this program then
						% wouldn't be self-applicable

numbered_object_var(Var, V0, V) :-
	V is V0 + 1,
	object_var(Var, V).




check_var_limit(Var, Predspec) :-
	max_variable(Max),
	(   Var >= Max ->
		format('! Predicate ~q has too many variables (~d)~n',
		       [Predspec, Var])
	;   true
	).



/************************************************************************

			Handling the Foreign Interface

************************************************************************/

%  process_foreign_decl(+Spec, +Defmod, +Preds0, -Preds)

process_foreign_decl(Spec, Defmod, Preds0, Preds) :-
	functor(Spec, Name, Arity),
	set_property(Name/Arity, Defmod, Pred, foreign, Preds0, Preds1),
	put_pred_code(Pred, foreign(Spec), Preds1, Preds).

