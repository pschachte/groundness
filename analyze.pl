%  file    : analyze
%  Authors : Peter Schachte
%  Purpose : Groundness analysis of a Prolog source file
%
%				Abstract
%
%  The top level of our Prolog global analyzer.

:- module(analyze, [
	anz/1					% just for debugging
   ]).


/**********************************************************************

			   The Abstract Interpreter

The basic algorithm:

++enumerate
    1.	Read in the whole program, forming the Clark completion.  This
	is done in calls.pl.
    2.  Find the strongly-connected components in the program call
	graph.  This is done in scc.pl.
    3.  Analyze the program one strongly-connected component at a time,
	starting from the bottom, giving us success patterns.  This is done in
	bottomup.pl.
    4.  Analyze the program one strongly-connected component at a time,
	starting from the top, giving us call patterns.  This is done in
	topdown.pl.
--enumerate

Note that in this approach we must have the whole program in memory
all at once.  This is probably not a very good approach for very large
programs.  A more sophisticated algorithm could avoid this by
analyzing the program bottom up a file at a time, and then again top
down a file at a time.  This could be done by making one pass over all
the files to construct the file dependency graph and collect module
imports and exports, and all multifile predicates, then making a
second pass over all the files for the bottom up analysis, and then a
third pass for the top down analysis.  Where there are circular file
dependencies, multiple files must be processed at once for the second
and third passes, but this is relatively rare.  We must be given the
names of all files in the program, or, better yet, most of the files
in the program must be loaded by other files in the program, so that
only one or a few of the files need be explicitly given to the
analyzer.

A planned later version of this analyzer will be incremental, which
means we must be careful in this version to choose data structures
that are amenable to incremental update.  This means that we resist
numbering our predicates in strongly-connected component order because
the strongly-connected components may change and we don't want to have
to renumber our predicates just to maintain the order.

**********************************************************************/


/*======================================================================

			     Future Work

For representations of boolean functions where equality checking is
very cheap (e.g., bryant graphs), we can sometimes substitute an
equality check for computing some meets and joins.  We can do this by
using the *Fixed* part of a clause's analysis to store the current
best approximation of that clause, and associating with each goal in
the variable part its current best approximation.  Then when analyzing
a clause, if the new analysis of a goal is the same as the old, we
don't meet it with the other analyses of the other goals.  And if the
analysis of a clause is the same as the previous analysis of that
clause, then we need not join it with the analyses of the other
clauses.  We get away with this by always meeting the previous
analysis of a clause with the analyses of the calls, and always
joining the analysis of the predicate with the analyses of the
clauses.  We can also tell that the analysis of a call will be the
same as last time if the analysis of the called predicate hasn't
changed since last time.  This could save a fair amount of meeting,
joining, and renaming.  But it only makes sense if equality comparison
is cheaper than meeting and joining.

Note that since the variable part only contains calls to predicates in
the same strongly-connected component, this trick would only save
effort for strongly-connected components with more than a single
predicate.  Still, it might be worth the effort for large
strongly-connected components.

  ======================================================================*/
/*======================================================================

			    Required External Code

'calls.pl' contains code to load a number of Prolog source files and
all the files they require, and return all the predicates in all those
files as result.  'scc.pl' finds the strongly connected components in
the program call graph.  'bottomup.pl' and 'topdown.pl' contain the
code for the bottom up and top down analysis, respectively.
'predstore.pl'
contains code to manipulate a predicate store abstract dataype; the
predicate store holds the collection of predicates in the program and
whatever analysis has been computed at a given point in the analysis.
'analysis.pl' contains code to store and manipulate predicate
analyses; at the moment, the analysis is just the groundness analysis,
but it will later be extended to contain other analyses as well.
'misc.pl', of course, contains miscellaneous auxiliary predicates used
by various parts of the analyzer.  'library(charsio)' is in the
standard Quintus Prolog library; it causes "chars" (lists of character
codes) to be printed out as double-quoted strings, making them much
more readable.

  ======================================================================*/

:- ensure_loaded(calls).
:- ensure_loaded(scc).
:- ensure_loaded(bottomup).
:- ensure_loaded(topdown).
%  :- ensure_loaded(boolfn).
:- ensure_loaded(predstore).
:- ensure_loaded(analysis).
:- ensure_loaded(millitime).
:- ensure_loaded(misc).
:- ensure_loaded(library(charsio)).

/*======================================================================
			    Command Line Handling
  ======================================================================*/

%  runtime_entry(+Condition)
%  Main predicate of a Quintus Prolog program.  Condition is 'start' if
%  program is just starting, or 'abort' if restarting after an abort.

user:runtime_entry(start) :-
	nogc,
	(   unix(args(Cmdline)),
	    Cmdline \== [] ->	
		default_opt(Opts),
		analyze_list(Cmdline, Opts)
	;   usage,
	    halt(1)
	).


%  goal
%  This is here only to make the program consistent with the other programs I
%  analyze.  I don't actually call this.

user:goal :- user:runtime_entry(start).


%  anz(List)
%  Do what runtime_entry(start) does, but take command line as argument.  This
%  predicate only exists for debugging.

anz(List) :-
	default_opt(Opt0),
	setopt(print, Opt0, on, Opt1),
	analyze_list(List, Opt1).


%  usage
%  Print out a usage message.

usage :-
	write('usage:  analyze [switches] file ...'), nl,
	write('  supported switches are:'), nl,
	write('	-p	print analysis results'), nl,
	write('	-c	turn on character escapes handling'), nl,
	write('	-g	turn on garbage collection'), nl,
	write('	-t	turn on GC and GC tracing'), nl,
	write('	-s	print statistics about analyzed files'), nl,
	write('	-q	quiet (omit analysis times)'), nl,
	write('	-j	just the goal-independent analysis'), nl,
	write('	-i Goal	use Goal as the initial goal.  Goal should be'), nl,
	write('		a well-formed Prolog term.'), nl,
	write('	-h	print this help text'), nl,
	nl,
	write('in addition, -np, -nc, -ng, -ns, and -nj turn off the,'), nl,
	write('corresponding single letter options.  The default') , nl,
	write('initial goal is `goal''; all the boolean options') , nl,
	write('default to `off''.  Note that you can intersperse'), nl,
	write('file names and options to use different options for'), nl,
	write('different files.'), nl, nl. 


%  analyze_list(+Files, +Opts0)
%  Analyze Files respecting the options specified in Opts.

analyze_list([], _).
analyze_list([File|Files], Opts) :-
	(   atom_chars(File, [0'-,Optchar]),
	    bool_option(Optchar, Optname, Value) ->
		Files1 = Files,
		setopt(Optname, Opts, Value, Opts1)
	;   atom_chars(File, [0'-,0'n,Optchar]),
	    bool_option(Optchar, Optname, Negvalue) ->
		Files1 = Files,
		negate_bool_option(Negvalue, Value),
		setopt(Optname, Opts, Value, Opts1)
	;   atom_chars(File, [0'-,Optchar]),
	    other_option(Optchar, Opts, Opts1, Files, Files1) ->
		true
	;   atom_chars(File, [0'-|_]) ->
		format('unrecognized switch:  `~w~n', [File]),
		usage,
		halt(1)
	;   Opts1 = Opts,
	    Files1 = Files,
	    (   analyze(File, Opts) ->
		    true
	    ;   format('~n! Analysis failed !~n~n', [])
	    )
	),
	analyze_list(Files1, Opts1).



%  bool_option(+Letter, -Optionname, -Value)
%  When -Letter option is selected, give option Optionname the value Value.

bool_option(0'p, print, on).
bool_option(0's, stats, on).
bool_option(0'g, gc, on).
bool_option(0't, gctrace, on).
bool_option(0'c, charex, on).
bool_option(0'j, topdown, off).
bool_option(0'q, times, off).


negate_bool_option(on, off).
negate_bool_option(off, on).


other_option(0'i, Opts0, Opts, [Goalatom|Files], Files) :-
	atom_chars(Goalatom, Goalchars),
	append(Goalchars, ".", Goalchars1),
	(   with_input_from_chars(read_term([syntax_errors(quiet)], Goal),
				  Goalchars1) ->
		setopt(goal, Opts0, Goal, Opts)
	;   Opts = Opts0,
	    getopt(goal, Opts0, Goal),
	    format('Syntax error in goal "~w"~nusing "~w" as initial goal',
		   [Goalatom, Goal])
	).
other_option(0'h, Opts, Opts, Files, Files) :-
	usage,
	halt.


/*======================================================================
			       The Options term
  ======================================================================*/

%  getopt(+Optname, +Options, -Value)
%  Value is the value of option Optname in options term Options.

getopt(print,	options(P, _S, _G, _T, _D), P).
getopt(stats,	options(_P, S, _G, _T, _D), S).
getopt(goal,	options(_P, _S, G, _T, _D), G).
getopt(times,	options(_P, _S, _G, T, _D), T).
getopt(topdown,	options(_P, _S, _G, _T, D), D).
getopt(gc,	_, State) :-
	prolog_flag(gc, State).
getopt(gctrace,	_, State) :-
	prolog_flag(gc_trace, State).
getopt(charex,	_, State) :-
	prolog_flag(character_escapes, State).

%  setopt(+Optname, +Options0, +Newvalue, -Options)
%  Newvalue is the value of option Optname in options term Options; all other
%  options have the same value as in Options0.

setopt(print,	options(_, S, G, T, D), P, options(P, S, G, T, D)).
setopt(stats,	options(P, _, G, T, D), S, options(P, S, G, T, D)).
setopt(goal,	options(P, S, _, T, D), G, options(P, S, G, T, D)).
setopt(times,	options(P, S, G, _, D), T, options(P, S, G, T, D)).
setopt(topdown,	options(P, S, G, T, _), D, options(P, S, G, T, D)).
setopt(gc,	Opt, State, Opt) :-
	prolog_flag(gc, _, State).
setopt(gctrace,	Opt, State, Opt) :-
	prolog_flag(gc, _, State),
	prolog_flag(gc_trace, _, State).
setopt(charex,	Opt, State, Opt) :-
	prolog_flag(character_escapes, _, State).


%  default_opt(-Options)
%  Options is an options term with all default values.

default_opt(Opt) :-
	Opt0 = options(off, off, user:goal, on, on),
	setopt(gctrace, Opt0, off, Opt1),
	setopt(charex, Opt1, off, Opt).


/*======================================================================
			     Running the analyzer
  ======================================================================*/

%  analyze(+File, +Opts)
%  This is the main top level predicate, which analyzes the file File
%  and all the files it loads.  Opts controls the details of the analysis.
%  Most of the code handles keeping runtime statistics;
%  the work of the predicate is done in the calls to file_contents/2, sccs/3,
%  analyze_bottom_up/3, and analyze_top_down/3.


analyze(File, Opts) :-
	format('===> ~w~n', File),
	flush_output(user_output),
	init_rep,
	getopt(stats, Opts, Stats),
	getopt(times, Opts, Times),
	getopt(topdown, Opts, Topdown),
	milli_time(Tread0),
	file_contents(File, Preds0, Maxvar0, Totvars),
	(   Topdown == on ->
		getopt(goal, Opts, Goal),
		simplify_goal(Goal, user, 0, V, Preds0, Preds1, Goal1),
		check_var_limit(V, initial_goal/0),
		max(Maxvar0, V, Maxvar)
	;   Preds1 = Preds0,
	    Maxvar = Maxvar0
	),
	max_variable(Limit),
	Maxvar < Limit,				% Fail analysis if over limit
	milli_time(Tread1),
	T1 is Tread1-Tread0,
	print_pred_stats(Stats, Preds1, Maxvar, Totvars),
    	print_stat(Times, 'Read time', T1, msecs),
	flush_output(user_output),
	milli_time(Tscc0),
	(   Topdown == on ->
		sccs(Preds1, Preds2, Goal1, SCCs)
	;   sccs(Preds1, Preds2, SCCs)
	),					% NB: SCCs is topologically
						% sorted at this point
	milli_time(Tscc1),
	T2 is Tscc1-Tscc0,
	length(SCCs, SCCcount),
	sum_of_lengths(SCCs, 0, Reachable),
    	print_stat(Stats, 'SCCs found', SCCcount, ''),
    	print_stat(Stats, 'Reachable predicates found', Reachable, ''),
    	print_stat(Times, 'SCCs time', T2, msecs),
	flush_output(user_output),
	milli_time(Tbu0),
	analyze_bottom_up(SCCs, Preds2, Preds3),
	milli_time(Tbu1),
	T3 is Tbu1-Tbu0,
	print_stat(Times, 'Bottom-up analysis time', T3, msecs),
	flush_output(user_output),
	(   Topdown == on ->
		reverse(SCCs, SCCsrev),
		milli_time(Ttd0),
		analyze_single_goal(Goal1, Preds3, Preds4),
		analyze_top_down(SCCsrev, Preds4, Preds),
		milli_time(Ttd1),
		T4 is Ttd1-Ttd0,
		print_stat(Times, 'Top-down analysis time', T4, msecs),
		T5 is T3+T4,
		print_stat(Times, 'Total analysis time', T5, msecs),
		flush_output(user_output)
	;   Preds = Preds3
	),
	(   getopt(print, Opts, on) ->
		print_analyses(Preds, Topdown)
	;   true
	),
	conclude_rep.


/**********************************************************************

			Printing Out Analysis Results

**********************************************************************/

%  print_analyses(+Preds, +Topdown)
%  Print the bottom-up and, if Topdown is 'on', the top-down analyses of all
%  the predicates in the predstore Preds so the user can hope to understand
%  them.  Well, actually, we print them out however the foreign code handles
%  printing internal representations.

print_analyses(Preds, Topdown) :-
	(   get_old_pred_ref(Spec, Preds, Ref),
	    get_pred_success(Ref, Preds, BU),

	    nl,
	    write(Spec),
	    write(' (g.i.):  '),
	    anal_print(BU),
	    nl,
	    (	Topdown == on ->
		    get_pred_call(Ref, Preds, TD),
		    write(Spec),
		    write(' (call):  '),
		    anal_print(TD),
		    nl,
		    write(Spec),
		    write(' (answ):  '),
		    anal_meet(TD, BU, Full),
		    anal_print(Full),
		    nl
	    ;	true
	    ),

	    fail
	;   true
	).


/*****************************************************************

			     Predicate Statistics

Here we collect information on the number of predicates, clauses, arguments,
and variables appearing in a file.

*****************************************************************/


%  print_stat(+Flag, +Descr, +Value, +Unit)
%  do_print_stat(Descr, Value, Unit) :-
%  If Flag is 'on', print out the statistic whose description is Descr, value
%  is Value, and unit is Unit.  If flag is 'off', do nothing.  do_print_stat/3
%  always prints the stat.

print_stat(off, _, _, _).
print_stat(on, Descr, Value, Unit) :-
	do_print_stat(Descr, Value, Unit).


do_print_stat(Descr, Value, Unit) :-
	tab(4),
	write(Descr),
	atom_chars(Descr, List),
	length(List, Columns),
	N is 36 - Columns,
	(   N > 0 ->
		tab(N)
	;   true
	),
	write(': '),
	write(Value),
	write(' '),
	write(Unit),
	nl.


%  print_pred_stats(+Flag, +Preds, +MaxVars, +TotalVars)
%  Print out interesting statistics about the predicates stored in Preds, a
%  predicate store if Flag is 'on'; don't if Flag is 'off'.

print_pred_stats(off, _, _, _).
print_pred_stats(on, Preds, MaxVars, TotalVars) :-
	pred_stats(Preds, PredCount, ClauseCount, TotalArity, MaxArity),
	AvgArity is TotalArity / PredCount,
	AvgVars is TotalVars / ClauseCount,
	do_print_stat('Total predicates', PredCount, ''),
	do_print_stat('Total clauses', ClauseCount, ''),
	do_print_stat('Total arity', TotalArity, ''),
	do_print_stat('Max arity', MaxArity, ''),
	do_print_stat('Average arity', AvgArity, ''),
	do_print_stat('Total distinct variables', TotalVars, ''),
    	do_print_stat('Maximum variables per clause', MaxVars, ''),
    	do_print_stat('Average variables per clause', AvgVars, '').


%  pred_stats(+Preds, -Predcount, -ClauseCount, -TotalArity, -Maxarity)
%  pred_stats(+Preds, +Cursor, +ClauseCount0, -ClauseCount,
%	      +TotalArity0, -TotalArity, +Maxarity0, +Maxarity)
%  ClauseCount is the number of clauses in Preds, a predicate store.
%  TotalArity is the sum of the arities of all the preds on Preds.  Maxarity
%  is the greatest arity on Predicates.

pred_stats(Preds, PredCount, ClauseCount, TotalArity, MaxArity) :-
	predstore_size(Preds, PredCount),
	predstore_first(Preds, Cursor),
	pred_stats(Preds, Cursor, 0, ClauseCount, 0, TotalArity, 0, MaxArity).


pred_stats(Preds, Cur, CC0, CC, TA0, TA, MA0, MA) :-
	(   predstore_next(Preds, Cur, Predref, Cur1) ->
		get_pred_code(Predref, Preds, Code),
		get_old_pred_ref(_:_/A, Preds, Predref),
		TA1 is TA0 + A,
		max(MA0, A, MA1),
		clause_count(Code, CC0, CC1),
		pred_stats(Preds, Cur1, CC1, CC, TA1, TA, MA1, MA)
	;   CC = CC0,
	    TA = TA0,
	    MA = MA0
	).


%  clause_count(+Code, +Count0, -Count)
%  Count is Count0 + the number of clauses in code.

clause_count(undefined, Count, Count).
clause_count(foreign(_), Count, Count).
clause_count(disj(Clauses), Count0, Count) :-
	length(Clauses, Count1),
	Count is Count0 + Count1.
% We handle other cases just in case we get around to putting in some code
% factoring or something.
clause_count(conj(L), Count0, Count) :-
	clause_count(L, Count0, Count).		% too lazy to add new pred
clause_count([], Count, Count).
clause_count([G|Gs], Count0, Count) :-		% This isn't really right
	clause_count(G, Count0, Count1),
	clause_count(Gs, Count1, Count).
clause_count(equal(_,_,_,_,_), Count, Count).
clause_count(eval(_,_,_,_,_), Count, Count).
clause_count(builtin(_,_,_), Count, Count).
clause_count(call(_,_,_,_), Count, Count).
clause_count(!, Count, Count).
clause_count(if_then_else(_,_,_), Count, Count).


sum_of_lengths([], S, S).
sum_of_lengths([L|Ls], S0, S) :-
	length(L, Len),
	S1 is S0 + Len,
	sum_of_lengths(Ls, S1, S).
