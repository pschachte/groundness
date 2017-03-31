%  File     : check.pl
%  RCS      : $Id: check.pl,v 1.1 1998/10/16 05:03:50 pets Exp $
%  Author   : Peter Schachte
%  Origin   : Wed Oct 14 08:42:45 1998
%  Purpose  : compare analysis results
%

%  Read in a file line by line converting random DNF notation into the output
%  you would get by printing ROBDDs.  Copies the rest of the input file
%  verbatim.

:- use_module(library(lineio)).

runtime_entry(start) :-
	prompt(_, ''),
	init_rep,
	convert.


convert :-
	(   get_line(Line) ->
		convert_and_print(Line),
		convert
	;   true
	).


convert_and_print(Line) :-
	(   append(Front, [0'), 0':, 0' , 0'  | DNF], Line),
	    anal_bottom(Fn0),
	    phrase(dnf(Fn0, Fn), DNF) ->
		put_chars(Front),
		put_chars("):  "),
		anal_print(Fn),
		nl
	;   put_line(Line)
	).


dnf(Fn0, Fn) -->
	whitespace,
	dnf1(Fn0, Fn).


dnf1(Fn0, Fn) -->
	"(",
	{ anal_top(Fn1) },
	conj(Fn1, Fn2),
	")",
	{ anal_join(Fn0, Fn2, Fn3) },
	(   dnf(Fn3, Fn) ->
		[]
	;   whitespace,
	    { Fn = Fn3 }
	).


conj(Fn0, Fn) -->
	whitespace,
	(   "~" ->
	    number(N),
	    { variable_rep(N, Fn1),
	      negate(Fn1, Fn2)
	    }
	;   number(N),
	    { variable_rep(N, Fn2) }
	),
	{ anal_meet(Fn0, Fn2, Fn3) },
	(   conj(Fn3, Fn) ->
		[]
	;   { Fn = Fn3 }
	).
		
	
whitespace -->
	(   " " ->
		whitespace
	;   "	" ->
		whitespace
	;   []
	).

negate(F, Fneg) :-
	anal_bottom(False),
	implies(F, False, Fneg).

number(N) -->
	number(0, N).

number(N0, N) -->
	[C],
	{ N1 is C - 0'0,
	  N1 >= 0,
	  N1 < 10,
	  N2 is N0*10 + N1
	},
	(   number(N2, N) ->
		[]
	;   { N = N2 }
	).


:- multifile portray/1.

portray(Fn) :-
	integer(Fn),
	Fn > 65535,
	!,
	anal_print(Fn).


% The foreign part of analysis.pl

foreign(initRep, c, init_rep).
foreign(trueVar, c, anal_top([-address])).
foreign(falseVar, c, anal_bottom([-address])).
foreign(variableRep, c, variable_rep(+integer, [-address])).
foreign(glb, c, anal_meet(+address, +address, [-address])).
foreign(lub, c, anal_join(+address, +address, [-address])).
foreign(implies, c, implies(+address, +address, [-address])).
foreign(printOut, c, anal_print(+address)).

foreign_file('bryant.so', [initRep, trueVar, falseVar, variableRep, glb,
                lub, implies, printOut]).

:- load_foreign_executable('bryant.so').
