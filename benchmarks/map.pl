% CVS: $Id: map.pl,v 1.3 1998/10/21 04:26:01 pets Exp $

goal :- main.

main :-
	map(200).

print_times(T1,T2,T3,L) :-
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        TT is TT1 - TT2,
        write('Net Time is: '), write(TT), nl,
        Lips is L / TT,
        Klips is Lips / 1000,
        write('KLips are: '), write(Klips), nl.

compens_loop(0).
compens_loop(X) :- Y is X - 1, compens_loop(Y).

el(X,[X|L]).
el(X,[Y|L]):-el(X,L).

list50([27,74,17,33,94,18,46,83,65,2,
       32,53,28,85,99,47,28,82,6,11,
       55,29,39,81,90,37,10,0,66,51,
        7,21,85,27,31,63,75,4,95,99,
       11,28,61,74,18,92,40,53,59,8]).

/* ------------------------------------ */
/* Map colouring problem                */
/*  map(200) is advised.                */

map(N) :- statistics(runtime,[X|_]),
          map_loop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 68 * N,
          print_times(X,Now,M,Li).

map_loop(0).
map_loop(X) :- \+ \+ map_top, Y is X - 1, map_loop(Y).

map_top:-
	el(X1,[b]),
	el(X2,[r]),
	el(X7,[g]),
	el(X13,[w]),
	el(X3,[b,r,g,w]),
	\+(X2=X3),
	\+(X3=X13),
	el(X4,[b,r,g,w]),
	\+(X2=X4),
	\+(X7=X4),
	\+(X3=X4),
	el(X5,[b,r,g,w]),
	\+(X13=X5),
	\+(X3=X5),
	\+(X4=X5),
	el(X6,[b,r,g,w]),
	\+(X13=X6),
	\+(X5=X6),
	el(X8,[b,r,g,w]),
	\+(X7=X8),
	\+(X13=X8),
	el(X9,[b,r,g,w]),
	\+(X13=X9),
	\+(X4=X9),
	\+(X8=X9),
	el(X10,[b,r,g,w]),
	\+(X4=X10),
	\+(X5=X10),
	\+(X6=X10),
	\+(X9=X10),
	el(X11,[b,r,g,w]),
	\+(X11=X13),
	\+(X11=X10),
	\+(X11=X6),
	el(X12,[b,r,g,w]),
	\+(X12=X13),
	\+(X12=X11),
	\+(X12=X9),
	write(user,[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13]),nl.

map_top.
