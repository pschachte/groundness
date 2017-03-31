% CVS: $Id: shape.pl,v 1.3 1998/10/21 04:26:15 pets Exp $
:- entry_mode(shapes(any, any)).
goal :- shapes(_,_).

shapes(Shape,N) :-
    lines(Shape,Vars),
    numbervars(Vars,0,N).

lines([],[]).
lines([L],[V]) :- pairs((L,L),(V,V)).
lines([L1,L2|Ls],[V1,V2|Vs]) :-
    pairs((L1,L2),(V1,V2)),
    lines([L2|Ls],[V2|Vs]).

pairs(([],[]),([],[])).
pairs(([X1],[X2]),([V1],[V2])) :- check(X1,X2,V1,V2).
pairs(([X11,X12|X1s],[X21,X22|X2s]), ([V11,V12|V1s],[V21,V22|V2s])) :-
    check(X11,X12,V11,V12),
    check(X11,X21,V11,V21),
    check(X11,X22,V11,V22),
    check(X12,X21,V12,V21),
    pairs(([X12|X1s], [X22|X2s]), ([V12|V1s], [V22|V2s])).

check(black,black,t,t).
check(black,white,t,_).
check(white,black,_,t).
check(white,white,X,X).




