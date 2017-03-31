% CVS: $Id: treeorder.pl,v 1.3 1998/10/20 03:23:53 pets Exp $
goal :- v2t(X, Y, Z).

v2t(X, Y, Z) :-
    visits2tree(X, Y, Z).


visits2tree([], [], nil).
visits2tree([Root|Ant], Sym, t(Root, Left, Right)) :-
    split(Root, Ant, Sym, LeftAnt, RightAnt, LeftSym, RightSym),
    visits2tree(LeftAnt, LeftSym, Left),
    visits2tree(RightAnt, RightSym, Right).

split(Root, Ant, Sym, LeftAnt, RightAnt, LeftSym, RightSym) :-
    split(Sym, Root,  LeftSym,  RightSym),
    select(Ant, LeftSym, LeftAnt),
    select(Ant, RightSym, RightAnt).

split([], _, [], []).
split([S|Ss], N, [S|L], R) :-
    S \== N,
    split(Ss, N, L, R).
split([S|Ss], S, [], Ss).

select([], _, []).
select([A|As], Ss, [A|RSs]) :-
    member(A, Ss),
    select(As, Ss, RSs).
select([_|As], Ss, RSs) :-
    select(As, Ss, RSs).

member(X, [X|_]).
member(X, [_|T]) :-
    member(X, T).
