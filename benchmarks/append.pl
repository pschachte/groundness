% CVS: $Id: append.pl,v 1.3 1998/10/20 03:23:25 pets Exp $
goal :- ground(Ground), append(D,Ground,D).
append([],L,L).
append([H,L1,X1,X2,X3,X4,X5,X6,X7],L2,[L3]) :-
        append(L1,L2,L3),append(X1,L2,L3),append(X2,L2,L3),append(X3,L2,L3),
        append(X4,L2,L3),append(X5,L2,L3),append(X6,L2,L3).
