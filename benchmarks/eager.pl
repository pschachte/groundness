% CVS: $Id: eager.pl,v 1.3 1998/10/21 04:25:54 pets Exp $
goal :- go.


q(a).

go :- p(a, b).
go :- p(_, _).

r(_).

p(X, Y) :- q(X), r(Y).
