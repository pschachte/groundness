% CVS: $Id: disj_r.pl,v 1.3 1998/10/20 03:23:31 pets Exp $
goal :- top(K).

top(K) :- 
   setup(K,Ende,Disj).

makevars([],[]).
makevars([H|T],[[H,D,A]|R]):-
   duration(H,D),
   memberEl(A,1,200),
   makevars(T,R).

memberEl(A,A,U).
memberEl(A,L,U) :-
  L =< U,
  L1 = L + 1,
  memberEl(A,L1,U).

setup(K,Ende,Disj):-
   jobs(L),
   makevars(L,K),
   resources(R),
   prec(M),
   makeprec(M,K),
   maxnf(M1),
   makemaxnf(M1,K),
   maxef(M2),
   makemaxef(M2,K),
   minaf(M3),
   makeminaf(M3,K),
   minsf(M4),
   makeminsf(M4,K),
   minnf(M5),
   makeminnf(M5,K),
   makedisj(R,K,[],Disj),
   disjunct(Disj),
   el([stop,Dummy,Ende],K).

makeprec([],Dummy).
makeprec([[A,B]|R],L):-
   el([A,Ad,Aa],L),
   el([B,Bd,Ba],L),
   gteqc(Ba,Aa,Ad),
   makeprec(R,L).

gteqc(X,Y,C) :-
  X >= Y + C.

makemaxnf([],Dummy).
makemaxnf([[A,B,C]|R],L):-
   el([A,Ad,Aa],L),
   el([B,Bd,Ba],L),
   C1 = C + Ad,
   smeqc(Ba,Aa,C1),
   makemaxnf(R,L).

makemaxef([],Dummy).
makemaxef([[A,B,C]|R],L):-
   el([A,Ad,Aa],L),
   el([B,Bd,Ba],L),
          C1 = Ad + C - Bd,
   smeqc(Ba,Aa,C1),
   makemaxef(R,L).

smeqc(X,Y,C) :-
  X =< Y + C.

makeminaf([],Dummy).
makeminaf([[A,B,C]|R],L):-
   el([A,Ad,Aa],L),
   el([B,Bd,Ba],L),
   gteqc(Ba,Aa,C),
   makeminaf(R,L).

makeminsf([],Dummy).
makeminsf([[A,B,C]|R],L):-
   el([A,Ad,Aa],L),
   el([B,Bd,Ba],L),
          C1 = C - Bd,
   smeqc(Ba,Aa,C1),
   makeminsf(R,L).

makeminnf([],Dummy).
makeminnf([[A,B,C]|R],L):-
   el([A,Ad,Aa],L),
   el([B,Bd,Ba],L),
          C1 = C + Ad,
   gteqc(Ba,Ad,C1),
   makeminnf(R,L).

makedisj([],R,D,D).
makedisj([[H,R]|T],K,Din,Dout):-
   ellist(R,K,R1),
   makedisj1(R1,Din,D1),
   makedisj(T,K,D1,Dout).
   
makedisj1([],D,D).
makedisj1([H|T],Din,Dout):-
   makedisj2(H,T,Din,D1),
   makedisj1(T,D1,Dout).

makedisj2(H,[],D,D).
makedisj2([A,B],[[C,D]|S],Din,Dout):-
   makedisj2([A,B],S,[[A,B,C,D]|Din],Dout).

ellist([],Dummy,[]).
ellist([H|T],L,[[A,D]|S]):-
   el([H,D,A],L),
   ellist(T,L,S).

disjunct([]).
disjunct([[A,B,C,D]|R]):-
   disj(A,B,C,D),
   disjunct(R).

disj(Aa,Ad,Ba,Bd):-
   gteqc(Ba,Aa,Ad).
disj(Aa,Ad,Ba,Bd):-
   gteqc(Aa,Ba,Bd).

reverse(L,K):-
   rev(L,[],K).

rev([],L,L).
rev([H|T],L,K):-
   rev(T,[H|L],K).

el(X,[X|R]).
el(X,[Y|R]):-
   el(X,R).

jobs(L) :-
   ground_list(L).

ground_list([]).
ground_list([F|T]) :-
   ground(F),
   ground_list(T).

duration(A, B) :-
    ground(A), ground(B).

maxnf(A) :-
    ground(A).

minsf(A) :-
    ground(A).

maxef(A) :-
    ground(A).

minnf(A) :-
    ground(A).

minaf(A) :-
    ground(A).

resources(A) :-
    ground(A).

prec(A) :-
    ground(A).
