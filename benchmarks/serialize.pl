% CVS: $Id: serialize.pl,v 1.3 1998/10/20 03:23:51 pets Exp $
%------------------------------------------------------------------------------
%	Benchmark Program - Palindrome
%
%	by D.H.D Warren
%	Date: 1977
%
%       from DhDW's thesis 
%
%	To test: run go(X,S). 
%          where X is output execution time, and S the solution for palin/1.
%------------------------------------------------------------------------------

goal :- serialize0(X, Y).

serialize0(L,R) :-
	pairlists(L,R,A),
	arrange0(A,T),
	numbered(T,1,N).

pairlists([X|L],[Y|R],[pair(X,Y)|A]) :- pairlists(L,R,A).
pairlists([],[],[]).

arrange0([X|L],tree(T1,X,T2)) :-
	split0(L,X,L1,L2),
	arrange0(L1,T1),
	arrange0(L2,T2).
arrange0([],void).

split0([pair(X1,N1)|L],pair(X2,Y2),[pair(X1,N1)|L1],L2) :-
	X1 < X2, !,
	split0(L, pair(X2,Y2), L1,L2).
split0([pair(X1,N1)|L],pair(X2,N2),L1,L2) :-
	X1 =:= X2, N1 = N2, !,
	split0(L,pair(X2,N2),L1,L2).
split0([pair(X1,N1)|L],pair(X2,Y2),L1,[pair(X1,N1)|L2]) :-
	X1 > X2, !,
	split0(L, pair(X2,Y2), L1,L2).
split0([],_,[],[]).

numbered(tree(T1,pair(X,N1),T2),N0,N) :-
	numbered(T1,N0,N1),
	N2 is N1 + 1,
	numbered(T2,N2,N).
numbered(void,N,N).

palin("ABLE WAS I ERE I SAW ELBA").

% ----------------------------------------------------------------------
% to test:

go(S) :- palin(X), serialize0(X,S).
