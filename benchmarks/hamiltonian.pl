% CVS: $Id: hamiltonian.pl,v 1.3 1998/10/21 04:25:58 pets Exp $

goal :- main.

main :-
	mham(1).

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

/* ---------------------------------------------- */
/*  Hamiltonian Graphs...                         */
/*  Extremely long (nearly half a million LI's !) */
/*  Only 1 advised !                              */

mham(N) :- statistics(runtime,[X|_]),
          mham_loop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 493824 * N,
          print_times(X,Now,M,Li).

mham_loop(0).
mham_loop(X) :- \+ \+ mham_top, Y is X - 1, mham_loop(Y).

mham_top:-
        cycle_ham([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t],X),
        fail.
mham_top.

cycle_ham([X|Y],[X,T|L]):-
        chain_ham([X|Y],[],[T|L]),
        edge(T,X).

chain_ham([X],L,[X|L]).
chain_ham([X|Y],K,L):-
        delete(Z,Y,T),
        edge(X,Z),
        chain_ham([Z|T],[X|K],L).

delete(X,[X|Y],Y).
delete(X,[U|Y],[U|Z]):-
        delete(X,Y,Z).

edge(X,Y):-
        connect(X,L),
        el(Y,L).

connect(0,[1,2,3,4,5,6,7,8,9]).
connect(1,[0,2,3,4,5,6,7,8,9]).
connect(2,[0,1,3,4,5,6,7,8,9]).
connect(3,[0,1,2,4,5,6,7,8,9]).
connect(4,[0,1,2,3,5,6,7,8,9]).
connect(5,[0,1,2,3,4,6,7,8,9]).
connect(6,[0,1,2,3,4,5,7,8,9]).
connect(7,[0,1,2,3,4,5,6,8,9]).
connect(8,[0,1,2,3,4,5,6,7,9]).
connect(9,[0,1,2,3,4,5,6,7,8]).

connect(a,[b,j,k]).
connect(b,[a,c,p]).
connect(c,[b,d,l]).
connect(d,[c,e,q]).
connect(e,[d,f,m]).
connect(f,[e,g,r]).
connect(g,[f,h,n]).
connect(h,[i,g,s]).
connect(i,[j,h,o]).
connect(j,[a,i,t]).
connect(k,[o,l,a]).
connect(l,[k,m,c]).
connect(m,[l,n,e]).
connect(n,[m,o,g]).
connect(o,[n,k,i]).
connect(p,[b,q,t]).
connect(q,[p,r,d]).
connect(r,[q,s,f]).
connect(s,[r,t,h]).
connect(t,[p,s,j]).
