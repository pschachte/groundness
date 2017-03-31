% CVS: $Id: semi.pl,v 1.3 1998/10/20 03:23:51 pets Exp $
/*----------------------------------------------------------------------------
Program:  Semigroup 
Author:   M. Carlsson
Date:     June 18 1990

Notes:
1. To run:
    ?- go(T,N).
where T is time and N should be output 313.

2. This version uses weird unrolling that for some unknown reason gives
it the best performance.  This may be because it avoids the append.

3. I played with the semigroup problem that Evan has been using in his
efforts to compare Prolog and GHC.  The attached program computes the
71-element semigroup in 210 msec on a Sun-4.  Ross's program (modified
by Evan) solved the problem in 21400 msec on the same machine.
----------------------------------------------------------------------------*/
%:- sequential.
%:- parallel member/2, umember/2.
goal :- go(N,T).
go(N,T) :- time(_), solution(X), time(T), count(X,N).

solution(Answer) :-
	g1(X),
	g2(Y),
	g3(Z),
	g4(W),
	%sort([X,Y,Z,W], Set),
	solution(Set, Set, Answer).

solution([], State, State).
solution([N|New0], State0, Sol) :-
	g1(N,N1), g2(N,N2), g3(N,N3), g4(N,N4),
	all_products(New0, Ps0, []),
	%sort([N1,N2,N3,N4|Ps0], Ps),
	merge_new(State0, Ps, State, New),
	solution(New, State, Sol).

all_products([],S,S).
all_products([X|Xs],[X1,X2,X3,X4|S0],S1) :-
	g1(X,X1), g2(X,X2), g3(X,X3), g4(X,X4),
	all_products(Xs,S0,S1).

/* merge_new(A, B, union(A,B), set_diff(B,A)). */

merge_new([], Set, Set, Set) :- !.
merge_new(Set, [], Set, []) :- !.
merge_new([O|Os], [N|Ns], Set, New) :-
	compare(C, O, N), 
	merge_new(C, O, Os, N, Ns, Set, New).
	
merge_new(<, O, [], N, Ns, [O, N|Ns], [N|Ns]) :- !.
merge_new(<, O1, [O|Os], N, Ns, [O1|Set], New) :-
	compare(C, O, N), 
	merge_new(C, O, Os, N, Ns, Set, New).
merge_new(=, _, Os, N, Ns, [N|Set], New) :-
	merge_new(Os, Ns, Set, New).
merge_new(>, O, Os, N, [], [N, O|Os], [N]) :- !.
merge_new(>, O, Os, N1, [N|Ns], [N1|Set], [N1|New]) :-
	compare(C, O, N), 
	merge_new(C, O, Os, N, Ns, Set, New).

time(T) :- statistics(runtime,[_,T]).

count(L,N) :- count(L,0,N).
count([X|Xs],M,N) :- M1 is M+1, count(Xs,M1,N).
count([],N,N).

/*
% 309+4 solutions: 
sos([tuple(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3, 4,4,4,4,4, 5,5,5,5,5,
                                 3,3,3,3,3, 5,5,5,5,5, 4,4,4,4,4),
     tuple(1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5,
                                 1,2,3,4,5, 1,3,2,4,5, 1,2,3,4,5),
     tuple(1,1,1,1,1, 2,2,2,2,2, 3,3,3,3,3, 5,5,5,5,5, 4,4,4,4,4,
                      2,2,2,2,2,            4,4,4,4,4, 3,3,3,3,3),
     tuple(1,2,3,5,4, 1,2,3,5,4, 1,2,3,5,4, 1,2,3,5,4, 1,2,3,5,4,
                                 1,2,3,4,5, 1,2,3,5,4, 1,2,3,5,4)]).
*/

%% Multiplication table,
m1(1,1). m1(2,1). m1(3,1). m1(4,1). m1(5,1).
m2(1,1). m2(2,2). m2(3,1). m2(4,4). m2(5,1).
m3(1,1). m3(2,1). m3(3,3). m3(4,1). m3(5,5).
m4(1,1). m4(2,1). m4(3,4). m4(4,1). m4(5,2).
m5(1,1). m5(2,5). m5(3,1). m5(4,3). m5(5,1).

%% Initial generators.
g1(*(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5)).
g2(*(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5)).
g3(*(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,5,5,5,5,5,4,4,4,4,4)).
g4(*(1,2,3,5,4,1,2,3,5,4,1,2,3,5,4,1,2,3,5,4,1,2,3,5,4)).

%% Compiled generators.
g1(*( _, _, _, _, _,Xf, _,Xh,Xi,Xj,Xk,Xl, _,Xn,Xo,
     Xp,Xq,Xr,Xs,Xt,Xu,Xv,Xw,Xx,Xy),
   *( 1, 1, 1, 1, 1,Yf, 2,Yh,Yi,Yj,Yk,Yl, 3,Yn,Yo,
     Yp,Yq,Yr,Ys,Yt,Yu,Yv,Yw,Yx,Yy)) :-
        m2(Xf,Yf),            m2(Xh,Yh), m2(Xi,Yi), m2(Xj,Yj), 
        m3(Xk,Yk), m3(Xl,Yl),            m3(Xn,Yn), m3(Xo,Yo), 
        m4(Xp,Yp), m4(Xq,Yq), m4(Xr,Yr), m4(Xs,Ys), m4(Xt,Yt), 
        m5(Xu,Yu), m5(Xv,Yv), m5(Xw,Yw), m5(Xx,Yx), m5(Xy,Yy).

g2(*( _,Xb,Xc,Xd,Xe, _, _,Xh,Xi,Xj, _,Xl, _,Xn,Xo,
      _,Xq,Xr,Xs,Xt, _,Xv,Xw,Xx,Xy),
   *( 1,Yb,Yc,Yd,Ye, 1, 2,Yh,Yi,Yj, 1,Yl, 3,Yn,Yo,
      1,Yq,Yr,Ys,Yt, 1,Yv,Yw,Yx,Yy)) :-
        m2(Xb,Yb),            m2(Xl,Yl), m2(Xq,Yq), m2(Xv,Yv), 
        m3(Xc,Yc), m3(Xh,Yh),            m3(Xr,Yr), m3(Xw,Yw), 
        m4(Xd,Yd), m4(Xi,Yi), m4(Xn,Yn), m4(Xs,Ys), m4(Xx,Yx), 
        m5(Xe,Ye), m5(Xj,Yj), m5(Xo,Yo), m5(Xt,Yt), m5(Xy,Yy).

g3(*( _, _, _, _, _,Xf, _,Xh,Xi,Xj,Xk,Xl, _,Xn,Xo,
     Xp,Xq,Xr,Xs,Xt,Xu,Xv,Xw,Xx,Xy),
   *( 1, 1, 1, 1, 1,Yf, 2,Yh,Yi,Yj,Yk,Yl, 3,Yn,Yo,
     Yp,Yq,Yr,Ys,Yt,Yu,Yv,Yw,Yx,Yy)) :-
        m2(Xf,Yf),            m2(Xh,Yh), m2(Xi,Yi), m2(Xj,Yj), 
        m3(Xk,Yk), m3(Xl,Yl),            m3(Xn,Yn), m3(Xo,Yo), 
        m4(Xu,Yu), m4(Xv,Yv), m4(Xw,Yw), m4(Xx,Yx), m4(Xy,Yy),
        m5(Xp,Yp), m5(Xq,Yq), m5(Xr,Yr), m5(Xs,Ys), m5(Xt,Yt). 

g4(*( _,Xb,Xc,Xd,Xe, _, _,Xh,Xi,Xj, _,Xl, _,Xn,Xo,
      _,Xq,Xr,Xs,Xt, _,Xv,Xw,Xx,Xy),
   *( 1,Yb,Yc,Yd,Ye, 1, 2,Yh,Yi,Yj, 1,Yl, 3,Yn,Yo,
      1,Yq,Yr,Ys,Yt, 1,Yv,Yw,Yx,Yy)) :-
        m2(Xb,Yb),            m2(Xl,Yl), m2(Xq,Yq), m2(Xv,Yv), 
        m3(Xc,Yc), m3(Xh,Yh),            m3(Xr,Yr), m3(Xw,Yw), 
        m4(Xe,Ye), m4(Xj,Yj), m4(Xo,Yo), m4(Xt,Yt), m4(Xy,Yy),
        m5(Xd,Yd), m5(Xi,Yi), m5(Xn,Yn), m5(Xs,Ys), m5(Xx,Yx). 

