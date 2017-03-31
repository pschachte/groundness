% CVS: $Id: ili.pl,v 1.3 1998/10/21 04:25:59 pets Exp $
/*****************************************************************************

Intuitionistic Logic Interpreter.
Written by Seif Haridi.
Translated to Quintus Prolog by Evan Tick 9/13/85.

*****************************************************************************/

goal :- s(_).

:- op(900, xfx, <->).
:- op(890, xfy, =>).
:- op(880, xfx, <-).
:- op(870, xfy, #).
:- op(860, xfy, &).
:- op(500, xfx, :).

s(P) :- sb([],P,[]).

sb(_L,tt,_CE) :- !.
sb(L,F1&F2,CE) :-  !,
    sb(L,F1,CE),
    sb(L,F2,CE).
sb(L,F1#F2,CE) :- !, sf(L,F1#F2,CE).
sb(L,a(V,F),CE) :- !,
    replace(X/V,a(V,F),a(X,F1)),
    star(X),
    freeVars(a(X,F1),VL),
    sb(L,F1,CE),
    checkBinding(VL,X).
sb(L,e(V,F),CE) :- !, sf(L,e(V,F),CE).
sb(L,F1<->F2,CE) :- !,
    sb(L,F1=>F2,CE),
    sb(L,F2=>F1,CE).
sb(L,F1=>F2,CE) :-  !, sb([F1|L],F2,CE).
sb(L,T1=T2,CE) :- !, sf(L,T1=T2,CE).
sb(_L,P,_CE) :- builtin(P), !, call(P).		/* not complete */
sb(L,A,CE) :- sf(L,A,CE).

sf([],e(V,F),CE) :-  !,
    replace(X/V,e(V,F),e(X,F1)),
    sb([],F1,CE).
sf([],F1#F2,CE) :-  !,
    (sb([],F1,CE) | sb([],F2,CE)).
sf([],T1 = T2,CE) :- !,
    unifyb(T1,T2,CE).
sf([],A,CE) :-  !,
  (findStatement(A,H<-B),
     unifyb(A,H,CE),
     sb([],B,CE)
    |
     findAtom(A,H,CE),
     unifyb(A,H,CE)).
sf([ff|_FR],_CF,_CE) :- !.
sf([F1#F2|FR],CF,CE) :- !,
    sf([F1|FR],CF,CE),
    sf([F2|FR],CF,CE).
sf([F1&F2|FR],CF,CE) :- !,
     sf([F1,F2|FR],CF,CE).
sf([a(V,F)|FR],CF,CE) :-  !,
     replace(X/V,a(V,F),a(X,F1)),
     sf([F1|FR],CF,CE).
sf([e(V,F)|FR],CF,CE) :-  !,
     replace(X/V,e(V,F),e(X,F1)),
     star(X),
     freeVars(e(X,F1),VL),
     sf([F1|FR],CF,CE),
     checkBinding(VL,X).

sf([F1<->F2|FR],CF,CE) :-  !,
   sf([F1=>F2,F2=>F1|FR],CF,CE).
sf([F1=>F2|FR],CF,CE) :- !,
    (sb(FR,F1,CE),
     sf([F2|FR],CF,CE)
    |
     sf(FR,CF,CE) ).
sf([T1 = T2|FR],CF,CE) :- !,
   X=sf([T1 = T2|FR],CF,CE),
   unifyf(T1,T2,CE,CE1,X),
   (CE1 = fail
    | sf(FR,CF,CE1) ) .
sf([Atom|FR],CF,CE) :-
   findStatement(Atom,H<->B), 
   unifyb(Atom,H,CE),
   append(FR,[B],FR1),
   sf(FR1,CF,CE).
sf([Atom|FR],CF,CE) :-
   sf(FR,CF,[Atom|CE]).

star(*(_)).

findAtom(A,A,E) :-
      atom(A), member(A,E), !.
findAtom(A,H,E) :-
      functor(A,F,N),
      functor(H,F,N),
      member(H,E).

freeVars(X,[]) :- isstar(X), !.
freeVars(X,[X]) :- var(X), !.
freeVars(X,L)  :-
     isdelay(X), !,
     dereference(X,X2,[]),
     (isdelay(X2) -> L=[X2] | L=[]).
freeVars(A,[]) :- atomic(A), !.
freeVars(a(X,F),V) :- !,
     freeVars(F,V1),
     del(X,V1,V).
freeVars(e(X,F),V) :- !,
     freeVars(F,V1),
     del(X,V1,V).
freeVars(T,V) :-
     compound(T),
     T =.. [_|Ts],
     freeVarsList(Ts,V).

freeVarsList([],[]).
freeVarsList([T|Ts],V) :-  !,
     freeVars(T,V1),
     freeVarsList(Ts,V2),
     append(V1,V2,V).

/* checkBinding(L,V) :-  \+in(L,V), !. */

checkBinding([],_) :- !.
checkBinding([X|L],V) :- 
    ( var(X) | isstar(X) ), !, 
    X \== V,
    checkBinding(L,V).
checkBinding([X|L],V) :-
    isdelay(X), !,
    dereference(X,X2,[]),
    (isdelay(X2) -> 
	 makedelay(X2,checkBinding([X],V)),
	 checkBinding(L,V)
	|
	 checkBinding([X2|L],V)
    ).
checkBinding([X|L],V) :- atomic(X), !,
   checkBinding(L,V).
checkBinding([X|L],V) :- compound(X), !,
     X =.. [_|T],
     checkBinding(T,V),
     checkBinding(L,V).

findStatement(_#_,_) :- !, fail.
findStatement(_&_,_) :- !, fail.
findStatement(P,H<-B) :-
      functor(P,F,N),
      functor(H,F,N),
      H <- B.
findStatement(P,H<-B) :-
       findStatement(P,H<->B).
findStatement(P,H<->B) :-
      functor(P,F,N),
      functor(H,F,N),
      H <-> B.

unifyb(T1,T2,E) :-  unifyb1([T1],[T2],E). 
unifyb1([],[],_) :- !.
unifyb1([X|L1],[Y|L2],E) :-
      dereference(X,X1,E),
      dereference(Y,Y1,E),
      unifyb2([X1|L1],[Y1|L2],E).

unifyb2([X|L1],[Y|L2],E) :- var(X), !,
      X=Y, unifyb1(L1,L2,E).
unifyb2([X|L1],[Y|L2],E) :- isstar(X), !,
      (var(Y)  -> X = Y |
       isstar(Y) -> X == Y |
       (isdelay(Y),  binddelay(Y,X)) ),
       unifyb1(L1,L2,E).
unifyb2([X|L1],[Y|L2],E) :- isdelay(X), !,
      (var(Y) -> X=Y |    
       isdelay(Y) -> (X\==Y -> joindelay(X,Y))|
       binddelay(X,Y)),
      unifyb1(L1,L2,E).
unifyb2([X|L1],[Y|L2],E) :- atomic(X), !,
      (var(Y) -> X=Y |
       isdelay(Y) -> binddelay(Y,X) |
       atomic(Y) -> X=Y |
       fail),
       unifyb1(L1,L2,E).
unifyb2([X|L1],[Y|L2],E) :- compound(X), !,
       (var(Y) -> (X=Y, unifyb1(L1,L2,E)) |
        isdelay(Y) -> (binddelay(Y,X), unifyb1(L1,L2,E)) |
        compound(Y) ->
              (functor(X,F,N), functor(Y,F,N),
               (X =.. [F|S1]), (Y =.. [F|S2]),
               append(S1,L1,M1), append(S2,L2,M2),
               unifyb1(M1,M2,E)) |
        fail
       ).

dereference(X,X1,E) :-
      (var(X) -> X=X1 |
       isstar(X) ->
              (binding(X,E,X2) -> dereference(X2,X1,E)| 
               X=X1) |
       isdelay(X) ->
              (hasdelayvalue(X,V) -> dereference(V,X1,E)| 
               X=X1) |
       X=X1).

isstar(X) :- nonvar(X), X = *(_).

isdelay(X) :- nonvar(X), X = delay(_,_).

hasdelayvalue(delay(V,_),V2) :-  nonvar(V), V=V2.

makedelay(delay(_,F),X) :- appvar(F,[X|_]).

binddelay(delay(V,F),V) :- calllist(F).

calllist(E) :- var(E), !.
calllist([H|T]) :- call(H), calllist(T).

joindelay(delay(V1,F1),delay(V2,F2)) :-
       V1=delay(V2,F2),
       appvar(F2,F1).

/*
clean(Dirty, Clean) :-
   clean(Dirty, 0, Clean, _).

clean(Var, Index, Var, Index) :- var(Var), !.
clean(delay(NonVar, _), Index, NonVar, Index) :- nonvar(NonVar), !.
clean(delay($(Index0),DirtyGoals), Index0, $(Index0):CleanGoals, Index) :- !,
   Index1 is Index0 + 1,
   clean(DirtyGoals, Index1, CleanGoals, Index).
clean(DirtyTerm, Index0, CleanTerm, Index) :-
   DirtyTerm=..[F|DirtyArgs],
   cleanlist(DirtyArgs, Index0, CleanArgs, Index),
   CleanTerm =.. [F|CleanArgs].

cleanlist([], Index, [], Index).
cleanlist([Dirty|DirtyArgs], Index0, [Clean|CleanArgs], Index) :-
   clean(Dirty, Index0, Clean, Index1),
   cleanlist(DirtyArgs, Index1, CleanArgs, Index).

portray(Dirty) :- 
   \+ \+ (clean(Dirty, Clean), write(Clean)).
*/

unifyf(T1,T2,E1,E2,D) :- unifyf1([T1],[T2],E1,E2,D).

unifyf1([],[],E,E,_) :- !.
unifyf1([X|L1],[Y|L2],E1,E2,D) :-
      dereference(X,X1,E1),
      dereference(Y,Y1,E1), 
      unifyf2([X1|L1],[Y1|L2],E1,E2,D).

unifyf2([X|L1],[Y|L2],E1,E2,D) :- var(X), !,
      (isstar(Y) -> (E3 = [(Y=X)|E1], unifyf1(L1,L2,E3,E2,D))| 
       (makedelay(X,D), E2=fail )).

unifyf2([X|L1],[Y|L2],E1,E2,D) :- isstar(X), !,
       E3 = [(X=Y)|E1], unifyf1(L1,L2,E3,E2,D).

unifyf2([X|L1],[Y|L2],E1,E2,D) :- isdelay(X), !,
       (isstar(Y)  -> (E3 = [(Y=X)|E1], unifyf1(L1,L2,E3,E2,D))| 
        (makedelay(X,D), E2 = fail)).

unifyf2([X|L1],[Y|L2],E1,E2,D) :- atomic(X), !,
       (atomic(Y) -> (X==Y -> unifyf1(L1,L2,E1,E2,D)| 
                        E2=fail)|
        compound(Y) -> E2=fail|
        isstar(Y) -> (E3 = [(Y=X)|E1], unifyf1(L1,L2,E3,E2,D))| 
        (makedelay(Y,D), E2=fail)).

unifyf2([X|L1],[Y|L2],E1,E2,D) :- compound(X), !,
        (compound(Y) ->
                ((functor(X,F,N), functor(Y,F,N)) ->
                      ((X =.. [F|S1]), (Y =.. [F|S2]),
                       append(S1,L1,M1), append(S2,L2,M2),
                       unifyf1(M1,M2,E1,E2,D))|
                 E2=fail)|
         atomic(Y) -> E2=fail|
         isstar(Y) -> (E3 = [(Y=X)|E1], unifyf1(L1,L2,E3,E2,D))| 
         (makedelay(Y,D), E2=fail)).

binding(X,[Y = T|_],T) :- X == Y, !.
binding(X,[_|L],T) :- 
    binding(X,L,T).

deref(X,E,T) :-  binding(X,E,T1), !,
    deref(T1,E,T).
deref(X,_,X).

/* utilities   */

rev(L1,L2) :-  rev([],L1,L2).
rev(L,[],L).
rev(L1,[X|L2],L3) :- rev([X|L1],L2,L3).

apply_all(_,[]).
apply_all(R,[X|Y]) :-
    apply(R,[X]),
    apply_all(R,Y).

apply_all(_,_,[]) :- !.
apply_all(R,C,[X|Y]) :-
         apply(R,[X,C]), !,
         apply_all(R,C,Y).

apply_either(R,C,M,[X|Y]) :-
        (apply(R,[X,C,M]) |
         apply_either(R,C,M,Y)).

apply_or(R,C,M,[X|Y]) :-
        (apply(R,[X,C,M]) |
         apply_or(R,C,M,Y)).

apply_or(M,[X|Y],E) :-
        (apply(M,[X,E]) |
         apply_or(M,Y,E)).

apply(R,Ts) :-
   X =.. [R|Ts],
   call(X).

apply_list(_,[],[]) :- !.
apply_list(R,[X1|L1],[X2|L2]) :-
    apply(R,[X1,X2]), !,
    apply_list(R,L1,L2).

apply_list(_,[],_,[]) :- !.
apply_list(R,[X1|L1],C,[X2|L2]) :-
    apply(R,[X1,C,X2]), 
    apply_list(R,L1,C,L2).

apply_list(_,[],_,_,[]) :- !.
apply_list(R,[X1|L1],C1,C2,[X2|L2]) :-
    apply(R,[X1,C1,C2,X2]), 
    apply_list(R,L1,C1,C2,L2).

iterate(_,[],X,X) :- !.
iterate(R,[X|L],B,B2) :-
       apply(R,[X,B,B1]),
       iterate(R,L,B1,B2).

/* not in use...
flatten([],[]) :- !.
flatten([X|L1],L2) :-
     element(X), !,
     flatten(L1,L3),
     union([X],L3,L2).
flatten([X|L1],L2) :-
     flatten(X,X1),
     flatten(L1,L3),
     union(X1,L3,L2).
*/

element(X) :- \+ list(X).

list([]).
list([_|_]).

compound(T) :-
   nonvar(T),
   \+atomic(T), !.

member(X,[X|_]).
member(X,[_|Z]) :- member(X,Z).

writel([]) :- !.
writel([X|L]) :- 
    nl, write(X), !,
    write(L).

/* replace(structure,oldv,newv,newstructure)      */

replace(N/O,X,N) :- O == X, !.
replace(_/_,X,X) :- (atomic(X) | var(X) | isstar(X)), !.
replace(N/O,S,S1) :-
     compound(S),
     S =.. [F|Ts],
     apply_list1(replace,Ts,N/O,Ts1),
     S1 =.. [F|Ts1].

apply_list1(_,[],_,[]) :- !.
apply_list1(R,[X1|L1],C,[X2|L2]) :-
    apply(R,[C,X1,X2]), apply_list1(R,L1,C,L2).

in(X,Y) :- X == Y.
in(P,X) :-
    compound(P),
    P =.. [_|Ts],
    apply_or(in,Ts,X).

del(_,[],[]).
del(X,[Y|L],L1) :- X==Y, !, del(X,L,L1).
del(X,[Y|L],[Y|L1]) :- del(X,L,L1).

delete(L1,L2,L3) :- iterate(del,L1,L2,L3).

head(H) :- (atom(H) ; integer(H) ; functor(H,F,_), F \== (:-)).

append([],L,L).
append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).


appvar(X,L) :- var(X), !, X = L.
appvar([_|X],L) :- appvar(X,L).

/*
builtin(P) :-				F.P. put this in - I don't understand
   predicate_property(P,built_in), !.	I replaced it with original, below
*/

builtin(diff(_,_,_)).
builtin(prod(_,_,_)).
builtin(quot(_,_,_)).
builtin(sum(_,_,_)).
builtin(rem(_,_,_)).
builtin(eq(_,_)).
builtin(gt(_,_)).
builtin(ge(_,_)).
builtin(le(_,_)).
builtin(lt(_,_)).
builtin(ne(_,_)).
builtin(atom(_)).
builtin(int(_)).
builtin(skel(_)).
builtin(var(_)).
builtin(write(_)).
builtin(ax(_,_)).
builtin(delax(_)).
builtin(op(_,_,_)).
builtin(fail).
builtin(op(_,_,_)).
builtin('=/'(_,_)).

/* queries */

q(1, a & b <-> b & a, provable).
q(2, a # b <-> b # a, provable).
q(3, (a & b) & c <-> (a & b) & c, provable).
q(4, (a # b) #c <-> a# (b # c), provable).
q(5, a # (b & c) <-> (a # b) & (a # c), provable).
q(6, a & (b # c) <-> (a & b) # (a & c), provable).
q(7, a => ((a => ff) => ff), provable).
q(8, (a => (b => c)) => (a & b => c), provable).
q(9, a => (b => a), provable).
q(10, a => ((a => ff) => b), provable).
q(11, ((a # b) => ff) => (a => ff) & (b => ff), provable).
q(12, ((a => ff) # (b => ff)) => (a & b => ff), provable).
q(13, ((a => ff) # b) => (a => b), provable).
q(14, (a => b) => ((b => ff) => (a => ff)), provable).
q(15, (a => b) => ((b => c) => (a => c)), provable).
q(16, ff <-> a & (a => ff), provable).
q(17, e(X, p(X) # r(X)) => e(Y, p(Y)) # e(Z, r(Z)), provable).
q(18, a(X, p(X) & r(X)) => a(Y, p(Y)) & a(Z,r(Z)), provable).
q(19, (e(X, p(X)) => ff) => a(Y, p(Y) => ff), provable).
q(20, e(X, p(X) => ff) => (a(Y, p(Y)) => ff), provable).
q(21, a(X, b => p(X)) => (b => a(Y, p(Y))), provable).
q(22, e(X, a => p(X)) => (a => e(Y, p(Y))), provable).
q(23, b # a(X, p(X)) => a(Y, b # p(Y)), provable).
q(24, b & e(X, p(X)) => e(Y, b & p(Y)), provable).
q(25, e(X, p(X) => b) => (a(Y, p(Y)) => b), provable).
q(26, a(X, p(X) => b) => (e(Y, p(Y)) => b), provable).
q(27, a(X, e(Y, Y = X)), provable).
q(28, e(X, a(Y, Y = X)), unprovable).
q(29, e(X, a(Y, f(Y) = X)), unprovable).
q(30, a(X, professor(X) => e(Y, teaches(X,Y))), provable).
q(31, e(X, e(Y, (X = Y => ff))), provable).
q(32, e(X, e(Y, (X = Y => ff) & X = 1 & Y = 2)), provable).
q(32, e(X, a(Y, X = Y => X = Y)), provable).
q(33, e(X, a(Y, X = Y => ff)), unprovable).
q(34, a(X, e(Y, X = Y => ff)), unprovable).
q(35, a(X, e(Y,(X = Y => Y = 1) & Y=1)), provable).
q(36, e(X, e(Y,(X = Y => ff) & X = 1 & Y = 1)), unprovable).
q(100, a(X, brother(X, _) => male(X)), provable).

% following proofs added by ET 8-10-86
q(101, e(X, lessall(X,[b,c,d])), provable).
q(102, e(X, lessall(X,[a,b,c,d])), unprovable).

mtest :-
    test(X, P, R),
    write('test '), write(X), write(': '), write(P), nl,
    write(R),
    nl, nl, fail.
mtest.

test(X, P, R) :-
   q(X, P, R0),
   ( s(P) -> agree(provable, R0, R)
   ; agree(unprovable, R0, R) ).

agree(R, R, success) :- !.
agree(_, _, failure).

test :-
	mtest,!,
	new(C1,C2,O,C),		% dirty, but it does the job
	fail.

new(C1,C2,O,C) :-
   sb([a(C1,a(C2,border(C1,C2)=>border(C2,C1))),
    e(O,ocean(O)&border(switzerland,O))=>false],
   country(C)&contain(europe,C),
   []).

% Test formulas

country(france) <- tt.
border(france,spain) <- tt.
contain(europe,C) <- country(C) & border(C,france).
country(spain) <- tt.
border(france,switzerland) <- tt.
country(switzerland) <- tt.
contain(europe,france) <- tt.
 
professor(X) <-> X = r # X = t.

teaches(r, mmk) <- tt.
teaches(r, spv) <- tt.
teaches(t, lp) <- tt.

employeeList(D,L) <-> a(X, employee(D,X) <-> listMember(X,L)).

el(D,L) <-> a(X, em(D,X) <-> listMember(X,L)).

employee(D,X) <->
   (D = cs & (X = r # X = s)) #
   (D = ts & (X = j # X = a)).

em(D,X) <->
   (D = cs & X = s) #
   (D = ts & X = a).

listMember(X, []) <-> ff.
listMember(X, [U|R]) <-> X = U # listMember(X,R).

mathMajor(X) <-> a(Y, mathCourse(Y) => takes(X, Y)).

takes(X, Y) <->
   (X = d & Y = c3) #
   (X = j & Y = c1) #
   (X = j & Y = c3).

mathCourse(Z) <-> Z = c1 # Z = c3.

subset(X, Y) <-> a(E, listMember(E, X) => listMember(E, Y)). 

equalSets(X, Y) <-> subset(X, Y) & subset(Y, X).

union(X, Y, Z) <->
   a(E, listMember(E, X) # listMember(E, Y) <-> listMember(E, Z)).


lessall(X, L) <-> a(Y,listMember(Y, L) => less(X, Y)).

lessl(a, b) <- tt.
lessl(b, c) <- tt.
lessl(c, d) <- tt.

less(X, Y) <-> lessl(X, Y) # e(Z, lessl(Z,Y) & less(X,Z)).

unique(X) <->
   a(E1, a(E2, listMember(E1, X) & listMember(E2, X) => E1 = E2)).

u([]) <-> tt.
u([X|L]) <-> a(E, m(E, L)  => E = X).

ul([]) <-> tt.
ul([X|L]) <-> ul(X, L).

ul(X, []) <-> tt.
ul(X, [X|L]) <-> ul(X, L).

m(X,L) <-> e(U, e(L1, L = [U|L1] & ( X = U # m(X,L1)))).

brother(X, Y) <-> male(X) & sibling(X, Y).

uniqMember(X, L) <->
      e(T, L = [X|T] & (memberList(X,T) => ff)) #
      e(T, e(X2, L = [X2|T] & (X = X2 => ff) & uniqMember(X,T))).

uniqUnion(X, Y, Z) <->
   a(E, uniqMember(E, X) # uniqMember(E, Y) <-> uniqMember(E, Z)).


