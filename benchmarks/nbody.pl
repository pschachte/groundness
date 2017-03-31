% CVS: $Id: nbody.pl,v 1.3 1998/10/20 03:23:42 pets Exp $
/*-------------------------------------------------------------------------
Program: N-Body Problem for Star Clusters
Author:  E. Tick
Date:    August 16 1989
Notes:

1. To run this program,
       ?- go(M,G).
where input M is the number of simulated time steps and G=N^(-1/3), where
N is the number of stars (this is just for accuracy, so it can be set to
zero during testing...)

2. This program requires an input database of stars, of the form:

star(X1,Y1,Z1).
star(X2,Y2,Z2).
...
star(Xn,Yn,Zn).

where each star is defined by a position vector in 3 dimensions.  The values
of X,Y,Z are fixed point, real numbers.  This database can be loaded from 
another file or included in this file.

3. This program assumes that all stars have equal mass.  The gravitational
constant times this mass, G*m, is removed from all computations, thus the
data is in effect scaled by 1/G*m.

4. The star position data was generated with the cluster model given by
S.J. Aarseth et. al. (p.186).  This model assumes that each star has equal
mass 1/N, and that the star distribution forms a uniform spherical mass.
Thus the mass as a function of radius (from some origin) is:

         M(r) = r^3(1+r^2)^(-3/2)

or inversely

          r = (K1^(-2/3) - 1)^(-1/2)

The trick is to uniformly select a random K1 for each star to determine its
radius from the origin.  At that point, one must randomly pick where the star
sits on the sphere defined by that radius.  This is done by two additional
random numbers as outlined by Aarseth.

5. The main force (acceleration) equation is: for star i,

         f(i,j) = [xi-xj, yi-yj, zi-zj] /
                  (
                       ((xi-xj)^2 + (yi-yj)^2 + (zi-zj)^2 + k) *
                   sqrt((xi-xj)^2 + (yi-yj)^2 + (zi-zj)^2 + k)
                  )

where k is some fudge constant that helps keep accuracy (according to Makino).
He claims k=N^(-2/3) is best value.  In any case, notice that a square root
is needed once per star (see next point).

6. Actually we don't need sqrt(p) for any p, but specifically we need 1/r, i.e.,

           1/sqrt((xi-xj)^2 + (yi-yj)^2 + (zi-zj)^2 + k)

The extra k has entered into things because of our previous kludge for accuracy.
Makino gives a fast iterative approximation algorithm for this instance.  Let

         r0 = (|xi-xj| + |yi-yj| + |zi-zj| + g)/2

where g = k^(1/2) = N^(-1/3).  Then,

         a0 = 2*r0/(r0^2 + r^2)

where 

         r^2 = ((xi-xj)^2 + (yi-yj)^2 + (zi-zj)^2 + k)

(I think this k belongs here, but I am not sure...)  By iteration,

         a(i+1) = (3*a(i) - (a(i)^3*r^2))/2

Makino says 4--5 iterations should be sufficient for convergence.  Notice
only multiplication and division by two is used.  Let us call this function
h(a0,r^2).  Then

         f(i,j) = [xi-xj, yi-yj, zi-zj] * h(a0,r^2) / r^2

                = [(xi-xj)*h(a0,r^2)/r^2,
                   (yi-yj)*h(a0,r^2)/r^2,
                   (zi-zj)*h(a0,r^2)/r^2]

7. Assuming four iterations per h(), each star-star force computation requires:

          addition            20
          multiplication      15 
          division             2
          divide-by-two        9

The number of divisions above is minimal (one in a0 computation, other in
h(a0,r^2)/r^2 computation).  However if this causes inaccuracies, the latter
calculation should be done after multiplication in final equation, giving
two additional divisions.

8. How to make an oct-tree.  Given the star/3 facts, we create a list of
leaf/5 facts.  From this we create an oct-tree, composed of node/10 nodes and
leaf/5 leafs.

leaf(Id,_,o(Xx,Xy,Xz),o(Vx,Vy,Vz),o(Ax,Ay,Az))

node(o(Xx,Xy,Xz),N,[S1,S2,S3,S4,S5,S6,S7,S8])

where o(Xx,Xy,Xz) is the center of the quadrant, N is the number of stars in
the quadrant, and S1..S8 are the subquadrants (either nodes, leafs, or unbound).
To easily create the oct-tree, it helps to restrict the master cube to have
equal fixed dimensions, e.g., 20 x 20 x 20.  

9. Optimizations...
I don't think the last leaf argument (acceleration) is necessary.
Note that each leaf has a junk second argument (it was used to create the 
oct-tree).  This could be removed if we rebuild the tree.  Also, the list of
eight subquadrants is not necessary after building --- the uninstantiated
(empty) subquadrants can be eliminated (the traversal procedure doesn't care,
it treats all subquadrants equally).  In addition, the leaf id number may not
be necessary.  I added it to ensure that we do not attempt to calculate the
force of a star upon itself (division by zero).  Comparing the position
vectors may be sufficient.  All these would save a significant percentage of
the space required by the oct-tree, but would require a pass of recopying.

When we calculate the
accelerations, and with them the new velocities and positions, we cannot
destructively update the tree.  Thus we have to copy the tree.  In fact, it
is quite difficult to implement this update by copying, because the accelerations
are in a list with no order.  Thus is it easier to actually rebuild the 
oct-tree from scratch.  An advantage of rebuilding from scratch is that we then
never have to worry about stars straying into the same quadrant.  On the other
hand, a real system probably checks for this only once every several simulated
cycles.  A disadvantage is that the previous space saving optimizations cannot
be implemented once at the start: a second tree-rebuilding pass would be 
required for EACH cycle.

10. some questions:
   a. is my sqrt approximation function correct, given the kludge factor?
   b. discuss how the oct-tree is built in FORTRAN -- seems my method is hairy...
   c. what is good value for theta?
   d. is dim(10) really best?
   e. how often do you rebuild the tree, since the stars are moving...
   f. when calculating new position, do we use new velocity or old velocity?
   g. what is the proper time step?  This is important!

11. There is weird case when the star sits in the dead center of a quadrant.
This must constantly be tested to avoid divide-by-zero.  This forces us to
ensure that all distances are floating point, to allow unification tests.
However, when we make the master cube dimension, dim(X), floating point, the
time to build the tree increased by a factor of FIVE!  Another method is
to check the vectors element by element using =:= (in ok/4).  In any case,
ok/4 is drastically debilitated by this check.  In general, it is not a
good idea to use hybrid mixtures of integer and real arithmetic in Prolog.


12. CURRENT BUGS: this version strangely slows down as each simulated time
step progresses.  This seems to be due to a slowdown in the sqrt function.
How can this be?  I can only hypothesize that floating point numbers are
implemented in some strange way that makes them run more slowly as they get
larger.  In the current version, the whole thing freezes shut in the third
simulated time step, inside sqrt!

Another bug is that as the stars move, they may escape from the predefined
quadrant of 20 x 20 x 20.  Actually I don't understand if the current test
data is valid in this regard.
--------------------------------------------------------------------------*/
goal :- ground(Ground), go(Ground,N).
delta_time(0.0001). % simulation time step
theta(0.2).         % make smaller for more accuracy -- what is a good value?
dim(10.0).          % largest quadrant dimension (K x K x K) 
dim2(20.0).         % master cube has dimensions (2K x 2K x 2K)

nOde(node(_,_,_)).
n(node(_,N,_),N).
leaf(leaf(_,_,_,_,_)).

go(N,G) :- 
    collect(Stars),
    time(_),    
    step(N,Stars,G).

step(0,Stars,_) :- !,
    nl,printStars(Stars),nl.
step(T,Stars,G) :- T1 is T-1,
    nl,printStars(Stars),nl,
    write('building...'),
    buildTree(Stars,Tree),
    printTime,
    printTree(Tree),
    write('traversing...'),
    traverse(Stars,Tree,G,NewStars),
    printTime,
    step(T1,NewStars,G).

/*
% this version does its own GC but I don't think this is required...
%
%
:- dynamic tic/1,wrap/1.

g(0).               % kludge factor: zero is ok for debugging...
go(N) :-
    collect(Stars),
    time(_),
    myput(N,Stars),
    step.

myput(T,Stars) :- assert(tic(T)), putStars(Stars), !.

putStars([]).
putStars([L|Ls]) :- assert(wrap(L)), putStars(Ls).

myget(T,Stars) :- retract(tic(T)), myfindall(Stars), !.

myfindall([L|Ls]) :- retract(wrap(L)),myfindall(Ls).
myfindall([]).

checkwrap :- findall(L,wrap(L),Q),length(Q,I),nl,write('check wrap = '),write(I),nl,!.

step :- 
    myget(T,Stars),
    step(T,Stars,T1,NewStars),
    myput(T1,NewStars),
    checkwrap,
    write('about to fail...'),
    fail.                         % homebrew GC...
step :- 
    tic(0),!,myget(_,Stars),      % end of simulation at tic=0
    nl,printStars(Stars),nl.
step :- 
    checkwrap,
    step.                         % continue simulation

step(T,Stars,T1,NewStars) :-
    T1 is T-1,
    nl,printStars(Stars),nl,
    write('building...'),
    buildTree(Stars,Tree),
    printTime,
    printTree(Tree),
    write('traversing...'),
    g(G),
    traverse(Stars,Tree,G,NewStars),
    printTime,!.                  % this cut should not be needed!!!!
*/

%--------------------------------------------------------------
% tree-traversal: 

/* 
% finds leafs by traversing tree... 
% this method is bad for Aurora, but has advantage of not
% requiring star list...
traverse(Root,G,S) :-
    Root = node(_,_,Node),
    traverseNode(Node,Root,G,S,[]).

traverseNode([],_,_,S,S) :- !.
traverseNode([Q|Qs],Root,G,S0,S2) :- 
    traverse(    Q, Root,G,S0,S1),
    traverseNode(Qs,Root,G,S1,S2).

traverse(V,_,_,S,S) :- var(V),!.
traverse(node(_,_,Node),Root,G,S0,S1) :- !,
    traverseNode(Node,Root,G,S0,S1).
traverse(leaf(Id,_,Pos,Vel,_),Root,G,S0,S1) :- 
    S0 = [NewLeaf|S1],
    dim2(K),
    force(Root,K,G,Pos,Acc,Id),
    combine(Id,Pos,Vel,Acc,NewLeaf).
*/

% find leafs by cdring list...
traverse([],_,_,[]).
traverse([Leaf|Ls],Root,G,[NewLeaf|S]) :-
    Leaf = leaf(Id,_,Pos,Vel,_),
    dim2(K),
    force(Root,K,G,Pos,Acc,Id),
    combine(Id,Pos,Vel,Acc,NewLeaf),
    traverse(Ls,Root,G,S).

/* 
% find leafs by membering list (parallel version)...
traverse(Stars,Root,G,NewStars) :-
    findall(X,findForce(Stars,Root,G,X),NewStars).

findForce(Stars,Root,G,NewLeaf) :-
    member(Leaf,Stars),
    Leaf = leaf(Id,_,Pos,Vel,_),
    dim2(K),
    force(Root,K,G,Pos,Acc,Id),
    combine(Id,Pos,Vel,Acc,NewLeaf).

member(X,[X|_]).
member(X,[_,X|_]).
member(X,[_,_,X|_]).
member(X,[_,_,_,X|_]).
member(X,[_,_,_,_,X|_]).
member(X,[_,_,_,_,_|Y]) :- member(X,Y).
*/

% be sure to make two copies of X1 for tree creation...
combine(Id,X,V,A,leaf(Id,X1,X1,V1,_)) :-
    delta_time(DT),
    scale(A,DT,DV),
    addvect(V,DV,V1),
    scale(V1,DT,DX),   % should we use V here?
    addvect(X,DX,X1).

% force(+,+,+,+,-)
%force(V,_,_,_,Acc,_) :- var(V),!,Acc = o(0,0,0).
force(node(Pos2,N,Node),K,G,Pos1,Acc,Id) :- 
    check(K,G,Pos1,Pos2,Node,N,Acc,Id).         
force(leaf(Id1,_,Pos2,_,_),K,G,Pos1,Acc,Id2) :- Id1 =\= Id2,!,
    write(Id2),write(' <===> '),
    acc(Pos1,Pos2,G,Acc),
    write(Id1),nl.
% no acceleration between star and itself...
force(leaf(_,_,_,_,_),_,_,_,o(0,0,0),_).   

% check(+,+,+,+,+,+,-)
check(K,G,Pos1,Pos2,_,N,Acc,Id) :-    
    ok(Pos1,Pos2,G,K),!,               % approx is ok...
    write(Id),write(' <===> '),
    acc(Pos1,Pos2,G,A),    
    scale(A,N,Acc),
    write(Pos2),nl.
check(K,G,Pos1,Pos2,Node,N,Acc,Id) :-  % approx is bad...  
    K1 is K/2,
    sumForces(Node,K1,G,Pos1,o(0,0,0),Acc,Id).

% sumForces(+,+,+,+,+,-)
sumForces([],_,_,_,Acc,Acc,_).
sumForces([Q|Qs],K,G,Pos,A1,Acc,Id) :-
    (var(Q) -> sumForces(Qs,K,G,Pos,A1,Acc,Id) ;
        force(Q,K,G,Pos,A2,Id),
        addvect(A1,A2,A3),
        sumForces(Qs,K,G,Pos,A3,Acc,Id)).

% ok(+,+,+,+)
% determine if approximation is ok to use...
% check for strange case when star sits at dead center of quadrant
ok(Pos1,Pos2,G,K) :- Pos1 \== Pos2,
    theta(Theta),
    sqrt(Pos1,Pos2,G,_,Rinv),
    K*Rinv < Theta.

% addvect(+,+,-)
addvect(o(X1,Y1,Z1),o(X2,Y2,Z2),o(X3,Y3,Z3)) :- 
    X3 is X1+X2, Y3 is Y1+Y2, Z3 is Z1+Z2.

% scale(+,+,-)
scale(o(Ax,Ay,Az),N,o(NAx,NAy,NAz)) :-
    NAx is Ax*N, NAy is Ay*N, NAz is Az*N.

% acc(+,+,+,-)
acc(I,J,G,o(Ax,Ay,Az)) :-
    sqrt(I,J,G,R2,Rinv),
    I = o(Xi,Yi,Zi),
    J = o(Xj,Yj,Zj),
    C is R2 / Rinv,
    Ax is (Xi-Xj)*C,
    Ay is (Yi-Yj)*C,
    Az is (Zi-Zj)*C.
    
%-----------------------------------------------------
% oct-tree creation...

% make two copies of position inside the leaf:
% the first copy will be munged up when building the oct-tree
% the second copy is the absolute coordinates of the position
collect(Stars) :-
    findall(leaf(_,o(X,Y,Z),o(X,Y,Z),o(0,0,0),_),star(X,Y,Z),Stars),
    id(Stars,0).

id([],_).
id([leaf(N,_,_,_,_)|Ls],N) :- N1 is N+1, id(Ls,N1).

buildTree(Stars,Root) :-
    Root0 = node(o(0,0,0),_,[_,_,_,_,_,_,_,_]),
    buildTree(Stars,Root0,Root),
    countTree(Root).

buildTree([],Root,Root).
buildTree([Star|Stars],Root0,Root2) :-
    dim(K),
    Star = leaf(_,Pos,_,_,_),
    quad(Pos,Q),
    insertNode(K,Q,Star,Root0,Root1),
    buildTree(Stars,Root1,Root2).

index(Q,node(_,_,Node),Tree) :-
    index(8,Q,Node,Tree).

index(N,N,[Q|_],Q) :- !.
index(N,M,[_|Qs],Tree) :- N1 is N-1, index(N1,M,Qs,Tree).

insertNode(K,Q,Leaf,Node,NewNode) :-
    index(Q,Node,Tree),
    ( var(Tree) ->
         NewNode = Node,
         Tree = Leaf
    ;
         copy(Node,Q,NewTree,NewNode),
         insertTree(Tree,K,Leaf,Q,NewTree,Node)
    ).

insertTree(Node,K,Leaf,Q,NewTree,_) :- nOde(Node), !, 
    new(K,Q,NewQ,Leaf,NewLeaf),
    K1 is K/2,
    insertNode(K1,NewQ,NewLeaf,Node,NewTree).
insertTree(Leaf2,K,Leaf1,Q,NewTree,Parent) :-
    conflict(K,Q,Leaf1,Leaf2,NewTree,Parent).

conflict(K,Q,Leaf1,Leaf2,Node,Parent) :-
    new(K,Q,Q1,Leaf1,NewLeaf1),
    new(K,Q,Q2,Leaf2,NewLeaf2),
    makeNode(K,Q,Parent,Node),
    (Q1 =:= Q2 ->
         K1 is K/2,
         index(Q1,Node,NewTree),
         conflict(K1,Q1,NewLeaf1,NewLeaf2,NewTree,Node)
    ;
         Node = node(_,_,Nodes),
         createNodes(8,Q1,Q2,Nodes,NewLeaf1,NewLeaf2)
    ).

createNodes(0,_,_,[],_,_) :- !.
createNodes(I1,I1,I2,[L1|Qs],L1,L2) :- !,N1 is I1-1,
    createNodes(N1,I1,I2,Qs,_,L2).
createNodes(I2,I1,I2,[L2|Qs],L1,L2) :- !,N1 is I2-1,
    createNodes(N1,I1,I2,Qs,L1,_).
createNodes(N,I1,I2,[_|Qs],L1,L2) :- N1 is N-1,
    createNodes(N1,I1,I2,Qs,L1,L2).

new(K,Q,NewQ,leaf(Id,Pos,P,V,A),leaf(Id,NewPos,P,V,A)) :-
    convert(Q,Pos,K,NewPos),
    quad(NewPos,NewQ).

% copy old node into new node but replace Qth quadrant...
copy(node(Pos,N,Nodes),Q,NewQ,node(Pos,N,NewNodes)) :-
    loop(8,Q,Nodes,NewQ,NewNodes).

loop(0,_,_,_,[]) :- !.
loop(N,N,[_|Qs],   Q,[Q|NQs]) :- !, N1 is N-1, loop(N1,N,Qs,   _,NQs).
loop(N,M,[Q|Qs],NewQ,[Q|NQs]) :-    N1 is N-1, loop(N1,M,Qs,NewQ,NQs).

makeNode(K,Q,node(Pos,_,_),node(NewPos,_,[_,_,_,_, _,_,_,_])) :-
    Q1 is 8-Q+1,   % don't ask why, it just works...
    convert(Q1,Pos,K,NewPos).

% convert(+,+,+,-) returns a new star position relative to subcube...
convert(1,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X-K, Y1 is Y-K, Z1 is Z-K.
convert(2,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X-K, Y1 is Y-K, Z1 is Z+K.
convert(3,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X-K, Y1 is Y+K, Z1 is Z-K.
convert(4,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X-K, Y1 is Y+K, Z1 is Z+K.
convert(5,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X+K, Y1 is Y-K, Z1 is Z-K.
convert(6,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X+K, Y1 is Y-K, Z1 is Z+K.
convert(7,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X+K, Y1 is Y+K, Z1 is Z-K.
convert(8,o(X,Y,Z),K,o(X1,Y1,Z1)) :- !, X1 is X+K, Y1 is Y+K, Z1 is Z+K.

% quad(+,-) returns which quadrant point is in...
quad(o(X,Y,Z), 1) :- X >= 0, Y >= 0, Z >= 0, !.
quad(o(X,Y,Z), 2) :- X >= 0, Y >= 0, Z <  0, !.
quad(o(X,Y,Z), 3) :- X >= 0, Y <  0, Z >= 0, !.
quad(o(X,Y,Z), 4) :- X >= 0, Y <  0, Z <  0, !.
quad(o(X,Y,Z), 5) :- X <  0, Y >= 0, Z >= 0, !.
quad(o(X,Y,Z), 6) :- X <  0, Y >= 0, Z <  0, !.
quad(o(X,Y,Z), 7) :- X <  0, Y <  0, Z >= 0, !.
quad(o(X,Y,Z), 8) :- X <  0, Y <  0, Z <  0, !.

%-------------------------------------------------
% 1/r function a la Makino

% sqrt(+,+,+,-,-)
sqrt(o(Xi,Yi,Zi),o(Xj,Yj,Zj),G,R2,Rinv) :-
    write('.'),
    DX is Xi-Xj,
    DY is Yi-Yj,
    DZ is Zi-Zj,
    (DX < 0.0 -> ADX is 0.0-DX ; ADX = DX),
    (DY < 0.0 -> ADY is 0.0-DY ; ADY = DY),
    (DZ < 0.0 -> ADZ is 0.0-DZ ; ADZ = DZ),
    R0 is (ADX + ADY + ADZ + G) * 0.5,  % cannot use shift for floating point
    R2 is DX*DX + DY*DY + DZ*DZ + G*G,  % we incur extra mult here for K=G*G...
    T is R0*R0 + R2,
%    (T =:= 0 -> nl,
%       write(o(Xi,Yi,Zi)),nl,
%       write(o(Xj,Yj,Zj)),nl
%       ;true),
    A0 is (2*R0) / T,
    sqrt(0,A0,R2,Rinv).

sqrt(4,Ai,_,Ai) :- !.
sqrt(I,Ai,R2,Rinv) :- I1 is I+1,
    write('.'),
    Ai1 is (Ai*3 - Ai*Ai*Ai*R2) * 0.5,
    sqrt(I1,Ai1,R2,Rinv).

time(T) :- statistics(runtime,[_,T]).
printTime :- time(T), write(T), write(msec), nl.

%--------------------------------------------------------------
% counting leafs in oct-tree...

countTree(node(_,N,Node)) :-
    countNode(Node,0,N).

countNode([],N,N).
countNode([Q|Qs],I,N) :- 
    howMany(Q,X),
    I1 is I+X,
    countNode(Qs,I1,N).

howMany(V,0) :- var(V),!.  
howMany(leaf(_,_,_,_,_),1) :- !.
howMany(node(_,N,Node),N) :- countNode(Node,0,N).


%--------------------------------------------------------------
% printing out an oct-tree...

printTree(Tree) :- nl,nl,
    printNode(0,Tree).

printNode(K,node(o(X,Y,Z),N,Node)) :-
    K1 is K+2,   % indent by only 2 as tree grows deep...
    write(center(X,Y,Z)),write(':'),write(N), nl,
    printNode1(Node,K1).

printNode1([],_).
printNode1([Q|Qs],K) :- 
    tab(K),
    printTree(K,Q), 
    printNode1(Qs,K).

printTree(_,V) :- var(V),!,write('_'),nl.
printTree(K,Node) :- nOde(Node), !, printNode(K,Node).
printTree(_,Leaf) :- printLeaf(Leaf).

printStars([]).
printStars([L|Ls]) :- printLeaf(L),printStars(Ls).

printLeaf(leaf(Id,_,Pos,Vel,_)) :- !,
    write(Id),write(':'),write(Pos),write(':'),write(Vel),nl.

%--------------------------------------------------------------
% test database... N=15
star(10.0,10.0,10.0).
star(10.0,10.0,-10.0).
star(10.0,-10.0,10.0).
star(10.0,-10.0,-10.0).
star(-10.0,10.0,10.0).
star(-10.0,10.0,-10.0).
star(-10.0,-10.0,10.0).
star(-10.0,-10.0,-10.0).
star(5.0,5.0,5.0).
star(15.0,15.0,15.0).
star(6.0,6.0,6.0).
star(4.0,4.0,4.0).
star(16.0,16.0,16.0).
star(4.0,3.0,2.0).
star(6.0,5.0,4.0).
