% CVS: $Id: gabriel.pl,v 1.3 1998/10/20 03:23:33 pets Exp $
goal :- main(X, Y).

main(V1,V2) :-
   init(100,10,4,
            V1,
      Symbols),


   randomize(Symbols,RSymbols,21),
   investigate(RSymbols,V2).


init(N,M,Npats,Ipats,Result) :- init(N,M,M,Npats,Ipats,Result).

init(0,_,_,_,_,_).
init(N,I,M,Npats,Ipats,[Symb|Rest]) :- 
   fill(I,[],L),
   get_pats(Npats,Ipats,Ppats),
   J = M - I,
   fill(J,[pattern(Ppats)|L],Symb),
   N1 = N - 1,
        test(I,I1,M),
   init(N1,I1,M,Npats,Ipats,Rest).

test(I,I1,M) :-
  I = 0,
  I1 = M.
test(I,I1,M) :-
   I1 = I - 1.

fill(0,L,L).
fill(N,L,[dummy([])|Rest]) :- N1 = N - 1, fill(N1,L,Rest).


randomize([],[],_).
randomize(In,[X|Out],Rand) :-
   length(In,Lin),
   Rand1 = Rand * 17,
   N = Rand1,
   split(N,In,X,In1),
   randomize(In1,Out,Rand1).

split(0,[X|Xs],X,Xs).
split(N,[X|Xs],RemovedElt,[X|Ys]) :-
   N1 = N - 1,
   split(N1,Xs,RemovedElt,Ys).


investigate([],_).
investigate([U|Units],Patterns) :-
   property(U,pattern,Data),
   p_investigate(Data,Patterns),
   investigate(Units,Patterns).


get_pats(Npats,Ipats,Result) :- get_pats(Npats,Ipats,Result,Ipats).

get_pats(0,_,[],_).
get_pats(N,[X|Xs],[X|Ys],Ipats) :-
   N1 = N - 1,
   get_pats(N1,Xs,Ys,Ipats).
get_pats(N,[],Ys,Ipats) :-
   get_pats(N,Ipats,Ys,Ipats).

property([Prop|RProps],P,Val) :-
   myfunctor(Prop,P,_),
   myarg(1,Prop,Val).
property([_|RProps],P,Val) :-
   property(RProps,P,Val).

myfunctor(f(a,b),f,2).
myfunctor([a|X],'.',2).
myfunctor(start(X),start,1).
myfunctor(pattern(X),pattern,1).

myarg(1,f(X,Y),X).
myarg(1,[X|Y],X).
myarg(1,g(X,Y,Z),X).
myarg(1,pattern(X),X).

p_investigate([],_).
p_investigate([D|Data],Patterns) :-
   p_match(Patterns,D),
   p_investigate(Data,Patterns).

p_match([],_).
p_match([P|Patterns],D) :-
   match(D,P).
% Who is the jerk who put this in the previous clause? RB
%   a = b.
p_match([P|Patterns],D) :-
   p_match(Patterns, D).

match([],[]).
match([X|PRest],[Y|SRest]) :-
   X = Y,
   match(PRest,SRest).
match(List,[Y|Rest]) :- 
   Y = star(X),
   concat(X,SRest,List),
   match(SRest,Rest).
match([X|PRest],[Y|SRest]) :-
        myatom(X),
   X = Y,
   match(PRest,SRest).
match([X|PRest],[Y|SRest]) :-
        match(X,Y),
   match(PRest,SRest).

myatom(a).
myatom(b).

concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).
