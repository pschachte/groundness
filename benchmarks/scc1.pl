% CVS: $Id: scc1.pl,v 1.5 1998/10/21 06:21:05 pets Exp $
goal :- main.

% based on algorithm presented in Mehlhorn:84 (page 31)
% Stephan Diehl, august 1994

% admittedly this code looks horrible,
% but at least it works !

% state(Current,Count1,S,Dfs_Nums,LowPt), OutState)

/*
main :-
    Edges=[(a,c),(a,a),
           (b,g),(b,c),
           (c,b),(c,d),
           (d,f),
           (e,d),(e,g),
           (f,e)],
    Nodes=[a,b,c,d,e,f,g], 
    compute_scc(Edges,Nodes,S,O),
    write('SCCs= '),write(S),nl,
    write('Order= '),write(O),nl. 
*/

main :-
    ground(Edges), ground(Nodes),
    compute_scc(Edges,Nodes,S,O),
    write('SCCs= '),writeln(S),
    write('Order= '),writeln(O). 

%qtest(R) :- qsort(ord([],qless),[2,4,5,3,1,8,3,9],R).

%qless(X,A,B) :- A < B.




compute_scc(Edges,Nodes,SCCs,Ord)
 :- process_nodes(Nodes,Edges,state([],0,[],[],[]),OutState),!,
    filter_sccs_and_ord(OutState,Nodes,SCCs,Ord).

process_nodes([],_,S,S).
process_nodes([V|R],Edges,state(Current,Count1,S,Dfs_Nums,LowPts),OutState)
 :- member(V,S),!,
    process_nodes(R,Edges,state(Current,Count1,S,Dfs_Nums,LowPts),OutState).
process_nodes([V|R],Edges,state(Current,Count1,S,Dfs_Nums,LowPts), OutState)
 :- replace((V,_),(V,NewCount1),Dfs_Nums,NewDfs_Nums),
    NewCount1 is Count1 + 1,
    add_node(V,Current,NewCurrent),
    dfs(V,Edges,
        state(NewCurrent,NewCount1,[V|S],NewDfs_Nums,LowPts),
        OutState1),
    process_nodes(R,Edges,OutState1,OutState).

dfs(V,Edges,state(Current,Count1,S,Dfs_Nums,LowPts),
            state(Temp2Current,TempCount1,TempS,TempDfs_Nums,TempLowPts))
:- member((V,DFSNUM),Dfs_Nums), !,
   replace((V,_),(V,DFSNUM),LowPts,NewLowPts),
   process_edges(V,Edges,Edges,
                 state(Current,Count1,S,Dfs_Nums,NewLowPts),
                 state(TempCurrent,TempCount1,TempS,TempDfs_Nums,TempLowPts)),
   member((V,LV),TempLowPts),
   member((V,DV),TempDfs_Nums),!,
   (DV==LV -> delete_all_nodes(DV,TempDfs_Nums,TempCurrent,Temp2Current)
           ;  Temp2Current=TempCurrent).
   

process_edges(V,[],_,S,S).
process_edges(V,[(V,W)|R],Edges,state(Current,Count1,S,Dfs_Nums,LowPts),
                OutState)
 :-  (member(W,S)
        -> ( TempCurrent=Current,
             TempCount1=Count1,
             TempS=S,
             TempDfs_Nums=Dfs_Nums,
             TempLowPts=LowPts)
         ; ( add_node(W,Current,NewCurrent),
             NewCount1 is Count1+1,
             replace((W,_),(W,NewCount1),Dfs_Nums,NewDfs_Nums),
             dfs(W,Edges,
                 state(NewCurrent,NewCount1,[W|S],NewDfs_Nums,LowPts),
                 state(TempCurrent,TempCount1,TempS,TempDfs_Nums,XLowPts)),
              member((V,LV),XLowPts),
              member((W,LW),XLowPts),!,
              (LW<LV -> replace((V,LV),(V,LW),XLowPts,TempLowPts)
                     ; TempLowPts=XLowPts))),
     ( member(W,TempCurrent)
        -> member((W,DW),TempDfs_Nums),
           member((V,DV),TempDfs_Nums),!,
           (DW<DV
            -> (member((V,LV),TempLowPts),!,
                 (DW<LV -> replace((V,_),(V,DW),TempLowPts,Temp2LowPts)
                        ; Temp2LowPts=TempLowPts))
            ; Temp2LowPts=TempLowPts)
        ; Temp2LowPts=TempLowPts),
     process_edges(V,R,Edges,
                   state(TempCurrent,TempCount1,TempS,TempDfs_Nums,Temp2LowPts),
                   OutState).
process_edges(V,[(X,W)|R],Edges,Instate,OutState)
 :- process_edges(V,R,Edges,Instate,OutState).


delete_all_nodes(DV,Dfs_Nums,[],[]).
delete_all_nodes(DV,Dfs_Nums,[W|R],Res)
 :- member((W,DW),Dfs_Nums),!,
    ( DW>=DV
       -> Res=R2
       ;  Res=[W|R2]),
    delete_all_nodes(DV,Dfs_Nums,R,R2).


% Quicksort cf. Bratko:86

sort_nodes(Dfs,[],[]).
sort_nodes(Dfs,[M|L],R) 
 :- partx(Dfs,M,L,U1,U2), sort_nodes(Dfs,U1,V1), 
    sort_nodes(Dfs,U2,V2), append(V1,[M|V2],R).

partx(Dfs,X,[],[],[]).
partx(Dfs,M,[E|L],[E|U1],U2) 
 :- lessx(Dfs,E,M), !, partx(Dfs,M,L,U1,U2).
partx(Dfs,M,[E|L],U1,[E|U2]) :- partx(Dfs,M,L,U1,U2).


lessx(Dfs,N1,N2)
 :- member((N1,D1),Dfs),
    member((N2,D2),Dfs),!,
    D1<D2.


add_node(V,[],[V]) :- !.
add_node(V,L,L) :- member(V,L),!.
add_node(V,L,[V|L]).

replace(Old,New,[],[New]).
replace(Old,New,In,[New|Rest])
 :- member(Old,In)
     -> unif_delete(Old,In,Rest)
     ;  Rest=In.

/* ----------------------- */

filter_sccs_and_ord(state(Current,Count1,S,Dfs_Nums,LowPt),Nodes,SCCs,SortedNodes)
 :- % writeln(LowPt),
    % writeln(Dfs_Nums),
    sort_nodes(Dfs_Nums,Nodes,SortedNodes),!,
    filter_sccs(1,LowPt,[],SCCs).
     
filter_sccs(N,[],SCCs,SCCs).

filter_sccs(N,LowPt,SCCs,SCCs2)
 :- member((V,N),LowPt),!,
    unif_delete((V,N),LowPt,TempLowPt),
    ( member((N,SCC),SCCs)
       -> replace((N,SCC),(N,[V|SCC]),SCCs,TempSCCs)
       ;  replace((N,_),(N,[V]),SCCs,TempSCCs)),
    filter_sccs(N,TempLowPt,TempSCCs,SCCs2).
filter_sccs(N,LowPt,SCCs,SCCs2)
 :- N1 is N + 1,
    filter_sccs(N1,LowPt,SCCs,SCCs2).

unif_delete(A,[],[]).
unif_delete(A,[A|B],B).
unif_delete(A,[X|B],[X|B2]) :- unif_delete(A,B,B2).





member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).


writeln(Text) :- write(Text), nl.
