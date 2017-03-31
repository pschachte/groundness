% CVS: $Id: trs.pl,v 1.2 1998/10/19 04:22:38 pets Exp $
/*-----------------------------------------------------------------------------
Program: TRS -- theorem prover
Author:  K. Sakai
Date:    

Notes:
0. An automated prover for first-order classical predicate
   logic based on a modified version of Gentzen's sequent calculus LK.
1. The full set of input data was not used by Takagi -- just the simplest
   ones.  If more work is needed, see the data at the end of this file.
2. Problem is that it does a lot of output to terminal during proof --
   this must be removed.
-----------------------------------------------------------------------------*/

%:- entry_mode(go).

:- op(940,xfy,**).    % x**F is equivalent to all(x,F)
:- op(940,xfy,++).    % x++F is equivalent to some(x,F)
:- op(950,fy,~).      % Strong (Classical) Negation */
:- op(960,xfy,/\).    % Conjunction */
:- op(970,xfy,\/).    % Disjunction */
:- op(980,xfy,->).    % Implication */
:- op(985,xfy,<->).   % Equivalence */
:- op(990,xfx,#).

go :- test(_,A), write(A), nl,nl,nl, fail.

test(1,PE) :- classical(((~ ~a) <-> a),PE).
test(2,PE) :- classical(~ (~a /\ a),PE).
test(3,PE) :- classical((~ (b /\ a) <-> (~b \/ ~a)),PE).
test(4,PE) :- classical((~ (b \/ a) <-> (~b /\ ~a)),PE).
test(5,PE) :- classical(((b -> a) <-> (~a -> ~b)),PE).
test(6,PE) :- classical(((~ (b -> a)) <-> (b /\ ~a)),PE).
test(7,PE) :- classical(((~a \/ b) <-> (a -> b)),PE).
test(8,PE) :- classical((((a<->b) <-> c) <-> (a <-> (b<->c))),PE).
 
:- public classical/2.
:- mode classical(+,+).
classical(Formula,Cont) :- !,
    classical0([Formula]#[SW],t([],[],[])#t([],[],[]),[],0,N,Figure,Cont),
    ttynl, !,
    write_figure(Figure,0).

:- mode classical(+,+,+,+,+,-,-,+).
:- mode classical0(+,+,+,+,-,-,+).

classical0([]#[],t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,c([]#[],[]#[],t(Free,L0,R0)#t(SF,SL0,SR0),FA),Cont) :-
    Cont > 0, Cont1 is Cont-1,
    choice(Free#SF1,Le,Ri,Vr-NewVar), !,
    classical(Le,Ri,t(Free,L0,R0)#t(SF2,SL0,SR0),NewVar,NI,NO,FA,Cont1),
    or_switch(SF1,SF2,SF).
classical0([~A|Ri]#[SW|SR],FLR,Vr,NI,NO,Fig,Cont) :- !,
    classical([A]#[SA],Ri#SR,FLR,Vr,NI,NO,FA,Cont),
    set_fig(FA#SA,c([]#[],[~A|Ri]#[SW|SR],FLR,FA),Fig,SW).
classical0([A\/B|Ri]#[SW|SR],FLR,Vr,NI,NO,Fig,Cont) :- !,
    classical0([A,B|Ri]#[SA,SB|SR],FLR,Vr,NI,NO,FAB,Cont),
    set_fig(FAB#(SA\/SB),c([]#[],[A\/B|Ri]#[SW|SR],FLR,FAB),Fig,SW).
classical0([A/\B|Ri]#[SW|SR],FLR#SF,Vr,NI,NO,Fig,Cont) :- !,
    classical0([A|Ri]#[SA|SRA],FLR#SFA,Vr,NI,NM,FA,Cont),
    classical0([B|Ri]#[SB|SRB],FLR#SFB,Vr,NM,NO,FB,Cont),
    set_switch(SA,SB,s([],SRA,SFA),s([],SRB,SFB),s([],SR,SF)),
    set_fig((FA#SA)/\(FB#SB),d([]#[],[A/\B|Ri]#[SW|SR],FLR#SF,FA,FB),Fig,SW).
classical0([A->B|Ri]#[SW|SR],FLR,Vr,NI,NO,Fig,Cont) :- !,
    classical([A]#[SA],[B|Ri]#[SB|SR],FLR,Vr,NI,NO,FAB,Cont),
    set_fig(FAB#(SA\/SB),c([]#[],[A->B|Ri]#[SW|SR],FLR,FAB),Fig,SW).
classical0([A<->B|Ri]#[SW|SR],FLR#SF,Vr,NI,NO,Fig,Cont) :- !,
    classical0([A->B|Ri]#[SA|SRA],FLR#SFA,Vr,NI,NM,FA,Cont),
    classical0([B->A|Ri]#[SB|SRB],FLR#SFB,Vr,NM,NO,FB,Cont),
    set_switch(SA,SB,s([],SRA,SFA),s([],SRB,SFB),s([],SR,SF)),
    set_fig((FA#SA)/\(FB#SB),d([]#[],[A<->B|Ri]#[SW|SR],FLR#SF,FA,FB),Fig,SW).
classical0([X**A|Ri]#[SW|SR],FLR,Vr,NI,NO,Fig,Cont) :- !,
    skolem_rep(X,Vr,A-AA,NI-NM),
    classical0([AA|Ri]#[SA|SR],FLR,Vr,NM,NO,FA,Cont),
    set_fig(FA#SA,c([]#[],[X**A|Ri]#[SW|SR],FLR,FA),Fig,SW).
classical0([X++A|Ri]#[SW|SR],t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,Fig,Cont) :- !,
    classical0(Ri#SR,t([X++A|Free],L0,R0)#t([SW|SF],SL0,SR0),
            Vr,NI,NO,Fig,Cont).
classical0([all(X,A)|Ri]#[SW|SR],FLR,Vr,NI,NO,Fig,Cont) :- !,
    skolem_rep(X,Vr,A-AA,NI-NM),
    classical0([AA|Ri]#[SA|SR],FLR,Vr,NM,NO,FA,Cont),
    set_fig(FA#SA,c([]#[],[all(X,A)|Ri]#[SW|SR],FLR,FA),Fig,SW).
classical0([some(X,A)|Ri]#[SW|SR],t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,Fig,Cont) :- !,
    classical0(Ri#SR,t([some(X,A)|Free],L0,R0)#t([SW|SF],SL0,SR0),
            Vr,NI,NO,Fig,Cont).
classical0([AA|Ri]#[SA|SR],t(Free,L0,R0)#t(SF,SL0,SR0),Vr,N,N,a(AA),Cont) :-
    unify_some(AA#SA,L0#SL0).
classical0([AA|Ri]#[SA|SR],t(Free,L0,R0)#t(SF,SL0,SR0),Vr,NI,NO,Fig,Cont) :-
    member(AA,R0), !,
    classical0(Ri#SR,t(Free,L0,R0)#t(SF,SL0,SR0),Vr,NI,NO,Fig,Cont).
classical0([AA|Ri]#[SA|SR],t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,Fig,Cont) :- !,
    classical0(Ri#SR,t(Free,L0,[AA|R0])#t(SF,SL0,[SA|SR0]),
            Vr,NI,NO,Fig,Cont).

classical([]#[],Right,FLR,Vr,NI,NO,Fig,Cont) :- !,
    classical0(Right,FLR,Vr,NI,NO,Fig,Cont).
classical([~A|Le]#[SW|SL],Ri#SR,FLR,Vr,NI,NO,Fig,Cont) :- !,
    classical(Le#SL,[A|Ri]#[SA|SR],FLR,Vr,NI,NO,FA,Cont),
    set_fig(FA#SA,c([~A|Le]#[SW|SL],Ri#SR,FLR,FA),Fig,SW).
classical([A\/B|Le]#[SW|SL],Ri#SR,FLR#SF,Vr,NI,NO,Fig,Cont) :-
    classical([A|Le]#[SA|SLA],Ri#SRA,FLR#SFA,Vr,NI,NM,FA,Cont),
    classical([B|Le]#[SB|SLB],Ri#SRB,FLR#SFB,Vr,NM,NO,FB,Cont),
    set_switch(SA,SB,s(SLA,SRA,SFA),s(SLB,SRB,SFB),s(SL,SR,SF)),
    set_fig((FA#SA)/\(FB#SB),d([A\/B|Le]#[SW|SL],Ri#SR,FLR#SF,FA,FB),Fig,SW).
classical([A/\B|Le]#[SW|SL],Ri,FLR,Vr,NI,NO,Fig,Cont) :- !,
    classical([A,B|Le]#[SA,SB|SL],Ri,FLR,Vr,NI,NO,FAB,Cont),
    set_fig(FAB#(SA\/SB),c([A/\B|Le]#[SW|SL],Ri,FLR,FAB),Fig,SW).
classical([B->A|Le]#[SW|SL],Ri#SR,FLR#SF,Vr,NI,NO,Fig,Cont) :- !,
    classical([A|Le]#[SA|SLA],Ri#SRA,FLR#SFA,Vr,NI,NM,FA,Cont),
    classical(Le#SLB,[B|Ri]#[SB|SRB],FLR#SFB,Vr,NM,NO,FB,Cont),
    set_switch(SA,SB,s(SLA,SRA,SFA),s(SLB,SRB,SFB),s(SL,SR,SF)),
    set_fig((FA#SA)/\(FB#SB),d([B->A|Le]#[SW|SL],Ri#SR,FLR#SF,FA,FB),Fig,SW).
classical([A<->B|Le]#[SW|SL],Ri#SR,FLR#SF,Vr,NI,NO,Fig,Cont) :- !,
    classical([A/\B|Le]#[SA|SLA],Ri#SRA,FLR#SFA,Vr,NI,NM,FA,Cont),
    classical(Le#SLB,[A\/B|Ri]#[SB|SRB],FLR#SFB,Vr,NM,NO,FB,Cont),
    set_switch(SA,SB,s(SLA,SRA,SFA),s(SLB,SRB,SFB),s(SL,SR,SF)),
    set_fig((FA#SA)/\(FB#SB),d([A<->B|Le]#[SW|SL],Ri#SR,FLR#SF,FA,FB),Fig,SW).
classical([X++A|Le]#[SW|SL],Ri,FLR,Vr,NI,NO,Fig,Cont) :- !,
    skolem_rep(X,Vr,A-AA,NI-NM),
    classical([AA|Le]#[SA|SL],Ri,FLR,Vr,NM,NO,FA,Cont),
    set_fig(FA#SA,c([X++A|Le]#[SW|SL],Ri,FLR,FA),Fig,SW).
classical([X**A|Le]#[SW|SL],Ri,t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,Fig,Cont) :- !,
    classical(Le#SL,Ri,t([X**A|Free],L0,R0)#t([SW|SF],SL0,SR0),
            Vr,NI,NO,Fig,Cont).
classical([some(X,A)|Le]#[SW|SL],Ri,FLR,Vr,NI,NO,Fig,Cont) :- !,
    skolem_rep(X,Vr,A-AA,NI-NM),
    classical([AA|Le]#[SA|SL],Ri,FLR,Vr,NM,NO,FA,Cont),
    set_fig(FA#SA,c([some(X,A)|Le]#[SW|SL],Ri,FLR,FA),Fig,SW).
classical([all(X,A)|Le]#[SW|SL],Ri,t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,Fig,Cont) :- !,
    classical(Le#SL,Ri,t([all(X,A)|Free],L0,R0)#t([SW|SF],SL0,SR0),
            Vr,NI,NO,Fig,Cont).
classical([AA|Le]#[SA|SL],Ri,t(Free,L0,R0)#t(SF,SL0,SR0),Vr,N,N,a(AA),Cont) :-
    unify_some(AA#SA,R0#SR0).
classical([AA|Le]#[SA|SL],Ri,t(Free,L0,R0)#t(SF,SL0,SR0),Vr,NI,NO,Fig,Cont) :-
    member(AA,L0), !,
    classical(Le#SL,Ri,t(Free,L0,R0)#t(SF,SL0,SR0),Vr,NI,NO,Fig,Cont).
classical([AA|Le]#[SA|SL],Ri,t(Free,L0,R0)#t(SF,SL0,SR0),
        Vr,NI,NO,Fig,Cont) :- !,
    classical(Le#SL,Ri,t(Free,[AA|L0],R0)#t(SF,[SA|SL0],SR0),
            Vr,NI,NO,Fig,Cont).

skolem_rep([],Vr,A-A,N-N) :- !.
skolem_rep([X|Vlist],Vr,AI-AO,NI-NO) :-
    Skolem =.. [skolem,NI|Vr], replace(AI,X,Skolem,AM), NM is NI + 1, !,
    skolem_rep(Vlist,Vr,AM-AO,NM-NO).
skolem_rep(X,Vr,AI-AO,NI-NO) :-
    Skolem =.. [skolem,NI|Vr], replace(AI,X,Skolem,AO), NO is NI + 1, !.

var_rep([],A-A,V-V) :- !.
var_rep([X|Vlist],AI-AO,IV-OV) :-
    replace(AI,X,Var,AM), !,
    var_rep(Vlist,AM-AO,[Var|IV]-OV).
var_rep(X,AI-AO,V-[Var|V]) :- replace(AI,X,Var,AO), !.

choice([X**A|Free]#[SW|SF],[NA|Le]#[SW|SL],Ri,IV-OV) :- !,
    var_rep(X,A-NA,IV-MV), choice(Free#SF,Le#SL,Ri,MV-OV).
choice([X++A|Free]#[SW|SF],Le,[NA|Ri]#[SW|SR],IV-OV) :- !,
    var_rep(X,A-NA,IV-MV), choice(Free#SF,Le,Ri#SR,MV-OV).
choice([all(X,A)|Free]#[SW|SF],[NA|Le]#[SW|SL],Ri,IV-OV) :- !,
    var_rep(X,A-NA,IV-MV), choice(Free#SF,Le#SL,Ri,MV-OV).
choice([some(X,A)|Free]#[SW|SF],Le,[NA|Ri]#[SW|SR],IV-OV) :- !,
    var_rep(X,A-NA,IV-MV), choice(Free#SF,Le,Ri#SR,MV-OV).
choice([]#[],[]#[],[]#[],V-V).

set_switch(f,_,A,_,A) :- !.
set_switch(_,f,_,B,B) :- !.
set_switch(_,_,s(SLA,SRA,t(SFA,SL0A,SR0A)),s(SLB,SRB,t(SFB,SL0B,SR0B)),
        s(SL,SR,t(SF,SL0,SR0))) :-
    or_switch(SLA,SLB,SL), or_switch(SRA,SRB,SR), or_switch(SFA,SFB,SF),
    or_switch(SL0A,SL0B,SL0), or_switch(SR0A,SR0B,SR0), !.

or_switch([],[],_) :- !.
or_switch([f|SA],[],[f|SA]) :- !.
or_switch([],[f|SB],[f|SB]) :- !.
or_switch([f|SA],[f|SB],[f|S]) :- !, or_switch(SA,SB,S).
or_switch([_|SA],[],[t|SA]) :- !.
or_switch([],[_|SB],[t|SB]) :- !.
or_switch([_|SA],[_|SB],[t|S]) :- !, or_switch(SA,SB,S).
or_switch(X,Y,Z) :- nl, write('!!! BUG !!! in '),
    !, write(or_switch(X,Y,Z)), fail.

set_fig(Fig#f,_,Fig,f) :- !.
set_fig((Fig#f)/\_,_,Fig,f) :- !.
set_fig(_/\(Fig#f),_,Fig,f) :- !.
set_fig(Fig#(f\/f),_,Fig,f) :- !.
set_fig(_,Fig,Fig,t) :- !.

write_sequent(Le,Ri,Free,L0,R0) :-
    separate_free(Free,Some,All),
    write(('|')), write_seq(Le), write_seq(L0), write_seq(All),
    write(' => |'),
    write_seq(Ri), write_seq(R0), write_seq(Some), fail.
write_sequent(_,_,_,_,_).

write_figure(c(Le,Ri,t(Free,L0,R0)#t(SF,SL0,SR0),F),N) :-
    write_sequent(Le,Ri,Free#SF,L0#SL0,R0#SR0),
    ttynl, write_space(N), !, write_figure(F,N).
write_figure(d(Le,Ri,t(Free,L0,R0)#t(SF,SL0,SR0),F1,F2),N) :-
    write_sequent(Le,Ri,Free#SF,L0#SL0,R0#SR0),
    ttynl, write_space(N), write('o '), !, write_figure(F1,s(N)),
    ttynl, write_space(N), write('o '), !, write_figure(F2,s(N)).
write_figure(a(AA),_) :-
    write(('|')), write_seq([AA]#[t]),
    write(' => |'),
    write_seq([AA]#[t]),
    write(' ----'), write(axiom), fail.
write_figure(_,_).

write_space(0).
write_space(s(N)) :- write('  '), write_space(N).

write_seq(_#[]) :- !.
write_seq([X|L]#[f|SL]) :- !, write_seq(L#SL).
write_seq([X|L]#[_|SL]) :-
    drop_skolem(X,M), write(M), write(('|')), !, write_seq(L#SL).

drop_skolem(A,$$) :- var(A), !.
drop_skolem(A,A) :- atomic(A), !.
drop_skolem(A,Symbol) :- A =.. [skolem,N|_], !,
    make_name(N,Symbol).
drop_skolem(A,AA) :- A =.. [F|Arg], !,
    drop_skolem_l(Arg,AArg), AA =.. [F|AArg].

drop_skolem_l([],[]) :- !.
drop_skolem_l([A|Arg],[AA|AArg]) :- !,
    drop_skolem(A,AA), drop_skolem_l(Arg,AArg).

make_name(N,Symbol) :- S is N/26, digitize(S,Suffix),
    T is N-S*26+65, name(Symbol,[T|Suffix]).

digitize(0,[]) :- !.
digitize(X,Y) :- !, name(X,Y).

% digitize(0,[]) :- !.
% digitize(N,[D|DL]) :- !, NL is N / 10 , D is (N mod 10)+48, digitize(NL,DL).

separate_free(_#[],[]#[],[]#[]) :- !.
separate_free([X**A|Free]#[SW|SF],Some,[X**A|All]#[SW|SA]) :- !,
    separate_free(Free#SF,Some,All#SA).
separate_free([X++A|Free]#[SW|SF],[X++A|Some]#[SW|SS],All) :- !,
    separate_free(Free#SF,Some#SS,All).
separate_free([all(X,A)|Free]#[SW|SF],Some,[all(X,A)|All]#[SW|SA]) :- !,
    separate_free(Free#SF,Some,All#SA).
separate_free([some(X,A)|Free]#[SW|SF],[some(X,A)|Some]#[SW|SS],All) :- !,
    separate_free(Free#SF,Some#SS,All).

replace(A,X,Ex,A) :- var(A), !.
replace(A,A,Ex,Ex) :- !.
replace(A,X,Ex,A) :- atom(A), !.
replace(X++A,X,Ex,X++A) :- !.
replace(List++A,X,Ex,X++A) :- member(X,List), !.
replace(Y++A,X,Ex,Y++NA) :- !, replace(A,X,Ex,NA).
replace(X**A,X,Ex,X**A) :- !.
replace(List**A,X,Ex,X**A) :- member(X,List), !.
replace(Y**A,X,Ex,Y**NA) :- !, replace(A,X,Ex,NA).
replace(some(X,A),X,Ex,some(X,A)) :- !.
replace(some(List,A),X,Ex,some(X,A)) :- member(X,List), !.
replace(some(Y,A),X,Ex,some(Y,NA)) :- !, replace(A,X,Ex,NA).
replace(all(X,A),X,Ex,all(X,A)) :- !.
replace(all(List,A),X,Ex,all(X,A)) :- member(X,List), !.
replace(all(Y,A),X,Ex,all(Y,NA)) :- !, replace(A,X,Ex,NA).
replace(A,X,Ex,NA) :- A =.. [F|Arg],
    replace_l(Arg,X,Ex,AArg), NA =.. [F|AArg].

replace_l([],X,Ex,[]) :- !.
replace_l([A|Arg],X,Ex,[AA|AArg]) :- !,
    replace(A,X,Ex,AA),
    replace_l(Arg,X,Ex,AArg).

unify_some(X#t,[Z|Y]#[t|SY]) :- unify(X,Z).
unify_some(X,[_|Y]#[_|SY]) :- unify_some(X,Y#SY).

member(A,[B|L]) :- A == B, !.
member(A,[B|L]) :- !, member(A,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unify.utl.* ---   programs around unification with occur check
%                   (1) unification with occur check
%                   (2) narrowing
%                   (3) equational unification (meta-unification)
%                   (4) renaming variable

:- public unify_l/2.
:- mode unify_l(?,?).
unify_l([],[]) :- !.
unify_l([X|L],[Y|M]) :- !, unify(X,Y), unify_l(L,M).

:- public unify/2.
:- mode unify(?,?).
unify(X,Y) :- X == Y, !.
unify(X,Y) :- var(X), !, occur_check(X,Y), X = Y.
unify(X,Y) :- var(Y), !, occur_check(Y,X), X = Y.
unify(X,Y) :- !, X =.. [F|XA], Y =.. [F|YA], unify_l(XA,YA).

:- mode occur_check(+,+).
occur_check(X,Y) :- X == Y, !, fail.
occur_check(X,Y) :- var(Y), !.
occur_check(X,Y) :- !, Y =.. [F|A], occur_check_l(X,A).

:- mode occur_check_l(+,+).
occur_check_l(X,[]) :- !.
occur_check_l(X,[Y|A]) :- !, occur_check(X,Y), occur_check_l(X,A).

:- op(990,xfx,>>).
:- mode meta_unify(?,?).
meta_unify(X,Y) :- unify(X,Y), !.
meta_unify(X,Y) :- narrowing(X,NX), meta_unify(NX,Y).
meta_unify(X,Y) :- narrowing(Y,NY), meta_unify(X,NY).

:- mode narrowing(?,-).
narrowing(X,NX) :- var(X), !, fail.
narrowing(X,NX) :- NA >> NX, unify(X,NA).
narrowing(X,NX) :- X =.. [F|Ar], narrowing_l(Ar,NAr), NX =.. [F|NAr].

:- mode narrowing_l(?,+,-).
narrowing_l([X|Ar],[NX|Ar]) :- narrowing(X,NX).
narrowing_l([X|Ar],[X|NAr]) :- narrowing_l(Ar,NAr).

% create new term with renamed_variable      by Yasukawa
% modified by Sakai    1983/8/25.
:- public my_copy_term/2.
:- mode my_copy_term(+, ?).
my_copy_term(Old_instance, New_instance) :-
        my_copy_term(Old_instance, New_instance, [], Varlist).

:- mode copy_term(+, ?, +, -).
my_copy_term(Old, New, Cur_Varlist, New_Varlist) :-
        var(Old), !, copy_var(Old, New, Cur_Varlist, New_Varlist).
my_copy_term(Old, New, Cur_Varlist, New_Varlist) :- !,
        Old =.. [F|Old_list],
        copy_list(Old_list, New_list, Cur_Varlist, New_Varlist),
        New =.. [F|New_list].

:- mode copy_list(+, ?, +, -).
copy_list([], [], Var_list, Var_list) :- !.
copy_list([Old|Old_list], [New|New_list], Cur_varlist, New_varlist) :- !,
        my_copy_term(Old, New, Cur_varlist, Varlist),
        copy_list(Old_list, New_list, Varlist, New_varlist).

:- mode copy_var(+, ?, +, -).
copy_var(Old, New, [], [(Old,New)]) :- !.
copy_var(Old, New, [(X, New)|Varlist], [(X, New)|Varlist]) :-  Old == X, !.
copy_var(Old, New, [Pair|Cur_Varlist], [Pair|New_Varlist]) :- !,
        copy_var(Old, New, Cur_Varlist, New_Varlist).
/*
% portray for debugging this prover.

portray(X) :- pportray(X), !.
portray(X) :- hportray(X).

pportray(classical0(Right#SR,t(Free,L0,R0)#_,Vr,NI,NO,Fig,Cont)) :- !,
    nl, write('**** premiss ****'), lportray(L0),
    nl, write('**** consequence ****'), lportray(Right), lportray(R0),
    nl, write('**** exitential ****'), lportray(Free).
pportray(classical(Left#SL,Right#SR,t(Free,L0,R0)#_,Vr,NI,NO,Fig,Cont)) :- !,
    nl, write('**** premiss ****'), lportray(Left), lportray(L0),
    nl, write('**** consequence ****'), lportray(Right), lportray(R0),
    nl, write('**** exitential ****'), lportray(Free).
*/


