% CVS: $Id: sim_v5-2.pl,v 1.3 1998/10/21 04:26:17 pets Exp $
/*                                   
ALGEBRA SIMPLIFIER     		
                                   
This file encloses a code SIM v5.5 for algebra calculation.
Please send me your comments and suggestions.

Note: The code is developed under SWI-Prolog 1.9.5, need to change power/3 for BIN-Prolog.

Copyright:
1, Using this file for commercial purpose without my permission
   is not allowed;
2. the file may be distributed freely.

Example:
*/

append([], L, L).
append([A|B], C, [A|BC]) :- append(B, C, BC).

reverse([], []).
reverse([A|BC], CBA) :-
	reverse(BC, CB),
	append(CB, [A], CBA).

goal :- go.

go:-
expression(E),				nl,pp(E),
simplify(E,NewE),                 	pp('=',NewE),
fail.

expression(E):-
E = (a^2-b^2)/(a+b).
expression(E):-
E = 3*x*y+2*a*b-2*y*x-b*a*2.
expression(E):-
E = 0+1*a/1-b^0.
expression(E):-
E = (a-b)^5/(b-a)^3.
expression(E):-
E = (2*a+3*a*b)/a.
expression(E):-
E = 6/(a*2*b+6).
expression(E):-
E = (a*a-b*(2*a-b))/(a-b).
expression(E):-
E = a^3/(c*(b/c*a^(-2))).

go1:-
expression1(E1,E2),
simplify1(E1*E2,E3),
simplify(E3/E1, Enew),
		nl,pp('E1 =',E1),pp('E2 =',E2),pp('E3 = E1*E2 ='),pp(E3),	
		pp('E3/E1 =',Enew),
fail.

expression1(E1,E2):-
E1 = 3*a^3+b*c+4*a*b*c-2*c,
E2 = 5*a*a-b*c.
expression1(E1,E2):-
E1 = a*b*c*d-b*c*3+a^4*2-c^3*3,
E2 = 2*a*b^2+3*d^3*b*c.

/*----------------------------------------------------------------*/
			   		/*        SIM v5.5        */
		                      	/*         made by        */
			              	/*  zhuhail@vax.sbu.ac.uk */
			              	/*         18/7/96        */
				      	/*   any comment welcome  */
%----------for SWI-Prolog

power(A, B, AB):-		
	AB is A^B.

sort_all(List, Lsor):-
	sort(List, Lsor).

rev_sort(List, Lsor):-
	sort(List, L1),	reverse(L1, Lsor).
%----------------
/*
%----------for BIN-Prolog
power(A, B, AB):- integer(A), integer(B), B > 0, !,
	pow(A, B, C), integer(C, AB).
power(A, B, AB):-
	pow(A, B, AB).

sort_all(List, Lsor):-
	prolog:merge_sort(<, List, Lsor).

rev_sort(List, Lsor):-
	prolog:merge_sort(>, List, Lsor).
%-------------
*/
			
any_f(sin(_)).
any_f(cos(_)).
any_f(exp(_)).
any_f(log(_)).

simplify(E, NewE):-
	check(E),
	sim(E, NewE).

sim(E, NewE):- numberf(E, Num), !,
	NewE = Num.
sim(E, NewE):- atom_n_exp(E), !,
	NewE = E.
sim(E, NewE):-
	e2l(E, List),
	del_list(List, Ldel),
	l2e(Ldel, E1),    
	minus_inv_b(Esigninv, E1),
	sim_again(E, Esigninv, NewE).

sim_again(E, E, NewE):- !,
	NewE = E.
sim_again(_, E, NewE):-
	sim(E, NewE).

simplify1(E, NewE):-
	check(E),
	sim1(E, NewE).

sim1(E, NewE):-
	expand(E, E1),
	sim(E1, E2),
	sim1_again(E, E2, NewE).
sim1_again(E, E, NewE):- !,
	NewE = E.
sim1_again(_, E, NewE):-
	sim1(E, NewE).

e2l(E, Ldiv):-                         
	removep(E, Erem),
	minus_inv(Erem, Einv),
	e2l1(Einv, List),	 	
	sum_plus_list(List, Lsum),	
	factorise(Lsum, Lfac),
	sort_list(Lfac, Lsor),
	collect_like(Lsor, Lcol),
	simplify_division(Lcol, Ldiv).

factorise(List, Lfac):-
	prime(Prime),
	factor_list(List, Prime, Lfac).

prime(Prime):-
	Prime = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,
		 83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,
		 167,173,179,181,191,193,197,199].

factor_list([], _, []).
factor_list([H|T], Prime, [H1|T1]):-
	f_list(H, Prime, H1),
	factor_list(T, Prime, T1).

f_list([Sum|T], Prime, [NewS|NewT]):-
	f_li(T, Sum, Prime, NewS, NewT).  	

f_li([], Sum, _, Sum, []).
f_li([H^M|T], Sum, Prime, NewS, [H1^M|T1]):- int_list(H), !,
	list_div_prime(Prime, H, H1, 1, S1),
	power(S1, M, SM),
	S2 is Sum*SM,
	f_li(T, S2, Prime, NewS, T1).
f_li([H|T], Sum, Prime, NewS, [H1|T1]):- int_list(H), !,
	list_div_prime(Prime, H, H1, Sum, S1),
	f_li(T, S1, Prime, NewS, T1).
f_li([H|T], Sum, Prime, NewS, [H|T1]):-
	f_li(T, Sum, Prime, NewS, T1).

int_list([]).
int_list([[H|_]|T]):-
	integer(H),
	int_list(T).

list_div_prime([], List, List, Sum, Sum).
list_div_prime([H|_], List, List, Sum, Sum):-
	big_than_list(List, H), !.
list_div_prime([H|T], List, NewL, Sum, NewS):-
	list_d_p(List, H, L1, Sum, SumH),
	list_div_prime(T, L1, NewL, SumH, NewS).

list_d_p(List, H, NewL, Sum, SumH):-
	list_div_int(List, H, L1), !,
	S1 is Sum*H,
	list_d_p(L1, H, NewL, S1, SumH).
list_d_p(List, _, List, Sum, Sum).

big_than_list([[0|_]|T], P):- !,
	big_than_list(T, P).
big_than_list([[H|_]|_], P):- H > 0, P > H, !.
big_than_list([[H|_]|_], P):- H < 0, P < H, !.
big_than_list([_|T], P):-
	big_than_list(T, P).

list_div_int([], _, []).
list_div_int([[H|T]|TT], N, [[H1|T]|TT1]):-
	real_int_div(H, N, H1),
	list_div_int(TT, N, TT1).

real_int_div(A, B, C):-
	D is A mod B,
	D = 0,
	C is A//B.

collect_like(List, Lcol):-
	collect_like_term(List, Lcot),
	collect_like_factor(Lcot, Lcof),
	collect_again(List, Lcof, Lcol).

collect_again(L, L, Lcol):- !,
	Lcol = L.
collect_again(_, L, Lcol):- 
	collect_like(L, Lcol).

sort_list([H|T], [H|Tsor]):-
	sort_time_list(T, Tsot),
	h2t(Tsot, T1),
	sort_all(T1, T2),
	h2t(Tsor, T2).

h2t([], []).		
h2t([[H|Tt]|Tp], [Tt-H|Tnew]):-
	h2t(Tp, Tnew).

sort_time_list([], []).
sort_time_list([H|T], [Hsot|Tsot]):-
	sort_time(H, Hsot),
	sort_time_list(T, Tsot).

sort_time([H|T], [H|Tsor]):-
	sort_plus_list(T, Tsop),
	sort_all(Tsop, Tsor).

sort_plus_list([], []).
sort_plus_list([H^M|T], [Hsol^M|Tsop]):-  H = [_|_], !,
	sort_list(H, Hsol),
	sort_plus_list(T, Tsop).
sort_plus_list([H|T], [Hsol|Tsop]):- H = [_|_], !,
	sort_list(H, Hsol),
	sort_plus_list(T, Tsop).
sort_plus_list([H|T], [H|Tsop]):-
	sort_plus_list(T, Tsop).
	
minus_inv(A, B):- numberf(A, N), !,
	B = N.
minus_inv(A, B):- atom(A), !,
	B = A^1.
minus_inv(A, B):- min_inv(A, B), !.
minus_inv(A, B):- any_f(A), !,
	B = A^1.
minus_inv(A, A).

min_inv(A+B, AB):-
	minus_inv(A, A1),
	minus_inv(B, B1),
	AB = A1+B1.
min_inv(A-B, AB):- !,
	minus_inv(A, A1),
	minus_inv(B, B1),
	AB = A1+B1*(-1).
min_inv(-A, B):-
	minus_inv(A, A1),
	B = A1*(-1).
min_inv(A*B, AB):-
	minus_inv(A, A1),
	minus_inv(B, B1),
	AB = A1*B1.
min_inv(A/B^N, AB):- !,
	Nneg is -1*N,
	minus_inv(A, A1),
	minus_inv(B, B1),
	AB = A1*B1^Nneg.
min_inv(A/B, AB):-
	minus_inv(A, A1),
	minus_inv(B, B1),
	AB = A1*B1^(-1).
min_inv(A^M, B):- atom_num(A), !,
	B = A^M.
min_inv(A^M, B):- any_f(A), !,
	B = A^M.
min_inv(A^M, B):- 
	minus_inv(A, A1),
	B = A1^M.

minus_inv_b(B, A):-
	min_inv_b(B, A) -> true
			;  B = A.

min_inv_b(AB, A+B):-
	minus_inv_b(A1, A),
	minus_inv_b(B1, B),
	AB = A1+B1.
min_inv_b(AB, A-B):- !,
	minus_inv_b(A1, A),
	minus_inv_b(B1, B),
	AB = A1-B1.
min_inv_b(B, -A):-
	minus_inv_b(A1, A),
	B = -A1.
min_inv_b(AB, A*B^(-1)):- !,
	minus_inv_b(A1, A),
	minus_inv_b(B1, B),
	AB = A1/B1.
min_inv_b(AB, B^(-1)*A):- !,
	minus_inv_b(A1, A),
	minus_inv_b(B1, B),
	AB = A1/B1.
min_inv_b(AB, A*B):-
	minus_inv_b(A1, A),
	minus_inv_b(B1, B),
	AB = A1*B1.
min_inv_b(B, A^(-1)):- !,
	minus_inv_b(A1, A),
	B = 1/A1.
min_inv_b(B, A^M):-
	minus_inv_b(A1, A),
	B = A1^M.

pp(A):-
	write(A),nl.
pp(A, B):-
	write(A),write('   '),write(B),nl.
pp(A, B, C):-
	write(A),write('   '),write(B),write('   '),write(C),nl.
pp(A, B, C, D):-
	write(A),write('   '),write(B),write('   '),write(C),write('   '),
	write(D),nl.

atom_num(A):- atom(A), !.
atom_num(A):- number(A).

zero(A):- number(A), A < 0.0000001, A > -0.0000001.

unit(A):- number(A), A < 1.0000001, A >  0.9999999.

not_deal(A):-
	atom_n_exp(A).
not_deal(A):-
	any_f(A).
not_deal(A^_):-
	any_f(A).

atom_n_exp(A^_):- atom_num(A), !.
atom_n_exp(A):- atom_num(A).

collect_like_factor(A, [Sum|Tnew]):- A = [Sum|T], !,
	collect_factor(T, Tnew).
collect_like_factor(A, A).

collect_factor([], []).
collect_factor([H|T], [Hnew|Tnew]):- 
	collect_fact(H, Hnew), 
	collect_factor(T, Tnew).

collect_fact([Sum|T], [Sum|Tnew]):-
	collect_fac(T, Tnew).

collect_fac([], []).
collect_fac([H|T], [Hnew|Tnew]):-
	collect_like_factor(H, HH), 
	collect_f(T, HH, Hnew, T1),
	collect_fac(T1, Tnew).

collect_f([], H, H, []).
collect_f([H1|Tt], H^M, Hnew, Tnew):-
	collect_like_factor(H1,HH),
	HH=H^N, !,
	MN is M+N,
	collect_f(Tt, H^MN, Hnew, Tnew).
collect_f([Th|Tt], H, Hnew, [Th|Tt1]):-
	collect_f(Tt, H, Hnew, Tt1).

check(X):- atom_num(X), !.
check(X):- var(X), !,
	pp('Encounter a variable!'),
	pp('(Begin with a lower case letter for elements in expression.)'),
	fail.
check(A+B):- !,
        check(A),
        check(B).
check(A-B):- !,
        check(A),
        check(B).
check(A*B):- !,
        check(A),
        check(B).
check(A/B):- !,
        check(A),
        check(B).
check(-B):- !,
        check(B).
check(A^B):- !, 
	check(A),
	check_num1(A^B).
check(F):- any_f(F), !,
	arg(1, F, A),
	check(A).
check(X):-
	pp('Either a wrong expression or I do not like it:',X),
	fail.

check_num1(_^B):- numberf(B, _), !.
check_num1(A^B):-
	pp(B,'must be a number for me to deal with',A^B),
	fail.

removep(X, T):- atom_n_exp(X), !,
	T = X.
removep(A+(B+C),T):- !,
        removep(A+B+C,T).
removep(A+(B-C),T):- !,
        removep(A+B-C,T).
removep(A-(B+C),T):- !,
        removep(A-B-C,T).
removep(A-(B-C),T):- !,
        removep(A-B+C,T).
removep(A+B,T):- !,
        removep(A,NewA),
        removep(B,NewB),
        T = NewA+NewB.
removep(A-B,T):- !,
        removep(A,NewA),
        removep(B,NewB),
        T = NewA-NewB.
removep(-B,T):- !,
        removep(B,NewB),
        T = -NewB.
removep(A/(B/C),T):- !,
        removep(A/B*C,T).
removep(A/(B*C),T):- !,
        removep(A/B/C,T).
removep(A/(-B),T):- !,
	removep(-A/B,T).
removep(A*(B/C),T):- !,
        removep(A*B/C,T).
removep(A*(B*C),T):- !,
        removep(A*B*C,T).
removep(A*(-B),T):-!,
	removep(-A*B,T). 
removep(A/B,T):- !,
        removep(A,NewA),
        removep(B,NewB),
        T = NewA/NewB.
removep(A*B,T):- !,
        removep(A,NewA),
        removep(B,NewB),
        T = NewA*NewB.
removep((A^B)^C,T):- !,
        removep(A,NewA),
        removep(B,NewB),
	removep(C,NewC),
        T = NewA^(NewB*NewC).
removep(A^B,T):- !,
	removep(A,NewA),
	T = NewA^B.
removep(T,T).

simd([], []). 
simd([[H]|T], Lsimd):- atom_num(H), !,
	Lsimd = [[H]|NewT],
	simd(T, NewT).
simd([H|T], [Hsimd|Tsimd]):-
	sim_div(H, Hsimd),
	simd(T, Tsimd).

sim_div([H|T], [H|NewT]):-  
	simdivision(T, NewT).

simdivision([],[]).
simdivision([H],NewL):- !,
	NewL=[H].
simdivision([H|T],NewL):- simdiv(H,T,ReduceL), !,
	simdivision(ReduceL,NewL).
simdivision([H|T],[H|NewT]):-
	simdivision(T,NewT).

simdiv(L1^M,[L2^N|T],New):- M > 0, N < 0,
	divide_list(L1,L2,Ldiv),
	Ldiv = [[A]],
	numberf(A,Num), !,
	MN is M+N,
	N1 is -1*N,
	New = [[[L1^MN,Num^N1],[0]]|T].
simdiv(L1^M,[L2^N|T],New):- M < 0, N > 0,
	divide_list(L1,L2,Ldiv),
	Ldiv = [[Num]],
	number(Num), !,
	MN is M+N,
	New = [[[L1^MN,Num^N],[0]]|T].
simdiv(A^(-1),[H|T],New):- divide_list(H,A,DivL), !,
	New = [DivL|T].
simdiv(A,[H^(-1)|T],New):- divide_list(A,H,DivL), !,
	New = [DivL|T].
simdiv(A^(-1),[H|T],New):- divide_list(A,H,DivL), !,
	New = [DivL^(-1)|T].
simdiv(A,[H^(-1)|T],New):- divide_list(H,A,DivL), !,
	New = [DivL^(-1)|T].
simdiv(A,[H|T],[H|NewT]):- 
	simdiv(A,T,NewT).

simplify_division(List,Lsim):- List = [[_]], !,
	Lsim = List.
simplify_division(List,Lsim):- List = [_,[_,_]], !,
	Lsim = List.
simplify_division(List,Lsim):-
	simd1(List,List1),
	simd(List1,Lsim).

simd1([],[]).
simd1([H|T],[Hc|Tc]):-
	simd2(H,Hc),
	simd1(T,Tc).

simd2([],[]). 
simd2([H|T],[H2|T2]):- not_deal(H), !,
	H2 = H,
	simd2(T,T2).
simd2([H^M|T],[H2|T2]):- number(M), !,
	simplify_division(H,H1),
	H2 = H1^M,
	simd2(T,T2).
simd2([H|T],[H2|T2]):-
	simplify_division(H,H2), !,
	simd2(T,T2).
simd2([H|T],[H2|T2]):-
	H2 = H,
	simd2(T,T2).

collect_like_term([H|T],[H|Tcol]):-
	collect1(T,List1),
	collect(List1,Tcol).

collect1([],[]).
collect1([H|T],[Hc|Tc]):-
	collect2(H,Hc),
	collect1(T,Tc).

collect2([],[]).
collect2([H|T],[H2|T2]):- not_deal(H), !,
	H2 = H,
	collect2(T,T2).
collect2([H^M|T],[H2|T2]):- !,
	collect_like_term(H,H1),
	H2 = H1^M,
	collect2(T,T2).
collect2([H|T],[H2|T2]):-
	collect_like_term(H,H2), !,
	collect2(T,T2).
collect2([H|T],[H2|T2]):-
	H2 = H,
	collect2(T,T2).

collect([],[]).
collect([H0|List1],[NewH|NewT]):-
	H0=[Para0|List0],
	coll(List1,Para0,List0,List2,NewPara),
	NewH=[NewPara|List0],
	collect(List2,NewT).

coll([],Para0,_,[],Para0).
coll([H1|T1],Para0,List0,T2,NewPara):- H1=[Para1|List1], List0 = List1, !,
	NewP is Para0+Para1,
	coll(T1,NewP,List0,T2,NewPara).
coll([H1|T1],Para0,List0,[H1|T2],NewPara):-
	coll(T1,Para0,List0,T2,NewPara).

sum_plus_list(A, B):- any_f(A), !,
	B = A.
sum_plus_list(A, B):- atom_num(A), !,
	B = A.
sum_plus_list(List,Lnew):-
	sum_plus(List,Sum,Rest),
	L1 = [[Sum]|Rest],
	sum_again(List, L1, Lnew).

sum_again(L, L, Lnew):- !,
	Lnew = L.
sum_again(_, L, Lnew):-
	sum_plus_list(L, Lnew).

sum_plus(List,Sum,Rest):- sum_p(List,0,Sum,Rest).

sum_p([],SoFar,SoFar,[]).
sum_p([[H]|T],SoFar,Sum,Rest):- numberf(H, NewH), !,
	New is SoFar+NewH,
	sum_p(T,New,Sum,Rest).
sum_p([H|T],SoFar,Sum,Rest):-
	sumt(H,Htime),
	sump_or_not(Htime,T,SoFar,Sum,Rest).

sump_or_not(Htime,T,SoFar,Sum,Rest):- numberf(Htime, NewH), !,
	New is SoFar+NewH,
	sum_p(T,New,Sum,Rest).
sump_or_not(Htime,T,SoFar,Sum,Rest):-
	Rest = [Htime|RestT],
	sum_p(T,SoFar,Sum,RestT).

sumt(List,NewList):-
	sum_time(List,Sum,Rest),
	NewList = [Sum|Rest].

sum_time(List,Sum,Rest):-
	sum_t(List,1,Sum,Rest).

sum_t([],SoFar,SoFar,[]).
sum_t([H|T],SoFar,Sum,Rest):- numberf(H, NewH), !,
	New is SoFar*NewH,
	sum_t(T,New,Sum,Rest).
sum_t([H|T],SoFar,Sum,Rest):- not_deal(H), !,
	Rest=[H|RestT],
	sum_t(T,SoFar,Sum,RestT).
sum_t([H^M|T],SoFar,Sum,Rest):- !,
	sum_plus_list(H,Hplus),
	sumt_or_not(Hplus^M,T,SoFar,Sum,Rest).
sum_t([H|T],SoFar,Sum,Rest):-
	sum_plus_list(H,Hplus),
	sumt_or_not(Hplus,T,SoFar,Sum,Rest).

sumt_or_not(Hplus,T,SoFar,Sum,Rest):- numberf(Hplus, NewH), !,
	New is SoFar+NewH,
	sum_t(T,New,Sum,Rest).
sumt_or_not(Hplus,T,SoFar,Sum,Rest):-
	Rest = [Hplus|RestT],
	sum_t(T,SoFar,Sum,RestT).

expand(E, Eexp):-
	timeinto(E, Etime),
	expand_again(E, Etime, Eexp).

expand_again(E, E, Eexp):- !,
	Eexp = E.
expand_again(_, E, Eexp):-
	expand(E, Eexp).

timeinto(A * (B + C), ABC):- !,
	ABC = A*B+A*C.
timeinto((B + C) * A, ABC):- !,
	ABC = B*A+C*A.
timeinto(A * (B - C), ABC):- !,
	ABC = A*B-A*C.
timeinto((B - C) * A, ABC):- !,
	ABC = B*A-C*A.
timeinto(A + B, AB):- !,
	timeinto(A, A1),
	timeinto(B, B1),
	AB = A1 + B1.
timeinto(A - B, AB):- !,
	timeinto(A, A1),
	timeinto(B, B1),
	AB = A1 - B1.
timeinto(- B, AB):- !,
	timeinto(B, B1),
	AB = - B1.
timeinto(A * B, AB):- !,
	timeinto(A, A1),
	timeinto(B, B1),
	AB = A1 * B1.
timeinto(A / B, AB):- !,
	timeinto(A, A1),
	timeinto(B, B1),
	AB = A1 / B1.
timeinto(F, NewF):- any_f(F), !,
	arg(1, F, A),
	timeinto(A, NewA),
	replace(NewA, F, NewF). 
timeinto(A, A).

numberf(A, N):- number(A), !,
	N is A.
numberf(F, N):- any_f(F), !,
	arg(1, F, A),
	numberf(A, NA),
	replace(NA, F, NewF),
	N is NewF.
numberf(A, N):-
	numf(A, N).

numf(A+B, N):-
	numberf(A, NA),
	numberf(B, NB),
	N is NA+NB.
numf(A-B, N):- !,
	numberf(A, NA),
	numberf(B, NB),
	N is NA-NB.
numf(-B, N):-
	numberf(B, NB),
	N is -NB.
numf(A*B, N):-
	numberf(A, NA),
	numberf(B, NB),
	N is NA*NB.
numf(A/B, N):-
	numberf(A, NA),
	numberf(B, NB),
	div_to_int(NA, NB, N).
numf(A^B, N):- 
	numberf(A, NA),
	numberf(B, NB),
	power(NA, NB, N).

divide_list(List, List, Div):- !,
	Div = 1.
divide_list(List1,List2,Div):- 
	deal_list(List1, Ldeal1),
	deal_list(List2, Ldeal2),
	(divide(Ldeal1,Ldeal2,L) -> true
				 ;  list_time_into(Ldeal1, Ltime1),
				    list_time_into(Ldeal2, Ltime2),	
				    divide(Ltime1,Ltime2,L)),	    
	sum_plus_list(L,Lsum),
	sort_list(Lsum, Div).

deal_list(A, Ldeal):- atom(A), !,
	Ldeal = [[0],[1,A^1]].
deal_list(List, List).

list_time_into(List, Ltime):-
	l2e(List, E),
	sim1(E, Esim1),
	e2l(Esim1, Ltime).

rev_sort_list(List, Lnew):- 
	rev_time(List, L1), 
	rev_sort(L1, L2), 
	t2h(L2, Lnew).

rev_time([], []).
rev_time([H|T], [Hrev|Trev]):-
	rev_sort(H, Hrev),
	rev_time(T, Trev).

t2h([], []). 
t2h([H|T], [Ht2h|Tt2h]):-
	t2h1(H, Ht2h),
	t2h(T, Tt2h).

t2h1(List, [Last|Rest]):- t2h1(List, Last, Rest).

t2h1([Last], L, R):- !, L = Last, R = [].
t2h1([H|T], L, [H|Tr]):-
	t2h1(T, L, Tr).

divide(List1,List2,L):-
	rev_sort_list(List1,Lsor1),
	rev_sort_list(List2,Lsor2),
	div(Lsor1,Lsor2,L).

div([[A]],_,T):- zero(A), !, T = [].
div(List1,List2,[H3|T3]):-
	div1(List1,List2,H3),
	H3=[H3H|H3T],
	H3negH is -1*H3H,
	H3neg=[H3negH|H3T],
	time_list2(List2,H3neg,L),
	append(List1,L,AppL),
	sum_plus_list(AppL,SumpL),
	sort_list(SumpL, Lsort),
	collect_like(Lsort,Lcoll),
	zero_term(Lcoll, Lrem),
	rev_sort_list(Lrem, Lsorti),
	div(Lsorti,List2,T3).

div1([H1|_],[H2|_],H3):-
	dd(H1,H2,H3).

dd([H1|T1],[H2|T2],[H3|T3]):-
	div_to_int(H1, H2, H3),
	d(T2,T1,T3).

d([],List1,List1).
d([H2|T2],List1,Ldivision):-
	d1(List1,H2,NewL1),
	NewL1 \== List1,
	d(T2,NewL1,Ldivision).

d1([],_,[]).
d1([A^M|T],A^N,Lnew):- N >= 0, M > N, !,  
	MN is M-N,
	Lnew = [A^MN|Tnew],
	d1(T,A^N,Tnew).
d1([A|T],A,Lnew):- !,
	d1(T,A,Lnew).
d1([H|T],A,[H|NewT]):-
	d1(T,A,NewT).

int_to_int(A, B, C):-
	integer(A),
	integer(B),
	real_int_div(A, B, C).

div_to_int(A, B, C):-
	int_to_int(A, B, C), !.
div_to_int(A, B, C):-
	C is A/B.

time_list2([],_,[]).
time_list2([H2|T2],H3,[H|T]):- number(H2), !,
	find_h(H2,H3,H),
	time_list2(T2,H3,T).
time_list2([H2|T2],H3,[H|T]):-
	append(H3,H2,AppH),
	sumt(AppH,H),
	time_list2(T2,H3,T).

find_h(H2,[H3|[]],H):- !,
	H is H2*H3.
find_h(H2,[H3|T3],[H|T3]):-
	H is H2*H3.

zero_term([H|T],[H|NewT]):-
	zero_term1(T,NewT).

zero_term1([],[]).
zero_term1([[A|_]|T],NewL):- zero(A), !,
	zero_term1(T,NewL).
zero_term1([H|T],[H|NewT]):-
	zero_term1(T,NewT).

del_list(List, Ldel):-
	del_plus_list(List, Ldelp),
	reduce_plus_list(Ldelp, Ldel).

del_plus_list(L1, L2):-
	del_plus(L1, L3),
	(L3 = [] -> L2 = [[0]]; L2 = L3).

del_plus([], L):- !, L = [].
del_plus([[A|_]|T], Ldelp):- zero(A), !,
	del_plus(T, Ldelp).
del_plus([[A]|T], Ldelp):- number(A), !,
	Ldelp = [[A]|NewT],
	del_plus(T, NewT).
del_plus([H|T], [NewH|NewT]):- !,
	del_time_list(H, NewH),
	del_plus(T, NewT).
del_plus(A, A).

del_time_list(List, Lnew):-
	del_time(List, Ldel),
	(Ldel = [] -> Lnew = [1]; Lnew = Ldel).	

del_time([], []).
del_time([H|T], Ldelt):- unit(H), !,
	del_time(T, Ldelt).
del_time([_^M|T], Ldelt):- zero(M), !,
	del_time(T, Ldelt).
del_time([H^M|T], [NewH|NewT]):- unit(M), not_deal(H), !,
	NewH = H,
	del_time(T, NewT).
del_time([H^M|T], [NewH|NewT]):- !,
	del_plus_list(H, H1),
	NewH = H1^M,
	del_time(T, NewT).
del_time([H|T], [NewH|NewT]):-
	del_plus_list(H, NewH),
	del_time(T, NewT).

reduce_plus_list([], L):- !, L = [].
reduce_plus_list([[-1,A]|T], Lred):- A = [_|_], !,
	insert_neg(A, Lred, NewT),
	reduce_plus_list(T, NewT).
reduce_plus_list([[A]|T], Lred):- A = [_|_], !,
	append(A, NewT, Lred),
	reduce_plus_list(T, NewT).
reduce_plus_list([H|T], [NewH|NewT]):- !,
	reduce_time_list(H, NewH),
	reduce_plus_list(T, NewT).
reduce_plus_list(A, A).

reduce_time_list([], []).
reduce_time_list([[A]|T], Lredt):- !,
	append(A, NewT, Lredt),
	reduce_time_list(T, NewT).
reduce_time_list([H|T], [NewH|NewT]):- not_deal(H), !,
	NewH = H,
	reduce_time_list(T, NewT).
reduce_time_list([H|T], [NewH|NewT]):-
	reduce_plus_list(H, NewH),
	reduce_time_list(T, NewT).

insert_neg([], Tneg, Tneg).
insert_neg([H|T], [Hinv|Tinv], NewT):-
	neg(H, Hinv),
	insert_neg(T, Tinv, NewT).

neg([-1, A], B):- !,
	B = [A].
neg([-1|A], B):- !,
	B = A.
neg(A, [-1|A]).

e2l1(E, List):-
	e2_plus_list(E, [], List).

e2_plus_list(A+B, Sofar, List):- !,
	e2_plus_list(B, Sofar, ListB),
	e2_plus_list(A, ListB, List).
e2_plus_list(A, Sofar, List):-
	e2_time_list(A, ListA),
	List = [ListA|Sofar].

e2_time_list(E, List):- e2_time(E, [], List).

e2_time(A*B, Sofar, List):- !,
	e2_time(B, Sofar, ListB),
	e2_time(A, ListB, List).
e2_time(A, Sofar, List):- atom_num(A), !,
	 List = [A|Sofar].
e2_time(A, Sofar, List):- any_f(A, A1), !,
	List = [A1|Sofar].
e2_time(A^M, Sofar, List):- any_f(A, A1), !,
	List = [A1^M|Sofar].
e2_time(A^M, Sofar, List):- !,
	e2l1_or_not(A, ListA),
	List = [ListA^M|Sofar].
e2_time(A, Sofar, List):-
	e2l1_or_not(A, ListA),
	List = [ListA|Sofar].

any_f(F, F1):-
	any_f(F),
	arg(1, F, A),
	sim(A, A1),
	replace(A1, F, F1).

replace(New, T1, T2):-
	functor(T1, Name, 1),
	functor(T2, Name, 1),
	arg(1, T2, New).

e2l1_or_not(E, LN):- atom_num(E), !,
	LN = E.
e2l1_or_not(E, LN):- any_f(E), !,
	LN = E.
e2l1_or_not(E, LN):-
	e2l1(E, LN).

plus_or_minus(Sofar, ET, T, E, EThead):- numberf(EThead,Num), Num < 0, !,
	l2e1(T, Sofar-ET, E).
plus_or_minus(Sofar, ET, T, E, _):-
	l2e1(T, Sofar+ET, E).

positive_or_negtive(ET, T, E, EThead):-
	numberf(EThead,Num), Num < 0, numberf(ET,En), !,
	ETneg is -En,
	l2e1(T, ETneg, E).
positive_or_negtive(ET, T, E, EThead):-
	numberf(EThead,Num), Num < 0, !,
	ETneg = -ET,
	l2e1(T, ETneg, E).
positive_or_negtive(ET, T, E, _):-
	l2e1(T, ET, E).

l2e(List^M, E):- List = [_|_], !, E = E1^M,
	l2e(List, E1).
l2e([H|T], E):- !,
	l2eT(H,ET,EThead),
	positive_or_negtive(ET, T, E, EThead).
l2e(A, A).

l2e1([], Sofar, Sofar).
l2e1([H|T], Sofar, E):-
	l2eT(H,ET,EThead),
	plus_or_minus(Sofar,ET,T,E,EThead).

l2eT([H|T], E, H):- numberf(H, Hn), Hn < 0, !,
	Hpositive is -1*Hn,
	l2eT1(T, Hpositive, E).
l2eT([H|T], E, H):- l2e(H, He),
	l2eT1(T, He, E).

l2eT1([], Sofar, Sofar).
l2eT1([H|T], Sofar, E):- 
	l2e(H, He),
	(unit(Sofar) -> l2eT1(T, He, E)
		     ;  l2eT1(T, Sofar*He, E)).		

