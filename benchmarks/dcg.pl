% CVS: $Id: dcg.pl,v 1.3 1998/10/20 02:13:54 pets Exp $
/*-----------------------------------------------------------------------------
ICOT/GIGALIPS Benchmark Series: dcg1.pl
Evan Tick
12-19-87

Following is a DCG program corresponding to the PIM Group
benchmark. It is a simple version of Kumon's grammar, used as the 
Kabuwake benchmark. This version has parallel annotations for Gigalips.
To execute:
		?- go(T).

NOTES:        
                   dcg       kl1 (Sato: 1-5-88)    sax
1. timings: PE(1)=27.77sec        31.47 
            PE(2)=15.28sec 
	    PE(4)= 8.19sec         
	    PE(8)= 6.40sec         4.26
	   PE(16)= 5.71sec         2.55
-----------------------------------------------------------------------------*/
:-parallel(sobj/3, pred/3, np/3, p/3, pro_noun/3, gen_noun/3).
:-parallel(adj/3, verb/3, lnol/3).

goal :- go(_).

go(_) :-
	time(_),
	sentence(T, [atarashii, nihon, no, icot, no, pim_group, no,
	             kenkyuusha, wa, atarashii, nihon, no, icot, no,
		     pim_group, no, computer, wo, kokoromiru], []), 
        fail.
go(T) :-
	time(T).

time(T) :- statistics(runtime,[_,T]).

sentence([sentence, T], X, Y) :- sobj(T, X, Y).

sobj([sobj, T], X, Y) :- pred(T, X, Y).
sobj([sobj, T1, T2], X, Z) :- case(T1, X, Y), sobj(T2, Y, Z).

pred([pred, T], X, Y) :- verb(T, X, Y).
pred([pred, T], X, Y) :- adj(T, X, Y).

case([case, T1, T2], X, Z) :- np(T1, X, Y), pp(T2, Y, Z).

np([np, T], X, Y) :- pro_noun(T, X, Y).
np([np, T], X, Y) :- gen_noun(T, X, Y).
np([np, T1, T2], X, Z) :- adj(T1, X, Y), np(T2, Y, Z).
np([np, T1, T2, T3], W, Z) :- lno(T2, W, X, Y), np(T1, X, []), np(T3, Y, Z).

pp([pp, T], X, Y) :- p(T, X, Y).
pp([pp, T1, T2], X, Z) :- p(T1, X, Y), p(T2, Y, Z).

p([p, ga], [ga|X], X).
p([p, wa], [wa|X], X).
p([p, ni], [ni|X], X).
p([p, wo], [wo|X], X).

pro_noun([pro_noun, icot], [icot|X], X).
pro_noun([pro_noun, nihon], [nihon|X], X).
pro_noun([pro_noun, pim_group], [pim_group|X], X).

gen_noun([gen_noun, computer], [computer|X], X).
gen_noun([gen_noun, kenkyuusha], [kenkyuusha|X], X).
gen_noun([gen_noun, system], [system|X], X).

adj([adj, atarashii], [atarashii|X], X).
adj([adj, muzukashii], [muzukashii|X], X).
adj([adj, ureshii], [ureshii|X], X).

verb([verb, kokoromiru], [kokoromiru|X], X).
verb([verb, kuraberu], [kuraberu|X], X).
verb([verb, omou], [omou|X], X).

lno([lno, no], W, X, Y) :- lnol(W, X, Y).

lnol([no|W], [], W).
lnol([H|W], [H|X], Y) :- lnol(W, X, Y).

