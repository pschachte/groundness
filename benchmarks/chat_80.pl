% CVS: $Id: chat_80.pl,v 1.4 1998/10/21 06:20:59 pets Exp $
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
:- op(900,xfx,`=).
:- op(900,xfx,=+).
:- op(900,xfx,=:).
:- op(450,xfy,:).
:- op(400,xfy,&).
:- op(300,fx,`).
:- op(200,xfx,--).
:- op(359,xf,ject).

% compile_load(X):-
% 	compile(X),
% 	load(X).

goal :- go.

go:-
    statistics(runtime,[_,_]),
    see('chat.input'),
    main,
    seen,
    statistics(runtime,[_,T]),
    write('execution time is '),write(T), write(' milliseconds').


	

/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Chat-80 : A small subset of English for database querying.

/**:-public main/0, hi/1, quote/1.

:- mode control(+),
	doing(+,+),
	uses(+,?),
        process(+),
        simplify(+,?),
        simplify(+,?,?),
	simplify_not(+,?),
        revand(+,+,?),
        report(?,+,+,+),
        report_item(+,?).**/

/* Control loop */

main :-
   hi(user).

hi(File) :-
   repeat,
      ask(File,P),
      control(P), !,
      end_of(File).

ask(user,P) :- /**!,**/
   write('Question: '),
   /**ttyflush,**/
   read_in(P).
/**ask(File,P) :-
   seeing(Old),
   see(File),
   read_in(P),
   nl,
   doing(P,0),
   nl,
   see(Old).**/

doing([],_) :- !.
doing([X|L],N0) :-
   out(X),
   advance(X,N0,N),
   doing(L,N).

out(nb(X)) :- !,
   write(X).
out(A) :-
   write(A).

advance(X,N0,N) :-
   uses(X,K),
   M is N0+K,
 ( M>72, !,
      nl,
      N is 0;
   N is M+1,
      put(0' )).

uses(nb(X),N) :- !,
   chars(X,N).
uses(X,N) :-
   chars(X,N).

chars(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars(_,2).

end_of(user) /**:- !**/.
/**end_of(F) :-
   close(F).**/

control([bye,'.']) :- !,
   write('Cheerio.'),
   nl.
control([trace,'.']) :- !,
   tracing `= on,
   write('Tracing from now on!'), nl, fail.
control([do,not,trace,'.']) :- !,
   tracing `= off,
   write('No longer tracing.'), nl, fail.
control(U0) :-
   check_words(U0,U),
   process(U),
   fail.

process(U) :-
   /**statistics(runtime,_),**/
   sentence(E,U,[],[],[]),
   /**statistics(runtime,[_,Et0]),
   report(E,'Parse',Et0,tree),
   statistics(runtime,_),**/
   i_sentence(E,QT),
   clausify(QT,UE),
   simplify(UE,S),
   /**statistics(runtime,[_,Et1]),
   report(S,'Semantics',Et1,expr),
   statistics(runtime,_),**/
   qplan(S,S1), !,
   /**statistics(runtime,[_,Et2]),
   report(S1,'Planning',Et2,expr),
   statistics(runtime,_),**/
   answer(S1), !, nl/**,
   statistics(runtime,[_,Et3]),
   report(_,'Reply',Et3,none)**/.
process(_) :-
   failure.

failure :-
   write('I don''t understand!'), nl.

/**report(Item,Label,Time,Mode) :-
   tracing =: on, !,
   nl, write(Label), write(': '), write(Time), write('msec.'), nl,
   report_item(Mode,Item).
report(_,_,_,_).

report_item(none,_).
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(tree,Item) :-
   print_tree(Item), nl.
report_item(quant,Item) :-
   pp_quant(Item,2), nl.

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_,_,_,_,_)).
quote(wh(_)).
quote(name(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_,_)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).
**/

simplify(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify(Q,R,true).

simplify(setof(X,P0,S),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,setof(X,P,S),R).
simplify((P,Q),R,R0) :-
   simplify(Q,R1,R0),
   simplify(P,R,R1).
simplify(true,R,R) :- !.
simplify(X^P0,R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,X^P,R).
simplify(numberof(X,P0,Y),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,numberof(X,P,Y),R).
simplify(\+P0,R,R0) :- !,
   simplify(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R).
simplify(P,R,R0) :-
   revand(R0,P,R).

simplify_not(\+P,P) :- !.
simplify_not(P,\+P).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C0,C) :-
   numbervars(C0,1,N),
   functor(V,v,N),
   functor(M,v,N),
   inv_map(C0,V,M,C).

inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
   functor(T,F,K),
   functor(R,F,K),
   inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
   arg(K0,T,A),
   arg(K0,R,B),
   inv_map(A,V,M,B),
   K is K0-1,
   inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
 ( I=\=J, !,
      irev(I,J,K,L), 
      arg(K,M,L),
      arg(K,V,X),
      arg(L,V,X);
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   deref(I,M,J),
   arg(J,V,T),
   arg(J,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   deref(I,M,J),
   arg(J,V,T),
   arg(J,M,0).
drop_eq(X,Y,_,_,X=Y).

deref(I,M,J) :-
   arg(I,M,X),
  (var(X), !, I=J;
   deref(X,M,J)).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
      arg(I,V,X),
       P=(X^P0);
   P=P0).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

/**:- mode check_words(+,-).**/

check_words([],[]).
check_words([Word|Words],[RevWord|RevWords]) :-
   check_word(Word,RevWord),
   check_words(Words,RevWords).

/**:- mode check_word(+,-).**/

check_word(Word,Word) :- word(Word), !.
check_word(Word,NewWord) :-
   write('? '), write(Word), write(' -> (!. to abort) '), /**ttyflush,**/
   read(NewWord0),
   NewWord0 \== !,
   check_word(NewWord0,NewWord).

/**:- mode `=(+,+), =+(+,-), =:(+,?).**/

Var `= Val :-
 ( recorded(Var,val(_),P), erase(P)
 ; true), !,
 recordz(Var,val(Val),_).

% Var =+ Val :-
%  ( recorded(Var,val(Val0),P), erase(P)
%  ; Val0 is 0), !,
%    Val is Val0+1,
%    recordz(Var,val(Val),_).

% Var =: Val :-
%    recorded(Var,val(Val),_).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/* Read a sentence */

 /**:- mode initread(-).
 :- mode readrest(+,-).
 :- mode word(-,?,?).
 :- mode words(-,?,?).
 :- mode alphanum(+,-).
 :- mode alphanums(-,?,?).
 :- mode digits(-,?,?).
 :- mode digit(+).
 :- mode lc(+,-).

 :- public read_in/1.**/

% :- mode('$ get'(A),uninit(A),true,(rderef(A),ground(A),integer(A),deref(A)),n).
% :- mode('@ get0'(A),uninit_reg(A),true,(integer(A),deref(A)),y).
% :- mode('$ skip'(A),deref(A),(ground(A),nonnegative(A)),ground(A),n).

/* Read sentence */

read_in(P):-initread(L),words(P,L,[]),!,to_nl.

initread([K1,K2|U]):-get(K1),get0(K2),readrest(K2,U).
% initread([K1,K2|U]):-'$ get'(K1),'@ get0'(K2),readrest(K2,U).

readrest(46,[]):-!.
readrest(63,[]):-!.
readrest(33,[]):-!.
readrest(K,[K1|U]):-K=<32,!,get(K1),readrest(K1,U).
readrest(K1,[K2|U]):-get0(K2),readrest(K2,U).
% readrest(K,[K1|U]):-K=<32,!,'$ get'(K1),readrest(K1,U).
% readrest(K1,[K2|U]):-'@ get0'(K2),readrest(K2,U).

words([V|U]) --> word(V),!,blanks,words(U).
words([]) --> [].

word(U1) --> [K],{lc(K,K1)},!,alphanums(U2),{name(U1,[K1|U2])}.
word(nb(N)) --> [K],{digit(K)},!,digits(U),{name(N,[K|U])}.
word(V) --> [K],{name(V,[K])}.

alphanums([K1|U]) --> [K],{alphanum(K,K1)},!,alphanums(U).
alphanums([]) --> [].

alphanum(95,95) :- !.
alphanum(K,K1):-lc(K,K1).
alphanum(K,K):-digit(K).

digits([K|U]) --> [K],{digit(K)},!,digits(U).
digits([]) --> [].

blanks--> [K],{K=<32},!,blanks.
blanks --> [].

digit(K):-K>47,K<58.

lc(K,K1):-K>64,K<91,!,K1 is K\/8'40.
lc(K,K):-K>96,K<123.

to_nl :-
%   repeat,
%   get0(10), !.
%  '$ skip'(10).
  skip(10).


/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/**:- mode terminal(?,+,?,+,?),
        gap(+),
        virtual(+,+,?).**/

terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

gap(x(gap,_,_,_)).
gap([]).

virtual(NT,x(_,nonterminal,NT,X),X).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/

sentence(B,C,D,E,F) :-
   declarative(B,C,G,E,H),
   terminator(.,G,D,H,F).
sentence(B,C,D,E,F) :-
   wh_question(B,C,G,E,H),
   terminator(?,G,D,H,F).
sentence(B,C,D,E,F) :-
   topic(C,G,E,H),
   wh_question(B,G,I,H,J),
   terminator(?,I,D,J,F).
sentence(B,C,D,E,F) :-
   yn_question(B,C,G,E,H),
   terminator(?,G,D,H,F).
sentence(B,C,D,E,F) :-
   imperative(B,C,G,E,H),
   terminator(!,G,D,H,F).


pp(B,C,D,E,F,F,G,H) :-
   virtual(pp(B,C,D,E),G,H).
pp(pp(B,C),D,E,F,G,H,I,J) :-
   prep(B,G,K,I,L),
   prep_case(M),
   np(C,N,M,O,D,E,F,K,H,L,J).


topic(B,C,D,x(gap,nonterminal,pp(E,compl,F,G),H)) :-
   pp(E,compl,F,G,B,I,D,J),
   opt_comma(I,C,J,H).


opt_comma(B,C,D,E) :-
   `(',',B,C,D,E).
opt_comma(B,B,C,C).


declarative(decl(B),C,D,E,F) :-
   s(B,G,C,D,E,F).


wh_question(whq(B,C),D,E,F,G) :-
   variable_q(B,H,I,J,D,K,F,L),
   question(I,J,C,K,E,L,G).


np(B,C,D,E,F,G,H,I,I,J,K) :-
   virtual(np(B,C,D,E,F,G,H),J,K).
np(np(B,C,[]),B,D,def,E,F,G,H,I,J,K) :-
   is_pp(F),
   pers_pron(C,B,L,H,I,J,K),
   empty(G),
   role(L,decl,D).
np(np(B,C,D),B,E,F,G,H,I,J,K,L,M) :-
   is_pp(H),
   np_head(C,B,F+N,O,D,J,P,L,Q),
   np_all(R),
   np_compls(N,B,G,O,R,I,P,K,Q,M).
np(part(B,C),3+D,E,indef,F,G,H,I,J,K,L) :-
   is_pp(G),
   determiner(B,D,indef,I,M,K,N),
   `(of,M,O,N,P),
   s_all(Q),
   prep_case(R),
   np(C,3+plu,R,def,F,Q,H,O,J,P,L).


variable_q(B,C,D,E,F,G,H,x(gap,nonterminal,np(I,C,E,J,K,L,M),N)) :-
   whq(B,C,I,D,F,G,H,N),
   trace(L,M).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,pp(pp(H,I),compl,J,K),L)) :-
   prep(H,E,M,G,N),
   whq(B,C,I,O,M,F,N,L),
   trace(J,K),
   compl_case(D).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,adv_phrase(pp(H,np(C,np_head(int_det(B),[],I),[])),J,K),L)) :-
   context_pron(H,I,E,F,G,L),
   trace(J,K),
   verb_case(D).
variable_q(B,C,compl,D,E,F,G,x(gap,nonterminal,pred(adj,value(H,wh(B)),I),J)) :-
   `(how,E,K,G,L),
   adj(quant,H,K,F,L,J),
   empty(I),
   verb_case(D).


adv_phrase(B,C,D,E,E,F,G) :-
   virtual(adv_phrase(B,C,D),F,G).
adv_phrase(pp(B,C),D,E,F,G,H,I) :-
   loc_pred(B,F,J,H,K),
   pp(pp(prep(of),C),compl,D,E,J,G,K,I).


pred(B,C,D,E,E,F,G) :-
   virtual(pred(B,C,D),F,G).
pred(B,C,D,E,F,G,H) :-
   adj_phrase(C,D,E,F,G,H).
pred(neg,B,C,D,E,F,G) :-
   s_all(H),
   pp(B,compl,H,C,D,E,F,G).
pred(B,C,D,E,F,G,H) :-
   s_all(I),
   adv_phrase(C,I,D,E,F,G,H).


whq(B,C,D,undef,E,F,G,H) :-
   int_det(B,C,E,I,G,J),
   s_all(K),
   np(D,C,L,M,subj,K,N,I,F,J,H).
whq(B,3+C,np(3+C,wh(B),[]),D,E,F,G,H) :-
   int_pron(D,E,F,G,H).


int_det(B,3+C,D,E,F,G) :-
   whose(B,C,D,E,F,G).
int_det(B,3+C,D,E,F,G) :-
   int_art(B,C,D,E,F,G).


np_head0(B,C,D,E,E,F,G) :-
   virtual(np_head0(B,C,D),F,G).
np_head0(name(B),3+sin,def+proper,C,D,E,F) :-
   name(B,C,D,E,F).
np_head0(np_head(B,C,D),3+E,F+common,G,H,I,J) :-
   determiner(B,E,F,G,K,I,L),
   adjs(C,K,M,L,N),
   noun(D,E,M,H,N,J).
np_head0(B,C,def+proper,D,E,F,x(nogap,nonterminal,gen_marker,G)) :-
   poss_pron(B,C,D,E,F,G).
np_head0(np_head(B,[],C),3+sin,indef+common,D,E,F,G) :-
   quantifier_pron(B,C,D,E,F,G).


gen_marker(B,B,C,D) :-
   virtual(gen_marker,C,D).
gen_marker(B,C,D,E) :-
   `('''',B,F,D,G),
   an_s(F,C,G,E).


whose(B,C,D,E,F,x(nogap,nonterminal,np_head0(wh(B),C,proper),x(nogap,nonterminal,gen_marker,G))) :-
   `(whose,D,E,F,G).


question(B,C,D,E,F,G,H) :-
   subj_question(B),
   role(subj,I,C),
   s(D,J,E,F,G,H).
question(B,C,D,E,F,G,H) :-
   fronted_verb(B,C,E,I,G,J),
   s(D,K,I,F,J,H).


det(B,C,D,E,E,F,G) :-
   virtual(det(B,C,D),F,G).
det(det(B),C,D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   det(I,C,B,D).
det(generic,B,generic,C,C,D,D).


int_art(B,C,D,E,F,x(nogap,nonterminal,det(G,C,def),H)) :-
   int_art(B,C,G,D,E,F,H).


subj_question(subj).


subj_question(undef).


yn_question(q(B),C,D,E,F) :-
   fronted_verb(nil,G,C,H,E,I),
   s(B,J,H,D,I,F).


verb_form(B,C,D,E,F,F,G,H) :-
   virtual(verb_form(B,C,D,E),G,H).
verb_form(B,C,D,E,F,G,H,I) :-
   terminal(J,F,G,H,I),
   verb_form(J,B,C,D).


neg(B,C,D,D,E,F) :-
   virtual(neg(B,C),E,F).
neg(aux+B,neg,C,D,E,F) :-
   `(not,C,D,E,F).
neg(B,pos,C,C,D,D).


fronted_verb(B,C,D,E,F,x(gap,nonterminal,verb_form(G,H,I,J),x(nogap,nonterminal,neg(K,L),M))) :-
   verb_form(G,H,I,N,D,O,F,P),
   verb_type(G,aux+Q),
   role(B,J,C),
   neg(R,L,O,E,P,M).


imperative(imp(B),C,D,E,F) :-
   imperative_verb(C,G,E,H),
   s(B,I,G,D,H,F).


imperative_verb(B,C,D,x(nogap,terminal,you,x(nogap,nonterminal,verb_form(E,imp+fin,2+sin,main),F))) :-
   verb_form(E,inf,G,H,B,C,D,F).


s(s(B,C,D,E),F,G,H,I,J) :-
   subj(B,K,L,G,M,I,N),
   verb(C,K,L,O,M,P,N,Q),
   empty(R),
   s_all(S),
   verb_args(L,O,D,R,T,P,U,Q,V),
   minus(S,T,W),
   plus(S,T,X),
   verb_mods(E,W,X,F,U,H,V,J).


subj(there,B,C+be,D,E,F,G) :-
   `(there,D,E,F,G).
subj(B,C,D,E,F,G,H) :-
   s_all(I),
   subj_case(J),
   np(B,C,J,K,subj,I,L,E,F,G,H).


np_head(B,C,D,E,F,G,H,I,J) :-
   np_head0(K,L,M,G,N,I,O),
   possessive(K,L,M,P,P,B,C,D,E,F,N,H,O,J).


np_compls(proper,B,C,[],D,E,F,F,G,G) :-
   empty(E).
np_compls(common,B,C,D,E,F,G,H,I,J) :-
   np_all(K),
   np_mods(B,C,L,D,E,M,K,N,G,O,I,P),
   relative(B,L,M,N,F,O,H,P,J).


possessive(B,C,D,[],E,F,G,H,I,J,K,L,M,N) :-
   gen_case(K,O,M,P),
   np_head0(Q,R,S,O,T,P,U),
   possessive(Q,R,S,V,[pp(poss,np(C,B,E))|V],F,G,H,I,J,T,L,U,N).
possessive(B,C,D,E,F,B,C,D,E,F,G,G,H,H).


gen_case(B,C,D,x(nogap,terminal,the,E)) :-
   gen_marker(B,C,D,E).


an_s(B,C,D,E) :-
   `(s,B,C,D,E).
an_s(B,B,C,C).


determiner(B,C,D,E,F,G,H) :-
   det(B,C,D,E,F,G,H).
determiner(B,C,D,E,F,G,H) :-
   quant_phrase(B,C,D,E,F,G,H).


quant_phrase(quant(B,C),D,E,F,G,H,I) :-
   quant(B,E,F,J,H,K),
   number(C,D,J,G,K,I).


quant(B,indef,C,D,E,F) :-
   neg_adv(G,B,C,H,E,I),
   comp_adv(G,H,J,I,K),
   `(than,J,D,K,F).
quant(B,indef,C,D,E,F) :-
   `(at,C,G,E,H),
   sup_adv(I,G,D,H,F),
   sup_op(I,B).
quant(the,def,B,C,D,E) :-
   `(the,B,C,D,E).
quant(same,indef,B,B,C,C).


neg_adv(B,not+B,C,D,E,F) :-
   `(not,C,D,E,F).
neg_adv(B,B,C,C,D,D).


sup_op(least,not+less).
sup_op(most,not+more).


np_mods(B,C,D,[E|F],G,H,I,J,K,L,M,N) :-
   np_mod(B,C,E,G,O,K,P,M,Q),
   trace(R),
   plus(R,O,S),
   minus(G,S,T),
   plus(O,G,U),
   np_mods(B,C,D,F,T,H,U,J,P,L,Q,N).
np_mods(B,C,D,D,E,E,F,F,G,G,H,H).


np_mod(B,C,D,E,F,G,H,I,J) :-
   pp(D,C,E,F,G,H,I,J).
np_mod(B,C,D,E,F,G,H,I,J) :-
   reduced_relative(B,D,E,F,G,H,I,J).


verb_mods([B|C],D,E,F,G,H,I,J) :-
   verb_mod(B,D,K,G,L,I,M),
   trace(N),
   plus(N,K,O),
   minus(D,O,P),
   plus(K,D,Q),
   verb_mods(C,P,Q,F,L,H,M,J).
verb_mods([],B,C,C,D,D,E,E).


verb_mod(B,C,D,E,F,G,H) :-
   adv_phrase(B,C,D,E,F,G,H).
verb_mod(B,C,D,E,F,G,H) :-
   is_adv(C),
   adverb(B,E,F,G,H),
   empty(D).
verb_mod(B,C,D,E,F,G,H) :-
   pp(B,compl,C,D,E,F,G,H).


adjs([B|C],D,E,F,G) :-
   pre_adj(B,D,H,F,I),
   adjs(C,H,E,I,G).
adjs([],B,B,C,C).


pre_adj(B,C,D,E,F) :-
   adj(G,B,C,D,E,F).
pre_adj(B,C,D,E,F) :-
   sup_phrase(B,C,D,E,F).


sup_phrase(sup(most,B),C,D,E,F) :-
   sup_adj(B,C,D,E,F).
sup_phrase(sup(B,C),D,E,F,G) :-
   sup_adv(B,D,I,F,J),
   adj(quant,C,I,E,J,G).


comp_phrase(comp(B,C,D),E,F,G,H,I) :-
   comp(B,C,F,J,H,K),
   np_no_trace(L),
   prep_case(M),
   np(D,N,M,O,compl,L,E,J,G,K,I).


comp(B,C,D,E,F,G) :-
   comp_adv(B,D,H,F,I),
   adj(quant,C,H,J,I,K),
   `(than,J,E,K,G).
comp(more,B,C,D,E,F) :-
   rel_adj(B,C,G,E,H),
   `(than,G,D,H,F).
comp(same,B,C,D,E,F) :-
   `(as,C,G,E,H),
   adj(quant,B,G,I,H,J),
   `(as,I,D,J,F).


relative(B,[C],D,E,F,G,H,I,J) :-
   is_pred(D),
   rel_conj(B,K,C,F,G,H,I,J).
relative(B,[],C,D,D,E,E,F,F).


rel_conj(B,C,D,E,F,G,H,I) :-
   rel(B,J,K,F,L,H,M),
   rel_rest(B,C,J,D,K,E,L,G,M,I).


rel_rest(B,C,D,E,F,G,H,I,J,K) :-
   conj(C,L,D,M,E,H,N,J,O),
   rel_conj(B,L,M,G,N,I,O,K).
rel_rest(B,C,D,D,E,E,F,F,G,G).


rel(B,rel(C,D),E,F,G,H,I) :-
   open4(F,J,H,K),
   variable(B,C,J,L,K,M),
   s(D,N,L,O,M,P),
   trace(Q),
   minus(N,Q,E),
   close(O,G,P,I).


variable(B,C,D,E,F,x(gap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),L)) :-
   `(that,D,E,F,L),
   trace(J,K).
variable(B,C,D,E,F,x(gap,nonterminal,np(G,H,I,J,K,L,M),N)) :-
   wh(C,B,G,H,I,D,E,F,N),
   trace(L,M).
variable(B,C,D,E,F,x(gap,nonterminal,pp(pp(G,H),compl,I,J),K)) :-
   prep(G,D,L,F,M),
   wh(C,B,H,N,O,L,E,M,K),
   trace(I,J),
   compl_case(O).


wh(B,C,np(C,wh(B),[]),C,D,E,F,G,H) :-
   rel_pron(I,E,F,G,H),
   role(I,decl,D).
wh(B,C,np(D,E,[pp(F,G)]),D,H,I,J,K,L) :-
   np_head0(E,D,M+common,I,N,K,O),
   prep(F,N,P,O,Q),
   wh(B,C,G,R,S,P,J,Q,L).
wh(B,C,D,E,F,G,H,I,J) :-
   whose(B,C,G,K,I,L),
   s_all(M),
   np(D,E,F,def,subj,M,N,K,H,L,J).


reduced_relative(B,C,D,E,F,G,H,I) :-
   is_pred(D),
   reduced_rel_conj(B,J,C,E,F,G,H,I).


reduced_rel_conj(B,C,D,E,F,G,H,I) :-
   reduced_rel(B,J,K,F,L,H,M),
   reduced_rel_rest(B,C,J,D,K,E,L,G,M,I).


reduced_rel_rest(B,C,D,E,F,G,H,I,J,K) :-
   conj(C,L,D,M,E,H,N,J,O),
   reduced_rel_conj(B,L,M,G,N,I,O,K).
reduced_rel_rest(B,C,D,D,E,E,F,F,G,G).


reduced_rel(B,reduced_rel(C,D),E,F,G,H,I) :-
   open4(F,J,H,K),
   reduced_wh(B,C,J,L,K,M),
   s(D,N,L,O,M,P),
   trace(Q),
   minus(N,Q,E),
   close(O,G,P,I).


reduced_wh(B,C,D,E,F,x(nogap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),x(nogap,nonterminal,verb_form(be,pres+fin,B,main),x(nogap,nonterminal,neg(L,M),x(nogap,nonterminal,pred(M,N,O),P))))) :-
   neg(Q,M,D,R,F,S),
   pred(M,N,O,R,E,S,P),
   trace(J,K),
   subj_case(G).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,np(np(B,wh(C),[]),B,G,H,I,J,K),x(nogap,nonterminal,verb(L,M,N,O),P))) :-
   participle(L,N,O,D,E,F,P),
   trace(J,K),
   subj_case(G).
reduced_wh(B,C,D,E,F,x(nogap,nonterminal,np(G,H,I,J,K,L,M),x(gap,nonterminal,np(np(B,wh(C),[]),B,N,O,P,Q,R),S))) :-
   s_all(T),
   subj_case(I),
   verb_case(N),
   np(G,H,U,J,subj,T,V,D,E,F,S),
   trace(L,M),
   trace(Q,R).


verb(B,C,D,E,F,F,G,H) :-
   virtual(verb(B,C,D,E),G,H).
verb(verb(B,C,D+fin,E,F),G,H,C,I,J,K,L) :-
   verb_form(M,D+fin,G,N,I,O,K,P),
   verb_type(M,Q),
   neg(Q,F,O,R,P,S),
   rest_verb(N,M,B,C,E,R,J,S,L),
   verb_type(B,H).


rest_verb(aux,have,B,C,[perf|D],E,F,G,H) :-
   verb_form(I,past+part,J,K,E,L,G,M),
   have(I,B,C,D,L,F,M,H).
rest_verb(aux,be,B,C,D,E,F,G,H) :-
   verb_form(I,J,K,L,E,M,G,N),
   be(J,I,B,C,D,M,F,N,H).
rest_verb(aux,do,B,active,[],C,D,E,F) :-
   verb_form(B,inf,G,H,C,D,E,F).
rest_verb(main,B,B,active,[],C,C,D,D).


have(be,B,C,D,E,F,G,H) :-
   verb_form(I,J,K,L,E,M,G,N),
   be(J,I,B,C,D,M,F,N,H).
have(B,B,active,[],C,C,D,D).


be(past+part,B,B,passive,[],C,C,D,D).
be(pres+part,B,C,D,[prog],E,F,G,H) :-
   passive(B,C,D,E,F,G,H).


passive(be,B,passive,C,D,E,F) :-
   verb_form(B,past+part,G,H,C,D,E,F),
   verb_type(B,I),
   passive(I).
passive(B,B,active,C,C,D,D).


participle(verb(B,C,inf,D,E),F,C,G,H,I,J) :-
   neg(K,E,G,L,I,M),
   verb_form(B,N,O,P,L,H,M,J),
   participle(N,C,D),
   verb_type(B,F).


passive(B+trans).
passive(B+ditrans).


participle(pres+part,active,[prog]).
participle(past+part,passive,[]).


close(B,B,C,D) :-
   virtual(close,C,D).


open4(B,B,C,x(gap,nonterminal,close,C)).


verb_args(B+C,D,E,F,G,H,I,J,K) :-
   advs(E,L,M,H,N,J,O),
   verb_args(C,D,L,F,G,N,I,O,K).
verb_args(trans,active,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
verb_args(ditrans,B,[arg(C,D)|E],F,G,H,I,J,K) :-
   verb_arg(np,D,L,H,M,J,N),
   object(C,E,L,G,M,I,N,K).
verb_args(be,B,[void],C,C,D,E,F,G) :-
   terminal(there,D,E,F,G).
verb_args(be,B,[arg(pred,C)],D,E,F,G,H,I) :-
   pred_conj(J,C,E,F,G,H,I).
verb_args(be,B,[arg(dir,C)],D,E,F,G,H,I) :-
   verb_arg(np,C,E,F,G,H,I).
verb_args(have,active,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
verb_args(B,C,[],D,D,E,E,F,F) :-
   no_args(B).


object(B,C,D,E,F,G,H,I) :-
   adv(J),
   minus(J,D,K),
   advs(C,L,K,F,M,H,N),
   obj(B,L,D,E,M,G,N,I).


obj(ind,[arg(dir,B)],C,D,E,F,G,H) :-
   verb_arg(np,B,D,E,F,G,H).
obj(dir,[],B,B,C,C,D,D).


pred_conj(B,C,D,E,F,G,H) :-
   pred(I,J,K,E,L,G,M),
   pred_rest(B,J,C,K,D,L,F,M,H).


pred_rest(B,C,D,E,F,G,H,I,J) :-
   conj(B,K,C,L,D,G,M,I,N),
   pred_conj(K,L,F,M,H,N,J).
pred_rest(B,C,C,D,D,E,E,F,F).


verb_arg(np,B,C,D,E,F,G) :-
   s_all(H),
   verb_case(I),
   np(B,J,I,K,compl,H,C,D,E,F,G).


advs([B|C],D,E,F,G,H,I) :-
   is_adv(E),
   adverb(B,F,J,H,K),
   advs(C,D,E,J,G,K,I).
advs(B,B,C,D,D,E,E).


adj_phrase(B,C,D,E,F,G) :-
   adj(H,B,D,E,F,G),
   empty(C).
adj_phrase(B,C,D,E,F,G) :-
   comp_phrase(B,C,D,E,F,G).


no_args(trans).
no_args(ditrans).
no_args(intrans).


conj(conj(B,C),conj(B,D),E,F,conj(B,E,F),G,H,I,J) :-
   conj(B,C,D,G,H,I,J).


noun(B,C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   noun_form(H,B,C).


adj(B,adj(C),D,E,F,G) :-
   terminal(C,D,E,F,G),
   adj(C,B).


prep(prep(B),C,D,E,F) :-
   terminal(B,C,D,E,F),
   prep(B).


rel_adj(adj(B),C,D,E,F) :-
   terminal(G,C,D,E,F),
   rel_adj(G,B).


sup_adj(adj(B),C,D,E,F) :-
   terminal(G,C,D,E,F),
   sup_adj(G,B).


comp_adv(less,B,C,D,E) :-
   `(less,B,C,D,E).
comp_adv(more,B,C,D,E) :-
   `(more,B,C,D,E).


sup_adv(least,B,C,D,E) :-
   `(least,B,C,D,E).
sup_adv(most,B,C,D,E) :-
   `(most,B,C,D,E).


rel_pron(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   rel_pron(G,B).


name(B,C,D,E,F) :-
   opt_the(C,G,E,H),
   terminal(B,G,D,H,F),
   name(B).


int_art(B,plu,quant(same,wh(B)),C,D,E,F) :-
   `(how,C,G,E,H),
   `(many,G,D,H,F).
int_art(B,C,D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   int_art(I,B,C,D).


int_pron(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   int_pron(G,B).


adverb(adv(B),C,D,E,F) :-
   terminal(B,C,D,E,F),
   adverb(B).


poss_pron(pronoun(B),C+D,E,F,G,H) :-
   terminal(I,E,F,G,H),
   poss_pron(I,B,C,D).


pers_pron(pronoun(B),C+D,E,F,G,H,I) :-
   terminal(J,F,G,H,I),
   pers_pron(J,B,C,D,E).


quantifier_pron(B,C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   quantifier_pron(H,B,C).


context_pron(prep(in),place,B,C,D,E) :-
   `(where,B,C,D,E).
context_pron(prep(at),time,B,C,D,E) :-
   `(when,B,C,D,E).


number(nb(B),C,D,E,F,G) :-
   terminal(H,D,E,F,G),
   number(H,B,C).


terminator(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   terminator(G,B).


opt_the(B,B,C,C).
opt_the(B,C,D,E) :-
   `(the,B,C,D,E).


conj(B,list,list,C,D,E,F) :-
   terminal(',',C,D,E,F).
conj(B,list,(end),C,D,E,F) :-
   terminal(B,C,D,E,F),
   conj(B).


loc_pred(B,C,D,E,F) :-
   terminal(G,C,D,E,F),
   loc_pred(G,B).


`(B,C,D,E,F) :-
   terminal(B,C,D,E,F),
   `B.
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Normal form masks

is_pp(#(1,_,_,_)).

is_pred(#(_,1,_,_)).

% is_trace(#(_,_,1,_)).

is_adv(#(_,_,_,1)).

trace(#(_,_,1,_),#(0,0,0,0)).

trace(#(0,0,1,0)).

adv(#(0,0,0,1)).

empty(#(0,0,0,0)).

np_all(#(1,1,1,0)).

s_all(#(1,0,1,1)).

np_no_trace(#(1,1,0,0)).

% Mask operations

plus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   or(B1,C1,D1),
   or(B2,C2,D2),
   or(B3,C3,D3),
   or(B4,C4,D4).

minus(#(B1,B2,B3,B4),#(C1,C2,C3,C4),#(D1,D2,D3,D4)) :-
   anot(B1,C1,D1),
   anot(B2,C2,D2),
   anot(B3,C3,D3),
   anot(B4,C4,D4).

or(1,_,1).
or(0,1,1).
or(0,0,0).

anot(X,0,X).
anot(X,1,0).

% Noun phrase position features

role(subj,_,#(1,0,0)).
role(compl,_,#(0,_,_)).
role(undef,main,#(_,0,_)).
role(undef,aux,#(0,_,_)).
role(undef,decl,_).
role(nil,_,_).

subj_case(#(1,0,0)).
verb_case(#(0,1,0)).
prep_case(#(0,0,1)).
compl_case(#(0,_,_)).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Modes

/**:- mode word(+).
:- mode `(+).
:- mode conj(+).
:- mode adverb(+).
:- mode sup_adj(+,?).
:- mode rel_adj(+,?).
:- mode adj(+,?).
:- mode name_template(+,-).
:- mode name(+).
:- mode terminator(+,?).
:- mode pers_pron(+,?,?,?,?).
:- mode poss_pron(+,?,?,?).
:- mode rel_pron(+,?).
:- mode regular_past(+,?).
:- mode regular_pres(+).
:- mode verb_root(+).
:- mode verb_form(+,?,?,?).
:- mode noun_sin(+).
:- mode noun_plu(+,?).
:- mode noun_form(+,?,?).
:- mode prep(+).
:- mode quantifier_pron(+,?,?).
:- mode tr_number(+,?).
:- mode number(+,?,?).
:- mode det(+,?,?,?).
:- mode int_art(+,?,?,?).
:- mode int_pron(+,?).**/

% =================================================================
% General Dictionary

word(Word) :- `(Word).
word(Word) :- conj(Word).
word(Word) :- adverb(Word).
word(Word) :- sup_adj(Word,_).
word(Word) :- rel_adj(Word,_).
word(Word) :- adj(Word,_).
word(Word) :- name(Word).
word(Word) :- terminator(Word,_).
word(Word) :- pers_pron(Word,_,_,_,_).
word(Word) :- poss_pron(Word,_,_,_).
word(Word) :- rel_pron(Word,_).
word(Word) :- verb_form(Word,_,_,_).
word(Word) :- noun_form(Word,_,_).
word(Word) :- prep(Word).
word(Word) :- quantifier_pron(Word,_,_).
word(Word) :- number(Word,_,_).
word(Word) :- det(Word,_,_,_).
word(Word) :- int_art(Word,_,_,_).
word(Word) :- int_pron(Word,_).
word(Word) :- loc_pred(Word,_).

`how.
`whose.
`there.
`of.
`('''').
`(',').
`s.
`than.
`at.
`the.
`not.
`as.
`that.
`less.
`more.
`least.
`most.
`many.
`where.
`when.
conj(and).
conj(or).

int_pron(what,undef).
int_pron(which,undef).
int_pron(who,subj).
int_pron(whom,compl).

int_art(what,X,_,int_det(X)).
int_art(which,X,_,int_det(X)).

det(the,No,the(No),def).
det(a,sin,a,indef).
det(an,sin,a,indef).
det(every,sin,every,indef).
det(some,_,some,indef).
det(any,_,any,indef).
det(all,plu,all,indef).
det(each,sin,each,indef).
det(no,_,no,indef).

number(W,I,Nb) :-
   tr_number(W,I),
   ag_number(I,Nb).

tr_number(nb(I),I).
tr_number(one,1).
tr_number(two,2).
tr_number(three,3).
tr_number(four,4).
tr_number(five,5).
tr_number(six,6).
tr_number(seven,7).
tr_number(eight,8).
tr_number(nine,9).
tr_number(ten,10).

ag_number(1,sin).
ag_number(N,plu) :- N>1.

quantifier_pron(everybody,every,person).
quantifier_pron(everyone,every,person).
quantifier_pron(everything,every,thing).
quantifier_pron(somebody,some,person).
quantifier_pron(someone,some,person).
quantifier_pron(something,some,thing).
quantifier_pron(anybody,any,person).
quantifier_pron(anyone,any,person).
quantifier_pron(anything,any,thing).
quantifier_pron(nobody,no,person).
quantifier_pron(nothing,no,thing).

prep(as).
prep(at).
prep(of).
prep(to).
prep(by).
prep(with).
prep(in).
prep(on).
prep(from).
prep(into).
prep(through).

noun_form(Plu,Sin,plu) :- noun_plu(Plu,Sin).
noun_form(Sin,Sin,sin) :- noun_sin(Sin).
noun_form(proportion,proportion,_).
noun_form(percentage,percentage,_).

verb_form(V,V,inf,_) :- verb_root(V).
verb_form(V,V,pres+fin,Agmt) :-
   regular_pres(V),
   root_form(Agmt),
   verb_root(V).
verb_form(Past,Root,past+_,_) :-
   regular_past(Past,Root).
verb_form(am,be,pres+fin,1+sin).
verb_form(are,be,pres+fin,2+sin).
verb_form(is,be,pres+fin,3+sin).
verb_form(are,be,pres+fin,_+plu).
verb_form(was,be,past+fin,1+sin).
verb_form(were,be,past+fin,2+sin).
verb_form(was,be,past+fin,3+sin).
verb_form(were,be,past+fin,_+plu).
verb_form(been,be,past+part,_).
verb_form(being,be,pres+part,_).
verb_form(has,have,pres+fin,3+sin).
verb_form(having,have,pres+part,_).
verb_form(does,do,pres+fin,3+sin).
verb_form(did,do,past+fin,_).
verb_form(doing,do,pres+part,_).
verb_form(done,do,past+part,_).
verb_form(rises,rise,pres+fin,3+sin).
verb_form(rose,rise,past+fin,_).
verb_form(risen,rise,past+part,_).
verb_form(borders,border,pres+fin,3+sin).
verb_form(bordering,border,pres+part,_).
verb_form(contains,contain,pres+fin,3+sin).
verb_form(containing,contain,pres+part,_).
verb_form(drains,drain,pres+fin,3+sin).
verb_form(draining,drain,pres+part,_).
verb_form(exceeds,exceed,pres+fin,3+sin).
verb_form(exceeding,exceed,pres+part,_).
verb_form(flows,flow,pres+fin,3+sin).
verb_form(flowing,flow,pres+part,_).

verb_root(be).
verb_root(have).
verb_root(do).
verb_root(border).
verb_root(contain).
verb_root(drain).
verb_root(exceed).
verb_root(flow).
verb_root(rise).

root_form(1+sin).
root_form(2+_).
root_form(1+plu).
root_form(3+plu).

verb_type(be,aux+be).
verb_type(have,aux+have).
verb_type(do,aux+ditrans).
verb_type(rise,main+intrans).
verb_type(border,main+trans).
verb_type(contain,main+trans).
verb_type(drain,main+intrans).
verb_type(exceed,main+trans).
verb_type(flow,main+intrans).

regular_pres(have).
regular_pres(do).
regular_pres(rise).
regular_pres(border).
regular_pres(contain).
regular_pres(drain).
regular_pres(exceed).
regular_pres(flow).

regular_past(had,have).
regular_past(bordered,border).
regular_past(contained,contain).
regular_past(drained,drain).
regular_past(exceeded,exceed).
regular_past(flowed,flow).

rel_pron(who,subj).
rel_pron(whom,compl).
rel_pron(which,undef).

poss_pron(my,_,1,sin).
poss_pron(your,_,2,_).
poss_pron(his,masc,3,sin).
poss_pron(her,fem,3,sin).
poss_pron(its,neut,3,sin).
poss_pron(our,_,1,plu).
poss_pron(their,_,3,plu).

pers_pron(i,_,1,sin,subj).
pers_pron(you,_,2,_,_).
pers_pron(he,masc,3,sin,subj).
pers_pron(she,fem,3,sin,subj).
pers_pron(it,neut,3,sin,_).
pers_pron(we,_,1,plu,subj).
pers_pron(them,_,3,plu,subj).
pers_pron(me,_,1,sin,compl(_)).
pers_pron(him,masc,3,sin,compl(_)).
pers_pron(her,fem,3,sin,compl(_)).
pers_pron(us,_,1,plu,compl(_)).
pers_pron(them,_,3,plu,compl(_)).

terminator(.,_).
terminator(?,?).
terminator(!,!).

name(Name) :-
   name_template(Name,_), !.

% =================================================================
% Specialised Dictionary

loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).

adj(minimum,restr).
adj(maximum,restr).
adj(average,restr).
adj(total,restr).
adj(african,restr).
adj(american,restr).
adj(asian,restr).
adj(european,restr).
adj(great,quant).
adj(big,quant).
adj(small,quant).
adj(large,quant).
adj(old,quant).
adj(new,quant).
adj(populous,quant).

rel_adj(greater,great).
rel_adj(less,small).
rel_adj(bigger,big).
rel_adj(smaller,small).
rel_adj(larger,large).
rel_adj(older,old).
rel_adj(newer,new).

sup_adj(biggest,big).
sup_adj(smallest,small).
sup_adj(largest,large).
sup_adj(oldest,old).
sup_adj(newest,new).

noun_sin(average).
noun_sin(total).
noun_sin(sum).
noun_sin(degree).
noun_sin(sqmile).
noun_sin(ksqmile).
noun_sin(thousand).
noun_sin(million).
noun_sin(time).
noun_sin(place).
noun_sin(area).
noun_sin(capital).
noun_sin(city).
noun_sin(continent).
noun_sin(country).
noun_sin(latitude).
noun_sin(longitude).
noun_sin(ocean).
noun_sin(person).
noun_sin(population).
noun_sin(region).
noun_sin(river).
noun_sin(sea).
noun_sin(seamass).
noun_sin(number).

noun_plu(averages,average).
noun_plu(totals,total).
noun_plu(sums,sum).
noun_plu(degrees,degree).
noun_plu(sqmiles,sqmile).
noun_plu(ksqmiles,ksqmile).
noun_plu(million,million).
noun_plu(thousand,thousand).
noun_plu(times,time).
noun_plu(places,place).
noun_plu(areas,area).
noun_plu(capitals,capital).
noun_plu(cities,city).
noun_plu(continents,continent).
noun_plu(countries,country).
noun_plu(latitudes,latitude).
noun_plu(longitudes,longitude).
noun_plu(oceans,ocean).
noun_plu(persons,person).  noun_plu(people,person).
noun_plu(populations,population).
noun_plu(regions,region).
noun_plu(rivers,river).
noun_plu(seas,sea).
noun_plu(seamasses,seamass).
noun_plu(numbers,number).

adverb(yesterday).
adverb(tomorrow).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
i_sentence(q(S),question([],P)) :-
   i_s(S,P,[],0).
i_sentence(whq(X,S),question([X],P)) :-
   i_s(S,P,[],0).
i_sentence(imp(s(_,Verb,VArgs,VMods)),imp(V,Args)) :-
   i_verb(Verb,V,_,active,pos,Slots0,[],transparent),
   i_verb_args(VArgs,[],[],Slots0,Slots,Args,Args0,Up,-0),
   conc(Up,VMods,Mods),
   i_verb_mods(Mods,_,[],Slots,Args0,Up,+0).

i_np(there,Y,quant(void,X,`true,`true,[],Y),[],_,_,XA,XA).
i_np(NP,Y,Q,Up,Id0,Index,XA0,XA) :-
   i_np_head(NP,Y,Q,Det,Det0,X,Pred,QMods,Slots0,Id0),
   held_arg(XA0,XA,Slots0,Slots,Id0,Id),
   i_np_rest(NP,Det,Det0,X,Pred,QMods,Slots,Up,Id,Index).

i_np_head(np(_,Kernel,_),Y,
      quant(Det,T,Head,Pred0,QMods,Y),
      Det,Det0,X,Pred,QMods,Slots,Id) :-
   i_np_head0(Kernel,X,T,Det0,Head,Pred0,Pred,Slots),
   Type-_=Y, Type-_=T.

i_np_rest(np(_,_,Mods),Det,Det0,X,Pred,QMods,Slots,Up,Id,Index) :-
   index_args(Det0,Index,Id,Det,IndexA),
   i_np_mods(Mods,X,Slots,Pred,QMods,Up,Id,IndexA).

held_arg(held_arg(Case,-Id,X),[],S0,S,Id,+Id) :-
   in_slot(S0,Case,X,Id,S,_).
held_arg(XA,XA,S,S,Id,Id).

i_np_head0(np_head(Det,Adjs,Noun),X,T,Det,Head0,Pred0,Pred,Slots) :-
   i_adjs(Adjs,X,T,X,Head0,Head,Pred0,Pred),
   i_noun(Noun,X,Head,Slots).
i_np_head0(np_head(int_det(V),Adjs,Noun),
      Type-X,Type-X,Det,`true,Pred,Pred,
      [slot(prep(of),Type,X,_,comparator)]) :-
   comparator(Noun,Type,V,Adjs,Det).
i_np_head0(np_head(quant(Op0,N),Adjs,Noun),
      Type-X,Type-X,void,`P,Pred,Pred,[]) :-
   measure(Noun,Type,Adjs,Units),
   conversion(N,Op0,Type,V,Op),
   measure_op(Op,X,V--Units,P).
i_np_head0(name(Name),
      Type-Name,Type-Name,id,`true,Pred,Pred,[]) :-
   name_template(Name,Type).
i_np_head0(wh(X),X,X,id,`true,Pred,Pred,[]).

i_np_mods(Mods,_,[],`true,[],Mods,_,_).
i_np_mods([Mod|Mods],X,Slots0,Pred0,QMods0,Up,Id,Index) :-
   i_np_mod(Mod,X,Slots0,Slots,
            Pred0,Pred,QMods0,QMods,Up0,-Id,Index),
   conc(Up0,Mods,Mods0),
   i_np_mods(Mods0,X,Slots,Pred,QMods,Up,+Id,Index).
i_np_mods(Mods,_,[Slot|Slots],`true,QMods,Mods,Id,_) :-
   i_voids([Slot|Slots],QMods,Id).

i_voids([],[],_).
i_voids([Slot|Slots],[quant(void,X,`true,`true,[],_)|QMods],Id) :-
   nominal_slot(Slot,X,-Id), !,
   i_voids(Slots,QMods,+Id).
i_voids([_|Slots],QMods,Id) :-
   i_voids(Slots,QMods,Id).

i_rel(rel(X,S),X,P&Pred,Pred,QMods,QMods,Up,Id) :-
   i_s(S,P,Up,Id).
i_rel(reduced_rel(X,S),X,Pred,Pred,[A|QMods],QMods,Up,Id) :-
   i_s(S,A,Up,Id).
i_rel(conj(Conj,Left,Right),X,
      conj(Conj,LPred,LQMods,RPred,RQMods)&Pred,Pred,
      QMods,QMods,Up,Id) :-
   i_rel(Left,X,LPred,`true,LQMods,[],[],-Id),
   i_rel(Right,X,RPred,`true,RQMods,[],Up,+Id).

i_np_mod(pp(Prep,NP),
      X,Slots0,Slots,Pred,Pred,[QMod|QMods],QMods,Up,Id0,Index0) :-
   i_np_head(NP,Y,Q,LDet,LDet0,LX,LPred,LQMods,LSlots0,Id0),
   i_bind(Prep,Slots0,Slots1,X,Y,Id0,Function,P,PSlots,XArg),
   conc(PSlots,Slots1,Slots),
   i_np_modify(Function,P,Q,QMod,Index0,Index),
   held_arg(XArg,[],LSlots0,LSlots,Id0,Id),
   i_np_rest(NP,LDet,LDet0,LX,LPred,LQMods,LSlots,Up,Id,Index).
i_np_mod(Mod,X,Slots,Slots,Pred0,Pred,QMods0,QMods,Up,Id,_) :-
   i_rel(Mod,X,Pred0,Pred,QMods0,QMods,Up,Id).

i_noun(Noun,Type-X,P,Slots) :-
   noun_template(Noun,Type,X,P,Slots).

i_bind(Prep,Slots0,Slots,_,X,Id,arg,P,[],[]) :-
   in_slot(Slots0,Case,X,Id,Slots,P),
   deepen_case(Prep,Case).
i_bind(prep(Prep),Slots,Slots,X,Y,_,adjoin,`P,PSlots,XArg) :-
   i_adjoin(Prep,X,Y,PSlots,XArg,P).

i_np_modify(adjoin,P,N,N&P,_,unit).
i_np_modify(arg,F,N,N,Index0,Index) :-
   index_slot(F,Index0,Index).

in_slot([Slot|Slots],Case,X,Id,Slots,F) :-
   slot_match(Slot,Case,X,Id,F).
in_slot([Slot|Slots0],Case,X,Id,[Slot|Slots],F) :-
   in_slot(Slots0,Case,X,Id,Slots,F).

slot_match(slot(Case,Type,X,Id,F),Case,Type-X,Id,F).

i_adjs([],X,T,T,Head,Head,Pred,Pred).
i_adjs([Adj|Adjs],X,T,T0,Head0,Head,Pred0,Pred) :-
   i_adj(Adj,X,T,T1,Head0,Head1,Pred0,Pred1),
   i_adjs(Adjs,X,T1,T0,Head1,Head,Pred1,Pred).

i_adj(adj(Adj),Type-X,T,T,Head,Head,`P&Pred,Pred) :-
   restriction(Adj,Type,X,P).
i_adj(adj(Adj),TypeX-X,TypeV-V,_,
   aggr(F,V,[X],Head,Pred),Head,`true,Pred) :-
   aggr_adj(Adj,TypeV,TypeX,F).
i_adj(sup(Op0,adj(Adj)),Type-X,Type-V,_,
      aggr(F,V,[Y,X],Head,`P&Pred),Head,`true,Pred) :-
   sign(Adj,Sign),
   inverse(Op0,Sign,Op),
   i_sup_op(Op,F),
   attribute(Adj,Type,X,_,Y,P).
%i_adj(adj(Adj),TypeX-X,T,T,_,
%      Head,Head,quant(void,TypeX-Y,`P,`Q&Pred,[],_),Pred) :-
%   attribute(Adj,TypeX,X,_,Y,P),
%   standard(Adj,TypeX,Y,Q).

i_s(s(Subj,Verb,VArgs,VMods),Pred,Up,Id) :-
   i_verb(Verb,P,Tense,Voice,Neg,Slots0,XA0,Meta),
   i_subj(Voice,Subj,Slots0,Slots1,QSubj,SUp,-(-Id)),
   conc(SUp,VArgs,TArgs),
   i_verb_args(TArgs,XA0,XA,Slots1,Slots,Args0,Args,Up0,+(-Id)),
   conc(Up0,VMods,Mods),
   i_verb_mods(Mods,Tense,XA,Slots,Args,Up,+Id),
   reshape_pred(Meta,QSubj,Neg,P,Args0,Pred).

i_verb(verb(Root,Voice,Tense,Aspect,Neg),
      P,Tense,Voice,Det,Slots,XArg,Meta) :-
   verb_template(Root,P,Slots,XArg,Meta),
   i_neg(Neg,Det).

reshape_pred(transparent,S,N,P,A,pred(S,N,P,A)).
reshape_pred(have,Subj,Neg,Verb0,
      [quant(Det,X,Head0,Pred,QArgs,Y)|MRest],
      pred(Subj,Neg,Verb,[quant(Det,X,Head,Pred,QArgs,Y)|MRest])) :-
   have_pred(Head0,Verb0,Head,Verb).

have_pred(`Head,Verb,`true,(Head,Verb)).
have_pred(Head,Verb,Head,Verb) :-
   meta_head(Head).

meta_head(apply(_,_)).
meta_head(aggr(_,_,_,_,_)).

i_neg(pos,id).
i_neg(neg,not).

i_subj(Voice,Subj,Slots0,Slots,Quant,Up,Id) :-
   subj_case(Voice,Case),
   verb_slot(arg(Case,Subj),[],[],Slots0,Slots,[Quant],[],Up,Id).

i_verb_args(VArgs,XA0,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   fill_verb(VArgs,XA0,XA,Slots0,Slots,Args0,Args,Up,Id).

subj_case(active,subj).
subj_case(passive,s_subj).

fill_verb([],XA,XA,Slots,Slots,Args,Args,[],_).
fill_verb([Node|Nodes0],XA0,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   verb_slot(Node,XA0,XA1,Slots0,Slots1,Args0,Args1,Up0,-Id),
   conc(Up0,Nodes0,Nodes),
   fill_verb(Nodes,XA1,XA,Slots1,Slots,Args1,Args,Up,+Id).

verb_slot(pp(Prep,NP),
      XArg0,XArg,Slots0,Slots,[Q|Args],Args,Up,Id) :-
   i_np(NP,X,Q,Up,Id,unit,XArg0,XArg),
   in_slot(Slots0,Case,X,Id,Slots,_),
   deepen_case(Prep,Case).
verb_slot(void,XA,XA,Slots,Slots,Args,Args,[],_) :-
   in_slot(Slots,pred,_,_,_,_).
verb_slot(pp(prep(Prep),NP),
      TXArg,TXArg,Slots0,Slots,[Q& `P|Args],Args,Up,Id0) :-
   in_slot(Slots0,pred,X,Id0,Slots1,_),
   i_adjoin(Prep,X,Y,PSlots,XArg,P),
   i_np_head(NP,Y,Q,LDet,LDet0,LX,LPred,LQMods,LSlots0,Id0),
   held_arg(XArg,[],LSlots0,LSlots,Id0,Id),
   i_np_rest(NP,LDet,LDet0,LX,LPred,LQMods,LSlots,Up,Id,free),
   conc(PSlots,Slots1,Slots).
verb_slot(arg(SCase,NP),
      XArg0,XArg,Slots0,Slots,[Q|Args],Args,Up,Id) :-
   i_np(NP,X,Q,Up,Id,unit,XArg0,XArg),
   in_slot(Slots0,Case,X,Id,Slots,_),
   deepen_case(SCase,Case).
%verb_slot(adverb(Adv),XA,XA,Slots0,Slots,[`P|Args],Args,[],Id) :-
%   adv_template(Adv,Case,X,P),
%   in_slot(Slots0,Case,X,Id,Slots,_).
verb_slot(arg(pred,AP),XA,XA,Slots0,Slots,Args0,Args,Up,Id) :-
   in_slot(Slots0,pred,X,Id,Slots,_),
   i_pred(AP,X,Args0,Args,Up,Id).

i_pred(conj(Conj,Left,Right),X,
      [conj(Conj,`true,LQMods,`true,RQMods)|QMods],
      QMods,Up,Id) :-
   i_pred(Left,X,LQMods,[],[],-Id),
   i_pred(Right,X,RQMods,[],Up,+Id).
i_pred(AP,T,[`Head&Pred|As],As,[],_) :-
   i_adj(AP,T,_,_,Head,true,Pred,`true).
i_pred(value(adj(Adj),wh(TypeY-Y)),Type-X,[`H|As],As,[],_) :-
   attribute(Adj,Type,X,TypeY,Y,H).
i_pred(comp(Op0,adj(Adj),NP),X,[P1 & P2 & `P3,Q|As],As,Up,Id) :-
   i_np(NP,Y,Q,Up,Id,unit,[],[]),
   sign(Adj,Sign),
   i_measure(X,Adj,Type,U,P1),
   i_measure(Y,Adj,Type,V,P2),
   inverse(Op0,Sign,Op),
   measure_op(Op,U,V,P3).
i_pred(pp(prep(Prep),NP),X,[`H,Q|As],As,Up,Id) :-
   i_np(NP,Y,Q,Up,Id,unit,[],[]),
   adjunction(Prep,X,Y,H).

i_adjoin(with,TS-S,TV-Y,[slot(prep(of),TV,Z,_,free)],
	held_arg(poss,-Id,TS-S),
	Y=Z).
i_adjoin(Prep,X,Y,[],[],P) :-
   adjunction(Prep,X,Y,P).

i_measure(Type-X,Adj,Type,X,`true) :-
   units(Adj,Type).
i_measure(TypeX-X,Adj,TypeY,Y,quant(void,TypeY-Y,`P,`true,[],_)) :-
   attribute(Adj,TypeX,X,TypeY,Y,P).

i_verb_mods(Mods,_,XA,Slots0,Args0,Up,Id) :-
   fill_verb(Mods,XA,[],Slots0,Slots,Args0,Args,Up,-Id),
   i_voids(Slots,Args,+Id).

nominal_slot(slot(Kind,Type,X,Id,_),Type-X,Id) :-
   nominal_kind(Kind).

nominal_kind(prep(_)).
nominal_kind(poss).
nominal_kind(subj).
nominal_kind(dir).
nominal_kind(ind).

i_sup_op(least,min).
i_sup_op(most,max).

conversion(wh(Type-X),same,Type,X,id).
conversion(nb(N),Op,_,N,Op).

measure_op(id,X,X,true).
measure_op(same,X,Y,X=Y).
measure_op(less,X,Y,exceeds(Y,X)).
measure_op(not+less,X,Y,\+exceeds(Y,X)).
measure_op(more,X,Y,exceeds(X,Y)).
measure_op(not+more,X,Y,\+exceeds(X,Y)).

inverse(most,-,least).
inverse(least,-,most).
inverse(same,-,same).
inverse(less,-,more).
inverse(more,-,less).
inverse(X,+,X).

noun_template(Noun,TypeV,V,`P,
      [slot(poss,TypeO,O,Os,index)|Slots]) :-
   property(Noun,TypeV,V,TypeO,O,P,Slots,Os,_).
noun_template(Noun,TypeV,V,aggr(F,V,[],`true,`true),
   [slot(prep(of),TypeS,_,_,free)]) :-
   aggr_noun(Noun,TypeV,TypeS,F).
noun_template(Noun,Type,X,`P,Slots) :-
   thing(Noun,Type,X,P,Slots,_).
noun_template(Noun,TypeV,V,apply(F,P),
      [slot(prep(of),TypeX,X,_,apply)]) :-
   meta_noun(Noun,TypeV,V,TypeX,X,P,F).

verb_template(have,Y=Z,
		[slot(subj,TypeS,S,-Id,free),
		 slot(dir,TypeV,Y,_,free),
		 slot(prep(of),TypeV,Z,_,free)],
		held_arg(poss,-(-(+Id)),TypeS-S), have).
verb_template(have,Y=Z,
	[slot(subj,TypeS,S,-(-(Id)),free),
	 slot(dir,TypeV,Y,_,free),
	 slot(prep(as),TypeV,Z,_,free)],
	held_arg(poss,-(-(-(+Id))),TypeS-S), have).
verb_template(Verb,Pred,
      [slot(subj,TypeS,S,_,free)|Slots],[],transparent) :-
   verb_type(Verb,_+Kind),
   verb_kind(Kind,Verb,TypeS,S,Pred,Slots).

verb_kind(be,_,TypeS,S,S=A,[slot(dir,TypeS,A,_,free)]).
verb_kind(be,_,TypeS,S,true,[slot(pred,TypeS,S,_,free)]).
verb_kind(intrans,Verb,TypeS,S,Pred,Slots) :-
   intrans(Verb,TypeS,S,Pred,Slots,_).
verb_kind(trans,Verb,TypeS,S,Pred,
      [slot(dir,TypeD,D,SlotD,free)|Slots]) :-
   trans(Verb,TypeS,S,TypeD,D,Pred,Slots,SlotD,_).
%verb_kind(ditrans,Verb,TypeS,S,Pred,
%      [slot(dir,TypeD,D,SlotD,free),
%       slot(ind,TypeI,I,SlotI,free)|Slots]) :-
%   ditrans(Verb,TypeS,S,TypeD,D,TypeI,I,Pred,Slots,SlotD,SlotI,_).

deepen_case(prep(at),time).
deepen_case(s_subj,dir).
deepen_case(s_subj,ind).
deepen_case(prep(by),subj).
deepen_case(prep(to),ind).
deepen_case(prep(of),poss).
deepen_case(X,X).

% ================================================================
% Determiner Indexing Table

index_slot(index,I,I).
index_slot(free,_,unit).
index_slot(apply,_,apply).
index_slot(comparator,_,comparator).

index_args(det(the(plu)),unit,I,set(I),index(I)) :- !.
index_args(int_det(X),index(I),_,int_det(I,X),unit) :- !.
index_args(generic,apply,_,lambda,unit) :-!.
index_args(D,comparator,_,id,unit) :-
 ( indexable(D); D=generic), !.
index_args(D,unit,_,D,unit) :- !.
index_args(det(D),I,_,I,I) :-
   indexable(D),
   index(I), !.
index_args(D,I,_,D,I).

indexable(the(plu)).
indexable(all).

index(index(I)).

% ================================================================
% Utilities

conc([],L,L).
conc([X|L1],L2,[X|L3]) :-
   conc(L1,L2,L3).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
clausify(question(V0,P),(answer(V):-B)) :-
   quantify(P,Quants,[],R0),
   split_quants(question(V0),Quants,HQuants,[],BQuants,[]),
   chain_apply(BQuants,R0,R1),
   head_vars(HQuants,B,R1,V,V0).

quantify(quant(Det,X,Head,Pred,Args,Y),Above,Right,true) :-
   close_tree(Pred,P2),
   quantify_args(Args,AQuants,P1),
   split_quants(Det,AQuants,Above,[Q|Right],Below,[]),
   pre_apply(Head,Det,X,P1,P2,Y,Below,Q).
quantify(conj(Conj,LPred,LArgs,RPred,RArgs),Up,Up,P) :-
   close_tree(LPred,LP0),
   quantify_args(LArgs,LQs,LP1),
   chain_apply(LQs,(LP0,LP1),LP),
   close_tree(RPred,RP0),
   quantify_args(RArgs,RQs,RP1),
   chain_apply(RQs,(RP0,RP1),RP),
   conj_apply(Conj,LP,RP,P).
quantify(pred(Subj,Op,Head,Args),Above,Right,P) :-
   quantify(Subj,SQuants,[],P0),
   quantify_args(Args,AQuants,P1),
   split_quants(Op,AQuants,Up,Right,Here,[]),
   conc(SQuants,Up,Above),
   chain_apply(Here,(P0,Head,P1),P2),
   op_apply(Op,P2,P).
quantify(`P,Q,Q,P).
quantify(P&Q,Above,Right,(S,T)) :-
   quantify(Q,Right0,Right,T),
   quantify(P,Above,Right0,S).
   
head_vars([],P,P,L,L0) :-
   strip_types(L0,L).
head_vars([Quant|Quants],(P,R0),R,[X|V],V0) :-
   extract_var(Quant,P,X),
   head_vars(Quants,R0,R,V,V0).

strip_types([],[]).
strip_types([_-X|L0],[X|L]) :-
   strip_types(L0,L).

extract_var(quant(_,_-X,P,_-X),P,X).

chain_apply(Q0,P0,P) :-
   sort_quants(Q0,Q,[]),
   chain_apply0(Q,P0,P).    

chain_apply0([],P,P).
chain_apply0([Q|Quants],P0,P) :-
   chain_apply0(Quants,P0,P1),
   det_apply(Q,P1,P).

quantify_args([],[],true).
quantify_args([Arg|Args],Quants,(P,Q)) :-
   quantify_args(Args,Quants0,Q),
   quantify(Arg,Quants,Quants0,P).

pre_apply(`Head,set(I),X,P1,P2,Y,Quants,Quant) :-
   indices(Quants,I,Indices,RestQ),
   chain_apply(RestQ,(Head,P1),P),
   setify(Indices,X,(P,P2),Y,Quant).
pre_apply(`Head,Det,X,P1,P2,Y,Quants,quant(Det,X,(P,P2),Y)) :-
 ( unit_det(Det);
   index_det(Det,_)),
   chain_apply(Quants,(Head,P1),P).
pre_apply(apply(F,P0),Det,X,P1,P2,Y,
      Quants0,quant(Det,X,(P3,P2),Y)) :-
   but_last(Quants0,quant(lambda,Z,P0,Z),Quants),
   chain_apply(Quants,(F,P1),P3).
pre_apply(aggr(F,Value,L,Head,Pred),Det,X,P1,P2,Y,Quants,
      quant(Det,X,
            (S^(setof(Range:Domain,P,S),
                aggregate(F,S,Value)),P2),Y)) :-
   close_tree(Pred,R),
   complete_aggr(L,Head,(R,P1),Quants,P,Range,Domain).

but_last([X|L0],Y,L) :-
   but_last0(L0,X,Y,L).

but_last0([],X,X,[]).
but_last0([X|L0],Y,Z,[Y|L]) :-
   but_last0(L0,X,Z,L).

close_tree(T,P) :-
   quantify(T,Q,[],P0),
   chain_apply(Q,P0,P).

meta_apply(`G,R,Q,G,R,Q).
meta_apply(apply(F,(R,P)),R,Q0,F,true,Q) :-
   but_last(Q0,quant(lambda,Z,P,Z),Q).

indices([],_,[],[]).
indices([Q|Quants],I,[Q|Indices],Rest) :-
   open_quant(Q,Det,_,_,_),
   index_det(Det,I),
   indices(Quants,I,Indices,Rest).
indices([Q|Quants],I,Indices,[Q|Rest]) :-
   open_quant(Q,Det,_,_,_),
   unit_det(Det),
   indices(Quants,I,Indices,Rest).

setify([],Type-X,P,Y,quant(set,Type-([]:X),true:P,Y)).
setify([Index|Indices],X,P,Y,Quant) :-
   pipe(Index,Indices,X,P,Y,Quant).

pipe(quant(int_det(_,Z),Z,P1,Z),
      Indices,X,P0,Y,quant(det(a),X,P,Y)) :-
   chain_apply(Indices,(P0,P1),P).
pipe(quant(index(_),_-Z,P0,_-Z),Indices,Type-X,P,Y,
      quant(set,Type-([Z|IndexV]:X),(P0,P1):P,Y)) :-
   index_vars(Indices,IndexV,P1).

index_vars([],[],true).
index_vars([quant(index(_),_-X,P0,_-X)|Indices],
      [X|IndexV],(P0,P)) :-
   index_vars(Indices,IndexV,P).

complete_aggr([Att,Obj],`G,R,Quants,(P,R),Att,Obj) :-
   chain_apply(Quants,G,P).
complete_aggr([Att],Head,R0,Quants0,(P1,P2,R),Att,Obj) :-
   meta_apply(Head,R0,Quants0,G,R,Quants),
   set_vars(Quants,Obj,Rest,P2),
   chain_apply(Rest,G,P1).
complete_aggr([],`G,R,[quant(set,_-(Obj:Att),S:T,_)],
      (G,R,S,T),Att,Obj).

set_vars([quant(set,_-(I:X),P:Q,_-X)],[X|I],[],(P,Q)).
set_vars([],[],[],true).
set_vars([Q|Qs],[I|Is],R,(P,Ps)) :-
   open_quant(Q,Det,X,P,Y),
   set_var(Det,X,Y,I), !,
   set_vars(Qs,Is,R,Ps).
set_vars([Q|Qs],I,[Q|R],P) :-
   set_vars(Qs,I,R,P).

set_var(Det,_-X,_-X,X) :-
   setifiable(Det).

sort_quants([],L,L).
sort_quants([Q|Qs],S,S0) :-
   open_quant(Q,Det,_,_,_),
   split_quants(Det,Qs,A,[],B,[]),
   sort_quants(A,S,[Q|S1]),
   sort_quants(B,S1,S0).

split_quants(_,[],A,A,B,B).
split_quants(Det0,[Quant|Quants],Above,Above0,Below,Below0) :-
   compare_dets(Det0,Quant,Above,Above1,Below,Below1),
   split_quants(Det0,Quants,Above1,Above0,Below1,Below0).

compare_dets(Det0,Q,[quant(Det,X,P,Y)|Above],Above,Below,Below) :-
   open_quant(Q,Det1,X,P,Y),
   governs(Det1,Det0), !,
   bubble(Det0,Det1,Det).
compare_dets(Det0,Q0,Above,Above,[Q|Below],Below) :-
   lower(Det0,Q0,Q).

open_quant(quant(Det,X,P,Y),Det,X,P,Y).

% =================================================================
% Determiner Properties

index_det(index(I),I).
index_det(int_det(I,_),I).

unit_det(set).
unit_det(lambda).
unit_det(quant(_,_)).
unit_det(det(_)).
unit_det(question(_)).
unit_det(id).
unit_det(void).
unit_det(not).
unit_det(generic).
unit_det(int_det(_)).
unit_det(proportion(_)).

det_apply(quant(Det,Type-X,P,_-Y),Q0,Q) :-
   apply(Det,Type,X,P,Y,Q0,Q).

apply(generic,_,X,P,X,Q,X^(P,Q)).
apply(proportion(Type-V),_,X,P,Y,Q,
      S^(setof(X,P,S),
         N^(numberof(Y,(one_of(S,Y),Q),N),
            M^(card(S,M),ratio(N,M,V))))).
apply(id,_,X,P,X,Q,(P,Q)).
apply(void,_,X,P,X,Q,X^(P,Q)).
apply(set,_,Index:X,P0,S,Q,S^(P,Q)) :-
   apply_set(Index,X,P0,S,P).
apply(int_det(Type-X),Type,X,P,X,Q,(P,Q)).
apply(index(_),_,X,P,X,Q,X^(P,Q)).
apply(quant(Op,N),Type,X,P,X,Q,R) :-
   value(N,Type,Y),
   quant_op(Op,Z,Y,numberof(X,(P,Q),Z),R).
apply(det(Det),_,X,P,Y,Q,R) :-
   apply0(Det,X,P,Y,Q,R).

apply0(Some,X,P,X,Q,X^(P,Q)) :-
   some(Some).
apply0(All,X,P,X,Q,\+X^(P,\+Q)) :-
   all(All).
apply0(no,X,P,X,Q,\+X^(P,Q)).
apply0(notall,X,P,X,Q,X^(P,\+Q)).

quant_op(same,X,X,P,P).
quant_op(Op,X,Y,P,X^(P,F)) :-
   quant_op(Op,X,Y,F).

quant_op(not+more,X,Y,X=<Y).
quant_op(not+less,X,Y,X>=Y).
quant_op(less,X,Y,X<Y).
quant_op(more,X,Y,X>Y).

value(wh(Type-X),Type,X).
value(nb(X),_,X).

all(all).
all(every).
all(each).
all(any).

some(a).
some(the(sin)).
some(some).

apply_set([],X,true:P,S,setof(X,P,S)).
apply_set([I|Is],X,Range:P,S,
      setof([I|Is]:V,(Range,setof(X,P,V)),S)).


governs(Det,set(J)) :-
   index_det(Det,I),
   I \== J.
governs(Det0,Det) :-
   index_det(Det0,_),
 ( index_det(Det,_);
   Det=det(_);
   Det=quant(_,_)).
governs(_,void).
governs(_,lambda).
governs(_,id).
governs(det(each),question([_|_])).
governs(det(each),det(each)).
governs(det(any),not).
governs(quant(same,wh(_)),Det) :-
   weak(Det).

governs(det(Strong),Det) :-
   strong0(Strong),
   weak(Det).

% strong(det(Det)) :-
%    strong0(Det).

strong0(each).
strong0(any).

weak(det(Det)) :-
   weak0(Det).
weak(quant(_,_)).
weak(index(_)).
weak(int_det(_,_)).
weak(set(_)).
weak(int_det(_)).
weak(generic).
weak(proportion(_)).

weak0(no).
weak0(a).
weak0(all).
weak0(some).
weak0(every).
weak0(the(sin)).
weak0(notall).

lower(question(_),Q,quant(det(a),X,P,Y)) :-
   open_quant(Q,det(any),X,P,Y), !.
lower(_,Q,Q).

setifiable(generic).
setifiable(det(a)).
setifiable(det(all)).

% =================================================================
% Operators (currently, identity, negation and 'and')

op_apply(id,P,P).
op_apply(not,P,\+P).

bubble(not,det(any),det(every)) :- !.
bubble(_,D,D).


conj_apply(and,P,Q,(P,Q)).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/* Nouns */

property(area,measure&area,X,feature&place&_,Y,area(Y,X),[],_,_).
property(capital,feature&city,X,feature&place&country,Y,
         capital(Y,X),[],_,_).
property(latitude,
         measure&position,X,feature&_,Y,latitude(Y,X),[],_,_).
property(longitude,measure&position,X,feature&_,Y,
         longitude(Y,X),[],_,_).
property(population,
         measure&heads,X,feature&_,Y,population(Y,X),[],_,_).

thing(place,feature&place&_,X,place(X),[],_).
thing(area,measure&area,X,area(X),[],_).
thing(capital,feature&city,X,capital(X),[],_).
thing(city,feature&city,X,city(X),[],_).
thing(continent,feature&place&continent,X,continent(X),[],_).
thing(country,feature&place&country,X,country(X),[],_).
thing(latitude,measure&position,X,latitude(X),[],_).
thing(longitude,measure&position,X,longitude(X),[],_).
thing(ocean,feature&place&seamass,X,ocean(X),[],_).
thing(person,_,X,person(X),[],_).
thing(population,measure&heads,X,population(X),[],_).
thing(region,feature&place&_,X,region(X),[],_).
thing(river,feature&river,X,river(X),[],_).
thing(sea,feature&place&seamass,X,sea(X),[],_).
thing(seamass,feature&place&seamass,X,seamass(X),[],_).

aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun(number,_,V,feature&_,X,P,numberof(X,P,V)).

/* Proper nouns */

name_template(X,feature&circle) :- circle_of_latitude(X).
name_template(X,feature&city) :- city(X).
name_template(X,feature&place&continent) :- continent(X).
name_template(X,feature&place&country) :- country(X).
name_template(X,feature&place&_) :- region(X).
name_template(X,feature&river) :- river(X).
name_template(X,feature&place&seamass) :- seamass(X).

/* Verbs */

trans(border,
      feature&place&_,X,feature&place&_,Y,borders(X,Y),[],_,_).
trans(contain,feature&place&_,X,feature&_,Y,in(Y,X),[],_,_).
trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).

intrans(drain,feature&river,X,drains(X,Y),
   [slot(prep(into),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y),
   [slot(prep(through),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y,Z),
   [slot(prep(into),feature&place&_,Z,_,free),
    slot(prep(from),feature&place&_,Y,_,free)],_).
intrans(rise,feature&river,X,rises(X,Y),
   [slot(prep(in),feature&place&_,Y,_,free)],_).

/* Adjectives */

restriction(african,feature&_,X,african(X)).
restriction(american,feature&_,X,american(X)).
restriction(asian,feature&_,X,asian(X)).
restriction(european,feature&_,X,european(X)).

attribute(large,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(small,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(great,measure&Type,X,measure&Type,Y,X=Y).
attribute(populous,feature&_,X,measure&heads,Y,population(Y,X)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Prepositions */

adjunction(in,feature&_-X,feature&place&_-Y,in(X,Y)).
adjunction(eastof,feature&_-X,feature&_-Y,eastof(X,Y)).
adjunction(westof,feature&_-X,feature&_-Y,westof(X,Y)).
adjunction(northof,feature&_-X,feature&_-Y,northof(X,Y)).
adjunction(southof,feature&_-X,feature&_-Y,southof(X,Y)).

/* Measure */

measure(ksqmile,measure&area,[],ksqmiles).
measure(sqmile,measure&area,[],sqmiles).
measure(degree,measure&position,[],degrees).
measure(thousand,measure&heads,[],thousand).
measure(million,measure&heads,[],million).

units(large,measure&_).
units(small,measure&_).

sign(large,+).
sign(small,-).
sign(great,+).

/* Proportions and the like */

comparator(proportion,_,V,[],proportion(V)).
comparator(percentage,_,V,[],proportion(V)).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% QPLAN - supplies the control information (ie. sequencing and cuts) needed
%         for efficient execution of a query.

/**:-public qplan/2.

:-mode
   qplan(+,-),
   qplan(+,+,-,-),
   mark(+,-,+,-),
   subquery(+,-,?,?,?,?),
   negate(+,+,-),
   negationcost(+,-),
   setofcost(+,+,-),
   variables(+,+,-),
   variables(+,+,+,-),
   quantificate(+,+,?,-),
   log2(+,-),
   schedule(+,+,-),
   schedule1(+,+,-),
   maybe_cut(+,+,?,-),
   plan(+,+,+,+,-),
   is_conjunction(+),
   marked(+,?,?,?),
   freevars(+,?),
   best_goal(+,+,+,?,?,-),
   instantiate(+,+,-),
   instantiate0(+,+,-),
   recombine(+,+,-),
   incorporate(+,+,+,+,+,-),
   incorporate0(+,+,+,+,-),
   minimum(+,+,-),
   add_keys(+,-),
   strip_keys(+,-),
   strip_key(+,?),
   variablise(+,+,-),
   variablise(+,+,+,+),
   cost(+,+,-),
   cost(+,+,+,+,-),
   instantiated(+,+,-).**/

qplan((P:-Q),(P1:-Q1)) :- qplan(P,Q,P1,Q1), !.
qplan(P,P).

qplan(X0,P0,X,P) :-
   numbervars(X0,0,I), variables(X0,0,Vg),
   numbervars(P0,I,N),
   mark(P0,L,0,Vl),
   schedule(L,Vg,P1),
   quantificate(Vl,0,P1,P2),
   functor(VA,$,N),
   variablise(X0,VA,X),
   variablise(P2,VA,P).

mark(X^P,L,Q0,Q) :- !, variables(X,Q0,Q1), mark(P,L,Q1,Q).
mark((P1,P2),L,Q0,Q) :- !,
   mark(P1,L1,Q0,Q1),
   mark(P2,L2,Q1,Q),
   recombine(L1,L2,L).
mark(\+P,L,Q,Q) :- !, mark(P,L0,0,Vl), negate(L0,Vl,L).
mark(SQ,[m(V,C,SQ1)],Q0,Q0) :- subquery(SQ,SQ1,X,P,N,Q), !,
   mark(P,L,0,Vl),
   L=[Q],   % Too bad about the general case!
   marked(Q,Vq,C0,_),
   variables(X,Vl,Vlx),
   setminus(Vq,Vlx,V0),
   setofcost(V0,C0,C),
   variables(N,V0,V).
mark(P,[m(V,C,P)],Q,Q) :-
   variables(P,0,V),
   cost(P,V,C).

subquery(setof(X,P,S),setof(X,Q,S),X,P,S,Q).
subquery(numberof(X,P,N),numberof(X,Q,N),X,P,N,Q).

negate([],_,[]).
negate([P|L],Vl,[m(Vg,C,\+P)|L1]) :-
   freevars(P,V),
   setminus(V,Vl,Vg),
   negationcost(Vg,C),
   negate(L,Vl,L1).

negationcost(0,0) :- !.
negationcost(V,1000).

setofcost(0,_,0) :- !.
setofcost(_,C,C).

variables('$VAR'(N),V0,V) :- !, setplusitem(V0,N,V).
variables(T,V,V) :- atomic(T), !.
variables(T,V0,V) :- functor(T,_,N), variables(N,T,V0,V).

variables(0,_,V,V) :- !.
variables(N,T,V0,V) :- N1 is N-1,
   arg(N,T,X),
   variables(X,V0,V1),
   variables(N1,T,V1,V).

quantificate(W-V,N,P0,P) :- !, N1 is N+18,
   quantificate(V,N,P1,P),
   quantificate(W,N1,P0,P1).
quantificate(0,_,P,P) :- !.
quantificate(V,N,P0,'$VAR'(Nr)^P) :-
   Vr is V /\ -(V),     % rightmost bit
   log2(Vr,I),
   Nr is N+I,
   N1 is Nr+1,
   V1 is V >> (I+1),
   quantificate(V1,N1,P0,P).

log2(1,0) :- !.
log2(2,1) :- !.
log2(4,2) :- !.
log2(8,3) :- !.
log2(N,I) :- N1 is N>>4, N1=\=0, log2(N1,I1), I is I1+4.

schedule([P],Vg,Q) :- !, schedule1(P,Vg,Q).
schedule([P1|P2],Vg,(Q1,Q2)) :- !, schedule1(P1,Vg,Q1), schedule(P2,Vg,Q2).

schedule1(m(V,C,P),Vg,Q) :-
   maybe_cut(V,Vg,Q0,Q),
   plan(P,V,C,Vg,Q0).

maybe_cut(V,Vg,P,{P}) :- disjoint(V,Vg), !.
maybe_cut(V,Vg,P,P).

plan(\+P,Vg,_,_,\+Q) :- !, Vg = 0,
   marked(P,V,C,P1),
   plan(P1,V,C,Vg,Q1),
   quantificate(V,0,Q1,Q).
plan(SQ,Vg,_,_,SQ1) :- subquery(SQ,SQ1,X,P,_,Q), !,
   marked(P,V,C,P1),
   variables(X,Vg,Vgx),
   setminus(V,Vgx,Vl),
   quantificate(Vl,0,Q1,Q),
   plan(P1,V,C,Vgx,Q1).
plan(P,V,C,Vg,(Q,R)) :- is_conjunction(P), !,
   best_goal(P,V,C,P0,V0,PP),
   plan(P0,V0,C,Vg,Q),
   instantiate(PP,V0,L),
   add_keys(L,L1),
   keysort(L1,L2),
   strip_keys(L2,L3),
   schedule(L3,Vg,R).
plan(P,_,_,_,P).

is_conjunction((_,_)).

marked(m(V,C,P),V,C,P).

freevars(m(V,_,_),V).

best_goal((P1,P2),V,C,P0,V0,m(V,C,Q)) :- !,
   ( marked(P1,Va,C,Pa), Q=(Pb,P2) ; marked(P2,Va,C,Pa), Q=(P1,Pb) ), !,
   best_goal(Pa,Va,C,P0,V0,Pb).
best_goal(P,V,C,P,V,true).

instantiate(true,_,[]) :- !.
instantiate(P,Vi,[P]) :- freevars(P,V), disjoint(V,Vi), !.
instantiate(m(V,_,P),Vi,L) :- instantiate0(P,V,Vi,L).

instantiate0((P1,P2),_,Vi,L) :-
   instantiate(P1,Vi,L1),
   instantiate(P2,Vi,L2),
   recombine(L1,L2,L).
instantiate0(\+P,V,Vi,L) :- !,
   instantiate(P,Vi,L0),
   freevars(P,Vf), setminus(Vf,V,Vl),
   negate(L0,Vl,L).
instantiate0(SQ,Vg,Vi,[m(V,C,SQ1)]) :- subquery(SQ,SQ1,X,P,_,Q), !,
   instantiate(P,Vi,L),
   L=[Q],   % Too bad about the general case!
   marked(Q,Vq,C0,_),
   setminus(Vg,Vi,V),
   variables(X,0,Vx),
   setminus(V,Vx,V0),
   setofcost(V0,C0,C).
instantiate0(P,V,Vi,[m(V1,C,P)]) :-
   setminus(V,Vi,V1),
   cost(P,V1,C).

recombine(L,[],L) :- !.
recombine([],L,L).
recombine([P1|L1],[P2|L2],L) :-
   marked(P1,V1,C1,_), nonempty(V1),
   incorporate(P1,V1,C1,P2,L2,L3), !,
   recombine(L1,L3,L).
recombine([P|L1],L2,[P|L]) :- recombine(L1,L2,L).

incorporate(P0,V0,C0,P1,L1,L) :-
   marked(P1,V1,C1,_),
   intersect(V0,V1), !,
   setplus(V0,V1,V),
   minimum(C0,C1,C),
   incorporate0(m(V,C,(P0,P1)),V,C,L1,L).
incorporate(P0,V0,C0,P1,[P2,L1],[P1|L]) :- incorporate(P0,V0,C0,G2,L1,L).

incorporate0(P0,V0,C0,[P1|L1],L) :- incorporate(P0,V0,C0,P1,L1,L), !.
incorporate0(P,_,_,L,[P|L]).

minimum(N1,N2,N1) :- N1 =< N2, !.
minimum(N1,N2,N2).

add_keys([],[]).
add_keys([P|L],[C-P|L1]) :- marked(P,_,C,_), add_keys(L,L1).

strip_keys([],[]).
strip_keys([X|L],[P|L1]) :- strip_key(X,P), strip_keys(L,L1).

strip_key(C-P,P).

variablise('$VAR'(N),VV,V) :- !, N1 is N+1, arg(N1,VV,V).
variablise(T,_,T) :- atomic(T), !.
variablise(T,VV,T1) :-
   functor(T,F,N),
   functor(T1,F,N),
   variablise(N,T,VV,T1).

variablise(0,_,_,_) :- !.
variablise(N,T,VV,T1) :- N1 is N-1,
   arg(N,T,X),
   arg(N,T1,X1),
   variablise(X,VV,X1),
   variablise(N1,T,VV,T1).

cost(+P,0,N) :- !, cost(P,0,N).
cost(+P,V,1000) :- !.
cost(P,V,N) :- functor(P,F,I), cost(I,F,P,V,N).

cost(1,F,P,V,N) :-
   arg(1,P,X1), instantiated(X1,V,I1),
   nd(F,N0,N1),
   N is N0-I1*N1.
cost(2,F,P,V,N) :-
   arg(1,P,X1), instantiated(X1,V,I1),
   arg(2,P,X2), instantiated(X2,V,I2),
   nd(F,N0,N1,N2),
   N is N0-I1*N1-I2*N2.
cost(3,F,P,V,N) :-
   arg(1,P,X1), instantiated(X1,V,I1),
   arg(2,P,X2), instantiated(X2,V,I2),
   arg(3,P,X3), instantiated(X3,V,I3),
   nd(F,N0,N1,N2,N3),
   N is N0-I1*N1-I2*N2-I3*N3.

instantiated([X|_],V,N) :- !, instantiated(X,V,N).
instantiated('$VAR'(N),V,0) :- setcontains(V,N), !.
instantiated(_,_,1).

/*-------------------------Put in reserve--------------------

sort_parts([],[]) :- !.
sort_parts([X],[X]) :- !.
sort_parts(L,R) :-
   divide(L,L1,L2),
   sort_parts(L1,R1),
   sort_parts(L2,R2),
   merge(R1,R2,R).

divide([X1|L0],[X1|L1],[X2|L2]) :- list(L0,X2,L), !, divide(L,L1,L2).
divide(L,L,[]).

list([X|L],X,L).

merge([],R,R) :- !.
merge([X|R1],R2,[X|R]) :- precedes(X,R2), !, merge(R1,R2,R).
merge(R1,[X|R2],[X|R]) :- !, merge(R1,R2,R).
merge(R,[],R).

precedes(G1,[G2|_]) :- goal_info(G1,_,N1), goal_info(G2,_,N2), N1 =< N2.

-------------------------------------------------------------*/

/**:-mode
   nonempty(+),
   setplus(+,+,-),
   setminus(+,+,-),
   mkset(+,+,-),
   setplusitem(+,+,-),
   setcontains(+,+),
   intersect(+,+),
   disjoint(+,+).**/

nonempty(0) :- !, fail.
nonempty(_).

setplus(W1-V1,W2-V2,W-V) :- !, V is V1 \/ V2, setplus(W1,W2,W).
setplus(W-V1,V2,W-V) :- !, V is V1 \/ V2.
setplus(V1,W-V2,W-V) :- !, V is V1 \/ V2.
setplus(V1,V2,V) :- V is V1 \/ V2.

setminus(W1-V1,W2-V2,S) :- !, V is V1 /\ \(V2),
   setminus(W1,W2,W), mkset(W,V,S).
setminus(W-V1,V2,W-V) :- !, V is V1 /\ \(V2).
setminus(V1,W-V2,V) :- !, V is V1 /\ \(V2).
setminus(V1,V2,V) :- V is V1 /\ \(V2).

mkset(0,V,V) :- !.
mkset(W,V,W-V).

setplusitem(W-V,N,W-V1) :- N < 18, !, V1 is V \/ 1<<N.
setplusitem(W-V,N,W1-V) :- !, N1 is N-18, setplusitem(W,N1,W1).
setplusitem(V,N,V1) :- N < 18, !, V1 is V \/ 1<<N.
setplusitem(V,N,W-V) :- N1 is N-18, setplusitem(0,N1,W).

setcontains(W-V,N) :- N < 18, !, V /\ 1<<N =\= 0.
setcontains(W-V,N) :- !, N1 is N-18, setcontains(W,N1).
setcontains(V,N) :- N < 18, V /\ 1<<N =\= 0.

intersect(W1-V1,W2-V2) :- !, ( V1 /\ V2 =\= 0 ; intersect(W1,W2) ), !.
intersect(W-V1,V2) :- !, V1 /\ V2 =\= 0.
intersect(V1,W-V2) :- !, V1 /\ V2 =\= 0.
intersect(V1,V2) :- V1 /\ V2 =\= 0.

disjoint(W1-V1,W2-V2) :- !, V1 /\ V2 =:= 0, disjoint(W1,W2).
disjoint(W-V1,V2) :- !, V1 /\ V2 =:= 0.
disjoint(V1,W-V2) :- !, V1 /\ V2 =:= 0.
disjoint(V1,V2) :- V1 /\ V2 =:= 0.
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/* Simplifying and executing the logical form of a NL query. */

%:- entry((satisfy:-true)).

/**:-public write_tree/1, answer/1, satisfy/1.

:-mode write_tree(+).
:-mode wt(+,+).
:-mode header(+).
:-mode decomp(+,-,-).
:-mode complex(+).
:-mode othervars(+,-,-).**/

/**
write_tree(T):-
   numbervars(T,0,_),
   wt(T,0),
   fail.
write_tree(_).

wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt((P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put(0'&), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put(0'{), tab(1), wt(P,L1), tab(1), put(0'}).
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

header([]).
header([X|H]) :- write(X), tab(1), header(H).

decomp(setof(X,P,S),[S,=,setof,X],P).  
decomp(\+(P),[\+],P) :- complex(P).
decomp(numberof(X,P,N),[N,=,numberof,X],P).
decomp(X^P,[exists,X|XX],P1) :- othervars(P,XX,P1).

othervars(X^P,[X|XX],P1) :- !, othervars(P,XX,P1).
othervars(P,[],P).

complex((_,_)).
complex({_}).
complex(setof(_,_,_)).
complex(numberof(_,_,_)).
complex(_^_).
complex(\+P) :- complex(P).
**/

% Query execution.

/**:-mode respond(?).
:-mode holds(+,?).
:-mode answer(+).
:-mode yesno(+).         :-mode replies(+).
:-mode reply(+).
:-mode seto(?,+,-).
:-mode satisfy(+).
:-mode pickargs(+,+,+).
:-mode pick(+,?).**/

respond([]) :- write('Nothing satisfies your question.'), nl.
respond([A|L]) :- reply(A), replies(L).

answer((answer([]):-E)) :- !, holds(E,B), yesno(B).
answer((answer([X]):-E)) :- !, seto(X,E,S), respond(S).
answer((answer(X):-E)) :- seto(X,E,S), respond(S).

seto(X,E,S) :- setof(X,satisfy(E),S), !.
seto(X,E,[]).

holds(E,true) :- satisfy(E), !.
holds(E,false).

yesno(true) :- write('Yes.').
yesno(false) :- write('No.').

replies([]) :- write('.').
replies([A]) :- write(' and '), reply(A), write('.').
replies([A|X]) :- write(', '), reply(A), replies(X).

reply(N--U) :- !, write(N), write(' '), write(U).
reply(X) :- write(X).

satisfy((P,Q)) :- !, satisfy(P), satisfy(Q).
satisfy({P}) :- !, satisfy(P), !.
satisfy(X^P) :- !, satisfy(P).
satisfy(\+P) :- satisfy(P), !, fail.
satisfy(\+P) :- !.
satisfy(numberof(X,P,N)) :- !, setof(X,satisfy(P),S), length(S,N).
satisfy(setof(X,P,S)) :- !, setof(X,satisfy(P),S).
satisfy(+P) :- exceptionto(P), !, fail.
satisfy(+P) :- !.
satisfy(X<Y) :- !, X<Y.
satisfy(X=<Y) :- !, X=<Y.
satisfy(X>=Y) :- !, X>=Y.
satisfy(X>Y) :- !, X>Y.
satisfy(P) :- database(P).

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception(P1).

exception(P) :- database(P), !, fail.
exception(P).

pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

pick([X|S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% NDTABL - Meta-information about database relations.

/**:-mode
   nd(+,-,-),
   nd(+,-,-,-),
   nd(+,-,-,-,-).**/

nd(african,19,26).
nd(american,19,26).
nd(area,51,51).
nd(asian,21,26).
nd(capital,22,22).
nd(city,18,18).
nd(continent,8,8).
nd(country,22,22).
nd(european,19,26).
nd(latitude,23,23).
nd(longitude,26,26).
nd(ocean,7,7).
nd(place,23,23).
nd(population,51,51).
nd(region,12,12).
nd(river,16,16).
nd(sea,8,8).
nd(seamass,10,10).

nd(area,22,22,51).
nd(borders,29,22,22).
nd(capital,22,22,23).
nd(card,99,100,3).
nd(drains,16,16,10).
nd(eastof,40,22,22).
nd(exceeds,99,51,51).
nd(flows,19,16,22).
nd(in,29,26,15).
nd(latitude,22,22,23).
nd(longitude,22,22,26).
nd(northof,40,22,22).
nd(one_of,99,200,-99).
nd(population,23,23,51).
nd(rises,16,16,22).
nd(southof,40,22,22).
nd(westof,40,22,22).
nd(=<,99,51,51).
nd(<,99,51,51).
nd(>,99,51,51).
nd(>=,99,51,51).

nd(aggregate,103,3,100,51).
nd(flows,19,16,22,22).
nd(ratio,99,51,51,3).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/**:- public aggregate/3, one_of/2, ratio/3, card/2.

:- mode aggregate(+,+,?),
        dimensioned(+),
	one_of(+,?),
	i_aggr(+,+,?),
	u_aggr(+,+,?),
	i_total(+,?),
	i_maxs(+,?),
	i_mins(+,?),
	i_maxs0(+,+,+,?,?),
	i_mins0(+,+,+,?,?),
	u_total(+,?),
	u_sum(+,+,?),
	u_maxs(+,?),
	u_mins(+,?),
	i_maxs0(+,+,+,?,?),
	i_mins0(+,+,+,?,?),
	u_lt(+,+).**/

aggregate(Fn,Set,Val) :-
   dimensioned(Set), !,
   u_aggr(Fn,Set,Val).
aggregate(Fn,Set,Val) :-
   i_aggr(Fn,Set,Val).

i_aggr(average,Set,Val) :-
   i_total(Set,T),
   length(Set,N),
   Val is T//N.
i_aggr(total,Set,Val) :-
   i_total(Set,Val).
i_aggr(max,Set,Val) :-
   i_maxs(Set,List),
   one_of(List,Val).
i_aggr(min,Set,Val) :-
   i_mins(Set,List),
   one_of(List,Val).
i_aggr(maximum,[V0:O|S],V) :-
   i_maxs0(S,V0,[O],_,V).
i_aggr(minimum,[V0:O|S],V) :-
   i_mins0(S,V0,[O],_,V).

u_aggr(average,Set,V--U) :-
   u_total(Set,T--U),
   length(Set,N),
   V is T//N.
u_aggr(total,Set,Val) :-
   u_total(Set,Val).
u_aggr(max,Set,Val) :-
   u_maxs(Set,List),
   one_of(List,Val).
u_aggr(min,Set,Val) :-
   u_mins(Set,List),
   one_of(List,Val).
u_aggr(maximum,[V0:O|S],V) :-
   u_maxs0(S,V0,[O],_,V).
u_aggr(minimum,[V0:O|S],V) :-
   u_mins0(S,V0,[O],_,V).

i_total([],0).
i_total([V:_|R],T) :-
   i_total(R,T0),
   T is V+T0.

i_maxs([V:X|Set],List) :-
   i_maxs0(Set,V,[X],List,_).

i_maxs0([],V,L,L,V).
i_maxs0([V0:X|R],V0,L0,L,V) :- !,
   i_maxs0(R,V0,[X|L0],L,V).
i_maxs0([U:X|R],V,_,L,W) :-
   U>V, !,
   i_maxs0(R,U,[X],L,W).
i_maxs0([_|R],V,L0,L,W) :-
   i_maxs0(R,V,L0,L,W).

i_mins([V:X|Set],List) :-
   i_mins0(Set,V,[X],List,_).

i_mins0([],V,L,L,V).
i_mins0([V:X|R],V,L0,L,W) :- !,
   i_mins0(R,V,[X|L0],L,W).
i_mins0([U:X|R],V,_,L,W) :-
   U<V, !,
   i_mins0(R,U,[X],L,W).
i_mins0([_|R],V,L0,L,W) :-
   i_mins0(R,V,L0,L,W).

u_total([],0--U).
u_total([V:_|R],T) :-
   u_total(R,T0),
   u_sum(T0,V,T).

u_sum(X--U,Y--U,Z--U) :- !,
   Z is X+Y.
u_sum(X--U,Y--U1,Z--U) :-
   ratio(U,U1,M,M1), M>M1, !,
   Z is X + (Y*M1)//M.
u_sum(X--U1,Y--U,Z--U) :-
   ratio(U,U1,M,M1), M>M1, !,
   Z is (X*M1)//M + Y.

u_maxs([V:X|Set],List) :-
   u_maxs0(Set,V,[X],List,_).

u_maxs0([],V,L,L,V).
u_maxs0([V0:X|R],V0,L0,L,V) :- !,
   u_maxs0(R,V0,[X|L0],L,V).
u_maxs0([U:X|R],V,_,L,W) :-
   u_lt(V,U), !,
   u_maxs0(R,U,[X],L,W).
u_maxs0([_|R],V,L0,L,W) :-
   u_maxs0(R,V,L0,L,W).

u_mins([V:X|Set],List) :-
   u_mins0(Set,V,[X],List,_).

u_mins0([],V,L,L,V).
u_mins0([V:X|R],V,L0,L,W) :- !,
   u_mins0(R,V,[X|L0],L,W).
u_mins0([U:X|R],V,_,L,W) :-
   u_lt(U,V), !,
   u_mins0(R,U,[X],L,W).
u_mins0([_|R],V,L0,L,W) :-
   u_mins0(R,V,L0,L,W).

u_lt(A,X--U) :-
   Y is -X,
   u_sum(A,Y--U,Z--_),
   Z<0.

dimensioned([(_--_):_|_]).

one_of([X|_],X).
one_of([_|R],X) :-
   one_of(R,X).

ratio(N,M,R) :- R is (N*100)//M.

card(S,N) :- length(S,N).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Data for the World Database.
% ---------------------------


% Interface.
% ---------

% Interface.
% ---------

database(aggregate(X,Y,Z)) :- aggregate(X,Y,Z).
database(one_of(X,Y)) :- one_of(X,Y).
database(ratio(X,Y,Z)) :- ratio(X,Y,Z).
database(card(X,Y)) :- card(X,Y).
database(african(X)) :- african(X).
database(american(X)) :- american(X).
database(area(X)) :- area(X).
database(area(X,Y)) :- area(X,Y).
database(asian(X)) :- asian(X).
database(borders(X,Y)) :- borders(X,Y).
database(capital(X)) :- capital(X).
database(capital(X,Y)) :- capital(X,Y).
database(circle_of_latitude(X)) :- circle_of_latitude(X).
database(city(X)) :- city(X).
database(continent(X)) :- continent(X).
database(country(X)) :- country(X).
database(drains(X,Y)) :- drains(X,Y).
database(eastof(X,Y)) :- eastof(X,Y).
database(european(X)) :- european(X).
database(exceeds(X,Y)) :- exceeds(X,Y).
database(flows(X,Y)) :- flows(X,Y).
database(flows(X,Y,Z)) :- flows(X,Y,Z).
database(in(X,Y)) :- in(X,Y).
database(latitude(X)) :- latitude(X).
database(latitude(X,Y)) :- latitude(X,Y).
database(longitude(X)) :- longitude(X).
database(longitude(X,Y)) :- longitude(X,Y).
database(northof(X,Y)) :- northof(X,Y).
database(ocean(X)) :- ocean(X).
database(place(X)) :- place(X).
%database(person(X)) :- person(X).
database(population(X)) :- population(X).
database(population(X,Y)) :- population(X,Y).
database(region(X)) :- region(X).
database(rises(X,Y)) :- rises(X,Y).
database(river(X)) :- river(X).
database(sea(X)) :- sea(X).
database(seamass(X)) :- seamass(X).
database(southof(X,Y)) :- southof(X,Y).
database(westof(X,Y)) :- westof(X,Y).

:-op(500,xfy,--).

exceeds(X--U,Y--U) :- !, X > Y.
exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.

ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).
ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).

area(X--ksqmiles).
capital(C) :- capital(X,C).
city(C) :- city(C,_,_).
country(C) :- country(C,_,_,_,_,_,_,_,_,_).
latitude(X--degrees).
longitude(X--degrees).
place(X) :- continent(X); region(X); seamass(X); country(X).
population(X--million).
population(X--thousand).
region(R) :- in_continent(R,_).

african(X) :- in(X,africa).
american(X) :- in(X,america).
asian(X) :- in(X,asia).
european(X) :- in(X,europe).

in(X,Y) :- var(X), nonvar(Y), !, contains(Y,X).
in(X,Y) :- in0(X,W), ( W=Y ; in(W,Y) ).

in0(X,Y) :- in_continent(X,Y).
in0(X,Y) :- city(X,Y,_).
in0(X,Y) :- country(X,Y,_,_,_,_,_,_,_,_).
in0(X,Y) :- flows(X,Y).

eastof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L2,L1).
northof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L1,L2).
southof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L2,L1).
westof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L1,L2).

circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).

latitude(equator,0--degrees).
latitude(tropic_of_cancer,23--degrees).
latitude(tropic_of_capricorn,-23--degrees).
latitude(arctic_circle,67--degrees).
latitude(antarctic_circle,-67--degrees).

latitude(C,L--degrees) :- country(C,_,L,_,_,_,_,_,_,_).
longitude(C,L--degrees) :- country(C,_,_,L,_,_,_,_,_,_).
area(C,A--ksqmiles) :- country(C,_,_,_,A,_,_,_,_,_).
population(C,P--thousand) :- city(C,_,P).
population(C,P--million) :- country(C,_,_,_,_,_,P,_,_,_).
capital(C,Cap) :- country(C,_,_,_,_,_,_,_,Cap,_).

continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

in_continent(scandinavia, europe).
in_continent(western_europe, europe).
in_continent(eastern_europe, europe).
in_continent(southern_europe, europe).
in_continent(north_america, america).
in_continent(central_america, america).
in_continent(caribbean, america).
in_continent(south_america, america).
in_continent(north_africa, africa).
in_continent(west_africa, africa).
in_continent(central_africa, africa).
in_continent(east_africa, africa).
in_continent(southern_africa, africa).
in_continent(middle_east,  asia).
in_continent(indian_subcontinent, asia).
in_continent(southeast_east, asia).
in_continent(far_east, asia).
in_continent(northern_asia, asia).

seamass(X) :- ocean(X).
seamass(X) :- sea(X).

ocean(arctic_ocean).
ocean(atlantic).
ocean(indian_ocean).
ocean(pacific).
ocean(southern_ocean).

sea(baltic).
sea(black_sea).
sea(caspian).
sea(mediterranean).
sea(persian_gulf).
sea(red_sea).

river(R) :- river(R,L).

rises(R,C) :- river(R,L), last(L,C).

drains(R,S) :- river(R,L), first(L,S).

flows(R,C) :- flows(R,C,_).

flows(R,C1,C2) :- river(R,L), links(L,C2,C1).

first([X|_],X).

last([X],X).
last([_|L],X) :- last(L,X).

links([X1,X2|_],X1,X2).
links([_|L],X1,X2) :- links(L,X1,X2).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Facts about rivers.
% ------------------

river(amazon,[atlantic,brazil,peru]).
river(amu_darya,[aral_sea,soviet_union,afghanistan]).
river(amur,[pacific,soviet_union,china,mongolia]).
river(brahmaputra,[indian_ocean,bangladesh,china]).
river(colorado,[pacific,mexico,united_states]).
river(congo_river,[atlantic,zaire,zambia]).
river(cubango,[botswana,south_africa,angola]).
river(danube,[black_sea,romania,yugoslavia,hungary,czechoslovakia,austria,
              west_germany]).
river(don,[black_sea,soviet_union]).
river(elbe,[atlantic,west_germany,east_germany,czechoslovakia]).
river(euphrates,[persian_gulf,iraq,syria,turkey]).
river(ganges,[indian_ocean,india,china]).
river(hwang_ho,[pacific,china]).
river(indus,[indian_ocean,pakistan,india,china]).
river(irrawaddy,[indian_ocean,burma]).
river(lena,[arctic_ocean,soviet_union]).
river(limpopo,[indian_ocean,mozambique,south_africa]).
river(mackenzie,[arctic_ocean,canada]).
river(mekong,[pacific,vietnam,cambodia,laos,china]).
river(mississippi,[atlantic,united_states]).
river(murray,[indian_ocean,australia]).
river(niger_river,[atlantic,nigeria,niger,mali,guinea]).
river(nile,[mediterranean,egypt,sudan,uganda]).
river(ob,[arctic_ocean,soviet_union]).
river(oder,[baltic,poland,czechoslovakia]).
river(orange,[atlantic,south_africa,lesotho]).
river(orinoco,[atlantic,venezuela,colombia]).
river(parana,[atlantic,argentina,paraguay,brazil]).
river(rhine,[atlantic,netherlands,west_germany,switzerland]).
river(rhone,[mediterranean,france,switzerland]).
river(rio_grande,[atlantic,mexico,united_states]).
river(salween,[indian_ocean,burma,china]).
river(senegal_river,[atlantic,senegal,mali,guinea]).
river(tagus,[atlantic,portugal,spain]).
river(vistula,[baltic,poland]).
river(volga,[black_sea,soviet_union]).
river(volta,[atlantic,ghana,upper_volta]).
river(yangtze,[pacific,china]).
river(yenisei,[arctic_ocean,soviet_union,mongolia]).
river(yukon,[pacific,united_states,canada]).
river(zambesi,[indian_ocean,mozambique,zambia,angola]).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Facts about cities.
% ------------------

city(athens,greece,1368).
city(bangkok,thailand,1178).
city(barcelona,spain,1280).
city(berlin,east_germany,3481).
city(birmingham,united_kingdom,1112).
city(bombay,india,2839).
city(brussels,belgium,986).
city(bucharest,romania,1237).
city(budapest,hungary,1757).
city(buenos_aires,argentina,3404).
city(cairo,egypt,2373).
city(calcutta,india,2549).
city(canton,china,1496).
city(caracas,venezuela,488).
city(chicago,united_states,3621).
city(chungking,china,1100).
city(dairen,china,544).
city(delhi,india,1744).
city(detroit,united_states,1850).
city(glasgow,united_kingdom,1090).
city(hamburg,west_germany,1700).
city(harbin,china,760).
city(hongkong_city,hongkong,2440).
city(hyderabad,india,1086).
city(istanbul,turkey,1215).
city(jakarta,indonesia,533).
city(johannesburg,south_africa,880).
city(karachi,pakistan,1126).
city(kiev,soviet_union,991).
city(kobe,japan,765).
city(kowloon,china,547).
city(kyoto,japan,1204).
city(leningrad,soviet_union,2800).
city(lima,peru,835).
city(london,united_kingdom,8346).
city(los_angeles,united_states,1970).
city(madras,india,1416).
city(madrid,spain,1700).
city(manila,philippines,1025).
city(melbourne,australia,1595).
city(mexico_city,mexico,3796).
city(milan,italy,1269).
city(montreal,canada,1109).
city(moscow,soviet_union,4800).
city(mukden,china,1551).
city(nagoya,japan,1337).
city(nanking,japan,1020).
city(naples,italy,1012).
city(new_york,united_states,7795).
city(osaka,japan,2547).
city(paris,france,2850).
city(peking,china,2031).
city(philadelphia,united_states,2072).
city(pusan,south_korea,474).
city(rio_de_janeiro,brazil,2413).
city(rome,italy,1760).
city(saigon,vietnam,695).
city(santiago,chile,1350).
city(sao_paulo,brazil,2228).
city(seoul,south_korea,1446).
city(shanghai,china,5407).
city(sian,china,629).
city(singapore_city,singapore,1264).
city(sydney,australia,1898).
city(tehran,iran,1010).
city(tientsin,china,1795).
city(tokyo,japan,8535).
city(toronto,canada,668).
city(vienna,austria,1766).
city(warsaw,poland,965).
city(yokohama,japan,1143).
city(toronto,canada,668).
city(vienna,austria,1766).
city(warsaw,poland,965).
city(yokohama,japan,1143).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Facts about countries.
% ---------------------

% country(Country,Region,Latitude,Longitude,
%         Area/1000,Area mod 1000,
%         Population/1000000,Population mod 1000000 / 1000,
%         Capital,Currency)

country(afghanistan,indian_subcontinent,33,-65,254,861,18,290,kabul,afghani).
country(albania,southern_europe,41,-20,11,100,2,350,tirana,lek).
country(algeria,north_africa,35,-11,919,951,15,770,algiers,dinar).
country(andorra,southern_europe,42,-1,0,179,0,25,andorra_la_villa,
	franc_peseta).
country(angola,southern_africa,-12,-18,481,351,5,810,luanda,?).
country(argentina,south_america,-35,66,1072,67,23,920,buenos_aires,peso).
country(australia,australasia,-23,-135,2967,909,13,268,canberra,
	australian_dollar).
country(austria,western_europe,47,-14,32,374,7,520,vienna,schilling).
country(bahamas,caribbean,24,74,4,404,0,190,nassau,bahamian_dollar).
country(bahrain,middle_east,26,-50,0,231,0,230,manama,dinar).
country(bangladesh,indian_subcontinent,24,-90,55,126,71,317,dacca,taka).
country(barbados,caribbean,13,59,0,166,0,240,bridgetown,east_carribean_dollar).
country(belgium,western_europe,51,-5,11,779,9,711,brussels,franc).
country(belize,central_america,17,88,8,866,0,82,belize_town,?).
country(bhutan,indian_subcontinent,27,-90,19,305,1,150,thimphu,indian_rupee).
country(bolivia,south_america,-17,64,424,162,5,330,sucre,peso).
country(botswana,southern_africa,-22,-24,219,815,0,650,gaborone,
	south_african_rand).
country(brazil,south_america,-13,53,3286,470,105,137,brasilia,cruzeiro).
country(bulgaria,eastern_europe,43,-25,42,829,8,620,sofia,lev).
country(burma,southeast_east,21,-96,261,789,29,560,rangoon,kyat).
country(burundi,central_africa,-3,-30,10,739,3,600,bujumbura,franc).
country(cambodia,southeast_east,12,-105,69,898,7,640,phnom_penh,riel).
country(cameroon,west_africa,3,-12,183,568,6,170,yaounde,cfa_franc).
country(canada,north_america,60,100,3851,809,22,47,ottawa,canadian_dollar).
country(central_african_republic,central_africa,7,-20,241,313,1,720,bangui,
	cfa_franc).
country(chad,central_africa,12,-17,495,752,3,870,n_djamena,cfa_franc).
country(chile,south_america,-35,71,286,396,10,230,santiago,escudo).
country(china,far_east,30,-110,3691,502,840,0,peking,yuan).
country(colombia,south_america,4,73,455,335,23,210,bogota,peso).
country(congo,central_africa,-1,-16,132,46,1,1,brazzaville,cfa_franc).
country(costa_rica,central_america,10,84,19,653,1,890,san_jose,colon).
country(cuba,caribbean,22,79,44,218,8,870,havana,peso).
country(cyprus,southern_europe,35,-33,3,572,0,660,nicosia,pound).
country(czechoslovakia,eastern_europe,49,-17,49,371,14,580,prague,koruna).
country(dahomey,west_africa,8,-2,43,483,2,910,porto_novo,cfa_franc).
country(denmark,scandinavia,55,-9,16,615,5,130,copenhagen,krone).
country(djibouti,east_africa,12,-42,9,71,0,45,djibouti,?).
country(dominican_republic,caribbean,19,70,18,704,4,430,santa_domingo,peso).
country(east_germany,eastern_europe,52,-12,40,646,16,980,east_berlin,ddr_mark).
country(ecuador,south_america,-2,78,105,685,6,730,quito,sucre).
country(egypt,north_africa,28,-31,386,872,35,620,cairo,egyptian_pound).
country(eire,western_europe,53,8,26,600,3,30,dublin,irish_pound).
country(el_salvador,central_america,14,89,8,260,3,860,san_salvador,colon).
country(equatorial_guinea,west_africa,1,-10,10,832,0,300,santa_isabel,peveta).
country(ethiopia,east_africa,8,-40,457,142,26,80,addis_ababa,ethiopean_dollar).
country(fiji,australasia,-17,-179,7,55,0,550,suva,fiji_dollar).
country(finland,scandinavia,65,-27,130,119,4,660,helsinki,markka).
country(france,western_europe,47,-3,212,973,52,350,paris,franc).
country(french_guiana,south_america,4,53,34,740,0,27,cayenne,?).
country(gabon,central_africa,0,-10,102,317,0,520,libreville,cfa_franc).
country(gambia,west_africa,13,16,4,3,0,490,banjul,dalasi).
country(ghana,west_africa,6,1,92,100,9,360,accra,cedi).
country(greece,southern_europe,40,-23,50,547,9,30,athens,drachma).
country(grenada,caribbean,12,61,0,133,0,100,st_georges,east_caribbean_dollar).
country(guatemala,central_america,15,90,42,42,5,540,guatamala_city,quetzal).
country(guinea,west_africa,10,10,94,925,4,210,conakry,syli).
country(guinea_bissau,west_africa,12,15,13,948,0,510,bissau,pataca).
country(guyana,south_america,5,59,83,0,0,760,georgetown,guyana_dollar).
country(haiti,caribbean,19,72,10,714,5,200,port_au_prince,gourde).
country(honduras,central_america,14,86,43,277,2,780,tegucigalpa,lempira).
country(hungary,eastern_europe,47,-19,35,919,10,410,budapest,forint).
country(iceland,western_europe,65,19,39,702,0,210,reykjavik,krona).
country(india,indian_subcontinent,20,-80,1229,919,574,220,new_delhi,rupee).
country(indonesia,southeast_east,-5,-115,735,268,124,600,jakarta,rupiah).
country(iran,middle_east,33,-53,636,363,32,1,tehran,rial).
country(iraq,middle_east,33,-44,167,567,10,410,baghdad,dinar).
country(israel,middle_east,32,-35,34,493,3,228,jerusalem,israeli_pound).
country(italy,southern_europe,42,-13,116,303,55,262,rome,lira).
country(ivory_coast,west_africa,7,5,124,503,4,640,abidjan,cfa_franc).
country(jamaica,caribbean,18,77,4,411,1,980,kingston,jamaican_dollar).
country(japan,far_east,36,-136,143,574,108,710,tokyo,yen).
country(jordan,middle_east,31,-36,32,297,2,560,amman,dinar).
country(kenya,east_africa,1,-38,224,960,12,480,nairobi,kenya_shilling).
country(kuwait,middle_east,29,-47,7,780,0,880,kuwait_city,kuwaiti_dinar).
country(laos,southeast_east,18,-105,3,180,3,180,vientiane,kip).
country(lebanon,middle_east,34,-36,4,15,3,213,beirut,lebanese_pound).
country(lesotho,southern_africa,-30,-28,11,716,1,200,masero,rand).
country(liberia,west_africa,6,9,43,0,1,660,monrovia,us_dollar).
country(libya,north_africa,28,-17,679,536,2,257,tripoli,libyan_dinar).
country(liechtenstein,western_europe,47,-9,0,62,0,23,vaduz,swiss_franc).
country(luxembourg,western_europe,50,-6,0,999,0,350,luxembourg,
	luxembourg_franc).
country(malagasy,southern_africa,-20,-47,203,35,7,655,tananarive,ariary).
country(malawi,southern_africa,-13,-34,45,747,4,790,zomba,kwacha).
country(malaysia,southeast_east,5,-110,128,328,10,920,kuala_lumpa,
	malaysian_dollar).
country(maldives,indian_subcontinent,2,-73,0,115,0,123,male,rupee).
country(mali,west_africa,15,10,464,873,5,380,bamako,mali_franc).
country(malta,southern_europe,36,-14,0,122,0,319,valetta,pound).
country(mauritania,west_africa,21,10,419,229,1,260,nouakchott,ouguiya).
country(mauritius,southern_africa,-20,-57,0,787,0,870,port_louis,rupee).
country(mexico,central_america,20,100,761,601,54,300,mexico_city,peso).
country(monaco,southern_europe,44,-7,0,1,0,30,monaco,french_franc).
country(mongolia,northern_asia,47,-103,604,247,1,360,ulan_bator,tighrik).
country(morocco,north_africa,32,6,171,953,16,310,rabat,dirham).
country(mozambique,southern_africa,-19,-35,303,373,8,820,maputo,?).
country(nepal,indian_subcontinent,28,-84,54,362,12,20,katmandu,nepalese_rupee).
country(netherlands,western_europe,52,-5,14,192,13,500,amsterdam,guilder).
country(new_zealand,australasia,-40,-176,103,736,2,962,wellington,
	new_zealand_dollar).
country(nicaragua,central_america,12,85,57,143,2,10,managua,cordoba).
country(niger,west_africa,13,-10,489,206,4,300,niamey,cfa_franc).
country(nigeria,west_africa,8,-8,356,669,79,759,lagos,naira).
country(north_korea,far_east,40,-127,46,768,15,90,pvongvang,won).
country(norway,scandinavia,64,-11,125,181,3,960,oslo,krone).
country(oman,middle_east,23,-58,82,0,0,720,muscat,riyal_omani).
country(pakistan,indian_subcontinent,30,-70,342,750,66,750,islamad,rupee).
country(panama,central_america,9,80,28,753,1,570,panama,balboa).
country(papua_new_guinea,
          australasia,-8,-145,183,540,2,580,port_harcourt,australian_dollar).
country(paraguay,south_america,-23,57,157,47,2,670,asuncion,guarani).
country(peru,south_america,-8,75,496,222,14,910,lima,sol).
country(philippines,southeast_east,12,-123,115,707,40,220,quezon_city,piso).
country(poland,eastern_europe,52,-20,120,359,33,360,warsaw,zloty).
country(portugal,southern_europe,40,7,35,340,8,560,lisbon,escudo).
country(qatar,middle_east,25,-51,4,0,0,115,doha,riyal).
country(romania,eastern_europe,46,-25,91,699,5,690,bucharest,leu).
country(rwanda,central_africa,-2,-30,10,169,3,980,kigali,rwanda_franc).
country(san_marino,southern_europe,44,-12,0,24,0,20,san_marino,italian_lira).
country(saudi_arabia,middle_east,26,-44,873,0,8,100,riyadh,riyal).
country(senegal,west_africa,14,14,76,124,4,230,dakar,cfa_franc).
country(seychelles,east_africa,-4,-55,0,40,0,156,victoria,rupee).
country(sierra_leone,west_africa,9,12,27,925,2,860,freetown,leone).
country(singapore,southeast_east,1,-104,0,226,2,190,singapore,
	singapore_dollar).
country(somalia,east_africa,7,-47,246,155,3,100,mogadishu,somali_shilling).
country(south_africa,southern_africa,-30,-25,471,819,23,720,pretoria,rand).
country(south_korea,far_east,36,-128,38,31,33,333,seoul,won).
country(south_yemen,middle_east,15,-48,111,0,1,600,aden,dinar).
country(soviet_union,northern_asia,57,-80,8347,250,250,900,moscow,ruble).
country(spain,southern_europe,40,5,194,883,34,860,madrid,peseta).
country(sri_lanka,indian_subcontinent,7,-81,25,332,13,250,colombo,rupee).
country(sudan,central_africa,15,-30,967,491,16,900,khartoum,pound).
country(surinam,south_america,4,56,55,0,0,208,paramaribo,?).
country(swaziland,southern_africa,-26,-31,6,705,0,460,mbabane,lilageru).
country(sweden,scandinavia,63,-15,173,665,8,144,stockholm,krona).
country(switzerland,western_europe,46,-8,15,941,6,440,bern,franc).
country(syria,middle_east,35,-38,71,498,6,895,damascus,syrian_pound).
country(taiwan,far_east,23,-121,13,592,15,737,taipei,taiwan_dollar).
country(tanzania,east_africa,-7,-34,363,708,14,0,dar_es_salaam,
	tanzanian_shilling).
country(thailand,southeast_east,16,-102,198,455,39,950,bangkok,baht).
country(togo,west_africa,8,-1,21,853,2,120,lome,cfa_franc).
country(tonga,australasia,-20,173,0,269,0,90,nukualofa,pa_anga).
country(trinidad_and_tobago,caribbean,10,61,1,979,5,510,port_of_spain,
	trinidad_and_tobago_dollar).
country(tunisia,north_africa,33,-9,63,378,5,510,tunis,dinar).
country(turkey,middle_east,39,-36,301,380,37,930,ankara,lira).
country(uganda,east_africa,2,-32,91,134,10,810,kampala,uganda_shilling).
country(united_arab_emirates,middle_east,24,-54,32,278,0,210,abu_dhabi,dirham).
country(united_kingdom,western_europe,54,2,94,209,55,930,london,pound).
country(united_states,north_america,37,96,3615,122,211,210,washington,dollar).
country(upper_volta,west_africa,12,2,105,869,5,740,ouagadougou,cfa_franc).
country(uruguay,south_america,-32,55,68,548,2,990,montevideo,peso).
country(venezuela,south_america,8,65,352,143,11,520,caracas,bolivar).
country(vietnam,southeast_east,17,-107,126,436,41,850,hanoi,dong).
country(west_germany,western_europe,52,-9,95,815,61,970,bonn,deutsche_mark).
country(western_samoa,australasia,-14,172,1,133,0,150,apia,tala).
country(yemen,middle_east,15,-44,75,289,1,600,sana,rial).
country(yugoslavia,southern_europe,44,-20,98,766,21,126,belgrade,dinar).
country(zaire,central_africa,-3,-23,905,63,23,560,kinshasa,zaire).
country(zambia,southern_africa,-15,-28,290,724,4,640,lusaka,kwacha).
country(zimbabwe,southern_africa,-20,-30,150,333,5,690,salisbury,
	rhodesian_dollar).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
% Inversion of the 'in' relation.
% ------------------------------

/**:-mode contains0(+,?).**/

contains(X,Y) :- contains0(X,Y).
contains(X,Y) :- contains0(X,W), contains(W,Y).

contains0(africa,central_africa).
contains0(africa,east_africa).
contains0(africa,north_africa).
contains0(africa,southern_africa).
contains0(africa,west_africa).

contains0(america,caribbean).
contains0(america,central_america).
contains0(america,north_america).
contains0(america,south_america).

contains0(asia,far_east).
contains0(asia,indian_subcontinent).
contains0(asia,middle_east).
contains0(asia,northern_asia).
contains0(asia,southeast_east).

contains0(australasia,australia).
contains0(australasia,fiji).
contains0(australasia,new_zealand).
contains0(australasia,papua_new_guinea).
contains0(australasia,tonga).
contains0(australasia,western_samoa).

contains0(europe,eastern_europe).
contains0(europe,scandinavia).
contains0(europe,southern_europe).
contains0(europe,western_europe).

contains0(scandinavia,denmark).
contains0(scandinavia,finland).
contains0(scandinavia,norway).
contains0(scandinavia,sweden).

contains0(western_europe,austria).
contains0(western_europe,belgium).
contains0(western_europe,eire).
contains0(western_europe,france).
contains0(western_europe,iceland).
contains0(western_europe,liechtenstein).
contains0(western_europe,luxembourg).
contains0(western_europe,netherlands).
contains0(western_europe,switzerland).
contains0(western_europe,united_kingdom).
contains0(western_europe,west_germany).

contains0(eastern_europe,bulgaria).
contains0(eastern_europe,czechoslovakia).
contains0(eastern_europe,east_germany).
contains0(eastern_europe,hungary).
contains0(eastern_europe,poland).
contains0(eastern_europe,romania).

contains0(southern_europe,albania).
contains0(southern_europe,andorra).
contains0(southern_europe,cyprus).
contains0(southern_europe,greece).
contains0(southern_europe,italy).
contains0(southern_europe,malta).
contains0(southern_europe,monaco).
contains0(southern_europe,portugal).
contains0(southern_europe,san_marino).
contains0(southern_europe,spain).
contains0(southern_europe,yugoslavia).

contains0(north_america,canada).
contains0(north_america,united_states).

contains0(central_america,belize).
contains0(central_america,costa_rica).
contains0(central_america,el_salvador).
contains0(central_america,guatemala).
contains0(central_america,honduras).
contains0(central_america,mexico).
contains0(central_america,nicaragua).
contains0(central_america,panama).

contains0(caribbean,bahamas).
contains0(caribbean,barbados).
contains0(caribbean,cuba).
contains0(caribbean,dominican_republic).
contains0(caribbean,grenada).
contains0(caribbean,haiti).
contains0(caribbean,jamaica).
contains0(caribbean,trinidad_and_tobago).

contains0(south_america,argentina).
contains0(south_america,bolivia).
contains0(south_america,brazil).
contains0(south_america,chile).
contains0(south_america,colombia).
contains0(south_america,ecuador).
contains0(south_america,french_guiana).
contains0(south_america,guyana).
contains0(south_america,paraguay).
contains0(south_america,peru).
contains0(south_america,surinam).
contains0(south_america,uruguay).
contains0(south_america,venezuela).

contains0(north_africa,algeria).
contains0(north_africa,egypt).
contains0(north_africa,libya).
contains0(north_africa,morocco).
contains0(north_africa,tunisia).

contains0(west_africa,cameroon).
contains0(west_africa,dahomey).
contains0(west_africa,equatorial_guinea).
contains0(west_africa,gambia).
contains0(west_africa,ghana).
contains0(west_africa,guinea).
contains0(west_africa,guinea_bissau).
contains0(west_africa,ivory_coast).
contains0(west_africa,liberia).
contains0(west_africa,mali).
contains0(west_africa,mauritania).
contains0(west_africa,niger).
contains0(west_africa,nigeria).
contains0(west_africa,senegal).
contains0(west_africa,sierra_leone).
contains0(west_africa,togo).
contains0(west_africa,upper_volta).

contains0(central_africa,burundi).
contains0(central_africa,central_african_republic).
contains0(central_africa,chad).
contains0(central_africa,congo).
contains0(central_africa,gabon).
contains0(central_africa,rwanda).
contains0(central_africa,sudan).
contains0(central_africa,zaire).

contains0(east_africa,djibouti).
contains0(east_africa,ethiopia).
contains0(east_africa,kenya).
contains0(east_africa,seychelles).
contains0(east_africa,somalia).
contains0(east_africa,tanzania).
contains0(east_africa,uganda).

contains0(southern_africa,angola).
contains0(southern_africa,botswana).
contains0(southern_africa,lesotho).
contains0(southern_africa,malagasy).
contains0(southern_africa,malawi).
contains0(southern_africa,mauritius).
contains0(southern_africa,mozambique).
contains0(southern_africa,south_africa).
contains0(southern_africa,swaziland).
contains0(southern_africa,zambia).
contains0(southern_africa,zimbabwe).

contains0(middle_east,bahrain).
contains0(middle_east,iran).
contains0(middle_east,iraq).
contains0(middle_east,israel).
contains0(middle_east,jordan).
contains0(middle_east,kuwait).
contains0(middle_east,lebanon).
contains0(middle_east,oman).
contains0(middle_east,qatar).
contains0(middle_east,saudi_arabia).
contains0(middle_east,south_yemen).
contains0(middle_east,syria).
contains0(middle_east,turkey).
contains0(middle_east,united_arab_emirates).
contains0(middle_east,yemen).

contains0(indian_subcontinent,afghanistan).
contains0(indian_subcontinent,bangladesh).
contains0(indian_subcontinent,bhutan).
contains0(indian_subcontinent,india).
contains0(indian_subcontinent,maldives).
contains0(indian_subcontinent,nepal).
contains0(indian_subcontinent,pakistan).
contains0(indian_subcontinent,sri_lanka).

contains0(southeast_east,burma).
contains0(southeast_east,cambodia).
contains0(southeast_east,indonesia).
contains0(southeast_east,laos).
contains0(southeast_east,malaysia).
contains0(southeast_east,philippines).
contains0(southeast_east,singapore).
contains0(southeast_east,thailand).
contains0(southeast_east,vietnam).

contains0(far_east,china).
contains0(far_east,japan).
contains0(far_east,north_korea).
contains0(far_east,south_korea).
contains0(far_east,taiwan).

contains0(northern_asia,mongolia).
contains0(northern_asia,soviet_union).

contains0(afghanistan,amu_darya).

contains0(angola,cubango).
contains0(angola,zambesi).

contains0(argentina,buenos_aires).
contains0(argentina,parana).

contains0(australia,melbourne).
contains0(australia,murray).
contains0(australia,sydney).

contains0(austria,danube).
contains0(austria,vienna).

contains0(bangladesh,brahmaputra).

contains0(belgium,brussels).

contains0(brazil,amazon).
contains0(brazil,parana).
contains0(brazil,rio_de_janeiro).
contains0(brazil,sao_paulo).

contains0(burma,irrawaddy).
contains0(burma,salween).

contains0(cambodia,mekong).

contains0(canada,mackenzie).
contains0(canada,montreal).
contains0(canada,toronto).
contains0(canada,yukon).

contains0(chile,santiago).

contains0(china,amur).
contains0(china,brahmaputra).
contains0(china,canton).
contains0(china,chungking).
contains0(china,dairen).
contains0(china,ganges).
contains0(china,harbin).
contains0(china,hwang_ho).
contains0(china,indus).
contains0(china,kowloon).
contains0(china,mekong).
contains0(china,mukden).
contains0(china,peking).
contains0(china,salween).
contains0(china,shanghai).
contains0(china,sian).
contains0(china,tientsin).
contains0(china,yangtze).

contains0(colombia,orinoco).

contains0(czechoslovakia,danube).
contains0(czechoslovakia,elbe).
contains0(czechoslovakia,oder).

contains0(east_germany,berlin).
contains0(east_germany,elbe).

contains0(egypt,cairo).
contains0(egypt,nile).

contains0(france,paris).
contains0(france,rhone).

contains0(ghana,volta).

contains0(greece,athens).

contains0(guinea,niger_river).
contains0(guinea,senegal_river).

contains0(hungary,budapest).
contains0(hungary,danube).

contains0(india,bombay).
contains0(india,calcutta).
contains0(india,delhi).
contains0(india,ganges).
contains0(india,hyderabad).
contains0(india,indus).
contains0(india,madras).

contains0(indonesia,jakarta).

contains0(iran,tehran).

contains0(iraq,euphrates).

contains0(italy,milan).
contains0(italy,naples).
contains0(italy,rome).

contains0(japan,kobe).
contains0(japan,kyoto).
contains0(japan,nagoya).
contains0(japan,nanking).
contains0(japan,osaka).
contains0(japan,tokyo).
contains0(japan,yokohama).

contains0(laos,mekong).

contains0(lesotho,orange).

contains0(mali,niger_river).
contains0(mali,senegal_river).

contains0(mexico,colorado).
contains0(mexico,mexico_city).
contains0(mexico,rio_grande).

contains0(mongolia,amur).
contains0(mongolia,yenisei).

contains0(mozambique,limpopo).
contains0(mozambique,zambesi).

contains0(netherlands,rhine).

contains0(niger,niger_river).

contains0(nigeria,niger_river).

contains0(pakistan,indus).
contains0(pakistan,karachi).

contains0(paraguay,parana).

contains0(peru,amazon).
contains0(peru,lima).

contains0(philippines,manila).

contains0(poland,oder).
contains0(poland,vistula).
contains0(poland,warsaw).

contains0(portugal,tagus).

contains0(romania,bucharest).
contains0(romania,danube).

contains0(senegal,senegal_river).

contains0(singapore,singapore_city).

contains0(south_africa,cubango).
contains0(south_africa,johannesburg).
contains0(south_africa,limpopo).
contains0(south_africa,orange).

contains0(south_korea,pusan).
contains0(south_korea,seoul).

contains0(soviet_union,amu_darya).
contains0(soviet_union,amur).
contains0(soviet_union,don).
contains0(soviet_union,kiev).
contains0(soviet_union,lena).
contains0(soviet_union,leningrad).
contains0(soviet_union,moscow).
contains0(soviet_union,ob).
contains0(soviet_union,volga).
contains0(soviet_union,yenisei).

contains0(spain,barcelona).
contains0(spain,madrid).
contains0(spain,tagus).

contains0(sudan,nile).

contains0(switzerland,rhine).
contains0(switzerland,rhone).

contains0(syria,euphrates).

contains0(thailand,bangkok).

contains0(turkey,euphrates).
contains0(turkey,istanbul).

contains0(uganda,nile).

contains0(united_kingdom,birmingham).
contains0(united_kingdom,glasgow).
contains0(united_kingdom,london).

contains0(united_states,chicago).
contains0(united_states,colorado).
contains0(united_states,detroit).
contains0(united_states,los_angeles).
contains0(united_states,mississippi).
contains0(united_states,new_york).
contains0(united_states,philadelphia).
contains0(united_states,rio_grande).
contains0(united_states,yukon).

contains0(upper_volta,volta).

contains0(venezuela,caracas).
contains0(venezuela,orinoco).

contains0(vietnam,mekong).
contains0(vietnam,saigon).

contains0(west_germany,danube).
contains0(west_germany,elbe).
contains0(west_germany,hamburg).
contains0(west_germany,rhine).

contains0(yugoslavia,danube).

contains0(zaire,congo_river).

contains0(zambia,congo_river).
contains0(zambia,zambesi).
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/

borders(X,C) :- var(X), nonvar(C), !, borders(C,X).

% Facts about Europe.
% ------------------

borders(albania,greece).
borders(albania,yugoslavia).
borders(albania,mediterranean).

borders(andorra,france).
borders(andorra,spain).

borders(austria,czechoslovakia).
borders(austria,hungary).
borders(austria,italy).
borders(austria,liechtenstein).
borders(austria,switzerland).
borders(austria,west_germany).
borders(austria,yugoslavia).

borders(belgium,france).
borders(belgium,luxembourg).
borders(belgium,netherlands).
borders(belgium,west_germany).
borders(belgium,atlantic).

borders(bulgaria,greece).
borders(bulgaria,romania).
borders(bulgaria,turkey).
borders(bulgaria,yugoslavia).
borders(bulgaria,black_sea).

borders(cyprus,mediterranean).

borders(czechoslovakia,austria).
borders(czechoslovakia,east_germany).
borders(czechoslovakia,hungary).
borders(czechoslovakia,poland).
borders(czechoslovakia,soviet_union).
borders(czechoslovakia,west_germany).

borders(denmark,west_germany).
borders(denmark,atlantic).
borders(denmark,baltic).

borders(eire,united_kingdom).
borders(eire,atlantic).

borders(finland,norway).
borders(finland,soviet_union).
borders(finland,sweden).
borders(finland,baltic).

borders(france,andorra).
borders(france,belgium).
borders(france,italy).
borders(france,luxembourg).
borders(france,monaco).
borders(france,spain).
borders(france,switzerland).
borders(france,west_germany).
borders(france,atlantic).
borders(france,mediterranean).

borders(east_germany,czechoslovakia).
borders(east_germany,poland).
borders(east_germany,west_germany).
borders(east_germany,baltic).

borders(greece,albania).
borders(greece,bulgaria).
borders(greece,turkey).
borders(greece,yugoslavia).
borders(greece,mediterranean).

borders(hungary,austria).
borders(hungary,czechoslovakia).
borders(hungary,romania).
borders(hungary,soviet_union).
borders(hungary,yugoslavia).

borders(iceland,atlantic).

borders(italy,austria).
borders(italy,france).
borders(italy,san_marino).
borders(italy,switzerland).
borders(italy,yugoslavia).
borders(italy,mediterranean).

borders(liechtenstein,austria).
borders(liechtenstein,switzerland).

borders(luxembourg,belgium).
borders(luxembourg,france).
borders(luxembourg,west_germany).

borders(malta,mediterranean).

borders(monaco,france).
borders(monaco,mediterranean).

borders(netherlands,belgium).
borders(netherlands,west_germany).
borders(netherlands,atlantic).

borders(norway,finland).
borders(norway,sweden).
borders(norway,soviet_union).
borders(norway,arctic_ocean).
borders(norway,atlantic).

borders(poland,czechoslovakia).
borders(poland,east_germany).
borders(poland,soviet_union).
borders(poland,baltic).

borders(portugal,spain).
borders(portugal,atlantic).

borders(romania,bulgaria).
borders(romania,hungary).
borders(romania,soviet_union).
borders(romania,yugoslavia).
borders(romania,black_sea).

borders(san_marino,italy).
borders(san_marino,mediterranean).

borders(spain,andorra).
borders(spain,france).
borders(spain,portugal).
borders(spain,atlantic).
borders(spain,mediterranean).

borders(sweden,finland).
borders(sweden,norway).
borders(sweden,atlantic).
borders(sweden,baltic).

borders(switzerland,austria).
borders(switzerland,france).
borders(switzerland,italy).
borders(switzerland,liechtenstein).
borders(switzerland,west_germany).

borders(west_germany,austria).
borders(west_germany,belgium).
borders(west_germany,czechoslovakia).
borders(west_germany,denmark).
borders(west_germany,east_germany).
borders(west_germany,france).
borders(west_germany,luxembourg).
borders(west_germany,netherlands).
borders(west_germany,switzerland).
borders(west_germany,atlantic).
borders(west_germany,baltic).

borders(united_kingdom,eire).
borders(united_kingdom,atlantic).

borders(yugoslavia,albania).
borders(yugoslavia,austria).
borders(yugoslavia,bulgaria).
borders(yugoslavia,greece).
borders(yugoslavia,hungary).
borders(yugoslavia,italy).
borders(yugoslavia,romania).
borders(yugoslavia,mediterranean).

% Facts about Asia.
% ----------------

borders(afghanistan,china).
borders(afghanistan,iran).
borders(afghanistan,pakistan).
borders(afghanistan,soviet_union).

borders(bahrain,persian_gulf).

borders(bangladesh,burma).
borders(bangladesh,india).
borders(bangladesh,indian_ocean).

borders(bhutan,china).
borders(bhutan,india).

borders(burma,bangladesh).
borders(burma,china).
borders(burma,india).
borders(burma,laos).
borders(burma,thailand).
borders(burma,indian_ocean).

borders(cambodia,laos).
borders(cambodia,thailand).
borders(cambodia,vietnam).
borders(cambodia,pacific).

borders(china,afghanistan).
borders(china,bhutan).
borders(china,burma).
borders(china,india).
borders(china,laos).
borders(china,mongolia).
borders(china,nepal).
borders(china,north_korea).
borders(china,pakistan).
borders(china,soviet_union).
borders(china,vietnam).
borders(china,pacific).

borders(india,bangladesh).
borders(india,bhutan).
borders(india,burma).
borders(india,china).
borders(india,nepal).
borders(india,pakistan).
borders(india,indian_ocean).

borders(indonesia,malaysia).
borders(indonesia,papua_new_guinea).
borders(indonesia,indian_ocean).
borders(indonesia,pacific).

borders(iran,afghanistan).
borders(iran,iraq).
borders(iran,pakistan).
borders(iran,soviet_union).
borders(iran,turkey).
borders(iran,caspian).
borders(iran,persian_gulf).
borders(iran,indian_ocean).

borders(iraq,iran).
borders(iraq,jordan).
borders(iraq,kuwait).
borders(iraq,saudi_arabia).
borders(iraq,syria).
borders(iraq,turkey).
borders(iraq,persian_gulf).

borders(israel,egypt).
borders(israel,jordan).
borders(laos,burma).
borders(laos,cambodia).
borders(laos,china).
borders(laos,thailand).
borders(laos,vietnam).

borders(israel,lebanon).
borders(israel,syria).
borders(israel,mediterranean).
borders(israel,red_sea).

borders(japan,pacific).

borders(jordan,iraq).
borders(jordan,israel).
borders(jordan,saudi_arabia).
borders(jordan,syria).
borders(jordan,red_sea).

borders(kuwait,iraq).
borders(kuwait,saudi_arabia).
borders(kuwait,persian_gulf).

borders(lebanon,israel).
borders(lebanon,syria).
borders(lebanon,mediterranean).

borders(malaysia,indonesia).
borders(malaysia,singapore).
borders(malaysia,thailand).
borders(malaysia,indian_ocean).
borders(malaysia,pacific).

borders(maldives,indian_ocean).

borders(mongolia,china).
borders(mongolia,soviet_union).

borders(nepal,china).
borders(nepal,india).

borders(north_korea,china).
borders(north_korea,south_korea).
borders(north_korea,soviet_union).
borders(north_korea,pacific).

borders(oman,saudi_arabia).
borders(oman,united_arab_emirates).
borders(oman,south_yemen).
borders(oman,indian_ocean).

borders(pakistan,afghanistan).
borders(pakistan,china).
borders(pakistan,india).
borders(pakistan,iran).
borders(pakistan,indian_ocean).

borders(philippines,pacific).

borders(qatar,saudi_arabia).
borders(qatar,united_arab_emirates).
borders(qatar,persian_gulf).

borders(saudi_arabia,iraq).
borders(saudi_arabia,jordan).
borders(saudi_arabia,kuwait).
borders(saudi_arabia,oman).
borders(saudi_arabia,qatar).
borders(saudi_arabia,south_yemen).
borders(saudi_arabia,united_arab_emirates).
borders(saudi_arabia,yemen).
borders(saudi_arabia,persian_gulf).
borders(saudi_arabia,red_sea).

borders(singapore,malaysia).
borders(singapore,pacific).

borders(south_korea,north_korea).
borders(south_korea,pacific).

borders(south_yemen,oman).
borders(south_yemen,saudi_arabia).
borders(south_yemen,yemen).
borders(south_yemen,indian_ocean).

borders(soviet_union,afghanistan).
borders(soviet_union,china).
borders(soviet_union,czechoslovakia).
borders(soviet_union,finland).
borders(soviet_union,hungary).
borders(soviet_union,iran).
borders(soviet_union,mongolia).
borders(soviet_union,north_korea).
borders(soviet_union,norway).
borders(soviet_union,poland).
borders(soviet_union,romania).
borders(soviet_union,turkey).
borders(soviet_union,arctic_ocean).
borders(soviet_union,baltic).
borders(soviet_union,black_sea).
borders(soviet_union,caspian).
borders(soviet_union,pacific).

borders(sri_lanka,indian_ocean).

borders(syria,iraq).
borders(syria,israel).
borders(syria,jordan).
borders(syria,lebanon).
borders(syria,turkey).
borders(syria,mediterranean).

borders(taiwan,pacific).

borders(thailand,burma).
borders(thailand,cambodia).
borders(thailand,laos).
borders(thailand,malaysia).
borders(thailand,indian_ocean).
borders(thailand,pacific).

borders(turkey,bulgaria).
borders(turkey,greece).
borders(turkey,iran).
borders(turkey,iraq).
borders(turkey,soviet_union).
borders(turkey,syria).
borders(turkey,black_sea).
borders(turkey,mediterranean).

borders(united_arab_emirates,oman).
borders(united_arab_emirates,qatar).
borders(united_arab_emirates,saudi_arabia).
borders(united_arab_emirates,persian_gulf).

borders(vietnam,cambodia).
borders(vietnam,china).
borders(vietnam,laos).
borders(vietnam,pacific).

borders(yemen,south_yemen).
borders(yemen,saudi_arabia).
borders(yemen,red_sea).

% Facts about Africa.
% ------------------

borders(algeria,libya).
borders(algeria,mali).
borders(algeria,mauritania).
borders(algeria,morocco).
borders(algeria,niger).
borders(algeria,tunisia).
borders(algeria,mediterranean).

borders(angola,congo).
borders(angola,south_africa).
borders(angola,zaire).
borders(angola,zambia).
borders(angola,atlantic).

borders(botswana,south_africa).
borders(botswana,zimbabwe).

borders(burundi,rwanda).
borders(burundi,tanzania).
borders(burundi,zaire).

borders(cameroon,central_african_republic).
borders(cameroon,chad).
borders(cameroon,congo).
borders(cameroon,equatorial_guinea).
borders(cameroon,gabon).
borders(cameroon,nigeria).
borders(cameroon,atlantic).

borders(central_african_republic,cameroon).
borders(central_african_republic,chad).
borders(central_african_republic,congo).
borders(central_african_republic,sudan).
borders(central_african_republic,zaire).

borders(chad,cameroon).
borders(chad,central_african_republic).
borders(chad,libya).
borders(chad,niger).
borders(chad,nigeria).
borders(chad,sudan).

borders(congo,angola).
borders(congo,cameroon).
borders(congo,central_african_republic).
borders(congo,gabon).
borders(congo,zaire).
borders(congo,atlantic).

borders(dahomey,niger).
borders(dahomey,nigeria).
borders(dahomey,togo).
borders(dahomey,upper_volta).
borders(dahomey,atlantic).

borders(djibouti,ethiopia).
borders(djibouti,somalia).
borders(djibouti,indian_ocean).

borders(egypt,israel).
borders(egypt,libya).
borders(egypt,sudan).
borders(egypt,mediterranean).
borders(egypt,red_sea).

borders(equatorial_guinea,cameroon).
borders(equatorial_guinea,gabon).
borders(equatorial_guinea,atlantic).

borders(ethiopia,djibouti).
borders(ethiopia,kenya).
borders(ethiopia,somalia).
borders(ethiopia,sudan).
borders(ethiopia,red_sea).

borders(gabon,cameroon).
borders(gabon,congo).
borders(gabon,equatorial_guinea).
borders(gabon,atlantic).

borders(gambia,senegal).
borders(gambia,atlantic).

borders(ghana,ivory_coast).
borders(ghana,togo).
borders(ghana,upper_volta).
borders(ghana,atlantic).

borders(guinea,guinea_bissau).
borders(guinea,ivory_coast).
borders(guinea,liberia).
borders(guinea,mali).
borders(guinea,senegal).
borders(guinea,sierra_leone).
borders(guinea,atlantic).

borders(guinea_bissau,guinea).
borders(guinea_bissau,senegal).
borders(guinea_bissau,atlantic).

borders(ivory_coast,ghana).
borders(ivory_coast,guinea).
borders(ivory_coast,liberia).
borders(ivory_coast,mali).
borders(ivory_coast,upper_volta).
borders(ivory_coast,atlantic).

borders(kenya,ethiopia).
borders(kenya,somalia).
borders(kenya,sudan).
borders(kenya,tanzania).
borders(kenya,uganda).
borders(kenya,indian_ocean).

borders(lesotho,south_africa).

borders(liberia,ivory_coast).
borders(liberia,guinea).
borders(liberia,sierra_leone).
borders(liberia,atlantic).

borders(libya,algeria).
borders(libya,chad).
borders(libya,egypt).
borders(libya,niger).
borders(libya,sudan).
borders(libya,tunisia).
borders(libya,mediterranean).

borders(malagasy,indian_ocean).

borders(malawi,mozambique).
borders(malawi,tanzania).
borders(malawi,zambia).

borders(mali,algeria).
borders(mali,guinea).
borders(mali,ivory_coast).
borders(mali,mauritania).
borders(mali,niger).
borders(mali,senegal).
borders(mali,upper_volta).

borders(mauritania,algeria).
borders(mauritania,mali).
borders(mauritania,morocco).
borders(mauritania,senegal).
borders(mauritania,atlantic).

borders(mauritius,indian_ocean).

borders(morocco,algeria).
borders(morocco,mauritania).
borders(morocco,atlantic).
borders(morocco,mediterranean).

borders(mozambique,malawi).
borders(mozambique,south_africa).
borders(mozambique,swaziland).
borders(mozambique,tanzania).
borders(mozambique,zambia).
borders(mozambique,zimbabwe).
borders(mozambique,indian_ocean).

borders(niger,algeria).
borders(niger,chad).
borders(niger,dahomey).
borders(niger,libya).
borders(niger,mali).
borders(niger,nigeria).
borders(niger,upper_volta).

borders(nigeria,cameroon).
borders(nigeria,chad).
borders(nigeria,dahomey).
borders(nigeria,niger).
borders(nigeria,atlantic).

borders(rwanda,burundi).
borders(rwanda,tanzania).
borders(rwanda,uganda).
borders(rwanda,zaire).

borders(senegal,gambia).
borders(senegal,guinea).
borders(senegal,guinea_bissau).
borders(senegal,mali).
borders(senegal,mauritania).
borders(senegal,atlantic).

borders(seychelles,indian_ocean).

borders(sierra_leone,guinea).
borders(sierra_leone,liberia).
borders(sierra_leone,atlantic).

borders(somalia,djibouti).
borders(somalia,ethiopia).
borders(somalia,kenya).
borders(somalia,indian_ocean).

borders(south_africa,angola).
borders(south_africa,botswana).
borders(south_africa,lesotho).
borders(south_africa,mozambique).
borders(south_africa,swaziland).
borders(south_africa,zambia).
borders(south_africa,zimbabwe).
borders(south_africa,atlantic).
borders(south_africa,indian_ocean).

borders(sudan,chad).
borders(sudan,central_african_republic).
borders(sudan,egypt).
borders(sudan,ethiopia).
borders(sudan,kenya).
borders(sudan,libya).
borders(sudan,uganda).
borders(sudan,zaire).
borders(sudan,red_sea).

borders(swaziland,mozambique).
borders(swaziland,south_africa).

borders(tanzania,burundi).
borders(tanzania,kenya).
borders(tanzania,malawi).
borders(tanzania,mozambique).
borders(tanzania,rwanda).
borders(tanzania,uganda).
borders(tanzania,zaire).
borders(tanzania,zambia).
borders(tanzania,indian_ocean).

borders(togo,dahomey).
borders(togo,ghana).
borders(togo,upper_volta).
borders(togo,atlantic).

borders(tunisia,algeria).
borders(tunisia,libya).
borders(tunisia,mediterranean).

borders(uganda,kenya).
borders(uganda,rwanda).
borders(uganda,sudan).
borders(uganda,tanzania).
borders(uganda,zaire).

borders(upper_volta,dahomey).
borders(upper_volta,ghana).
borders(upper_volta,ivory_coast).
borders(upper_volta,mali).
borders(upper_volta,niger).
borders(upper_volta,togo).

borders(zaire,angola).
borders(zaire,burundi).
borders(zaire,central_african_republic).
borders(zaire,congo).
borders(zaire,rwanda).
borders(zaire,sudan).
borders(zaire,tanzania).
borders(zaire,uganda).
borders(zaire,zambia).
borders(zaire,atlantic).

borders(zambia,angola).
borders(zambia,malawi).
borders(zambia,mozambique).
borders(zambia,south_africa).
borders(zambia,tanzania).
borders(zambia,zaire).
borders(zambia,zimbabwe).

borders(zimbabwe,botswana).
borders(zimbabwe,mozambique).
borders(zimbabwe,south_africa).
borders(zimbabwe,zambia).


% Facts about America.
% -------------------

borders(argentina,bolivia).
borders(argentina,brazil).
borders(argentina,chile).
borders(argentina,paraguay).
borders(argentina,uruguay).
borders(argentina,atlantic).

borders(bahamas,atlantic).

borders(barbados,atlantic).

borders(belize,guatemala).
borders(belize,mexico).
borders(belize,atlantic).

borders(bolivia,argentina).
borders(bolivia,brazil).
borders(bolivia,chile).
borders(bolivia,paraguay).
borders(bolivia,peru).

borders(brazil,argentina).
borders(brazil,bolivia).
borders(brazil,colombia).
borders(brazil,french_guiana).
borders(brazil,guyana).
borders(brazil,paraguay).
borders(brazil,peru).
borders(brazil,surinam).
borders(brazil,uruguay).
borders(brazil,venezuela).
borders(brazil,atlantic).

borders(canada,united_states).
borders(canada,arctic_ocean).
borders(canada,atlantic).
borders(canada,pacific).

borders(chile,argentina).
borders(chile,bolivia).
borders(chile,peru).
borders(chile,pacific).

borders(colombia,brazil).
borders(colombia,ecuador).
borders(colombia,panama).
borders(colombia,peru).
borders(colombia,venezuela).
borders(colombia,atlantic).
borders(colombia,pacific).

borders(costa_rica,nicaragua).
borders(costa_rica,panama).
borders(costa_rica,atlantic).
borders(costa_rica,pacific).

borders(cuba,atlantic).

borders(dominican_republic,haiti).
borders(dominican_republic,atlantic).

borders(ecuador,colombia).
borders(ecuador,peru).
borders(ecuador,pacific).

borders(el_salvador,guatemala).
borders(el_salvador,honduras).
borders(el_salvador,pacific).

borders(french_guiana,brazil).
borders(french_guiana,surinam).

borders(greenland,arctic_ocean).
borders(greenland,atlantic).

borders(grenada,atlantic).

borders(guatemala,belize).
borders(guatemala,el_salvador).
borders(guatemala,honduras).
borders(guatemala,mexico).
borders(guatemala,atlantic).
borders(guatemala,pacific).

borders(guyana,brazil).
borders(guyana,surinam).
borders(guyana,venezuela).
borders(guyana,atlantic).

borders(haiti,dominican_republic).
borders(haiti,atlantic).

borders(honduras,el_salvador).
borders(honduras,guatemala).
borders(honduras,nicaragua).
borders(honduras,atlantic).
borders(honduras,pacific).

borders(jamaica,atlantic).

borders(mexico,belize).
borders(mexico,guatemala).
borders(mexico,united_states).
borders(mexico,atlantic).
borders(mexico,pacific).

borders(nicaragua,costa_rica).
borders(nicaragua,honduras).
borders(nicaragua,atlantic).
borders(nicaragua,pacific).

borders(panama,colombia).
borders(panama,costa_rica).
borders(panama,atlantic).
borders(panama,pacific).

borders(paraguay,argentina).
borders(paraguay,bolivia).
borders(paraguay,brazil).

borders(peru,bolivia).
borders(peru,brazil).
borders(peru,chile).
borders(peru,colombia).
borders(peru,ecuador).
borders(peru,pacific).

borders(surinam,brazil).
borders(surinam,french_guiana).
borders(surinam,guyana).

borders(trinidad_and_tobago,atlantic).

borders(united_states,canada).
borders(united_states,mexico).
borders(united_states,arctic_ocean).
borders(united_states,atlantic).
borders(united_states,pacific).

borders(uruguay,argentina).
borders(uruguay,brazil).
borders(uruguay,atlantic).

borders(venezuela,brazil).
borders(venezuela,colombia).
borders(venezuela,guyana).
borders(venezuela,atlantic).

% Facts about Australasia.
% -----------------------

borders(australia,indian_ocean).
borders(australia,pacific).

borders(fiji,pacific).

borders(new_zealand,pacific).

borders(papua_new_guinea,indonesia).
borders(papua_new_guinea,pacific).

borders(tonga,pacific).

borders(western_samoa,pacific).

borders(antarctica,southern_ocean).

% Facts about oceans and seas.
% ---------------------------

borders(arctic_ocean,atlantic).
borders(arctic_ocean,pacific).

borders(atlantic,arctic_ocean).
borders(atlantic,indian_ocean).
borders(atlantic,pacific).
borders(atlantic,southern_ocean).
borders(atlantic,baltic).
borders(atlantic,mediterranean).

borders(indian_ocean,atlantic).
borders(indian_ocean,pacific).
borders(indian_ocean,southern_ocean).
borders(indian_ocean,persian_gulf).
borders(indian_ocean,red_sea).

borders(pacific,arctic_ocean).
borders(pacific,atlantic).
borders(pacific,indian_ocean).
borders(pacific,southern_ocean).

borders(southern_ocean,atlantic).
borders(southern_ocean,indian_ocean).
borders(southern_ocean,pacific).

borders(baltic,atlantic).

borders(black_sea,mediterranean).

borders(mediterranean,atlantic).
borders(mediterranean,black_sea).

borders(persian_gulf,indian_ocean).

borders(red_sea,indian_ocean).

% Countries bordering each ocean and sea.
% --------------------------------------

borders(arctic_ocean,norway).
borders(arctic_ocean,soviet_union).
borders(arctic_ocean,canada).
borders(arctic_ocean,greenland).
borders(arctic_ocean,united_states).

borders(atlantic,belgium).
borders(atlantic,denmark).
borders(atlantic,eire).
borders(atlantic,france).
borders(atlantic,iceland).
borders(atlantic,netherlands).
borders(atlantic,norway).
borders(atlantic,portugal).
borders(atlantic,spain).
borders(atlantic,sweden).
borders(atlantic,west_germany).
borders(atlantic,united_kingdom).
borders(atlantic,angola).
borders(atlantic,cameroon).
borders(atlantic,congo).
borders(atlantic,dahomey).
borders(atlantic,equatorial_guinea).
borders(atlantic,gabon).
borders(atlantic,gambia).
borders(atlantic,ghana).
borders(atlantic,guinea).
borders(atlantic,guinea_bissau).
borders(atlantic,ivory_coast).
borders(atlantic,liberia).
borders(atlantic,mauritania).
borders(atlantic,morocco).
borders(atlantic,nigeria).
borders(atlantic,senegal).
borders(atlantic,sierra_leone).
borders(atlantic,south_africa).
borders(atlantic,togo).
borders(atlantic,zaire).
borders(atlantic,argentina).
borders(atlantic,bahamas).
borders(atlantic,barbados).
borders(atlantic,belize).
borders(atlantic,brazil).
borders(atlantic,canada).
borders(atlantic,colombia).
borders(atlantic,costa_rica).
borders(atlantic,cuba).
borders(atlantic,dominican_republic).
borders(atlantic,french_guiana).
borders(atlantic,greenland).
borders(atlantic,grenada).
borders(atlantic,guatemala).
borders(atlantic,guyana).
borders(atlantic,haiti).
borders(atlantic,honduras).
borders(atlantic,jamaica).
borders(atlantic,mexico).
borders(atlantic,nicaragua).
borders(atlantic,panama).
borders(atlantic,surinam).
borders(atlantic,trinidad_and_tobago).
borders(atlantic,united_states).
borders(atlantic,uruguay).
borders(atlantic,venezuela).

borders(indian_ocean,bangladesh).
borders(indian_ocean,burma).
borders(indian_ocean,india).
borders(indian_ocean,indonesia).
borders(indian_ocean,iran).
borders(indian_ocean,malaysia).
borders(indian_ocean,maldives).
borders(indian_ocean,oman).
borders(indian_ocean,pakistan).
borders(indian_ocean,south_yemen).
borders(indian_ocean,sri_lanka).
borders(indian_ocean,thailand).
borders(indian_ocean,djibouti).
borders(indian_ocean,kenya).
borders(indian_ocean,malagasy).
borders(indian_ocean,mauritius).
borders(indian_ocean,mozambique).
borders(indian_ocean,seychelles).
borders(indian_ocean,somalia).
borders(indian_ocean,south_africa).
borders(indian_ocean,tanzania).
borders(indian_ocean,australia).

borders(pacific,cambodia).
borders(pacific,china).
borders(pacific,indonesia).
borders(pacific,japan).
borders(pacific,malaysia).
borders(pacific,north_korea).
borders(pacific,philippines).
borders(pacific,singapore).
borders(pacific,south_korea).
borders(pacific,soviet_union).
borders(pacific,taiwan).
borders(pacific,thailand).
borders(pacific,vietnam).
borders(pacific,canada).
borders(pacific,chile).
borders(pacific,colombia).
borders(pacific,costa_rica).
borders(pacific,ecuador).
borders(pacific,el_salvador).
borders(pacific,guatemala).
borders(pacific,honduras).
borders(pacific,mexico).
borders(pacific,nicaragua).
borders(pacific,panama).
borders(pacific,peru).
borders(pacific,united_states).
borders(pacific,australia).
borders(pacific,fiji).
borders(pacific,new_zealand).
borders(pacific,papua_new_guinea).
borders(pacific,tonga).
borders(pacific,western_samoa).

borders(southern_ocean,antarctica).

borders(baltic,denmark).
borders(baltic,finland).
borders(baltic,east_germany).
borders(baltic,poland).
borders(baltic,sweden).
borders(baltic,west_germany).
borders(baltic,soviet_union).

borders(black_sea,bulgaria).
borders(black_sea,romania).
borders(black_sea,soviet_union).
borders(black_sea,turkey).

borders(caspian,iran).
borders(caspian,soviet_union).

borders(mediterranean,albania).
borders(mediterranean,cyprus).
borders(mediterranean,france).
borders(mediterranean,greece).
borders(mediterranean,italy).
borders(mediterranean,malta).
borders(mediterranean,monaco).
borders(mediterranean,san_marino).
borders(mediterranean,spain).
borders(mediterranean,yugoslavia).
borders(mediterranean,israel).
borders(mediterranean,lebanon).
borders(mediterranean,syria).
borders(mediterranean,turkey).
borders(mediterranean,algeria).
borders(mediterranean,egypt).
borders(mediterranean,libya).
borders(mediterranean,morocco).
borders(mediterranean,tunisia).

borders(persian_gulf,bahrain).
borders(persian_gulf,iran).
borders(persian_gulf,iraq).
borders(persian_gulf,kuwait).
borders(persian_gulf,qatar).
borders(persian_gulf,saudi_arabia).
borders(persian_gulf,united_arab_emirates).

borders(red_sea,israel).
borders(red_sea,jordan).
borders(red_sea,saudi_arabia).
borders(red_sea,yemen).
borders(red_sea,egypt).
borders(red_sea,ethiopia).
borders(red_sea,sudan).
