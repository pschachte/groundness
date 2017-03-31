% CVS: $Id: lnprolog.pl,v 1.2 1998/10/19 04:21:39 pets Exp $
% LNPROLOG (Version A: written in standard Prolog)
%-------------------------------------------------------------------------
% This file contains the version of LnProlog written in standard Prolog
% without its debugger (The original version is written in Arity-Prolog
% and is stored in the file \chap8\lnprolog.ari; LnProlog with a tracer
% and a debugger is stored in the file \chap8\lnprolg2.ari and its
% standard version is in \chap8\lnprolg2.std).
% Note: the builtin predicate "system" used in this system (to test if
% a predicate is a builtin predicate) may correspond to "sys" (or something
% else) in your Prolog system. In which case, change this by using the
% following command:
%     ?- assert((system(F/N) :- sys(F))).
% Also, the predicate "cls" is used in this system to clear the screen.
% If your Prolog system provides a different builtin predicate, say
% "clscr", for the same purpose, then enter the following command:
%     ?- assert((cls :- clscr)).
% In case, your Prolog system has no such predicate, then define it using
% the following clauses (simply remove the symbols %):
%      cls :- cursor(&:,0,0),clear(23,80).
%
%      clear(0,0) :- !,cursor(&:,0,0).
%      clear(M,0) :- !,nl,M1 is M-1,clear(M1,80).
%      clear(M,N) :- put(32),N1 is N-1,clear(M,N1).
%
% where, the goal "cursor(&:,0,0)" moves the cursor to row 0 and column 0
% (find an equivalent predicate in your Prolog system). If your Prolog
% system has no such equivalent predicate, then remove all goals "cls"
% from this system before consulting it.
%   Another note: if your Prolog system already has a builtin predicate
% "all", then change the predicate name "all" in this system to "allx",
% say, before consulting this system.
%
% To run this system, perform the following steps:
%  a) Consult this file.
%  b) Enter the query ?- lnprolog. (a prompt ?: will appear)
%  c) From then on, you will be working with LnProlog. To start, bring in
%     a sample program. For example, suppose that you already copied the
%     file '\chap8\faulty.pro' from the diskette into your directory (this
%     program is on page 369), then enter the query:
%       ?: consult('c:\chap8\faulty.pro').
%     Now, you can enter positive, negative or quantified queries such
%     as the following (see pages 361, 370, 371):
%
%     ?: fault(a).                   % is node a faulty ?
%     ?: fault(b).                   % is node b faulty ?
%     ?: fault(X).                   % which node is faulty ?
%     ?: non(fault(X)).              % which node is not faulty ?
%     ?: all(fault(X)).              % are all nodes faulty ?
%     ?: all(non(fault(X))).         % are all nodes not faulty ?
%     ?: all([X],respond(X,_)).      % which node responds to all nodes?
%     ?: all([X],non(respond(X,_))). % which node dn't rspnd to any node?
%
%     All other usual commands such as 'listing', 'abolish', 'assert', etc.
%     have the same effect as in standard Prolog. We note that this version
%     is slower than Version 1 (in the file '\chap8\lnprolog.ari') due to
%     the replacement of some builtin string-handling predicates with their
%     explicit definitions .
%
%  d) To leave the system, use the query ?: halt. as usual.
%     Note that to run another program with LnProlog (version A), we must
%     remove the above program and reset the system by using the commands:
%         ?: abolish(fault/1).
%         ?: abolish(respond/2).
%         ?: reset.
%     before entering a new program.
%
%  e) You can also enter a program directly to LnProlog, by typing the rules
%     at the prompt ?:. Note, however, that any fact must be entered as	a
%     rule with a 'true' body to be distinguished from a query.
%     For example, enter the following rules:
%       ?: even(0) :- true.
%       ?: even(s(s(X))) :- even(X).
%       ?: odd(X) :- non(even(X)).
%     Then try the following queries, using ; to obtain alternative
%     answers:
%       ?: odd(s(0)).
%       ?: odd(X).
%
%============================ LNPROLOG ==============================
%================= A PROLOG SYSTEM THAT SUPPORTS ====================
%============ LOGICAL NEGATION AND QUANTIFIED QUERIES ===============
%====================================================================
%
% TOP LEVEL CONTROL
% ===================================================================
lnprolog :-
    introduction,
    repeat,
        receive_query(G,VL),
        process_query(G,VL),
    fail.

%initialize :-
%    introduction,
%    create_world(objworld),
%    code_world(_,objworld).

introduction :-
    nl,
    nl,write('======================LNPROLOG======================'),
    nl,write('==== is standard Prolog plus a logical negator. ===='),
    nl,write('====In LNPROLOG, the negating predicate is "non"===='),
    nl,write('==== and the success of non(A) provides values  ===='),
    nl,write('==== for the variables of A so that A is false. ===='),
    nl,write('====================================================').

receive_query(G,VL) :-
    nl,nl,write('?:'),write(' '),
    readline(L),
    convert_query(L,G,VL).

% read_query(S0,S) :-
%    read_string(100,S1),concat(S0,S1,S2),nl,
%    (end_query(S1),!,S = S2; read_query(S2,S)).

% end_query(S) :-
%    string_length(S,K),N is K-1,nth_char(N,S,46).

convert_query(L,G,VL) :-
    tokenize(L,TL),
    (member((':-'),TL),rule(G,[],VL,TL,[]),!;
                  conj_term(G,[],VL,TL,[]),!).
convert_query(_,_,_) :- nl,write('syntax error'),!,fail.

% -------------------------------------------------------------------

% PROCESS QUERY
% ===================================================================
process_query((A :- B),VL) :- !,process_clause((A :- B)).
process_query(G,VL) :- G =.. [P|Args],
    (member(P,[consult,reconsult]),!,process_file(Args);
     member(P,[assert,asserta,assertz]),!,process_assert(Args);
     true),
    process_goal(G,VL).

process_clause(Clause) :-
    extract_symbols(Clause),
    assert(Clause),
    nl,write(stored).

process_file([Filename]) :-
    see(Filename),
    readfile,
    seen.

readfile :-
    repeat,
        read(Clause),
        (Clause = end_of_file,!;
        extract_symbols(Clause),fail).

process_assert([Clause]) :-
    extract_symbols(Clause).

process_goal(Goal,VL) :-
    reverse(VL,[],VL1),
    evaluate(Goal,VL1),
    remove(answer(X)).

% -------------------------------------------------------------------

% EVALUATE A GOAL
% ===================================================================
evaluate(Goal,VL) :-
    prove(Goal,_,_),process_result(VL),!.
evaluate(_,_) :- nl,write('no').

prove(Goal,_,_) :- call(Goal).

process_result([]) :- !,nl,write('yes').
process_result(VL) :- duplicated_answer(VL),!,fail.
process_result(VL) :- print_answer(VL,VL1),
    assert(answer(VL1)), process_response.

print_answer([('_',V)|L],[('_',_)|L1]) :- !,
    print_answer(L,L1).
print_answer([(X,V)|L],[(X,V)|L1]) :-
    nl,write(X),write(' = '),write(V),
    print_answer(L,L1).
print_answer([],[]).

process_response :- write(' ->'),
    (get0(59),!,fail;nl,write('yes')).

duplicated_answer(A) :-
    answer(B),instanc(A,B).

instanc(A,B) :-
    var_list(A,L),bind_var(L,1),A = B.

bind_var([],_).
bind_var(['$@#'(N)|Vs],N) :- N1 is N+1,bind_var(Vs,N1).

remove(A) :- retract(A),fail.
remove(_) :- assert(answer('##')).

answer('##').
constsymbol('#','#').
functsymbol('#').

clear_objbase :-
     delete_world(objworld),
     create_world(objworld),
     code_world(_,objworld),!.

% -------------------------------------------------------------------

% EXTRACT CONSTANT AND FUNCTION SYMBOLS
% ===================================================================
extract_symbols((A :- B)) :- !,extract_symbols((A,B)).
extract_symbols((A;B)) :- !,extract_symbols((A,B)).
extract_symbols((A,B)) :- !,extract_symbols(A),
                            extract_symbols(B).
extract_symbols(true) :- !.
extract_symbols(not(A)) :- !,extract_symbols(A).
extract_symbols(non(A)) :- !,extract_symbols(A).
extract_symbols(A) :- A =.. [P|Args],!,extr_symb(Args).
extract_symbols(_).

extr_symb([]) :- !.
extr_symb([A|As]) :- extracts(A),extr_symb(As).

extracts(A) :- var(A),!.
extracts(A) :- constant(A),!,
    (constsymbol(A),!;assert(constsymbol(A))).
extracts(A) :- A =.. [F|Args],functor(A,F,N),
    (functsymbol(F,N),!; assert(functsymbol(F,N))),
    extr_symb(Args).

reset :-
    abolish(constsymbol/1),
    abolish(functsymbol/2).

%--------------------------------------------------------------------

% LNPROLOG'S LOGICAL NEGATION & QUATIFICATION
% ===================================================================

all(L,G) :- copy((L,G),(L1,G1)), L = L1,
            G1,not(non(G)).
all(G)  :-  G \== (A,B), not(non(G)).

non(non(A)) :- !,A.

non(A) :- var_list(A,L),eval_non(A,L).

eval_non(A,L) :- not(A),!.
eval_non(A,L) :- eval(A),uninstantiated(L),!,fail.
eval_non(A,L) :- instantiate(L),re_var_list(L,VL),eval_non(A,VL).

eval(A) :- A,!.
uninstantiated(L) :- var_pure(L),unrestricted(L,0).

var_pure([]) :- !.
var_pure([X|Xs]) :- var(X),var_pure(Xs).

unrestricted([],_) :- !.
unrestricted([N|Xs],N) :- N1 is N+1,unrestricted(Xs,N1).

instantiate([X|Xs]) :- get_term(X,Xs).
instantiate([X|Xs]) :- instantiate(Xs).

get_term(X,Xs) :- constsymbol(X).
get_term(X,Xs) :- functsymbol(F,N),functor(X,F,N).
get_term(X,Xs) :- get_var(X,Xs).

get_var(X,[X|Xs]).
get_var(X,[Y|Xs]) :- get_var(X,Xs).

re_var_list(A,L) :- var_list(A,L1),shift_var(L1,L).

shift_var([],[]) :- !.
shift_var([X|L],L1) :- append(L,[X],L1).

% -------------------------------------------------------------------

% READ AN INPUT RULE OR QUERY
%====================================================================
    readline(L) :-
        get0(C),
        read_chars(C,[],L1),
        reverse(L1,[],L).

    read_chars(13,[46|L],L) :- !,nl.
    read_chars(13,L,L1) :- !,
        nl,get0(C),
        read_chars(C,[32|L],L1).
    read_chars(8,[C|L],L1) :- !,
        put(32),put(8),
        get0(C1),
        read_chars(C1,L,L1).
    read_chars(C,L,L1) :-
        get0(C1),
        read_chars(C1,[C|L],L1).

    reverse([],L,L).
    reverse([H|T],L,L1) :-
        reverse(T,[H|L],L1).

%----------------------------------------------------------------
%
% OBTAIN TOKENS FROM INPUT TEXT
%=================================================================
    tokenize([],[]).
    tokenize(CharList,[Token|TList]) :-
        append(_,[C|List],CharList), C \== 32,!,
        get_token([C|List],Token,Rest),
        tokenize(Rest,TList).

    get_token(List,Token,Rest) :-
        get_chars(List,Lchars,Rest),
        name(Token,Lchars).

    get_chars(L,S,L1) :- separator(L,S,L1),!.
    get_chars([C|L],[C|Lc],L1) :-
        check_start(S,C),
        get_word_chars(S,L,Lc,L1).

    get_word_chars(S,L,Lc,L1) :-
        check_end(S,L,Lc,L1).
    get_word_chars(S,[C|L],[C|Lc],L1) :-
        legal_char(S,C),
        get_word_chars(S,L,Lc,L1).

    legal_char(quote,C) :- C \== 39.
    legal_char(num,C)   :- digit(C).
    legal_char(symb,C)  :- valid_char(C).

    check_start(quote,39).
    check_start(num, C)  :- digit(C).
    check_start(symb,C)  :-
        valid_char(C), not(digit(C)).

    check_end(_,[],[],[]) :- !.
    check_end(quote,[39|L],[39],L) :- !.
    check_end(num, [C|L],[],[C|L]) :- not(digit(C)),!.
    check_end(symb,[C|L],[],[C|L]) :- not(valid_char(C)).

  separator([C,D,E|L],[C,D,E],L) :- name(S,[C,D,E]),
    member(S,['=:=','=\=','\==','@=<','@>=','=..','-->']),!.
  separator([C,D|L],[C,D],L) :- name(S,[C,D]),
    member(S,[(':-'),'\+','->','\=','==','@<','@>','=<','>=',
               '//']),!.
  separator([C|L],[C],L) :-
    member(C,[44,40,41,58,91,93,124,43,45,42,47,59,61,60,62,94]).

    valid_char(C) :- letter(C); digit(C); C = 95.
    letter(C) :-  97 =< C, C =< 122; 65 =< C, C =< 90.
    digit(C)  :-  48 =< C, C =< 57.

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

%--------------------------------------------------------------------
%
% DCG DEFINITION OF PROLOG TERMS AND RULES
%====================================================================
    term(T,VL,VL1) --> formal_term(T,VL,VL1).
    term(T,VL,VL1) --> infix_term(T,VL,VL1).

    formal_term(T,VL,VL1) --> variable(T,VL,VL1).
    formal_term(T,VL,VL1) --> constant(T,VL,VL1).
    formal_term(T,VL,VL1) --> list(T,VL,VL1).
    formal_term(T,VL,VL1) --> prefix_term(T,VL,VL1).

    variable(V,VL,VL1) -->
        [X],{variable_name(X,V,VL,VL1)}.
    constant(T,VL,VL) -->
        [X],{(constsymb(X,T);numbr(X,T))}.

    list([],VL,VL) --> ['[',']'].
    list([T|L],VL,VL1) --> ['['],
        term(T,VL,VL2),tail(L,VL2,VL1).

    tail([],VL,VL) --> [']'].
    tail([T|L],VL,VL1) -->
        [','],term(T,VL,VL2),tail(L,VL2,VL1).
    tail(L,VL,VL1) -->
        ['|'],variable(L,VL,VL1),[']'].
    tail(L,VL,VL1) -->
        ['|'],list(L,VL,VL1),[']'].

    prefix_term(T,VL,VL1) -->
        functer(F),['('],
        arguments(Args,VL,VL1),[')'],
        {T =.. [F|Args]}.

    functer(X) -->  [X],{functsymb(X)}.

    arguments([Arg],VL,VL1) -->
        term(Arg,VL,VL1).
    arguments([A|Args],VL,VL1) -->
        term(A,VL,VL2),[','],
        arguments(Args,VL2,VL1).

    infix_term(T,VL,VL1) --> rightas_term(T,VL,VL1).
    infix_term(T,VL,VL1) --> bracket_term(T,VL,VL1).

    rightas_term(T,VL,VL1) -->
        formal_term(T,VL,VL1).
    rightas_term(T,VL,VL1) -->
        formal_term(A,VL,VL2),[F],{operator(F)},
        rightas_term(B,VL2,VL1),
        {T =.. [F,A,B]}.

    bracket_term(T,VL,VL1) -->
        ['('],rightas_term(T,VL,VL1),[')'].
    bracket_term(T,VL,VL1) -->
        ['('],rightas_term(A,VL,VL2),[')',F],{operator(F)},
        rightas_term(B,VL2,VL1),
        {T =.. [F,A,B]}.
    bracket_term(T,VL,VL1) -->
        ['('],rightas_term(A,VL,VL2),[')',F],{operator(F)},
        bracket_term(B,VL2,VL1),
        {T =.. [F,A,B]}.

    variable_name(X,V,VL,VL1) :-
        name(X,[C|L]),
        ((capital(C); C = 95, L \== []),
            (member((X,V),VL),!,VL1 = VL;
                       VL1 = [(X,V)|VL]);
         C = 95,L = [],VL1 = [(X,V)|VL]).

    constsymb(X,X) :- atom_name(X).
    constsymb(X,T) :-	char_string(X,T).

    functsymb(X) :- atom_name(X); system(X/N);
        member(X,[abs,exp,ln,log,sqrt,acos,asin,
                  atan,cos,sin,tan]).

    atom_name(X) :- name(X,[C|L]),97 =< C, C =< 122.
    char_string(X,T) :- name(X,[C|L]),
        C = 39, string(L,R), name(T,R).

    capital(C) :- 65 =< C, C =< 90.

    string([39],[]).
    string([H|T],[H|R]) :- string(T,R).

    numbr(X,T) :- name(X,[C|L]),
        (48 =< C, C =< 57),chars_int([C|L],0,T).

    chars_int([],N,N).
    chars_int([C|L],M,N) :-
        M1 is 10*M + C - 48,chars_int(L,M1,N).

    operator(F) :-
    member(F,[is,':','+','-','*','/','=','<','>','^',mod]);
    member(F,['->','\=','==','@<','@>','=<','>=','//']);
    member(F,['=:=','=\=','\==','@=<','@>=','=..','-->']).

    rule((A :- B),VL,VL1) -->
        head_term(A,VL,VL2),[(':-')],
        conj_term(B,VL2,VL1).

    head_term(T,VL,VL1) --> constant(T,VL,VL1).
    head_term(T,VL,VL1) --> prefix_term(T,VL,VL1).

    conj_term(T,VL,VL1) --> term(T,VL,VL1).
    conj_term((A,B),VL,VL1) --> term(A,VL,VL2),[','],
        conj_term(B,VL2,VL1).

%-------------------------------------------------------------------
%
% COLLECT THE VARIABLES OF A FORMULA
% ===================================================================
    var_list(A,[]) :- ground(A),!.
    var_list(A,L) :-  collect_var(A,Q-[]),elim_dup(Q,[],L).

    collect_var(A,Q-Q) :- constant(A),!.
    collect_var(A,[A|Q]-Q) :- var(A), !.
    collect_var(A,Q) :- A =.. [P|Args],collect_vars(Args,Q).

    collect_vars([],Q-Q) :- !.
    collect_vars([A|As],Q-Qt) :-
        collect_var(A,Q-Qs),collect_vars(As,Qs-Qt).

    constant(A) :- atom(A); integer(A); float(A).

    ground(A) :- copy(A,B), A == B.
    copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

%    append([],L,L).
%    append([H|T],L,[H|T1]) :- append(T,L,T1).

    elim_dup([],_,[]).
    elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
    elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).

    occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).

%--------------------------------------------------------------------
% ========================== END LNPROLOG ===========================











