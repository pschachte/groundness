% CVS: $Id: dbqas.pl,v 1.3 1998/10/21 04:25:52 pets Exp $
% FIGURE 10.11: A query-answering system
%====================================================================
% This file contains a query-answering system and a portion of a
% supplier-part database at the end. Consult this file and enter
% the following query to invoke the system:
%    ?- dbqas.
% When the prompt Q:> occurs, enter the following queries to perform
% experiments on the database system. For each query, press <Return>
% to obtain alternative answers; press 'e' to terminate the current
% query.
%    Q:> which agents supply item b1?
%    Q:> which agents do not supply item b2?
%    Q:> which agents supply all items?
%    Q:> which agents supply most items?
%    Q:> which agents supply few items?
%    Q:> which agents do not supply most items?
%    Q:> all agents supply item b1?
%    Q:> most agents supply most items?
%    Q:> few agents supply few items?
%
% Now enter the following queries to change the database. Press e
% after the command has been executed.
%    Q:> delete agent 'Rossianno Co' supplies no items.
%    Q:> add agent 'Rossianno Co' supplies all items.
%    Q:> add agent 'Extra Co'.
%    Q:> add agent 'Extra Co' supplies all items.
%
% Then enter the following queries again and observe the answers:
%    Q:> most agents supply most items?
%    Q:> few agents supply few items?
%
% Next change the database again by entering the query:
%    Q:> add all agents supply item b1.
%
% Then try the following query and observe the answer:
%    Q:> which agents supply item b1?
%
% Finally, enter the following queries to add a general knowledge
% to the database and test it:
%    Q:> add all agents supply all items.
%    Q:> which agents supply all items?
%
% To exit the system, enter the query Q:> e.
%=====================================================================
:- op(500,xfy,:).

goal :- dbqas.

    dbqas :-
        repeat,
            receive_query(Q),
            process_query(Q,R),
            print_answer(R),
        terminate(Q).

    process_query(Q,R) :-
        once(sentence(S,Q,[])),
        translate(S,T,A),
        call(T),response(A,R).
    process_query([e],[e]) :- !.
    process_query(_,[no]) :-
        not(answer([yes])).
    process_query(_,_) :-
        abolish(answer/1),fail.

    response(confirm,[yes]) :- !,
        assert(answer([yes])).
    response(A,R) :-
        transback(A,B),
        once(sentence(B,R,[])),
        not(answer(R)),assert(answer(R)).

    terminate(Q) :- Q = [e]; (get0(101),nl,dbqas).
    once(P) :- P,!.

    answer('#','#').

% Read a query sentence into a list of words
%--------------------------------------------------------------------
    receive_query(S) :-
        nl,write('Q:> '),
        read_sentence(S),nl.

    read_sentence(L) :- read_sent(32,L).

    read_sent(C,L) :- end_sent(C),!,L = [].
    read_sent(C,[W|L]) :-
        read_word(W,C1),read_sent(C1,L).

    read_word(W,C1) :-
        read_first_char(S,C),
        read_word_chars(S,C,[],L,C1),
        reverse(L,[],L1),name(W,L1).

    read_first_char(S,C) :-
        get_char(S),start_word(S,C),!;
        read_first_char(S,C).

    read_word_chars(S,C,L,L,C1) :- end_word(S,C,C1),!.
    read_word_chars(S,8,[C|L],L1,C1) :- !,
        put(32),put(8),
        get_char(C2),
        read_word_chars(S,C2,L,L1,C1).
    read_word_chars(S,C,L,L1,C1) :-
        legal_char(S,C),
        get_char(C2),
        read_word_chars(S,C2,[C|L],L1,C1).

    get_char(C) :- get0(C),(C = 13,!,nl; true).
    valid_char(C) :-
        96 < C, C < 123;  % a-z
        64 < C, C < 91;   % A-Z
        47 < C, C < 58;   % 0-9
        C = 45.        % hyphen.

    start_word(C,C)   :- valid_char(C),!.
    start_word(39,C)  :- get_char(C).
    end_word(39,39,C) :- get_char(C).  % quotation marks
    end_word(Q,C,C)   :- Q \== 39, C \== 8,not(valid_char(C)).
    end_sent(C)       :- (C = 46; C = 63). % period or ?

    legal_char(39,_) :- !.
    legal_char(_,C)  :- valid_char(C).

    reverse([],L,L).
    reverse([H|T],L,L1) :- reverse(T,[H|L],L1).


% Print out the answer
%--------------------------------------------------------------------
    print_answer([e]) :- !.
    print_answer(R) :-
        write('A:> '),write_list(R).

    write_list([]) :- nl.
    write_list([X|T]) :-
        write(X),write(' '), write_list(T).


% FIGURE 10.5: A definite clause grammar parser
%---------------------------------------------------------------------
    sentence(S) -->
        noun_phrase(P,S,E,VP),verb_phrase(P,E,VP).

    noun_phrase(P,NP,E,F) -->
        determiner(P,NP,E,F),noun(P,E).
    noun_phrase(P,F,E,F) -->
        noun(P,E),proper_noun(P,E).

    verb_phrase(P,X,VP) -->
        verb(P,F,X,Y),noun_phrase(_,VP,Y,F).
    verb_phrase(P,X,VP) -->
        neg_verb(P,F,X,Y),noun_phrase(_,VP,Y,F).

    determiner(P,all(E,F),E,F) -->
        [W],{once(member(W:P,
        [all:plural,every:sgular,each:sgular]))}.
    determiner(P,exist(E,F),E,F) -->
        [W],{member(W,[some,which])}.
    determiner(P,no(E,F),E,F) --> [no].
    determiner(plural,A,E,F) -->
        [W],{member(W,[most,few]), A =.. [W,E,F]}.

    noun(sgular,T:X) -->
        [T],{member(T,[agent,item])}.
    noun(plural,agent:X) --> [agents].
    noun(plural,item:X) -->  [items].

    proper_noun(sgular,T:X) -->
        [X],{proper_name(T:X)}.

    verb(sgular,supply(X,Y),X,Y) --> [supplies].
    verb(plural,supply(X,Y),X,Y) --> [supply].
    neg_verb(P,non(F),X,Y) -->
        negation(P),verb(plural,F,X,Y).

    negation(sgular) --> [does,not].
    negation(plural) --> [do,not].

    proper_name(N) :- constsymbol(N).


% Extension of the DCG to parse database manager commands
%-----------------------------------------------------------------
    sentence(S) -->
        command(S,F),fact(F).

    command(add(S),S) --> [add].
    command(append(S),S) --> [append].
    command(delete(S),S) --> [delete].

    fact(constsymbol(T:X)) --> noun(_,T:X),[X].
    fact(F) --> sentence(F).


% FIGURE 10.6: Definition of 'most' and 'few' predicates
%--------------------------------------------------------------------
    most(X,Y,A) :-
        complement(X,Y,A,L1,L2),
        large(L1,L2).

    few(X,Y,A) :-
        complement(X,Y,A,L1,L2),
        large(L2,L1).

    complement(X,Y,A,L1,L2) :-
        (setof(X,Y^A,L1),!; L1 = []),
        (setof(X,(constsymbol(X),
         not(member(X,L1))),L2),!; L2 = []).

    large(L1,L2) :-
        length(L1,N1),length(L2,N2),
        N1 >= 5*N2.


% FIGURE 10.7-10.8: Prolog representation of logical formulas
%--------------------------------------------------------------------
    translate(exist(X,exist(Y,A)),A,(X,Y,A)) :- !.
    translate(exist(X,all(Y,A)),(A,not(non(A1))),(X,all(Y1,A1))) :- !,
        copy((X,Y,A),(X1,Y1,A1)),X = X1.
    translate(exist(X,no(Y,A)),(non(A),not(A1)),(X,no(Y1,A1))) :- !,
        copy((X,Y,A),(X1,Y1,A1)),X = X1.
    translate(exist(X,most(Y,A)),(A,most(Y1,0,A1)),(X,most(Y1,A1))) :- !,
        copy((X,Y,A),(X1,Y1,A1)),X = X1.
    translate(exist(X,few(Y,A)),(non(A),few(Y1,0,A1)),(X,few(Y1,A1))) :- !,
        copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(exist(X,A),A,(X,A)) :- !.

    translate(all(X,exist(Y,A)),not((non(A),not(A1))),_) :- !,
        copy((X,A),(X1,A1)), X = X1.
    translate(all(X,all(Y,A)),not(non(A)),_) :- !.
    translate(all(X,no(Y,A)),not(A),_) :- !.
    translate(all(X,most(Y,A)),not((non(A),not(most(Y1,0,A1)))),_)
        :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(all(X,few(Y,A)),not((A,not(few(Y1,0,A1)))),_) :- !,
        copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(all(X,A),not(non(A)),_) :- !.

    translate(no(X,exist(Y,A)),not(A),_) :- !.
    translate(no(X,all(Y,A)),not((A,not(non(A1)))),_) :- !,
        copy((X,A),(X1,A1)), X = X1.
    translate(no(X,no(Y,A)),not((non(A),not(A1))),_) :- !,
        copy((X,A),(X1,A1)), X = X1.
    translate(no(X,most(Y,A)),not((A,most(Y1,0,A1))),_) :- !,
        copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(no(X,few(Y,A)),not((non(A),few(Y1,0,A1))),_) :- !,
        copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(no(X,A),not(A),_) :- !.

    translate(most(X,exist(Y,A)),most(X,Y,A),_) :- !.
    translate(most(X,all(Y,A)),few(X,Y,non(A)),_) :- !.
    translate(most(X,no(Y,A)),few(X,Y,A),_) :- !.
    translate(most(X,most(Y,A)),most(X,Y,(A,most(Y1,0,A1))),_)
        :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(most(X,few(Y,A)),most(X,Y,(non(A),few(Y1,0,A1))),_)
        :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(most(X,A),most(X,0,A),_) :- !.

    translate(few(X,exist(Y,A)),few(X,Y,A),_) :- !.
    translate(few(X,all(Y,A)),most(X,Y,non(A)),_) :- !.
    translate(few(X,no(Y,A)),most(X,Y,A),_) :- !.
    translate(few(X,most(Y,A)),few(X,Y,(A,most(Y1,0,A1))),_)
        :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(few(X,few(Y,A)),few(X,Y,(non(A),few(Y1,0,A1))),_)
        :- !, copy((X,Y,A),(X1,Y1,A1)), X = X1.
    translate(few(X,A),few(X,0,A),_) :- !.


% Extension of the translator to handle manager commands
%------------------------------------------------------------------
    translate(add(A),asserta(B),_)    :- !,trans(A,B).
    translate(append(A),assertz(B),_) :- !,trans(A,B).
    translate(delete(A),retract(B),_) :- !,trans(A,B).

    translate(A,A,_).

    transback((T:X,A),all(T:X,B)) :- var(X),!,transback(A,B).
    transback((_,A),B) :- !,transback(A,B).
    transback(A,A).

    trans(all(X,A),B) :- !,trans(A,B).
    trans(no(X,A),non(B)) :- !,trans(A,B).
    trans(A,A).


% FIGURE 10.15: Portion of a supplier-part database
%--------------------------------------------------------------------

    supply(agent:'Adams & Sons',item:b1).
    supply(agent:'Adams & Sons',item:b2).
    supply(agent:'Adams & Sons',item:b3).
    supply(agent:'Adams & Sons',item:b4).
    supply(agent:'Adams & Sons',item:b5).
    supply(agent:'Adams & Sons',item:b6).
    supply(agent:'Johnny Ltd',item:b1).
    supply(agent:'Johnny Ltd',item:b2).
    supply(agent:'Johnny Ltd',item:b3).
    supply(agent:'Johnny Ltd',item:b4).
    supply(agent:'Johnny Ltd',item:b5).
    supply(agent:'Mitre 10',item:b1).
    supply(agent:'Yoshima Kuru',item:X).
    non(supply(agent:'Rossianno Co',item:X)).

    constsymbol(agent:'Adams & Sons').
    constsymbol(agent:'Johnny Ltd').
    constsymbol(agent:'Mitre 10').
    constsymbol(agent:'Rossianno Co').
    constsymbol(agent:'Yoshima Kuru').
    constsymbol(item:b1).
    constsymbol(item:b2).
    constsymbol(item:b3).
    constsymbol(item:b4).
    constsymbol(item:b5).
    constsymbol(item:b6).


% FIGURE 10.9-10.10: The logical negation evaluator
%--------------------------------------------------------------------
    non(non(A)) :- !,A.
    non(A) :- tvar_list(A,L),eval_non(A,L).

    eval_non(A,L) :- not(A),!.
    eval_non(A,L) :- eval(A),uninstantiated(L),!,fail.
    eval_non(A,L) :- instantiate(A,L,VL),eval_non(A,VL).

    eval(A) :- A,!.
    uninstantiated(L) :- tvar_pure(L),unrestricted(L,0).

    tvar_pure([]) :- !.
    tvar_pure([T:V|TVs]) :- var(V),tvar_pure(TVs).

    unrestricted([],_) :- !.
    unrestricted([T:N|TVs],N) :-
        N1 is N+1,unrestricted(TVs,N1).

    instantiate(A,L,VL) :- domain(A),instant(L,VL).

    instant([X|Xs],Xs) :- get_term(X,Xs).
    instant([X|Xs],[X|VL]) :- instant(Xs,VL).

    get_term(T:V,TVs) :- constsymbol(T:V).
    get_term(X,Xs) :- get_var(X,Xs).

    get_var(T:V,[T:V|TVs]).
    get_var(X,[Y|Xs]) :- get_var(X,Xs).

    tvar_list(A,[]) :- ground(A),!.
    tvar_list(A,L) :-  A =.. [P|Args],
        setof(T:X,(member(T:X,Args),var(X)),L).

    domain(supply(S:X,T:Y)) :-
        type(S,agent),type(T,item).

    type(T,T).
    type(S,T) :- subtype(S,T).

    subtype('#').

    ground(A) :- copy(A,B), A == B.
    copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

%====================================================================



