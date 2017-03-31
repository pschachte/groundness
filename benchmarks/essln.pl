% CVS: $Id: essln.pl,v 1.3 1998/11/26 05:18:40 pets Exp $
% ESSLN - Version 2, written in standard Prolog
%======================================================================
% This file contains the code of ESSLN's version 2, which is written
% in standard Prolog (This version does not use the special built-in
% predicates of Arity-Prolog mentioned on page 365. The first version
% of ESSLN, which runs faster, is stored in file \CHAP9\ESSLN.ARI of
% the diskette).
%   To experiment with this system, first check the following points:
%   1: The builtin predicate "system" used in this program (to test
%      if a predicate is a builtin predicate) may be equivalent to
%      "sys" (or something else) in your Prolog system. In which case
%      change it by adding the following clause (simply remove %):
%          system(F/N) :- sys(F).
%   2: The builtin predicate "cls" is used in this program to clear
%      the screen before displaying menus. Check your Prolog system
%      for the equivalent predicate and replace it if necessary. For
%      example, if it is equivalent to "clscr" in your system, then
%      add the following clause (by simply removing the symbol %):
%          cls :- clscr.
%      If your Prolog system has no such predicate, then define it
%      using the following clauses (again by removing the symbols %):
%          cls :- cursor(&:,0,0),clear(23,80).
%
%          clear(0,0) :- !,cursor(&:,0,0).
%          clear(M,0) :- !,nl,M1 is M-1,clear(M1,80).
%          clear(M,N) :- put(32),N1 is N-1,clear(M,N1).
%      Here, the goal "cursor(&:,0,0)" moves the cursor to row 0 and
%      column 0. If the cursor moving predicate is also not available,
%      then remove all goals "cls" from this program.
%
%   3: The arithmetic function "round" used in this program may be
%      equivalent to some function "floor" (or "int"), say, in your
%      Prolog system. In which case, change this by adding the following
%      clause (again by removing the symbol %):
%          roundz(X,Y) :- Y is floor(X).
%
% Now to run ESSLN, perform the following steps:
% a) Consult this file.
% b) Enter the following query to invoke the system:
%        ?- essln.
%    Then press any key to start (press 'e' to exit).
%
% c) Select one of the three languages available in this package:
%    English, French or Vietnamese, and type the number terminated with
%    a period, for example, type 3.
%
% d) When the prompt Q:> appears, load an existing knowledge-base
%    (this diskette contains the files \chap9\supplier.eng  and
%    \chap9\detectiv.eng in English and also their versions in French
%    and Vietnamese, which should have been copied to your hard disk),
%    by entering the following query, say:
%        Q:> load 'c:\chap9\supplier.eng'.
%
% e) Enter the queries as presented on pages 430-432 to experiment
%    with the system (Observe the use of the underscore to indicate
%    types). In response to the prompt ->, you must type 'how.' (to
%    ask for explanation) or 'ok.' (to accept the answer) or 'more.'
%    (to ask for alternative answers). Note the required period at
%    the end. For example, try the following queries:
%        Q:> which _agent is a fractional supplier?
%        Q:> which _agent is not a fractional supplier?
%	 Q:> which _agent supply _item b1?
%        Q:> which _agent not supply _item b2?
%        Q:> which _agent supply all _item?
%        Q:> which _agent supply no _item?
%        Q:> _agent 'Adams & Sons' supply which _item?
%        Q:> _agent 'Mitre 10' not supply which _item?
%        Q:> which _agent supply which _item?
%        Q:> which _agent not supply which _item?
%    Then add the following new information:
%        Q:> _agent 'Rossianno Co' supply no _item.
%        Q:> _agent 'Yoshima Kuru' supply all _item.
%    and try the above queries again.
%
% f) To terminate the session, enter the query Q:> bye.
%
% g) Now re-start the system (by pressing any key), and load the second
%    file by entering:
%        Q:> load 'c:\chap9\detectiv.eng'.
%    then enter the queries as presented on pages 434-436, 457-460.
%    Start with the query:
%        Q:> which _suspect murdered _victim 'J.R.'?
%    (Beware, the name 'J.R.' has two dots in it. Also remember to use
%    a question mark at the end, as any sentence terminated with a period
%    is accepted as a new information to be stored in the knowledge-base).
%    You can use the query Q:> list base. to see the knowledge-base
%    in Prolog form. Similarly a query Q:> list predicate. will
%    show all the predicate definitions, including the built-in ones;
%    Q:> list domain. lists all the domains, Q:> list constsymbol. shows
%    all the constant symbols currently available, etc.
%    (For other languages, see the dictionaries in this file for the
%    corresponding commands. For example, 'list' in French is 'montrez'
%    and in Vietnamese is 'liet-ke'; 'bye' in French is 'aurevoir' and
%    in Vietnamese is 'het',  etc.).
%
%    To terminate this session, enter the query Q:> bye.
%
% h) Now re-start the system again (by pressing any key), select the
%    French language by typing the number 4., and load the French file
%    by entering:
%        Q:> chargez 'c:\chap9\supplier.fre'.
%    Then enter the following queries. In response to the prompt ->,
%    you must type 'comment.' (to ask for explanation) or 'ok.' (to
%    accept the answer) or 'encore.' (to ask for alternative answers).
%    Again, note the required period at the end:
%        Q:> quel _agent est un fournisseur fractionnaire?
%        Q:> quel _agent ne est pas un fournisseur fractionnaire?
%	 Q:> quel _agent fournit _item b1?
%        Q:> quel _agent ne fournit pas _item b2?
%        Q:> quel _agent fournit tout _item?
%        Q:> quel _agent fournit aucun-de _item?
%        Q:> _agent 'Adams & Sons' fournit quel _item?
%        Q:> _agent 'Mitre 10' ne fournit quel _item?
%        Q:> quel _agent fournit quel _item?
%        Q:> quel _agent ne fournit quel _item?
%    Then add the following new information:
%        Q:> _agent 'Rossianno Co' fournit aucun-de _item.
%        Q:> _agent 'Yoshima Kuru' fournit tout _item.
%    and try the above queries again.
%
%    To terminate, enter the query  Q:> aurevoir.
%    You may like to try the file \chap9\supplier.vie  as well.
%
% NOTE: To study the separate components of ESSLN such as: the query
%    converter, the goal displayer and the meta-interpreter, see files
%        \chap9\figure21.pro (or \chap9\convertr.pro),
%        \chap9\figure31.pro (or \chap9\displayr.pro), and
%        \chap9\intpretr.pro.
%
%==========================================================================
%=========== ESSLN: AN EXPERT SYSTEM SHELL THAT SUPPORTS ==================
%========== NEGATIVE AND QUANTIFIED QUERIES, AND SUBTYPES =================
%==========================================================================

% TOP-LEVEL CONTROL
% =========================================================================
:- op(500,xfx,#).
:- op(500,xfy,:).

goal :- essln.

essln :-
    initialize,
    repeat,
        receive_query(Q,C),
        process_query(Q,C),
    terminate(Q).
essln.

initialize :-
    display_banner,
    create_workspace,
    choose_language.

create_workspace :-
    clear_workspace,
    prepare_tools.

prepare_tools :-
    builtin(P),
    assertz(P),
    fail.
prepare_tools :-
    assert(symbol_index(0)).

choose_language :-
    display_languages,
    read(Language),
    (available(Language),!,
     get_dictionary(Language); learn_new(Language)).

available(L) :- member(L,[3,4,15]).

get_dictionary(L) :-
    language(L,A),assertz(A),fail.
get_dictionary(L) :-
	true.  % cls.

% --------------------------------------------------------------------
% RECEIVE QUERY
% ====================================================================
receive_query(Q,C) :-
    nl, write('Q:> '),
    readstring(T,C),
    scan_text(T,Q),!.
receive_query(Q,C) :-
    error_query,
    receive_query(Q,C).

readstring(T,C1) :-
    get0(C),
    read_chars(C,[],[C1|L]),
    reverse([C1|L],[],T).

    read_chars(13,[C|L],[C|L]) :-
        nl,end_char(C),!.
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

    chars_int([],N,N).
    chars_int([C|L],M,N) :-
        M1 is 10*M + C - 48,
        chars_int(L,M1,N).

end_char(C) :- C = 46; C = 63.  % period or ?

scan_text([],[]) :- !.
scan_text(T,[W|L]) :-
    append(_,[C|T1],T), \+(member(C,[32,13,10])),!,% skip spaces, eoln.
    get_word([C|T1],W,T2), scan_text(T2,L).

get_word([C|T],C,T) :- member(C,[58,44,46,63]),!. % (: , . ?)
get_word([C|T],[C],T) :- C = 61,!. % (=)
get_word([C|T],[C|W],T2) :- bracket(C,C1),!,get_chars(0,C1,T,W,T2).
get_word([C|T],[C|W],T2) :- valid_start(C),!, get_chars(1,32,T,W,T2).

get_chars(K,C1,[C|T],[C|W],T2) :-
     valid_char(K,C,C1),!,get_chars(K,C1,T,W,T2).
get_chars(0,39,[39|T],[],T) :- !.
get_chars(0,C,[C|T],[C],T) :- (C = 41; C = 93),!. % ) or ]
get_chars(1,C1,[C|T],[],[C|T]) :- member(C,[32,61,58,44,46,63,13,10]).

valid_start(C) :- valid(C); C = 37.  % (%)
valid_char(K,C,C1) :- K = 0,!, \+(C = C1); K = 1, valid(C).

bracket(39,39).  % single quotes
bracket(40,41).  % round brackets ()
bracket(91,93).  % square brackets []

% --------------------------------------------------------------------
% PROCESS QUERY
% ====================================================================
process_query(Q,_) :-
     special_query(Q),!.
process_query(Q,46) :- !,
     convert_clause(c,[],Q,E),
     store_clause(E).
process_query(Q,63) :-
     convert_query([],Q,G),
     evaluate(G).

special_query([[95|W],A,46]) :- !,
     atomname(A,A1),
     name(T,W),assert(constsymbol(T:A1)).
special_query([[95|W],63]) :- !,
     name(T,W),constsymbol(T:X),
     nl,write_list([T,' ',X,' ->']),get0(13).
special_query([[95|W],X,[95|W1],46]) :-
     name(Is,X),trans_word(is,Is),!,
     name(T,W),name(T1,W1),process_subtype(T,T1).
special_query([Q|R]) :-
     name(Q1,Q),trans_word(W,Q1),
     member(W,[bye,list,load,save,reload]),!,
     process_special_query(W,R).

process_special_query(bye,_) :- !.
process_special_query(list,R) :- !,
     (R = [46],!,nl,listing;
      R = [P,46],name(N,P),
      nl,(trans_word(base,N),listing('#'),!; listing(N))).
process_special_query(reload,[[39|N],46]) :- !,
     (retract(symbol_index(_)),!; true),
     name(Filename,N),reconsult(Filename).
process_special_query(load,[[39|N],46]) :- !,
     trans_word(bye,W),name(W,L),append(L,[46],E),
     name(Filename,N),
     see(Filename),
     readfile(E),
     seen.
process_special_query(save,[[39|N],46]) :- !,
     name(Filename,N),
     tell(Filename),
     listing,
     told.

readfile(E) :-
    readline(0,[],L),
    (append(_,E,L),!; 
     process_line(L), readfile(E)).

readline(0,[46|L],L1) :- !,reverse(L,[46],L1).
readline(S,L,L1) :-
    get0(C),put(C),
    (C = 39,!,R is (S+1) mod 2; R is S),
    readline(R,[C|L],L1).

reverse([],L,L).
reverse([H|T],L,R) :- reverse(T,[H|L],R).

process_line(L) :-
    scan_text(L,Q),
    process_query(Q,46),!.
process_line(_).
	      
% -------------------------------------------------------------------
% PROCESS SUBTYPE
% ===================================================================
process_subtype(T1,T2) :-
    \+(subtype(T1,T2)),
    asserta(subtype(T1,T2)),
    subtype(S,T),
      clause(predicate(Phrase,A),true),
        append(W,[(T,R:X)|Rest],Phrase),
        append(W,[(S,R:X)|Rest],Phrase1),
        process_subphrase(Phrase1,R,S,T,A),
    fail.
process_subtype(_,_).

process_subphrase(Phrase,R,S,T,A) :-
    clause(predicate(Phrase,A1),true),!,
      \+(clause((A # 1),A1)),
      assert((A # 1 :- A1));
    generate_symbol(P1),
    A =.. [P|L1], A1 =..[P1|L1],
    assert(predicate(Phrase,A1)),
    assert((A # 1 :- A1)),
    clause(domain(A),Types),conv(TL,Types),
    append(W,[type(R1,T)|Rest],TL), R == R1,!,
    append(W,[type(R1,S)|Rest],TL1),conv(TL1,Types1),
    assert((domain(A1) :- Types1)).

conv([X|T],(X,R)) :- T \== [],!,conv(T,R).
conv([X],(X)).

type(T,T).
type(S,T) :- subtype(S,T).

%-------------------------------------------------------------------
% TERMINATION
%===================================================================
end([Q,46]) :- name(Q1,Q),trans_word(bye,Q1).

terminate([Q,_]) :-
     name(Q1,Q),trans_word(bye,Q1),
     abolish('#'/2),
     abolish(symbol_index/1),
     abolish(predicate/2),
     abolish(domain/1),
     abolish(constsymbol/1),
     abolish(subtype/2),
     abolish(transword/2),
     abolish(already_asked/2),
     essln.

%------------------------------------------------------------------
% CONVERT CLAUSES
%==================================================================
convert_clause(S,VS,Q,E) :-
     append(P,[C|Rest],Q),member(C,[58,44,46]),!, \+(P = []),
     convert(S,P,VS,VS1,A),      % [ :  ,  .]
     (C = 58,!, E = (A :- B), convert_clause(t,VS1,Rest,B);
      C = 44,!, E = (A,B), convert_clause(t,VS1,Rest,B);
      C = 46,!, E = A).

convert(c,[[37|N]|P],VS,VS1,(A # C)) :- !,
     chars_int(N,0,I),
     C is I/100,
     convert(h,P,VS,VS1,A).
convert(c,P,VS,VS1,(A # 1)) :- !,
     convert(h,P,VS,VS1,A).

convert(h,P,VS,VS1,A) :- !,
     convert_atom(P,VS,VS1,NP,P1,AL,_,[0,0,0,0],[_,Nt,No,All]),
     (bad_atom(Nt,No,All),!,fail;
     (predicate(P1,A1),!; construct(P1,A1)),
     (No = 1,!,A = non(A1); quantify([_,Nt,0,0],NP,_,A1,A)),
     store_constants(AL)).

convert(t,P,VS,VS1,A) :-
     convert_atom(P,VS,VS1,NP,P1,AL,P2,[0,0,0,0],QL),
     (predicate(P1,A1),!; construct(P1,A1)),
     quantify(QL,NP,P2,A1,A),
     store_constants(AL),!.

quantify([_,0,0,0],_,_,A,A) :- !.
quantify([0,0,1,0],_,_,A,not(A)) :- !.
quantify([0,0,0,1],_,_,A,not(non(A))) :- !.
quantify([1,0,0,1],_,P,A,all(A,not(non(B)))) :- !,predicate(P,B).
quantify([1,0,1,0],_,P,A,all(non(A),not(B))) :- !,predicate(P,B).
quantify([_,1,0,0],NP,_,A,non(A)) :- !,
     (predicate(NP,_),!;
      functor(A,F,N),copy_clause(_,NP,VL,_),
      A1 =.. [F|VL],assert(predicate(NP,non(A1)))).
quantify([_,Nt,No,All],_,_,_,_) :-
     (bad_atom(Nt,No,All); unknown_concept),!,fail.

convert_atom([Q,[95|W]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V2)|P2],[E,Nt,No,All],QL) :-
     universal(Q),!,name(T,W),
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[E,Nt,No,1],QL).
convert_atom([Q,[95|W]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V2)|P2],[E,Nt,No,All],QL) :-
     neguniversal(Q),!,name(T,W),
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[E,Nt,1,All],QL).
convert_atom([Q,[95|W]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V1)|P2],[E,Nt,No,All],QL) :-
     existential(Q),!,name(T,W),
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[1,Nt,No,All],QL).
convert_atom([Q|Rest],VS,VS1,[Q1|NP],P1,AL,P2,[E,Nt,No,All],QL) :-
     negation(Q,Q1),!,
     convert_atom(Rest,VS,VS1,NP,P1,AL,P2,[E,1,No,All],QL).
convert_atom([[95|W],[C|L]|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:V1)|P1],[T:V1|AL],[(T,S1:V1)|P2],[E,Nt,No,All],QL) :-
     capital(C),!,name(X,[C|L]),name(T,W),
     (member((X,T,S1:V1),VS),!, VS2 = VS; VS2 = [(X,T,S1:V1)|VS]),
     convert_atom(Rest,VS2,VS1,NP,P1,AL,P2,[1,Nt,No,All],QL).
convert_atom([[95|W],A|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:A1)|P1],[T:A1|AL],[(T,S1:A1)|P2],[E,Nt,No,All],QL) :- !,
     aterm(A,VS,E,A1,VS2,E1),name(T,W),
     convert_atom(Rest,VS2,VS1,NP,P1,AL,P2,[E1,Nt,No,All],QL).
convert_atom([W|Rest],VS,VS1,[(T,S:V)|NP],
     [(T,S1:W1)|P1],[T:W1|AL],[(T,S1:W1)|P2],[E,Nt,No,All],QL) :-
     special_term(W,VS,E,T,W1,VS2,E1),
     convert_atom(Rest,VS2,VS1,NP,P1,AL,P2,[E1,Nt,No,All],QL).
convert_atom([W|Rest],VS,VS1,[W1|NP],[W1|P1],AL,[W1|P2],QL0,QL) :-
     !,name(W1,W),convert_atom(Rest,VS,VS1,NP,P1,AL,P2,QL0,QL).
convert_atom([],VS,VS,[],[],[],[],QL,QL).

convert_query(VS,Q,G) :-
     append(P,[C|Rest],Q),member(C,[44,63]),!,\+(P = []),
     convert(g,P,VS,VS1,A),      % [ ,  ?]
     (C = 44,!, G = (A,B), convert_query(VS1,Rest,B);
      C = 63,!, G = A).

convert(g,P,VS,VS1,A) :-
     convert_atom(P,VS,VS1,NP,P1,_,P2,[0,0,0,0],QL),
     (predicate(P1,A1),!, quantify(QL,NP,P2,A1,A);
      unknown_concept,!,fail).


% QUANTIFIERS
% -----------------------------------------------------------------------
universal(Q) :- name(Q1,Q),(trans_word(all,Q1);trans_word(every,Q1)),!.
neguniversal(Q) :- name(Q1,Q),trans_word(no,Q1),!.
existential(Q) :- name(Q1,Q),(trans_word(some,Q1);trans_word(which,Q1)),!.
negation(Q,Q1) :- name(Q1,Q),trans_word(not,Q1),!.
% -----------------------------------------------------------------------

capital(C) :- 65 =< C, C =< 90.

aterm(W,VS,E,W1,VS,E) :- atomname(W,W1),!.
aterm(W,VS,E,W1,VS2,E1) :- special_term(W,VS,E,_,W1,VS2,E1).

atomname([39|W],W1) :- name(W1,W).
atomname([C|W],W1) :- 96 < C, C < 123,name(W1,[C|W]).

special_term(W,VS,E,T,W1,VS,E) :-   % integer number
%    list_text(W,S),int_text(W1,S),
    chars_int(W,W1),!,trans_word(number,T).
special_term([C|W],VS,E,T,W1,VS2,E1) :-
    (C = 40,!,trans_word(number,T); % ( for numeric expression
     C = 91,  trans_word(list,T)),  % [ for list
     tokenize([C|W],TL),term(W1,[],VL,TL,[]),
     (VL = [],!, VS = VS2, E = E1;
     check_vars(VL,VS,VS2), E1 = 1).

%    list_text([C|W],S),string_term(S,W1),
%    list_var_symbols([C|W],0,[],WVS),
%    (WVS = [],!, VS = VS2, E = E1;
%     var_list(W1,WVL), match_vars(WVS,WVL,VS,VS2), E1 = 1).

check_vars([],VS,VS) :- !.
check_vars([(X,V)|VL],VS,VS2) :-
    (member((X,T,S:V),VS),!,check_vars(VL,VS,VS2);
     check_vars(VL,[(X,T,S:V)|VS],VS2)).


%match_vars([],[],VS,VS) :- !.
%match_vars([X|XL],[V|VL],VS,VS2) :-
%    (member((X,T,S:V),VS),!,match_vars(XL,VL,VS,VS2);
%     match_vars(XL,VL,[(X,T,S:V)|VS],VS2)).

%----------------------------------------------------------------
% GET TOKENS FROM A LIST OF CHARACTERS
%================================================================
    tokenize([],[]).
    tokenize(CharList,[Token|TList]) :-
        append(_,[C|List],CharList), \+(C = 32),!,
        get_token([C|List],Token,Rest),
        tokenize(Rest,TList).

    get_token(List,Token,Rest) :-
        get_char(List,Lchars,Rest),
        name(Token,Lchars).

    get_char(L,S,L1) :- separator(L,S,L1),!.
    get_char([C|L],[C|Lc],L1) :-
        check_start(S,C),
        get_word_chars(S,L,Lc,L1).

    get_word_chars(S,L,Lc,L1) :-
        check_end(S,L,Lc,L1).
    get_word_chars(S,[C|L],[C|Lc],L1) :-
        legal_char(S,C),
        get_word_chars(S,L,Lc,L1).

    legal_char(quote,C) :- \+(C = 39).
    legal_char(num,C)   :- digit(C).
    legal_char(symb,C)  :- valid_cha(C).

    check_start(quote,39).
    check_start(num, C)  :- digit(C).
    check_start(symb,C)  :-
        valid_cha(C), \+(digit(C)).

    check_end(_,[],[],[]) :- !.
    check_end(quote,[39|L],[39],L) :- !.
    check_end(num, [C|L],[],[C|L]) :- \+(digit(C)),!.
    check_end(symb,[C|L],[],[C|L]) :- \+(valid_cha(C)).

  separator([C,D,E|L],[C,D,E],L) :- name(S,[C,D,E]),
    member(S,['=:=','=\=','\==','@=<','@>=','=..','-->']),!.
  separator([C,D|L],[C,D],L) :- name(S,[C,D]),
    member(S,[(':-'),'\+','->','\=','==','@<','@>','=<','>=',
               '//']),!.
  separator([C|L],[C],L) :-
    member(C,[44,40,41,58,91,93,124,43,45,42,47,59,61,60,62,94]).

    valid_cha(C) :- letter(C); digit(C); C = 95.
    letter(C) :-  97 =< C, C =< 122; 65 =< C, C =< 90.
    digit(C)  :-  48 =< C, C =< 57.


%----------------------------------------------------------------
% CONVERT A LIST OF TOKENS INTO A PROLOG TERM
%================================================================
    term(T,VL,VL1) --> formal_term(T,VL,VL1).
    term(T,VL,VL1) --> infix_term(T,VL,VL1).

    formal_term(T,VL,VL1) --> variable(T,VL,VL1).
    formal_term(T,VL,VL1) --> constants(T,VL,VL1).
    formal_term(T,VL,VL1) --> list(T,VL,VL1).
    formal_term(T,VL,VL1) --> prefix_term(T,VL,VL1).

    variable(V,VL,VL1) --> 
        [X],{variable_name(X,V,VL,VL1)}.
    constants(T,VL,VL) -->
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
        ((capital(C); C = 95, \+(L = [])),
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
       
    string([39],[]).
    string([H|T],[H|R]) :- string(T,R).

    numbr(X,T) :- name(X,[C|L]),
        (48 =< C, C =< 57),chars_int([C|L],0,T).
     
    operator(F) :- 
    member(F,[is,':','+','-','*','/','=','<','>','^',mod]);
    member(F,['->','\=','==','@<','@>','=<','>=','//']);
    member(F,['=:=','=\=','\==','@=<','@>=','=..','-->']).


%----------------------------------------------------------------
% STORE CLAUSES IN KNOWLEDGE-BASE
%================================================================
store_clause((A # C :- B)) :- !,assert((A # C :- B)).
store_clause((non(A) # C)) :- !,domain(A),assert((non(A) # C)).
store_clause((A # C)) :- domain(A),assert((A # C)).

construct([Find|P1],tobe_filled(A)) :-
     trans_word(find,Find),!,
     construct(P1,A).
construct(P1,A) :-
     copy_clause(P1,P2,VL,TL),
     generate_symbol(P), E =.. [P|VL], assert(predicate(P2,E)),
     convert(TL,Types), assert((domain(E) :- Types)),
     predicate(P1,A).

copy_clause([],[],[],[]) :- !.
copy_clause([(T,S:A)|P1],[(T,R:V)|P2],[R:V|VL],[type(R,T)|TL]) :- !,
     copy_clause(P1,P2,VL,TL).
copy_clause([W|P1],[W|P2],VL,TL) :-
     copy_clause(P1,P2,VL,TL).

store_constants([]) :- !.
store_constants([T:A|AL]) :-
     (atom(A),\+(constsymbol(T:A)),!,
           assert(constsymbol(T:A)); true),
     store_constants(AL).

generate_symbol(Symb) :-
     common_symbol(P),
     retract(symbol_index(I)),I1 is I+1,
     int_to_chars(I1,[],L),
     name(Symb,[P|L]),
     assert(symbol_index(I1)).

int_to_chars(0,L,L) :- !.
int_to_chars(I,L,L1) :-
     C is 48 + (I mod 10),
     I1 is I//10,
     int_to_chars(I1,[C|L],L1).

common_symbol(112).

bad_atom(Nt,No,All) :-
     Nt+No+All > 1,
     nl,write('A:> '),
     write_word('Bad clause!'),nl,tab(4),
   write_word('A clause cannot have more than one word "not","no","all".').

unknown_concept :-
     nl,write('A:> '),
     write_word('I dont know this concept. Check your typing'),
     nl,tab(4),write_word('If this is a new concept, please teach me').

error_query :- nl,write('A:> '),write_word('Illegal query !').
terminate_message :- nl,tab(4),write_word('No (more) answers !').

%--------------------------------------------------------------------
% EVALUATE GOAL
%====================================================================
evaluate(Goal) :-
     prove(Goal,20,Certainty,Proof,[]),
     process_result(Goal,Certainty,Proof),!,
     clear_workspace.
evaluate(_) :-
     terminate_message,
     clear_workspace.

process_result(Goal,Certainty,Proof) :-
     print_answer(Goal,Certainty),!,
     read_response(Response),
     process_response(Response,Proof).

clear_workspace :-
     abolish(answer/2),assert(answer(nul,nul)),
     abolish(save_all/2),assert(save_all(nul,nul)),
     abolish(failproof/1),assert(failproof(nul)).

print_answer(Goal,0) :- !,
     nl,write('A:> '),
     copy(Goal,Goal1),
     display_goal(Goal1,no),!.
print_answer(Goal,Certainty) :-
     nl,write('A:> '),
     write_goal_certainty(Certainty),
     copy(Goal,Goal1),
     display_goal(Goal1,yes),!.

write_goal_certainty(C) :-
     C =:= 1,!;
     C1 is C*100,
     roundz(C1,C2),!,
% float_text(C1,T,fixed(0)),int_text(C2,T),
     write_word('I am'),
     write_list([' ',C2,'% ']),write_word('positive that'),
     nl,tab(4).

write_rule_certainty(C) :-
     (var(C); C =:= 1),!;
     C =:= 0,!,write_word('is false');
     C1 is C*100,
     roundz(C1,C2),!,
% float_text(C1,T,fixed(0)),int_text(C2,T),
     write_word(with),
     write_list([' ',C2,'% ']),write_word(certainty).

roundz(X,Y) :- Y is round(X,0).

read_response(R) :-
    nl,write(' -> '),
    read(S),
    (trans_word(T,S),
     member(T,[ok,more,how]),!, R = T;
     write_word('    Type ok., how. or more.'),
     read_response(R)).

trans_word(W,W1) :- transword(W,W1),!.
write_word(W) :- trans_word(W,W1),write(W1).

write_list([]) :- !.
write_list([X|T]) :- write(X),write_list(T).

%---------------------------------------------------------------------
% ENGLISH DICTIONARY
%=====================================================================
language(3,transword(X,X)).

%---------------------------------------------------------------------
% FRENCH DICTIONARY
%=====================================================================
language(4,transword(all,tout)).
language(4,transword(all,toute)).
language(4,transword(every,chaque)).
language(4,transword(some,quelque)).
language(4,transword(which,quel)).
language(4,transword(which,quelle)).
language(4,transword(no,'aucun-de')).
language(4,transword(not,ne)).
language(4,transword(not,pas)).
language(4,transword(none,nul)).
language(4,transword(none,nulle)).
language(4,transword(none,aucun)).
language(4,transword(none,aucune)).
language(4,transword(deny,rejetez)).
language(4,transword(is,est)).
language(4,transword(true,vrai)).
language(4,transword(fact,fait)).
language(4,transword(usergiven,'useur-donne')).
language(4,transword(nofact,sansfait)).
language(4,transword(system:true,systeme:vrai)).
language(4,transword(system:false,systeme:faux)).
language(4,transword(nonterminate,nontermine)).
language(4,transword(ok,ok)).
language(4,transword(more,encore)).
language(4,transword(how,comment)).
language(4,transword(find,trouver)).
language(4,transword(base,base)).
language(4,transword(with,avec)).
language(4,transword(certainty,certain)).
language(4,transword(number,nombre)).
language(4,transword(eq,eg)).
language(4,transword(dif,dif)).
language(4,transword(lt,mq)).
language(4,transword(gt,pq)).
language(4,transword(le,me)).
language(4,transword(ge,pe)).
language(4,transword(lta,mqa)).
language(4,transword(gta,pqa)).
language(4,transword(lea,mea)).
language(4,transword(gea,pea)).
language(4,transword(in,dans)).
language(4,transword(fail,manque)).
language(4,transword(bye,aurevoir)).
language(4,transword(list,montrez)).
language(4,transword(list,liste)).
language(4,transword(reload,rechargez)).
language(4,transword(load,chargez)).
language(4,transword(load,load)).
language(4,transword('reading..........','lisant..........')).
language(4,transword(save,reservez)).
language(4,transword('Because','Parce que')).
language(4,transword('IF','SI')).
language(4,transword('THEN','ALORS')).
language(4,transword('is true','est vrai')).
language(4,transword('is false','est mauvais')).
language(4,transword('    Type ok., how. or more.',
    '    Repondez ok., comment. ou encore.')).
language(4,transword(stop,stop)).
language(4,transword('Illegal query !','Question illegal !')).
language(4,transword('No (more) answers !','Pas (encore) de reponse !')).
language(4,transword('No more explanations',
                      'Pas davantage d''explications')).
language(4,transword('No more explanations for this',
                      'Pas davantage d''explications pour ceci')).
language(4,transword('Give a percentage on your certainty',
                      'Donnez un pourcentage de certain')).
language(4,transword('I am','Je suis')).
language(4,transword('positive that','certain que')).
language(4,transword('has the form','a de la forme')).
language(4,transword('Bad clause!','Mauvais clause!')).
language(4,
  transword('A clause cannot have more than one word "not","no","all".',
   'Clause ne peut pas avoir plus qu''un mot "ne-pas","aucun","tout" .')).
language(4,transword('I dont know this concept. Check your typing',
            'Je ne sais pas ce concept. Verifiez votre composition')).
language(4,transword('If this is a new concept, please teach me',
                   'S''il est un nouveau concept,  enseignez moi')).
language(4,transword('using the substitution','par la substitution')).
language(4,transword('evaluating negation shows',
                'evaluation de negation montre que')).
language(4,transword('evaluating goal shows',
                 'evaluation de terme montre que')).
language(4,transword('uninstantiated','non-instancie')).
language(4,transword('using the instantiation','par l''instanciation')).
language(4,transword('and the substitution','et la substitution')).
language(4,transword('is variable-pure','pur variable')).
language(4,transword('is unrestricted','est sans duplication')).
language(4,transword('is a variable','est un variable')).
language(4,transword('the domain of','le domaine de')).
language(4,transword('by instantiating','par l''instanciation')).

% -----------------------------------------------------------------------
% VIETNAMESE DICTIONARY
% =======================================================================
language(15,transword(all,'tat-ca')).
language(15,transword(every,moi)).
language(15,transword(some,vai)).
language(15,transword(which,tim)).
language(15,transword(no,chang)).
language(15,transword(not,khong)).
language(15,transword(none,'chang-gi')).
language(15,transword(deny,'phu-nhan')).
language(15,transword(is,la)).
language(15,transword(true,that)).
language(15,transword(fact,'du-kien')).
language(15,transword(usergiven,'duoc-cung-cap')).
language(15,transword(nofact,'khong-du-kien')).
language(15,transword(system:true,hethong:dung)).
language(15,transword(system:false,hethong:sai)).
language(15,transword(nonterminate,'khong-ngung')).
language(15,transword(ok,cn)).
language(15,transword(more,nua)).
language(15,transword(how,lamsao)).
language(15,transword(find,tim)).
language(15,transword(base,'co-he')).
language(15,transword(with,voi)).
language(15,transword(certainty,'xac-suat')).
language(15,transword(number,so)).
language(15,transword(eq,bg)).
language(15,transword(dif,kh)).
language(15,transword(lt,nh)).
language(15,transword(gt,lh)).
language(15,transword(le,nb)).
language(15,transword(ge,lb)).
language(15,transword(lta,nha)).
language(15,transword(gta,lha)).
language(15,transword(lea,nba)).
language(15,transword(gea,lba)).
language(15,transword(in,trong)).
language(15,transword(fail,hong)).
language(15,transword(bye,het)).
language(15,transword(list,'liet-ke')).
language(15,transword(list,liet)).
language(15,transword(reload,molai)).
language(15,transword(load,mo)).
language(15,transword(load,load)).
language(15,transword('reading..........','dang mo..........')).
language(15,transword(save,cat)).
language(15,transword('Because','Boi vi')).
language(15,transword('IF','NEU')).
language(15,transword('THEN','THI')).
language(15,transword('is true','la dung')).
language(15,transword('is false','la sai')).
language(15,transword('    Type ok., how. or more.',
    '    Tra loi cn., lamsao. hoac nua.')).
language(15,transword(stop,ngung)).
language(15,transword('Illegal query !','Cau hoi bat hop le !')).
language(15,transword('No (more) answers !','Khong (con) giai-dap !')).
language(15,transword('No more explanations',
                      'Khong con giai thich nao')).
language(15,transword('No more explanations for this',
                      'Khong con giai thich nao')).
language(15,transword('Give a percentage on your certainty',
                      'Cho biet xac-suat phan tram')).
language(15,transword('I am','Toi xac-dinh')).
language(15,transword('positive that','rang')).
language(15,transword('has the form','co dang')).
language(15,transword('Bad clause!','Cau sai luat!')).
language(15,
  transword('A clause cannot have more than one word "not","no","all".',
  'Moi cau chi duoc dung mot trong cac chu "khong","chang","tat-ca".')).
language(15,transword('I dont know this concept. Check your typing',
                      'Toi khong ro cau nay. Xin kiem lai')).
language(15,transword('If this is a new concept, please teach me',
                      'Neu la khai-niem moi, thi hay chi toi')).
language(15,transword('using the substitution','dung thay the')).
language(15,transword('evaluating negation shows',
                      'xac-dinh phu-nhan chung to')).
language(15,transword('evaluating goal shows','xac-dinh muc-tieu chung to')).
language(15,transword('uninstantiated','khong thay doi')).
language(15,transword('using the instantiation','dung phep thay the')).
language(15,transword('and the substitution','va phep thay the')).
language(15,transword('is variable-pure','co bien-so thuan')).
language(15,transword('is unrestricted','khong trung lap')).
language(15,transword('is a variable','la mot bien-so')).
language(15,transword('the domain of','vung xac-dinh cua')).
language(15,transword('by instantiating','thay hang-so vao')).

%---------------------------------------------------------------------
% BUILTIN PREDICATES
% ====================================================================
builtin((predicate([Phrase,B],tvar_list(A,B)) :-
    trans_word('using the substitution',Phrase))).
builtin((predicate([Phrase|PNA],eval_non(A,B)) :-
    domain(A),
    (predicate(PNA,non(A)),!;
     predicate(PA,A),trans_word(deny,W),PNA = [W|PA]),
    trans_word('evaluating negation shows',Phrase))).
builtin((predicate([Phrase|PA],eval(A)) :-
    domain(A),predicate(PA,A),
    trans_word('evaluating goal shows',Phrase))).
builtin((predicate([Phrase|PA],not(A)) :-
    domain(A),predicate(PA,A),
    trans_word(not,Phrase))).
builtin((predicate([Phrase|PA],not(call(A))) :-
    domain(A),predicate(PA,A),
    trans_word(not,Phrase))).
builtin((predicate([L,Phrase],uninstantiated(L)) :-
    trans_word('uninstantiated',Phrase))).
builtin((predicate([Phrase,L],instantiate(A,L,VL)) :-
    trans_word('using the instantiation',Phrase))).
builtin((predicate([Find|Phrase],tobe_filled(A)) :-
    domain(A),predicate(Phrase,A),
    trans_word(find,Find))).
builtin((predicate([Phrase],!) :- trans_word(stop,Phrase))).
builtin((predicate([Fail],fail) :- trans_word(fail,Fail))).
builtin((predicate([L,Phrase],tvar_pure(L)) :-
    trans_word('is variable-pure',Phrase))).
builtin((predicate([L,Phrase],unrestricted(L,0)) :-
    trans_word('is unrestricted',Phrase))).
builtin((predicate([Phrase,A],domain(A)) :-
    trans_word('the domain of',Phrase))).
builtin((predicate([Phrase,L],instant(L,VL)) :-
    trans_word('by instantiating',Phrase))).
builtin((predicate([A,Phrase],var(A)) :-
    trans_word('is a variable',Phrase))).
builtin((predicate([(T,A),Is,(T,B)],(A = B)) :- trans_word(is,Is))).
builtin((predicate([(T,A),Op,(T,B)],(A is B)) :- (Op == '='))).
builtin((predicate([(T,A),Eq,(T,B)],(A =:= B)) :- trans_word(eq,Eq))).
builtin((predicate([(T,A),Dif,(T,B)],(A =\= B)) :- trans_word(dif,Dif))).
builtin((predicate([(T,A),Lt,(T,B)],(A < B)) :- trans_word(lt,Lt))).
builtin((predicate([(T,A),Gt,(T,B)],(A > B)) :- trans_word(gt,Gt))).
builtin((predicate([(T,A),Le,(T,B)],(A =< B)) :- trans_word(le,Le))).
builtin((predicate([(T,A),Ge,(T,B)],(A >= B)) :- trans_word(ge,Ge))).
builtin((predicate([(T,A),Lta,(T,B)],(A @< B)) :- trans_word(lta,Lta))).
builtin((predicate([(T,A),Gta,(T,B)],(A @> B)) :- trans_word(gta,Gta))).
builtin((predicate([(T,A),Lea,(T,B)],(A @=< B)) :- trans_word(lea,Lea))).
builtin((predicate([(T,A),Gea,(T,B)],(A @>= B)) :- trans_word(gea,Gea))).
builtin((predicate([(T,A),In,(list,B)],member(A,B)) :- trans_word(in,In))).

builtin((domain(A) :- nonvar(A),functor(A,F,N),(system(F/N);
    member(F,[tvar_list,eval_non,eval,instantiate,
              uninstantiated,member,tobe_filled])),!)).
builtin(constsymbol('$':'@')).
builtin(subtype('#','%')).
builtin(already_asked('@','#')).

%---------------------------------------------------------------------
% ESSLN META-INTERPRETER
% ====================================================================
prove(Goal,Depth,Certainty,Proof,Rules) :-
     prove_branch(Goal,Depth,Certainty,Proof,Rules),
     check_result(Goal,Depth,Certainty,Proof).
prove(Goal,Depth,0,FailProof,Rules) :-
     collect_failproof(Goal,Depth,FailProof).

prove_branch(true,D,1,fact,Rules) :- !.
prove_branch(A,0,0,nonterminate,Rules) :- !.
prove_branch(call(A),D,C,Proof,Rules) :- !,
     prove_branch(A,D,C,Proof,Rules).
prove_branch(not(A),D,C,(not(A) # C :- Proof),Rules) :- !,
     copy(A,A1),collect_proof(A1,D,Rules,C,List),!,
     convert(List,Proof).
prove_branch(all(A,B),D,C,(all(A,B) # C :- Proof),Rules) :- !,
     prove_all(A,B,D,C,Proof,Rules).
prove_branch((A,B),D,C,(ProofA,ProofB),Rules) :- !,
     prove_branch(A,D,CA,ProofA,Rules),
     prove_conj(CA,B,D,C,ProofB,Rules).
prove_branch(A,D,C,(A # C :- system:R),Rules) :-
     syst(A),!,(A, C = 1, R = true; not(A), C = 0, R = false).
prove_branch(A,D,C,(A # C :- Proof),Rules) :-
     \+(find_clause((A # C),B)),!,
     find_out(A,C,Proof,Rules).
prove_branch(A,D,C,(A # C :- Proof),Rules) :-
     threshold(C0),
     find_clause((A # RC),B),D1 is D-1,
     detect_cut(B,B1,B2,Cut),
     (Cut = yes,prove_branch(B1,D1,C1,Proof1,[(A # RC :- B)|Rules]),
        (C1 < C0, C is RC*C1,return_proof(Proof1,B2,Proof);
         C1 >= C0,!,prove_branch(B2,D1,C2,Proof2,[(A # RC :- B)|Rules]),
            C is RC*C1*C2,conjunct(Proof1,Proof2,Proof));
      Cut = no,prove_branch(B,D1,CB,Proof,[(A # RC :- B)|Rules]),
               C is RC*CB).

detect_cut(B,B1,B2,yes) :- cutin(B,B1,B2),!.
detect_cut(_,_,_,no).

cutin(!,!,true) :- !.
cutin((!,B),!,B) :- !.
cutin((A,!),(A,!),true) :- !.
cutin((A,B),(A,As),Bs) :- cutin(B,As,Bs).

return_proof(Proof1,B,Proof) :-
     B = true,!, Proof1 = Proof;
     conjunct(Proof1,(B : nottried),Proof).

conjunct(A,fact,A) :- !.
conjunct((A,As),L,(A,Bs)) :- !,conjunct(As,L,Bs).
conjunct((A),L,(A,L)).

collect_proof(A,D,Rules,0,List) :-
     threshold(T),asserta(bag(1,[],D)),
     prove(A,D,C,P,Rules),
       once(retract(bag(C0,L,D))),
       C1 is C0* (1-C),asserta(bag(C1,[P|L],D)),
       C1 < 1-T, retract(bag(_,List,D)),remove(answer(A,D)),!.
collect_proof(A,D,_,C,List) :-
     retract(bag(C,List,D)),remove(answer(A,D)).

convert([P],P) :- !.
convert([P|L],(P,NP)) :-
     convert(L,NP).

prove_all(A,B,D,C,Proof,Rules) :-
     prove(A,D,CA,ProofA,Rules),
     check_all(CA,ProofA,B,D,C,Proof,Rules).
prove_all(A,B,D,_,_,_) :-
     remove(answer(A,D)),remove(save_all(B,D)),fail.

check_all(0,ProofA,B,D,0,ProofA,Rules) :- !.
check_all(_,_,B,D,C,ProofB,Rules) :-
     \+((save_all(B1,D),instanc(B,B1))),
     asserta(save_all(B,D)),
     prove_branch(B,D,C,(B # C :- ProofB),Rules).

prove_conj(C,B,D,0,(B : nottried),Rules) :-
     threshold(T), C < T,!.
prove_conj(CA,B,D,C,ProofB,Rules) :-
     prove_branch(B,D,CB,ProofB,Rules),
     C is CA*CB.

check_result(G,D,C,Proof) :-
     threshold(C0), C < C0,!,
     \+((failproof((G1,D):_),instanc(G,G1))),
     assert(failproof((G,D):Proof)),fail.
check_result(G,D,C,Proof) :-
     collect_failbranches(G,D,_),
     (answer(G1,D1),D1 < D,retract(answer(G1,D1)),fail;
     \+((answer(G1,D),instanc(G,G1))),save_answer(G,D)).

collect_failproof(G,D,FailProof) :-
     collect_failbranches(G,D,FailList),
     \+(answer(G,D)),convert(FailList,FailProof).

collect_failbranches(G,D,[FailBranch|Rest]) :-
     copy(G,G1),retract(failproof((G1,D):FailBranch)),!,
     collect_failbranches(G,D,Rest).
collect_failbranches(_,_,[]).


find_out(A,C,usergiven,Rules) :-
     tobe_filled(A) # 1,!,
     (already_asked(A,C),!; ask_user(A,C,Rules)).
find_out(A,0,nofact,_).

find_clause((A # RC),B) :- clause((A # RC),B).
%find_clause((A # RC),B) :-
%     code_world(X,api),
%     clause((A # RC),B),
%     code_world(_,X).

save_answer(Goal,Depth) :-
     trim_answer(Goal,Goal1),assert(answer(Goal1,Depth)).

trim_answer(all(A,B),all(_,B)) :- !.
trim_answer((A,B),(A1,B1)) :- !,
     trim_answer(A,A1),trim_answer(B,B1).
trim_answer(A,A).

syst(A) :- functor(A,F,N),
          (system(F/N),!;
           member(F,['!',member,append,var_list,tvar_list,
                     uninstantiated,instantiate])).

copy(A,B) :- assert(zzzz(A)),retract(zzzz(B)).
maxdepth(20).
threshold(0.3).

remove(A) :- copy(A,B),retract(B),!,remove(A).
remove(_).

once(P) :- call(P),!.

%---------------------------------------------------------------------
% USER INTERFACE
% ====================================================================

% QUERYING USER
% --------------------------------------------------------------------
ask_user(Goal,C,Rules) :-
    display_question(Goal),
    read(Answer),
    process_answer(Answer,Goal,C,Rules).

display_question(Goal) :-
    trans_word(which,Q),
    copy(Goal,Goal1),
    nl,print_goal(Goal1,Q),write('? ').

process_answer(why,Goal,C,[Rule|Rules]) :- !,
    display_rule(Rule),nl,
    ask_user(Goal,C,Rules).
process_answer(why,Goal,C,[]) :- !,
    nl,write('  > '),
    write_word('No more explanations'),nl,
    ask_user(Goal,C,[]).
process_answer(X,Goal,C,_) :-
    var_list(Goal,[V]), V = X,
    nl,tab(4),write_word('Give a percentage on your certainty'),
    write(' : '),read(C1), C is C1/100,
    assert(already_asked(Goal,C)).

% ANSWERING USER
% --------------------------------------------------------------------
process_response(ok,_) :- !.
process_response(more,_) :- !,fail.
process_response(how,(Proof1,Proof2)) :- !,
    process_response(how,Proof1),
    read_response(Response),
    process_response(Response,Proof2).
process_response(how,(A # C :- Proof)) :- !,
    recollect_body(Proof,Body),
    display_proof((A # C :- Body)),
    read_response(Response),!,
    process_response(Response,Proof).
process_response(how,End) :-
    nl,tab(4),write_word('No more explanations for this'),
    read_response(Response),
    process_response(Response,End).

recollect_body(B,(B,1)) :-
     member(B,[fact,system:true,usergiven]),!.
recollect_body(B,(B,0)) :-
     member(B,[nofact,system:false,nonterminate]),!.
recollect_body((B:nottried),(B:nottried,0)) :- !.
recollect_body((B # C :- D),(B,C)) :- !.
recollect_body(((B # C :- D),Rest),((B,Bs),(C,Cs))) :-
     recollect_body(Rest,(Bs,Cs)).

%---------------------------------------------------------------------
% EXPLANATION GENERATOR
% ====================================================================

% DISPLAY RULE
% --------------------------------------------------------------------
display_rule((A # C :- true)) :- !,
    copy(A,A1),
    nl,write('  > '),write_word('Because'),
    nl,tab(6),display_rule_body(A1,65,_),
    nl,tab(4),write_word('is true'),
    write_rule_certainty(C).
display_rule((A # C :- B)) :-
    copy((A,B),(A1,B1)),
    nl,write('  > '),write_word('Because'),
    nl,tab(4),write_word('IF'),
    nl,tab(6),display_rule_body(B1,65,NextChar),
    nl,tab(4),write_word('THEN'),
    nl,tab(6),display_rule_body(A1,NextChar,_),
    write_rule_certainty(C).

display_rule_body(all(A,not(non(B))),Char,NextChar) :- !,
    tvar_list(A,VL),bind_var_char(VL,Char,NextChar),
    univ_quantify(yes,Q),print_goal(B,Q).
display_rule_body(all(non(A),not(B)),Char,NextChar) :- !,
    tvar_list(A,VL),bind_var_char(VL,Char,NextChar),
    univ_quantify(no,Q),print_goal(B,Q).
display_rule_body(not(non(A)),Char,Char) :- !,
    univ_quantify(yes,Q),print_goal(A,Q).
display_rule_body(not(A),Char,Char) :- !,
    univ_quantify(no,Q),print_goal(A,Q).
display_rule_body((A,B),Char,NextChar) :- !,
    display_rule_body(A,Char,Char1),
    nl,tab(6),display_rule_body(B,Char1,NextChar).
display_rule_body(A,Char,NextChar) :-
    tvar_list(A,VL),bind_var_char(VL,Char,NextChar),
    print_goal(A,_).


% DISPLAY PROOF
% --------------------------------------------------------------------
display_proof((A # C :- (B,1))) :-
    member(B,[fact,system:true,usergiven]),!,
    nl,write('  > '),write_word('Because'),
    nl,tab(6),copy(A,A1),display_goal(A1,yes),
    nl,tab(4),write_word(is),write(' '),write_word(B),
    write(' '),write_rule_certainty(C).
display_proof((A # 0 :- (B,0))) :-
    member(B,[nofact,system:false,nonterminate]),!,
    nl,write('  > '),write_word('Because'),
    nl,tab(6),copy(A,A1),display_goal(A1,some),
    nl,tab(4),write_word(is),write(' '),write_word(B).
display_proof((A # C :- Body)) :-
    nl,write('  > '),write_word('Because'),
    nl,tab(4),write_word('IF'),
    nl,tab(6),display_proof_body(Body),
    nl,tab(4),write_word('THEN'),
    nl,tab(6),display_proof_body((A,C)).

display_proof_body(((B,Bs),(C,Cs))) :- !,
    display_proof_body((B,C)),
    nl,tab(6),display_proof_body((Bs,Cs)).
display_proof_body((B:nottried,C)) :- !,
    write('...').
display_proof_body((B,C)) :-
    (C = 0,!,Q = some; Q = yes),
    copy(B,B1),
    display_goal(B1,Q),
    write_rule_certainty(C).


% DISPLAY GOALS
% --------------------------------------------------------------------
display_goal(all(A,not(non(B))),S) :- !,
    univ_quantify(S,Q),univ_quantify(yes,Q1),
    tvar_list(A,VL),bind_vars(VL,Q),
    print_goal(B,Q1).
display_goal(all(non(A),not(B)),S) :- !,
    univ_quantify(S,Q),univ_quantify(no,Q1),
    tvar_list(A,VL),bind_vars(VL,Q),
    print_goal(B,Q1).
display_goal(not(non(A)),S) :- !,
    (S = no,!,univ_quantify(some,Q),print_goal(non(A),Q);
     univ_quantify(yes,Q), print_goal(A,Q)).
display_goal(not(A),S) :- !,
    (S = no,!,univ_quantify(some,Q),print_goal(A,Q);
     ground(A),!,print_goal(non(A),_);
     univ_quantify(no,Q), print_goal(A,Q)).
display_goal(non(A),S) :- !,
    (S = no,!,univ_quantify(yes,Q),print_goal(A,Q);
     univ_quantify(S,Q), print_goal(non(A),Q)).
display_goal((A,B),no) :- !,
    (ground((A,B)),!,write_word(no); write_word(none)).
display_goal((A,B),yes) :- !,
    display_goal(A,yes),
    nl,tab(4),display_goal(B,yes).
display_goal(A,S) :-
    (S = no,ground(A),!,print_goal(non(A),_);
    univ_quantify(S,Q),print_goal(A,Q)).

univ_quantify(yes,Q) :- trans_word(all,Q).
univ_quantify(no,Q) :- trans_word(no,Q).
univ_quantify(some,Q) :- trans_word(some,Q).


% PRINT GOALS
% --------------------------------------------------------------------
print_goal(non(call(A)),Q) :- !,print_goal(non(A),Q).
print_goal(non(A),Q) :- !,
    domain(A),
    (predicate(PNA,non(A)),!,print_phrase(PNA,Q);
     predicate(PA,A),negate_quantify(Q,Q1,W),
     print_phrase([W|PA],Q1)).
print_goal(A,Q) :-
    domain(A),
    predicate(Phrase,A),!,
    print_phrase(Phrase,Q).
print_goal(call(A),Q) :- !,
    print_goal(A,Q).
print_goal(A,Q) :-
    write(Q),write(' '),write(A).

print_phrase([],_) :- !.
print_phrase([(T,S:X)|Rest],Q) :- !,
    (var(X),!,write_list([Q,' ',S,' ']);
     quantifier(X),!,write_list([X,' ',S,' ']);
     write_list([T,' ',X,' '])),
    print_phrase(Rest,Q).
print_phrase([W|Rest],Q) :-
    write_list([W,' ']),
    print_phrase(Rest,Q).

negate_quantify(Q,Q1,W) :-
    trans_word(deny,W),
    (trans_word(some,Q),trans_word(all,Q1);
     trans_word(all,Q),trans_word(some,Q1)).

quantifier(X) :- trans_word(W,X),member(W,[all,no,some]).

%---------------------------------------------------------------------
% META-PROGRAMMING TOOLS
% ====================================================================

% BIND VARIABLES
% --------------------------------------------------------------------
bind_var_char([],Char,Char) :- !.
bind_var_char([T:X|Xs],Char,NextChar) :-
    name(X,[Char]),Char1 is Char + 1,
    bind_var_char(Xs,Char1,NextChar).

bind_vars([],_) :- !.
bind_vars([T:S|Xs],S) :- bind_vars(Xs,S).

ground(A) :- copy(A,B), A == B.

bind_var([],_) :- !.
bind_var(['$@#'(N)|Vs],N) :- N1 is N+1,bind_var(Vs,N1).

instanc(A,B) :-
     ground(A),!,A = B.
instanc(A,B) :-
     functor(A,F,N),functor(B,F,N),
     var_list(A,VL),bind_var(VL,1),A = B.

% -------------------------------------------------------------------
% FIND THE VARIABLES OF A GOAL
% ===================================================================

var_list(A,[]) :- ground(A),!.
var_list(A,L) :-  collect_var(A,Q-[]),
%    setof(X,member(X,Q),L).
     elim_dup(Q,[],L).

collect_var(A,Q-Q) :- constant(A),!.
collect_var(A,[A|Q]-Q) :- var(A), !.
collect_var(A,Q) :- A =.. [P|Args],collect_vars(Args,Q).

collect_vars([],Q-Q) :- !.
collect_vars([A|As],Q-Qt) :-
     collect_var(A,Q-Qs),collect_vars(As,Qs-Qt).

constant(A) :- atom(A); integer(A); float(A).

elim_dup([],_,[]).
elim_dup([X|Xs],L,L1) :- occurs(X,L),!,elim_dup(Xs,L,L1).
elim_dup([X|Xs],L,[X|L1]) :- elim_dup(Xs,[X|L],L1).

occurs(T:X,_) :- nonvar(X).
occurs(X,[Y|Ys]) :- X == Y; occurs(X,Ys).

%--------------------------------------------------------------------
% EXTRACT VARIABLE SYMBOLS FROM A FORMULA
% ===================================================================

list_var_symbols(L,Q,VT,VS) :-
    append(A,[B,C|D],L),
    (quote_found(Q,B,Q1),!,
        list_var_symbols([C|D],Q1,VT,VS);
     var_found(Q,B,C),!,
        append(A1,[C1|D1],[C|D]),not(valid(C1)),!,
        name(V,A1),
        (member(V,VT),\+(V = '_'), VS = VS1,!; VS = [V|VS1]),
        list_var_symbols([C1|D1],0,[V|VT],VS1)).
list_var_symbols(_,_,_,[]).

quote_found(0,B,B) :- member(B,[34,36,39]),!.
quote_found(Q,Q,0).

var_found(0,B,C) :- \+(valid(B)),var_start(C).

var_start(C) :- (65 =< C,C =< 90);C = 95.
valid(C) :-  (65 =< C, C =< 90);    % A - Z
             (97 =< C, C =< 122);   % a - z
             (48 =< C, C =< 57);    % 0 - 9
             C = 95; C = 45.  % underscore; hyphen

member(X,[X|_]).
member(X,[_|T]) :- member(X,T).

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

%---------------------------------------------------------------------
% ESSLN LOGICAL NEGATION EVALUATOR
% ====================================================================

builtin((non(A) # 1 :- tvar_list(A,L),eval_non(A,L))).

builtin((eval_non(A,L) # 1 :- not(A),!)).
builtin((eval_non(A,L) # 1 :- eval(A),uninstantiated(L),!,fail)).
builtin((eval_non(A,L) # 1 :- instantiate(A,L,VL),eval_non(A,VL))).

builtin((eval(A) # 1 :- A,!)).

    uninstantiated(L) :- tvar_pure(L),unrestricted(L,0).

    tvar_pure([]) :- !.
    tvar_pure([T:V|TVs]) :- var(V),tvar_pure(TVs).

    unrestricted([],_) :- !.
    unrestricted([T:N|TVs],N) :- N1 is N+1,unrestricted(TVs,N1).

    instantiate(A,L,VL) :- domain(A),instant(L,VL).

    instant([X|Xs],Xs) :- get_term(X,Xs).
    instant([X|Xs],[X|VL]) :- instant(Xs,VL).

    get_term(T:V,TVs) :- constsymbol(T:V).
    get_term(X,Xs) :- get_var(X,Xs).

    get_var(T:V,[T:V|TVs]).
    get_var(X,[Y|Xs]) :- get_var(X,Xs).

    tvar_list(A,[]) :- ground(A),!.
    tvar_list(A,L) :-  A =.. [P|Args],
%       setof(T:X,(member(T:X,Args),var(X)),L).
        elim_dup(Args,[],L).

%====================================================================

% PRINT PROOF
  print_proof(Proof) :-
      printp(Proof,1),
      get0(C),get0(D),get0(E).

  printp((A,B),N) :- !,
      nl,write('<'), printq(A,N), write('>'),
      printp(B,N).
  printp(A,N) :-
      nl,write('<'), printq(A,N), write('>').

  printq((A,B),N) :- !,
      printq(A,N),nl,write(' '),printq(B,N).
  printq((A :- B),N) :- !,
      write_space(N),write(A),write(' :- '),
      get0(C),N1 is N+1,
      nl,write(' '),printq(B,N1).
  printq(A,N) :-
      write_space(N),write(A),
      get0(C).

  write_space(0) :- !.
  write_space(N) :-
      write('  '),N1 is N-1,write_space(N1).

% --------------------------------------------------------------------
% DISPLAY ESSLN'S BANNER
% ====================================================================
display_banner :-
   % cls,
    banner,
    get0(C),(C = 101,!,fail; true).

banner :-
    nl,nl,
    tab(4),put(218),place(65,196),put(191),nl,
    blankline,
    halfline(l,15),put(194),place(7,196),put(191),tab(13),put(194),
    halfline(r,27),
    halfline(l,15),put(179),tab(21),put(179),tab(6),put(194),
    place(5,196),put(191),halfline(r,14),
    halfline(l,15),put(179),tab(6),put(218),place(6,196),tab(8),
    put(179),tab(6),put(179),tab(5),put(179),halfline(r,14),
    halfline(l,15),put(195),place(4,196),put(180),put(32),put(179),
    tab(6),put(218),place(6,196),put(32),put(179),tab(6),
    put(179),tab(5),put(179),halfline(r,14),
    halfline(l,15),put(179),tab(6),put(192),place(5,196),put(191),
    put(179),tab(7),put(179),tab(6),
    put(179),tab(5),put(179),halfline(r,14),
    halfline(l,15),put(179),tab(12),put(179),put(192),place(5,196),
    put(191),put(32),put(179),tab(6),
    put(179),tab(5),put(179),halfline(r,14),
    halfline(l,15),put(193),place(7,196),put(217),tab(4),
    put(179),tab(6),put(179),put(32),put(193),
    place(5,196),put(217),put(193),
    tab(5),put(193),halfline(r,14),
    halfline(l,19),place(9,196),put(217),tab(6),put(179),
    halfline(r,29),
    halfline(l,26),place(9,196),put(217),halfline(r,29),
    blankline,
    halfline(l,16),write('A MULTILINGUAL EXPERT SYSTEM SHELL'),
    halfline(r,15),
    halfline(l,17),write('THAT SUPPORTS QUANTIFIED QUERIES'),
    halfline(r,16),
    blankline,
    halfline(l,12),write('Copyright : Van Le, University of Canberra'),
    halfline(r,11),
    tab(4),put(192),place(65,196),put(217),nl,    
    tab(5),write('Press any key to start, "e" to exit').

blankline :-
    tab(4),put(179),tab(65), put(179),nl.

halfline(_,0).
halfline(l,N) :- tab(4),put(179),tab(N).
halfline(r,N) :- tab(N),put(179),nl.
     
place(0,_).
place(N,C) :- put(C), M is N - 1, place(M,C).

%-------------------------------------------------------------

display_languages :-
    % cls,
    nl,nl,
    tab(16),write('PLEASE CHOOSE A LANGUAGE FOR OUR CONVERSATION'),
    nl,nl,nl,
    tab(16),write('1. Algerian'),
    tab(20),write(' 9. Philippino'),nl,
    tab(16),write('2. Dutch'),
    tab(23),write('10. Portuguese'),nl,
    tab(16),write('3. English'),
    tab(21),write('11. Romanian'),nl,
    tab(16),write('4. French'),
    tab(22),write('12. Senegalese'),nl,
    tab(16),write('5. Indonesian'),
    tab(18),write('13. Spanish'),nl,
    tab(16),write('6. Italian'),
    tab(21),write('14. Swedish'),nl,
    tab(16),write('7. Moroccain'),
    tab(19),write('15. Vietnamese'),nl,
    tab(16),write('8. Norwegian'),
    tab(19),write('16. Other'),nl,nl,
    tab(16),write('Your language of choice: ').

learn_new(Language) :-
    tab(16),write('This language is not available in this package.'),
    nl,tab(16),write('Please use English, French or Vietnamese.'),
    pause(2000),choose_language.

pause(0).
pause(N) :-
    N > 0, N1 is N-1,
    pause(N1).

% ====================================================================
% ============================ END ESSLN =============================
% ====================================================================
