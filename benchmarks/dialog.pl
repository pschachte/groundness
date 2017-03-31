% CVS: $Id: dialog.pl,v 1.3 1998/10/21 04:25:53 pets Exp $
% A simple user-system dialogue program
%------------------------------------------------------------------
% consult this program and invoke the system by entering the query:
%   ?- dialogue.
% then enter the following query in text form:
%   Q:> find student name and test mark
%       where student id is 91-2123 and test id is 91-2123.
% Extend the student database below and try a number of queries of
% the above form.
%------------------------------------------------------------------

goal :- dialogue.

    dialogue :-
        repeat,
           receive_query(Query),
           response(Query,Answer),
           print_answer(Answer),
        Answer = [bye].

    response(Query,Answer) :-
        parse(Query,GoalList,Answer),
        convert(GoalList,Goal),
        call(Goal).
    response([bye],[bye]).

% FIGURE 5.15: A simple parser
%----------------------------------------------------
    parse([W|WList],Goals,Answer) :-
        valid_command(W),
        split(WList,Query,Data),
        construct(Query,Goals,Answer),
        instantiate(Goals,Data).

    split(WList,Query,Data) :-
        append(Query,[where|Data],WList),!.
    split(WList,WList,[]).

    construct([and|Qs],Goals,Answer) :- !,
        construct(Qs,Goals,Answer).
    construct([P,A|Qs],[G|Gs],[P,A,X|Ans]) :-
        match_arg(P,A,G,X),
        construct(Qs,Gs,Ans).
    construct([],[],[]).

    convert([G],(G)) :- !.
    convert([G|Gs],(G,Ys)) :-
        convert(Gs,Ys).

    instantiate(Goals,[and|Data]) :- !,
        instantiate(Goals,Data).
    instantiate(Goals,[P,A,is,C|Data]) :-
        match_arg(P,A,G,C),
        member(G,Goals),
        instantiate(Goals,Data).
    instantiate(_,[]).

    match_arg(P,A,G,X) :-
        schema(P,N,Args),functor(G,P,N),
        index(A,Args,I),arg(I,G,X).

    index(A,[A|Args],1) :- !.
    index(A,[B|Args],I) :-
        index(A,Args,J), I is J+1.

    schema(student,2,[id,name]).
    schema(test,2,[id,mark]).

    valid_command(find).
    valid_command(show).
    valid_command(give).


% Print the answer
%-----------------------------------------------------
    print_answer(Answer) :-
        nl,nl, write('A:> '),
        print_line(Answer).

    print_line([]) :- !, nl.
    print_line([H|T]) :-
        write(H),write(' '),
        print_line(T).


% Student database
%-------------------------------------
    student('90-4032','Adams P.T.').
    student('86-1024','Bolland K.').
    student('88-1260','Collins L.').
    student('91-2123','Smith J.R.').

    test('90-4032',90).
    test('86-1024',54).
    test('88-1260',65).
    test('91-2123',40).

    member(X,[X|_]).
    member(X,[_|T]) :- member(X,T).

    append([],L,L).
    append([H|T],L,[H|R]) :- append(T,L,R).

% Read a sentence of several lines, terminated by a period.
%---------------------------------------------------------
    receive_query(L) :-
        nl,write('Q:> '),
        read_sentence(L).

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

    get_char(C) :- get0(C),(C = 13,!,nl,tab(4); true).
    valid_char(C) :-
        96 < C, C < 123;  % a-z
        64 < C, C < 91;   % A-Z
        47 < C, C < 58;   % 0-9
        C = 45.        % hyphen.

    start_word(C,C)   :- valid_char(C),!.
    start_word(39,C)  :- get_char(C).
    end_word(39,39,C) :- get_char(C).  % quotation marks
    end_word(Q,C,C)   :- Q \== 39,C \== 8,not(valid_char(C)).
    end_sent(C)       :- (C = 46; C = 63). % period or ?

    legal_char(39,_) :- !.
    legal_char(_,C)  :- valid_char(C).

    reverse([],L,L).
    reverse([H|T],L,L1) :- reverse(T,[H|L],L1).

%===================================================================




