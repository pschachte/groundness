% CVS: $Id: robot.pl,v 1.3 1998/10/21 04:26:09 pets Exp $
% PROBLEM 7.5: Instruct a robot to perform some house-work
%-----------------------------------------------------------
% Note: If your Prolog system has a built-in predicate
% "argrep", then use the comment symbols % to neutralize its
% definition given at the end of this program.
% Consult this program and enter the following query to find
% a sequence of actions the robot should take to clean the
% house:
%   ?- depth_first_search(Answer),
%      print_answer(Answer),get0(_).
% Inspect the answer and observe some unnecessary actions.
% Press any key to terminate.
% Now, rerun the program with best_first_search using the
% query:
%   ?- best_first_search(Answer),
%      print_answer(Answer),get0(_).
% Compare the answer with the one previously obtained.
% Press any key to terminate.
% Now, rerun the program with best_cost_search using the
% query:
%   ?- best_cost_search(Answer),
%      print_answer(Answer),get0(_).
% Compare the result with that produced by best_first_search.
% Press any key to terminate.
%------------------------------------------------------------

goal :- depth_first_search(Answer), print_answer(Answer).
goal :- best_first_search(Answer), print_answer(Answer).
goal :- best_cost_search(Answer), print_answer(Answer).



    initial_state((none,(2,[1,0,(1,0)]-[1,1,(0,0)]))).
    final_state((_,(R,[0,0,(0,0)]-[0,0,(0,0)]))) :-
         R = 1; R = 2.

    next_state((_,(I,States)),(Action,(I,NewStates))) :-
        I \== 0, arg(I,States,RmState),
        work(I,RmState,NewRmState,Action),!,
        argrep(States,I,NewRmState,NewStates).
    next_state((_,(0,States)),(Action,(0,NewStates))) :-
        dispose_trash(States,NewStates,Action),!.
    next_state((_,(I,States)),(move(I,J),(J,States))) :-
        J is (I+1) mod 3; J is (I+2) mod 3.

    work(I,[1,C,B],[0,1,B],dust_room(I)).
    work(I,[0,1,(B,P)],[0,0,(1,P)],vacuum_room(I)).
    work(I,[0,0,(1,0)],[0,0,(1,1)],pickup_bin(I)).
    work(I,[0,0,(0,1)],[0,0,(0,0)],putdown_bin(I)).

    dispose_trash([0,0,(1,1)]-R2,[0,0,(0,1)]-R2,empty_bin(1)).
    dispose_trash(R1-[0,0,(1,1)],R1-[0,0,(0,1)],empty_bin(2)).

    print_answer([]) :- nl.
    print_answer([X|T]) :-
        write(X),nl,print_answer(T).


% Program depth-first search
%------------------------------------------------
    depth_first_search(AnsPath) :-
        initial_state(Init),
        depth_first([Init],AnsPath).

    depth_first([S|Path],[S]) :-
        final_state(S),!.
    depth_first([S|Path],[S|AnsPath]) :-
        extend([S|Path],S1),
        depth_first([S1,S|Path],AnsPath).

    extend([S|Path],S1) :-
        next_state(S,S1),
        not(member_state(S1,[S|Path])).

    member_state((_,X),[(_,X)|_]).
    member_state(X,[_|T]) :- member_state(X,T).

% Program best-first-search
%-------------------------------------------------------
    best_first_search(AnsPath) :-
        initial_state(Init),value(Init,V),
        best_first([[V,Init]],AnsPath).

    best_first([[_,S|Path]|_],AnsPath) :-
        final_state(S),!,
        reverse([S|Path],[],AnsPath).
    best_first([Path|Rest],AnsPath) :-
        expand(Path,NPaths),
        merge(NPaths,Rest,NewList),
        best_first(NewList,AnsPath).

    expand([_|Path],NPaths) :-
        setof([V,S|Path],
              (extend(Path,S),value(S,V)),NPaths),!.
    expand(_,[]).

    % extend is given in procedure depth-first-search

    reverse([],L,L).
    reverse([H|T],L,R) :- reverse(T,[H|L],R).

    merge([],L,L) :- !.
    merge(L,[],L) :- !.
    merge([X|P],[Y|Q],[X|R]) :-
        less(X,Y),!,merge(P,[Y|Q],R).
    merge(L,[Y|Q],[Y|R]) :-
        merge(L,Q,R).

    less([V1|Path1],[V2|Path2]) :- V1 < V2.

% The evaluation function to be used in best-first-serach
%--------------------------------------------------------
    value((_,(I,R1-R2)),Value) :-
        room_value(I,1,R1,V1),
        room_value(I,2,R2,V2),
        Value is V1 + V2 - I.

    room_value(I,J,[F,C,(B,P)],V) :-
        travel_need(I,J,TR),
        travel_need(I,0,TC),
        bin_state(B,P,BD,GC),
        V is TR + F*7 + (1-F)* (C*6 +
           (1-C)*BD* (3 + 2* (B-P) + GC* (TC-TR))).

    travel_need(I,I,0) :- !.
    travel_need(I,J,1).

    bin_state(0,0,0,0).
    bin_state(0,1,1,0).
    bin_state(1,0,1,0).
    bin_state(1,1,1,1).


% Program best-cost-serach
%-------------------------------------------------------
    best_cost_search(AnsPath) :-
        initial_state(Init),
        best_cost([[0,Init]],AnsPath).

    best_cost([[_,S|Path]|_],AnsPath) :-
        final_state(S),!,
        reverse([S|Path],[],AnsPath).
    best_cost([Path|Rest],AnsPath) :-
        expand(Path,NPaths),
        merge(NPaths,Rest,NewList),
        best_cost(NewList,AnsPath).

    expand([C,S|Path],NPaths) :-
        setof([C1,S1,S|Path],
              (extend([S|Path],S1),
               costsum(S,C,S1,C1)),NPaths),!.
    expand(_,[]).

    costsum(S,C,S1,C1) :-
        cost(S,S1,D), C1 is C + D.


% The cost function to be used in best-cost-search
%-------------------------------------------------
    cost(_,(dust_room(I),_),6).
    cost(_,(vacuum_room(I),_),8).
    cost(_,(pickup_bin(I),_),3).
    cost(_,(putdown_bin(I),_),1).
    cost(_,(empty_bin(I),_),5).
    cost(_,(move(I,J),_),C) :-
        I+J =:= 3,!, C = 4; C = 10.

%-------------------------------------------------
% Definition of "argrep"
%-------------------------------------------------
    argrep(Term,N,Value,NewTerm) :-
        Term =.. [F|L],
        replace(N,L,Value,L1),
        NewTerm =.. [F|L1].

    replace(1,[X|L],Y,[Y|L]) :- !.
    replace(N,[X|L],Y,[X|L1]) :-
        N > 1, N1 is N-1,
        replace(N1,L,Y,L1).

%=================================================





