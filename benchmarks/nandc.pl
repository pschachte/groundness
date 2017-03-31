% CVS: $Id: nandc.pl,v 1.3 1998/10/20 03:23:40 pets Exp $
goal :- play(Result).

/* MSc Prolog Tutorial Exercise 5 (Assessable)

        Author: David Green, davidg@aipna.

        Date:   February 1991.

        Description: "A program to play an interactive game of noughts &
            crosses."

        Predicates provided by this file:

    initialise/2    - sets up the start board and first player.

    end_state/2 - checks if a game state gives an ending result.

    announce/1  - announces an ending result.

    display_game/2  - displays current game state and player to play.

    choose_move/3   - obtains move for a given player for a given state.

    move/3      - implements legal moves and updates the game state.

    next_player/2   - alternates between two different players.

    
    Other predicates used:

    play/1      - as provided in the exercise description.

    play/3      - as provided in the exercise description.

    member/2    - test and/or generator for members of a list.

    display_board/1 - subpredicate used to display current game state.

    replace/4   - replaces positions on board with 'o' or 'x'.


        Implementation:

    The most interesting thing about this program is its use of a 3x3
    'magic square' to represent the playing area. This means that each
    position on the noughts & crosses board is given a numerical value:

            8 3 4
            1 5 9
            6 7 2

    It is a property of this magic square that each winning line in
    noughts & crosses has a total sum of 15. Thereore, 15 as a sum
    of three numbers uniquely defines a particular winning position of
    the game. The game state is stored as a list of the numbers which
    have been 'taken' so far by the user, a list of the numbers which
    have similarly been taken by the computer, and finally a list
    representing the current board state, in which positions which
    have previously been filled by either player are marked with an
    'o' or an 'x' as is appropriate. Furthermore, this representation
    makes it possible to refer to any vacant board position by a
    single number from 1 to 9.

    Although this representation may seem somewhat bizarre at first
    glance, it does seem to suit the game particularly well, given
    the strong isomophism between 'getting a line of 3' and obtaining
    the sum total of 15 from 3 numbers. And, as we will see, it does
    simplify the code needed to produce reasonable computer moves.

*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: play/1.
%%% Arguments:
%%%     Result      - the result of a completed game.
%%%
%%% This is the top-level predicate, as provided by the exercise.
%%%

play(Result):-                          % To play, with some final Result:
        initialise(State, Player),      % First make an initial game state
        display_game(State, Player),    % Then display the game
        play(State, Player, Result).    % Then play the game, with some Result



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: play/3.
%%% Arguments:
%%%     State       - the current game state.
%%%     Player      - the current player.
%%%     Result      - the result of a completed game.
%%%
%%% Once again, as provided by the exercise, only the calls to next_player
%%% and display_game in the second case have been exchanged so as to make
%%% more sense to the user when the player 'to play' message is given.
%%%

%%% Base case:      checks if an end-state of the game has been reached.

play(State, _, Result):-
        end_state(State, Result), !,    % If some terminating state is reached
        announce(Result).               % Then just report the result


%%% Recursive case:     continues playing until an end-state is reached.

play(State, Player, Result):-           % Otherwise, the current player
        choose_move(State, Player, Move),% chooses a move.
        move(Move, State, State1),      % That move is implemented
    next_player(Player, Player1),   % We find out who the next player is
        display_game(State1, Player1),!,% The game is redisplayed
    play(State1, Player1, Result).  % And give him/her/it a turn



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: member/2
%%% Arguments:
%%%     Item        - item to be checked or obtained.
%%%     List        - list of which Item is a potential member.
%%%
%%% The usual test/generator for list membership.
%%%

member(X,[X | _]).

member(X,[_ | Y]) :-
        member(X,Y).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: initialise/2.
%%% Arguments:
%%%     State       - the starting game state.
%%%     Player      - the player who goes first.
%%%
%%% This predicate initialises the game state to two empty lists and the
%%% 'magic square' described under 'Implementation'. It would be quite easy
%%% to change this predicate so that the computer player could start instead.
%%%

initialise( [ [], [], [8,3,4,1,5,9,6,7,2] ] , user ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: announce/1.
%%% Arguments: 
%%%     Result      - the result of a completed game.
%%%
%%% - simply announces the result of a completed game.
%%%

announce(Result) :-
    nl, write(Result), nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: display_game/2.
%%% Arguments:
%%%     State       - current game state.
%%%     Player      - current player (i.e. next to play)
%%%
%%% This predicate displays the board on the screen via a call to display_
%%% board/1. It then says which player is to go next.
%%%

display_game( [ _, _, Board], Player ) :-
    nl,
    display_board(Board), 
    write(Player), write(' to play.'), nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: display_board/1.
%%% Arguments:
%%%     State       - current game state.
%%%
%%% This predicate displays the board on the screen via a number of write
%%% instructions. Very little special processing of the board-state list
%%% is required for this.
%%%

%%% Base case:      when there is only 3 items in the board-state list.

display_board([A, B, C]) :-
    write(A),write(' | '),write(B),write(' | '),write(C),nl,nl.


%%% Recursive case: print out first three items on the list, then recurse.

display_board([A, B, C | Tail ]) :-
    write(A),write(' | '),write(B),write(' | '),write(C),nl,
    write('---------'),nl,
    display_board(Tail).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: end_state/2.
%%% Arguments:
%%%     State       - current game state.
%%%     Result      - the result of a completed game.
%%%
%%% This predicate checks to see if the game has reached a legal end state;
%%% there are three distinct conditions under this may occur. If one of
%%% them applies, the predicate returns the appropriate result. Checking
%%% to see if one of the players has won is made easy by the magic square.
%%%

%%% 'Draw' condition:       when a total of 9 moves have been made.

end_state( [Usermoves, Compmoves, Board], [its_a_draw]) :-
    length(Usermoves, X),
    length(Compmoves, Y),
    9 is X + Y.


%%% 'User wins' condition:  when the user has three squares whose
%%%             total value is equal to 15.

end_state( [ Usermoves, _, Board], [user_wins_with_line, A, B, C]) :-
    member(A, Usermoves),
    member(B, Usermoves),
    \+ B == A,
    member(C, Usermoves),
    \+ C == B,
    \+ C == A,
    15 is A + B + C.


%%% 'Computer wins' condition:  as above, only for the computer.

end_state( [ _, Compmoves, Board], [computer_wins_with_line, A, B, C]) :-
    member(A, Compmoves),
    member(B, Compmoves),
    \+ B == A,
    member(C, Compmoves),
    \+ C == B,
    \+ C == A,
    15 is A + B + C.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: choose_move/3.
%%% Arguments:
%%%     State       - the current game state.
%%%     Player      - player whose turn it is to move.
%%%     Move        - a legal move for that player (also
%%%               specifies the player who made the move).
%%%
%%% This is a general-purpose predicate which both obtains the move of
%%% the user and selects appropriate moves for the computer. There are
%%% 5 possible conditions which can apply under these circumstances.
%%%

%%% Condition 1 - the user enters a legal move.

choose_move( [_, _, Board], user, [user, Move] ) :-
    write('Enter the number of the square you wish to move to'), nl,
    read(Move),
    number(Move),
    member(Move, Board), !.


%%% Condition 2 - condition (1) must have failed, therefore the user
%%%     attempted to enter an illegal move. So repeat the request.

choose_move( [_, _, Board], user, [user, Move]) :-
    write('ILLEGAL MOVE'), nl,
    choose_move( [_, _, Board], user, [user, Move]).


%%% Condition 3 - it's the computer's turn, and it has an opportunity to
%%%     win. So select that move.

choose_move( [ Usermoves, Compmoves, Board ], computer, [computer, Move] ) :-
    member(A, Compmoves),
    member(B, Compmoves),
    \+ B == A,
    member(Move, Board),
    number(Move),
    15 is A + B + Move.


%%% Condition 4 - it's the computer's turn, and it needs to block the user,
%%%     who has 2 unblocked x's in a row. Select that move.

choose_move( [ Usermoves, Compmoves, Board ], computer, [computer, Move] ) :-
    member(A, Usermoves),
    member(B, Usermoves),
    \+ B == A,
    member(Move, Board),
    number(Move),
    15 is A + B + Move.


%%% Condition 5 - conditions 3 and 4 must have failed, so the computer selects
%%%     the next available move from the list of 'good ones' provided. 

choose_move( [_, _, Board], computer, [computer, Move]) :-
    member(Move, [8,5,4,2,6,7,9,1,3]),
    member(Move, Board).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: move/3.
%%% Arguments:
%%%     Move        - A legal move for a player.
%%%     State       - the current game state.
%%%     Newstate    - the new game state after that move.
%%%
%%% This implements the move selected by choose_move/3. The way in which
%%% the move is implemented depends on which player has made it, so this
%%% information is used to select the appropriate predicate. Basically
%%% move/3 adds the move to the list of moves already made by that player
%%% and updates the board-state via a call to replace/4.
%%%

%%% Condition 1 - the user has placed an 'x', so add to the first list.

move(   [user, Move], 
    [ Usermoves, Compmoves, Board ], 
    [ [ Move | Usermoves ], Compmoves, Newboard ]  ) :-
    
    replace(Move, x, Board, Newboard).


%%% Condition 2 - the computer has placed an 'o', so add to the second list.

move(   [computer, Move], 
    [ Usermoves, Compmoves, Board ], 
    [ Usermoves, [ Move | Compmoves ], Newboard ]  ) :-

    write('Computer moves to square '), write(Move), nl,
    replace(Move, o, Board, Newboard).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: replace/4.
%%% Arguments:
%%%     Move        - the square to be replaced.
%%%     Item        - the 'x' or 'o' to replace it with.
%%%     State       - the current board state.
%%%     Newstate    - the new board state afterwards.
%%%

%%% Base case:      the square to be replaced is the head of the list.

replace(Move, Item, [ Move | Tail ], [ Item | Tail ]).


%%% Recursive case:     keep going down the list until the base case applies.

replace(Move, Item, [ Head | Tail ], [ Head | Newtail ]) :-
    \+ Head == Move,
    replace(Move, Item, Tail, Newtail).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATE: next_player/2.
%%% Arguments:
%%%     Player      - the current player.
%%%     Newplayer   - the new current player.
%%%
%%% This predicate simply swaps between the two players. The clauses used
%%% are fairly self-explanatory.
%%%

next_player(user, computer).

next_player(computer, user).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

What follows is a transcript of a typical interaction with the program:
=======================================================================

Script started on Tue Feb 19 19:05:27 1991

SICStus 0.6 #13: Thu Dec 20 17:25:28 GMT 1990
Copyright (C) 1987, Swedish Institute of Computer Science.
All rights reserved.
| ?- [ex7].
{consulting /usr/oak/mscs.90/davidg/prolog/ex7.pl...}
{ex7 consulted, 1060 msec 7356 bytes}

yes
| ?- play(X).

8 | 3 | 4
---------
1 | 5 | 9
---------
6 | 7 | 2

user to play.
Enter the number of the square you wish to move to
|: 1.

8 | 3 | 4
---------
x | 5 | 9
---------
6 | 7 | 2

computer to play.
Computer moves to square 8

o | 3 | 4
---------
x | 5 | 9
---------
6 | 7 | 2

user to play.
Enter the number of the square you wish to move to
|: 3.

o | x | 4
---------
x | 5 | 9
---------
6 | 7 | 2

computer to play.
Computer moves to square 5

o | x | 4
---------
x | o | 9
---------
6 | 7 | 2

user to play.
Enter the number of the square you wish to move to
|: 2.

o | x | 4
---------
x | o | 9
---------
6 | 7 | x

computer to play.
Computer moves to square 4

o | x | o
---------
x | o | 9
---------
6 | 7 | x

user to play.
Enter the number of the square you wish to move to
|: 6.

o | x | o
---------
x | o | 9
---------
x | 7 | x

computer to play.
Computer moves to square 7

o | x | o
---------
x | o | 9
---------
x | o | x

user to play.
Enter the number of the square you wish to move to
|: 9.

o | x | o
---------
x | o | x
---------
x | o | x

computer to play.

[its_a_draw]

X = [its_a_draw] ? 

yes

| ?- play(X).

8 | 3 | 4
---------
1 | 5 | 9
---------
6 | 7 | 2

user to play.
Enter the number of the square you wish to move to
|: 5.

8 | 3 | 4
---------
1 | x | 9
---------
6 | 7 | 2

computer to play.
Computer moves to square 8

o | 3 | 4
---------
1 | x | 9
---------
6 | 7 | 2

user to play.
Enter the number of the square you wish to move to
|: 6.

o | 3 | 4
---------
1 | x | 9
---------
x | 7 | 2

computer to play.
Computer moves to square 4

o | 3 | o
---------
1 | x | 9
---------
x | 7 | 2

user to play.
Enter the number of the square you wish to move to
|: 1.

o | 3 | o
---------
x | x | 9
---------
x | 7 | 2

computer to play.
Computer moves to square 3

o | o | o
---------
x | x | 9
---------
x | 7 | 2

user to play.

[computer_wins_with_line,3,4,8]

X = [computer_wins_with_line,3,4,8] ? 

yes


*/
