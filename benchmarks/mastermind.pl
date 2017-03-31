% CVS: $Id: mastermind.pl,v 1.3 1998/10/20 03:23:37 pets Exp $
goal :-  play.

clause(X, Y).
put(X).
abolish(X, Y).

/*  MASTERMIND.PL  */


/****************************************************************/
/*                                                              */
/*                          Mastermind                          */
/*                                                              */
/*     Reference: Emde, M.v., Relational Programming,           */
/*                Research Report, CS-78-48, DCS, Univ. of      */
/*                Waterloo, Canada, 1978                        */
/*                                                              */
/*     also in:   Coelho, H., Cotta, J.C., Prolog by Example,   */
/*                Symbolic Computation, Springer-Verlag,        */
/*                Berlin, 1988.                                 */
/*                                                              */
/*     reimplemented by Herbert Koenig and Thomas Hoppe 1983    */
/*                                                              */
/*                 Technical University of Berlin               */
/*                  Faculty for Computer Science                */
/*                                                              */
/* Description: This program implements the game of mastermind. */
/*              either the user or the program tries to break   */
/*              the hidden color code, consisting of the colors */
/*              black, blue, red, green, yellow, white. The     */
/*              result of a guess (black, a color is at the     */
/*              right position; white, a color occurs in the    */
/*              code) are internally represented as successors  */
/*              of 0. This could be extended to handle integers.*/
/*              If the programm guesses, a simple but powerful  */
/*              generate-and-test procedure is used, where the  */
/*              scores of previous attempts are used as the test*/
/*              criterion.                                      */
/*                                                              */
/* Changes: We have slightly modified the internal represen-    */
/*          tation and the predicate names, documented the main */
/*          routines and have changed the output routines.      */
/*                                                              */              
/****************************************************************/
/* 'play' is the toplevel goal for invocation of the system.    */
/* The input of color codes has to be a list of four elements   */
/* containing the atomic colors.                                */
/****************************************************************/
 
/****************************************************************/
/* This is some M-, C-, and YAP-Prolog specific stuff.          */
/****************************************************************/
:- dynamic code/1.
:- dynamic random/1.
 
/****************************************************************/
/* operator: 's' is used for the internal representation of the */
/*               black and white scores.                        */
/****************************************************************/
:- op(150,fy,s).
 
/****************************************************************/
/* Interface to the user.                                       */
/****************************************************************/
 
play :-
    nl,
    abolish(random,1),
    abolish(code,1),
    write('Mastermind at your service !'), nl, nl,
    write('Color codes should be entered as Prolog list '),
    write('with four elements.'), nl, nl,
        write('Enter an Integer between 0 and 164: '),
        read(RandomSeed), nl,
        assert(random(RandomSeed)),
        game_loop.
 
game_loop :-
    write('Do you want to break the code (y/n)? '),                  
    read(Answer), nl,
    game(Answer).

terminatep :-
    write('Another game (y/n)? '),
    read(Answer), nl,
    next_game(Answer).

next_game(y) :-
    !, game_loop.
next_game(n) :-
    retract(random(_)),
    write('Mastermind was pleased to serve you.'),
        nl.

/****************************************************************/
/* Player gives Code, which has to be guessed by mastermind !   */
/****************************************************************/
game(n) :-
    write('Please enter color code, I promise not to look: '),
        read([C1,C2,C3,C4]), nl,
    assert(code([C1,C2,C3,C4])),
        guess(Code),
        write('First try is:'), put(9),
    write_code(Code), put(9),
        evaluate(Code,Score),
        put(9), write_score(Score), nl,
    extend_code([(Code,Score)]),
        retract(code(_)),
        !,
    terminatep.
 
/****************************************************************/
/* If the code is guessed, i.e. all positions have the correct  */                     
/* color, we can terminate the recursion.                       */
/****************************************************************/
extend_code([(Code,s s s s _,_)|_]) :-
    nl, write('The hidden code must be:'), put(9),
    write_code(Code), nl, nl.
 
/****************************************************************/
/* 'extend_code' extends the list of codes for every trial with */
/* a new code.                                                  */
/****************************************************************/
extend_code(CodeList) :-
    code_possible(CodeList,NewTry),
    write('Next try is:'), put(9), 
    write_code(NewTry), put(9),
    evaluate(NewTry,Score),
        put(9), write_score(Score), nl,
        extend_code([(NewTry,Score)|CodeList]).

write_code([Color]) :-
    write(Color).
write_code([Color|Colors]) :-
    write(Color), put(9),
    write_code(Colors).

write_score((Black,White)) :-
    count(Black,B),
    count(White,W),
    write('Black:'), write(' '), write(B), write(' '),
    write('White:'), write(' '), write(W).
 
count(0,0).
count(s(Ss),Y) :-
    count(Ss,X),
    Y is X + 1.
 
/****************************************************************/
/* 'code_possible' computes through 'mm' from the first try in  */
/* the CodeList a NewTry which isn't inconsistent with the      */
/* first Score. Of course, this NewTry must also be consistent  */
/* with the rest of the CodeList. Thus, we proceed with         */
/* recursion until we have processed all the codes.             */
/****************************************************************/
code_possible([],_).
code_possible([(FirstTry,FirstScore)|CodeList],NewTry) :-
     mm(FirstTry,NewTry,FirstScore),
     code_possible(CodeList,NewTry).
 
/****************************************************************/
/* Player tries to guess the code.                              */
/****************************************************************/
game(y) :-
    guess(Code),
        assert(code(Code)),
        put(9), put(9), put(9),
    write('Enter first try: '), put(9),
        read([C1,C2,C3,C4]),
    evaluate([C1,C2,C3,C4],Score),
        finished(Score),
        !, terminatep.
 
/****************************************************************/
/* If the guess of the user evaluates to 'all colors are on the */
/* right positions', we can finish. Otherwise we ask for another*/
/* Try or give the user the opportunity to quit.                */
/****************************************************************/
finished((s s s s _,_)) :-
    nl,
    write('You got it!'), nl, nl,
    retract(code(_)).
finished(Score) :-
    write_score(Score), put(9),
        write('Enter try or quit:'), put(9),
    read(Answer),
    proceedp(Answer).
 
/****************************************************************/
/* If the user quits we show him/her the code, otherwise we     */
/* evaluate his new try.                                        */
/****************************************************************/
proceedp(quit) :-
    retract(code(Code)), nl, nl,
    write('The hidden code is: '),  put(9),
    write_code(Code), nl, nl.
proceedp(Try) :-
    evaluate(Try,Score),
    finished(Score) .
 
/****************************************************************/
/* For evaluating a try we pick up the hidden code stored in the*/
/* database and pass it to the function 'mm', which computes the*/
/* Score.                                                       */
/****************************************************************/
evaluate(Try,Score) :-
    clause(code(HiddenCode),true),
    mm(Try,HiddenCode,Score).
 
/****************************************************************/
/* 'mm' is our work-horse, first we determine the number of     */
/* 'black pins which we have to set on the board'. ReducedTry   */
/* and ReducedCode are the remaining colors and unused          */
/* 'pinholes'.                                                  */
/* If the HiddenCode is uninstantiated, 'determine_blacks'      */
/* looks for the first code, which could produce the Score,     */
/* if one is found 'determine_whites' will check whether the    */
/* remaining colors correspond to the white Score. If so the    */
/* NextTry is determined. Otherwise, we backtrack to the next   */
/* possible combination producing the black score, and continue */
/* as before.                                                   */
/****************************************************************/
mm(Try,HiddenCode,(Black,White)) :-
    determine_blacks(Try,HiddenCode,ReducedTry,ReducedCode,Black),
    determine_whites(ReducedTry,ReducedCode,White,ReducedCode).

/****************************************************************/
/* If we have processed all positions, we have 0 blacks and     */
/* initialize ReducedTry and ReducedCode. Otherwise if there is */
/* the color in the Try and the Code in the same position, we   */
/* increment the counter for the blacks. We forget in ReducedTry*/
/* and ReducedCode the corresponding color information. Other-  */
/* wise, if the colors are different at the same position, we   */
/* have to keep the color information.                          */
/****************************************************************/
determine_blacks([],[],[],[],0).
determine_blacks([Color|Try],[Color|Code],ReducedTry,ReducedCode,s Black) :-
    determine_blacks(Try,Code,ReducedTry,ReducedCode,Black).
determine_blacks([Color1|Try],[Color2|Code],
         [Color1|ReducedTry],[Color2|ReducedCode],Black) :-
    color(_,Color1), color(_,Color2),
    not(Color1 == Color2),
    determine_blacks(Try,Code,ReducedTry,ReducedCode,Black).

/****************************************************************/
/* 'Determine_whites' tries to delete the Colors of the Try from*/
/* the Code, if this succeeds we can increment the counter for  */
/* the whites. If we reach the end and still have some colors in*/
/* the code table, we determine over 'tuple' a ????             */
/****************************************************************/
determine_whites([],Code,0,NewTry) :-
    tuple(Code,NewTry).
determine_whites([Color|Try],Code, s White,[UnboundColor|NewTry]) :-
        delete(Color,Code,ReducedCode,NewTry),
        determine_whites(Try,ReducedCode,White,NewTry).
determine_whites([Color|Try],Code,White,NewTry) :-
        not_in(Color,Code,NewTry),
        determine_whites(Try,Code,White,NewTry).
 
/****************************************************************/
/* The following is the random code generator                   */
/****************************************************************/
guess([C1,C2,C3,C4]) :-
    randomcolor(C1),
    randomcolor(C2),
    randomcolor(C3),
    randomcolor(C4) .
 
randomcolor(Color) :-
    randomnumber(X),
        Y is (X mod 6) + 1,
    color(Y,Color).
 
randomnumber(R) :-
    retract(random(R)),
        NR is ((R * 125) + 1) mod 165,
        assert(random(NR)).

color(1,black).
color(2,blue).
color(3,green).
color(4,red).
color(5,white).
color(6,yellow).

/*********************************************************************/
/* 'delete' deletes U from the second argument, if it occurs in the  */                      
/* head it and returns over tuple the rest of the original codelist  */
/* Ms. Otherwise, if U and the head are different, we search the rest*/
/* of the codelist.                                                  */
/*********************************************************************/
delete(U,[U|Y],Y,Ms) :-
    tuple(Y,Ms).
delete(U,[V|Y],[V|Y1],[_|Ms]) :-
    color(_,U), color(_,V),
    not(U == V),
    delete(U,Y,Y1,Ms).

/*********************************************************************/
/* If U is not in V, 'not_in' will succeed.                          */
/*********************************************************************/
not_in(_,[],[]).
not_in(U,[V|Vs],[_|Ws]) :-
    color(_,U), color(_,V),
    not(U == V),
    not_in(U,Vs,Ws).
 
tuple([],[]).
tuple([U|Us],[_|Vs]) :-
    color(_,U),
    tuple(Us,Vs).
