% CVS: $Id: rubik.pl,v 1.4 1998/11/26 05:18:41 pets Exp $
goal :- main.

sidecolor(cc(X,Y)) :-
  ground([X,Y]).
candmove(m(M,X,Y)) :-
  ground([X,Y,M]).
cand(X) :-
  ground(X).
gc.

flag(stepmode, on).
flag(stepmode, off).

crit(Crit) :-
  ground(Crit).
state(Cube) :-
  ground(Cube).
get(X) :-
  ground(X).
get0(X) :-
  integer(X).
tab(X) :-
  integer(X).
ghoul(G) :-
  ground(G).

random(X) :-
  float(X).

%  :- use_module(library(random)).

rand_move(M,RN):-
  random(R),
  RS is R*12,
  RN is integer(RS),
  arg(RN,m(pp(f),pp(b),pp(r),pp(l),pp(u),pp(d),mm(f),mm(b),mm(r),mm(l),mm(u),mm(d)),M).

% RUBDATA - Copyright (C) 1994, Amzi! inc.

%           This file contains all the data needed to drive
%           the main cube solving predicates.

% the sequences of moves used to perform special transformations
% such as twisting the corners without moving anything else
		  
seq(s, [pp(rr), mm(r), pp(l)]).
seq(tc1, [mm(l), pp(u), pp(r), mm(u), pp(l), pp(u), mm(r), mm(u)]).
seq(tc1u2, [pp(ru), pp(ru), pp(tc1), mm(ru), mm(ru)]).
seq(tc3, [pp(r), mm(u), mm(l), pp(u), mm(r), mm(u), pp(l), pp(u)]).
seq(ct1, [mm(r), pp(d), pp(r), pp(f), pp(d), mm(f), mm(u), pp(f),
          mm(d), mm(f), mm(r), mm(d), pp(r), pp(u)]).
seq(ct3, [mm(r), pp(d), pp(r), pp(f), pp(d), mm(f), pp(u), pp(u),
          pp(f), mm(d), mm(f), mm(r), mm(d), pp(r), pp(u), pp(u)]).
seq(ef1, [mm(u), pp(f), mm(r), pp(u), mm(f), mm(s), pp(f), mm(u),
          pp(r), mm(f), pp(u), pp(s)]).
seq(ef2, [pp(l), pp(f), mm(u), pp(f), mm(r), pp(u), mm(f), mm(s),
          pp(f), mm(u), pp(r), mm(f), pp(u), pp(s), mm(f), mm(l)]).
seq(et1, [pp(f), pp(f), pp(r), pp(r), pp(f), pp(f), pp(r), pp(r),
          pp(f), pp(f), pp(r), pp(r)]).
seq(h, [pp(l), pp(f), pp(u), mm(f), mm(u), mm(l)]).
seq(g, [mm(r), mm(f), mm(u), pp(f), pp(u), pp(r)]).
seq(pt, [pp(ru), pp(ru)]).
seq(mr2a, [pp(r), pp(f), mm(r), mm(f)]).
seq(mr2b, [mm(r), mm(u), pp(r), pp(u)]).
seq(mr3a, [mm(u), pp(r), pp(u)]).
seq(mr3b, [pp(f), mm(r), mm(f)]).

% cnd defines the moves which will be used in a given stage for search

cnd(1, [r, u, f]).
cnd(2, [r, mr2a, mr2b]).
cnd(3, [r, mr3a, mr3b]).
cnd(4, [r, tc1u2, ct1]).
cnd(5, [u, h, g, ef1, ef2]).
cnd(6, [u, tc1, tc3, ct1, ct3]).

% s_r is used by the shift_right heuristics.  it lists the move sequence
% needed to move a piece which is not on the right, to the right.  the
% first arguement is the position the piece is at

s_r(p('F','L','U'), [mm(mr2a)]).
s_r(p('F','L','D'), [pp(rr), mm(mr2a), mm(rr)]).
s_r(p('B','L','U'), [mm(rr), mm(mr2a), pp(rr)]).
s_r(p('B','L','D'), [pp(rr), pp(rr), mm(mr2a), mm(rr), mm(rr)]).
s_r(p('F','U'), [mm(mr3a)]).
s_r(p('F','D'), [pp(s), mm(mr3a), mm(s)]).
s_r(p('B','U'), [mm(s), mm(mr3a), pp(s)]).
s_r(p('B','D'), [pp(s), pp(s), mm(mr3a), mm(s), mm(s)]).
s_r(p('L','U'), [pp(u), pp(u)]).
s_r(p('F','L'), [pp(f), pp(f)]).
s_r(p('L','D'), [pp(d), pp(d)]).
s_r(p('B','L'), [pp(b), pp(b)]).

% orientation defines the rotation moves necessary to position the
% cube to take advantage of symmetry for each piece

orientation(p('F','L','U'), []).
orientation(p('F','L','D'), [pp(rr)]).
orientation(p('B','L','U'), [mm(rr)]).
orientation(p('B','L','D'), [pp(rr), pp(rr)]).
orientation(p('F','U'), []).
orientation(p('F','D'), [pp(s)]).
orientation(p('B','U'), [mm(s)]).
orientation(p('B','D'), [pp(s), pp(s)]).
orientation(p('L','U'), []).
orientation(p('F','L'), [pp(rr)]).
orientation(p('L','D'), [pp(rr), pp(rr)]).
orientation(p('B','L'), [mm(rr)]).
orientation(_, []).

% pln lists the target pieces for each stage

pln(1, [p('L','U'),p('F','L'),p('L','D'),p('B','L')]).
pln(2, [p('B','L','D'),p('F','L','D'),p('B','L','U')]).
pln(3, [p('F','U'),p('F','D'),p('B','U'),p('B','D')]).
pln(4, [p('F','L','U')]).
pln(5, [p('R','U'),p('F','R'),p('R','D'),p('B','R')]).
pln(6, [p('F','R','U'),p('B','R','U'),p('B','R','D'),p('F','R','D')]).

% vw defines the preferred orientation for a stage

vw(5, [mm(rf)]).
vw(6, [mm(rf)]).
vw(_, []).

% this is the pristine state

pristine(cube('F','R','U','B','L','D',
 'F','R','U','F','R','D','F','L','U','F','L','D','B','R','U','B','R','D',
 'B','L','U','B','L','D','R','U','R','D','L',
 'U','L','D','F','U','F','D','B','U',
 'B','D','F','R','F','L','B','R','B','L')).

% the initial mapping of sides and colors

side_color([cc('F','G'), cc('R','R'), cc('U','W'), cc('B','Y'), cc('L','O'), cc('D','B')]).

% RUBDISP - Copyright (C) 1993, Amziod
% This file contains the display predicates.  

:- op(500,xfy,:).

% cube_print - displays the full color cube. Both variables and
%              blanks appear as spaces.  unification is again used
%              to map the input cube to the individual displays

cube_print(cube(F, R, U, B, L, D,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, 
  V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, 
  V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, 
  V41, V42, V43, V44, V45, V46, V47, V48, V49, 
  V50, V51, V52, V53, V54)) :-
  nl,
  tab(6), pc([V28, V45, V22]),
  tab(6), pc([V53, B, V51]),
  tab(6), pc([V25, V43, V19]),
  pc([V29, V54, V26, V27, V44, V21, V20, V52, V23]),
  pc([V37, L, V35, V36, U, V32, V31, R, V33]),
  pc([V17, V50, V14, V15, V40, V9, V8, V48, V11]),
  tab(6), pc([V13, V39, V7]),
  tab(6), pc([V49, F, V47]),
  tab(6), pc([V16, V41, V10]),
  tab(6), pc([V18, V42, V12]),
  tab(6), pc([V38, D, V34]),
  tab(6), pc([V30, V46, V24]),
  check_step,
  !.

check_step :-
  get_flag(stepmode, on),
  write('Hit Enter to continue'),
  get0(_).
check_step.
  

pc([]):- nl.
pc([V1| V2]):-
  sidecolor(cc(V1, C)),
  write(C), tab(1),
%  write(V1), tab(1),
  pc(V2).

% wrfield & rdfield - allow input and output to a named field

wrfield(F,X):-
  field(F,P),
  write(P),
  write(X),
  nl.

rdfield(F,X):-
  field(F,P),
  write(P),
  read(X).

rdchar(F,X):-
  field(F,P),
  write(P),
  get(X).


% field - these are the field definitions for the cube program

field(prob, 'Problem: ').
field(stage, '\nStage:   ').
field(target, 'Target:  ').
field(rot, 'Rotation: ').
field(try, 'Trying: ').
field(prompt, '>').
field(error, 'Error: ').
field(done, 'Done: ').
field(continue, 'Hit Enter to continue.').
field(stepmode, 'Stepmode? (y/n): ').
field(history, 'History? (y/n): ').
field(move, 'Enter move\n(end with period, ex. u., -l., ct1., -tc3.) : ').
field(moves, 'Moves: ').
field(rotations, 'Rotations: ').
field(sequences, 'Sequences: ').
field(end_disp, 'Enter q. to end').
field(msg20, ' ').
field(msg21, ' ').

m_disp(Menu):-
  menu(Menu, Choices),
  m_dis(1, Choices), !.

m_dis(_, []) :- nl.
m_dis(N, [H|T]) :-
  write('['),write(N),write(']'),
  write(H), tab(1),
  NN is N + 1,
  m_dis(NN, T).

m_choose(Menu,Choice):-
  write('Choice: '),
  get(Nascii),
  N is Nascii - 0'0,
  menu(Menu, Choices),
  m_ch(N, Choices, Choice).

m_ch(N, [], _) :- write('Bad menu choice, try again'), nl, fail.
m_ch(1, [X|_], X) :- !.
m_ch(N, [H|T], X) :-
  NN is N - 1,
  m_ch(NN, T, X).

menu(main, [solve, manual, help, exit]).
menu(solve, [random, manual, edit]).

% RUBEDIT - Copyright (C) 1994, Amzi! inc.
%           This module allows the user to easily enter a scrambled
%           cube position.  the cube is displayed in goal form.
%           the cursor keys move from tile to tile, and the f1 key
%           selects the color for the tile.  repeated hits of f1
%           changes the color.  f1 was chosen since that allows a
%           machine with a pcmouse to do cube editing with the mouse and
%           and the left button (f1) with no special changes.

redit(Y):-
  ghoul(G),
  cube_print(G),
  write('Enter single letters separated by spaces in the pattern'),nl,
  write('of the display.  The letters should represent the colors'),nl,
  write('on your cube.  Exact spacing isn\'t critical.'),nl,
  read_cube(X),            % read it off the screen
  trans_cube(X,Y),         % change colors to side notation
  cube_print(Y).
redit(_):-
  error('failing edit'),halt.

% read_cube - reads the edited cube directly from the screen, there was
% no need to save information about colors during the cursor movement
% stage ("edi").  it was for this reason that "change_color" writes the
% letter of the color in the tile.

% read_cube looks exactly like print_cube, only in reverse

read_cube(cube(F, R, U, B, L, D,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, 
  V17, V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, 
  V29, V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, 
  V41, V42, V43, V44, V45, V46, V47, V48, V49, 
  V50, V51, V52, V53, V54)):-
  rc([V28, V45, V22]),
  rc([V53, B, V51]),
  rc([V25, V43, V19]),
  rc([V29, V54, V26, V27, V44, V21, V20, V52, V23]),
  rc([V37, L, V35, V36, U, V32, V31, R, V33]),
  rc([V17, V50, V14, V15, V40, V9, V8, V48, V11]),
  rc([V13, V39, V7]),
  rc([V49, F, V47]),
  rc([V16, V41, V10]),
  rc([V18, V42, V12]),
  rc([V38, D, V34]),
  rc([V30, V46, V24]),
  !.

rc([]):- !.
rc([V1| V2]):-
  get(X),
  name(V1,[X]),
  !,rc(V2).

trans_cube(X,Y):-
  get_color(X),        % establish new side colors
  rewrite(X,Y).        % translate color notation to side notation

get_color(X):-
  X=..[cube,F,R,U,B,L,D|_],         % the sides in color notation
  set_tcolor([cc('F',F),cc('R',R),cc('U',U),cc('B',B),cc('L',L),cc('D',D)]).

rewrite(C,S):-
  var(S),                  % this one if color input and side output
  C=..[cube|Clist],
  rewrit(Clist,Slist),
  S=..[cube|Slist],!.
rewrite(C,S):-
  var(C),              % this one if side input, and color out.  it
  S=..[cube|Slist],    % is called by the manual routine when building
  rewrit(Clist,Slist), % a cube to solve.  rotate moves might have been used
  C=..[cube|Clist],!.  % which changed the side colors

rewrit([],[]):- !.
rewrit([X|Ctail],[Y|Stail]):-
  var(X), var(Y),
  !, rewrit(Ctail,Stail).
rewrit([Color|Ctail],[Side|Stail]):-
  sidecolor(cc(Side,Color)),
  !,rewrit(Ctail,Stail).

set_tcolor([]):- !.
set_tcolor([cc(S,C)|Tail]):-
  retract(sidecolor(cc(S,_))),
  assert(sidecolor(cc(S,C))),
  !,set_tcolor(Tail).

% RUBHELP - Copyright (C) 1994, Amzi! inc.
% This the the help you get when you ask for help.

rub_help:-
  helpscreen(_),
  nl,write('[more - hit any key to continue]'),
  get0(_),
  fail.
rub_help.

helpscreen(intro):-
  write('INTRODUCTION'),nl,nl,
  write('The cube solver will generate a sequence of moves that will'),nl,
  write('solve any given cube (if solvable).  See rubdoc1.txt for'),nl,
  write('notes on the method.'),nl,nl.
helpscreen('menu options'):-
  write('MAIN MENU OPTIONS'),nl,nl,
  write('Solve - solves three types of cubes (from submenu)'),nl,nl,
  write('      random - generate a random cube to solve'),nl,
  write('      manual - allows you to scramble your own'),nl,
  write('      edit   - allows you to describe a real cube'),nl,nl,
  write('        with the option (prompts)'),nl,nl,
  write('      stepmode - stops after each sequence (useful if'),nl,
  write('                 solving a real cube)'),nl,
  write('Manual - allows manipulation of cube (useful to see the'),nl,
  write('         effects of all the legal moves)'),nl,nl,
  write('Help   - this stuff'),nl,nl,
  write('Exit   - return to dos'),nl.
helpscreen(notation):-
  write('NOTES ON NOTATION'),nl,nl,
  write('The cube is unfolded so all six sides are visible.  All moves'),nl,
  write('are labeled by the side they affect.  The letters used are:'),nl,nl,
  write('                 B - back'),nl,
  write('       L - left  U - up    R - right'),nl,
  write('                 F - front'),nl,
  write('                 D - down'),nl,nl,
  write('Directions - + clockwise, - counterclockwise'),nl,nl,
  write('Pieces are referred to by color.  The colors are:'),nl,nl,
  write('         W - white, G - green, B - blue, Y - yellow,'),nl,
  write('         R - red (PC magenta), O - orange (PC red) '),nl,nl,
  write('Moves - three types'),nl,nl,
  write('     Side moves - represented by single side letter, ex +r'),nl,
  write('     Rotations - rotate entire cube, preface side with r'),nl,
  write('                 ex. -ru, +rr (used to exploit symmetry)'),nl,
  write('     Sequences - sequence of moves by name ex. +ct1'),nl.
helpscreen('solve display'):-
  write('SOLVE DISPLAY FIELDS'),nl,nl,
  write('Stage - the current stage (see rubdoc1.txt)'),nl,nl,
  write('Target - the piece being solved for'),nl,nl,
  write('Trying - the n-1 nodes of the breadth first search'),nl,nl,
  write('Rotation - the chosen sequence of moves for the current goal'),nl,nl,
  write('Hit any key to end').

% RUBHIST - Copyright (C) 1994, Amzi! inc.

%  This module records history information so you can unscramble
%  a real cube by looking at the log file.

% add_history takes a list of moves as input.  as output it sends
% the expanded version of the moves to the logfile.  That is, sequences
% are broken down into primitive moves before being written to the
% window

add_history(V1):-
  expand(V1, V2),          % expand the list
  de_list(V2,V3),          % remove inbedded lists (flatten the list)
  segment_list(V3,V4),     % break into pieces that fit in window
  write_hist(V4),
  !.
add_history(X):-
  error([add_history,X]).

write_hist([]).
write_hist([FirstLine|Rest]) :-
  write('  Moves: '),
  wr_hist(FirstLine),
  nl,
  write_hist(Rest).

wr_hist([]).
wr_hist([H|T]) :-
  tab(2),
  write(H),
  wr_hist(T).

% expand pushes its way through a list of moves and sequences, making
% sequences into other move lists. it takes care to preserve the
% meaning of a counterclockwise sequence by reversing the list defining
% the sequence.  this reverse also changes the sign of each term along
% the way.  the first argument is the imput list, the second is output

expand([], []) :- !.
expand([Term|V3], [Term|V4]):-
  moveterm(Term, X),            % strip the sign
  (move(X,_,_); rot(X,_,_)),    % its a primitive
  !, expand(V3, V4).
expand([Seq|V3], [Termlist|V5]):-
  moveterm(Seq,S),              % we can guess its a sequence
  seq(S, SL),
  (signterm(Seq,mm), reverse(SL,Sterms);   % flip if necessary
   Sterms = SL),
  expand(Sterms,Termlist),      % double recursion, on this sequence
  !, expand(V3, V5).            % and the rest of the list
expand(X,_):-
  error(['expand fails on',X]).

% separate the move and sign of a term, first arg is input, second output

moveterm(pp(X), X) :- !.
moveterm(mm(X), X) :- !.

signterm(pp(X), pp) :- !.
signterm(mm(X), mm) :- !.

% "expand" left imbedded lists where sequences used to be, flatten them
% out since they arn't necessary

de_list([], []) :- !.
de_list(V1, [V1]):-
  (V1 = pp(X); V1 = mm(X)).
de_list([V1|V2], V3):-
  de_list(V1, V4),          % double recursion on the head and tail
  de_list(V2, V5),
  append(V4, V5, V3).

% having flattened it, segment_list breaks a long list into smaller
% lists that will fit in the display window.  this is because the
% window routine is too lazy to deal with lines that are too long

segment_list([A,B,C,D,E|Tin],[[A,B,C,D,E]|Tout]):-
  segment_list(Tin,Tout).
segment_list([],[]) :- !.
segment_list(L,[L]) :- !.

% CUBE SOLVER II
%   A Rubik's Cube Solver
%   written by Dennis Merritt
%   as described in Building Expert Systems in Prolog (Springer-Verlag)
%   available from:
%     Amzi! inc.
%     40 Samuel Prescott Dr.
%     Stow, MA 01775 USA
%     Tel 508/897-7332, FAX 508/897-2784
%     e-mail amzi@world.std.com
%
%  This program may be copied, modified and redistributed although proper
%  acknowledgement is appreciated.
%
%  This implementation was done with Cogent Prolog, also available
%  from Amzi! inc.
%
%  This is the main module which contains the predicates for 
%         the main control loop,
%         manual mode,
%         solve mode, and
%         utility functions.
%
% Note - The Cogent/Prolog compiler supports modules.  The export declarations
%        are for predicates defined in the current module which may be used
%        by other modules.  The import declarations are for predicates 
%        defined in other modules.

:-op(500,xfy,:).

main :- banner, go.  % The start up entry point

go:-                       % The main control loop 
  repeat,
  init_color,
  m_disp(main),            % The main menu
  m_choose(main,X),        % Select an item
  do(X),                   % Execute it
  fail.                    % Go back to the repeat

% These are the predicates which are called for the various
% main menu choices.  The cut after each ensures they wont be
% backtracked into when the main loop fails.

do(solve):-solve,!.        % in this module
do(manual):-manual,!.      % in this module
do(help):-rub_help,!.      % in rubhelp
do(exit):-halt.            % built-in predicate to exit

banner:-
  nl,nl,
  write('Cube Solver II'),nl,
  write('An illustrative Prolog program from'),nl,
  write('Building Expert Systems in Prolog (Springer-Verlag) by Dennis Merritt'),nl,
  write('implemented in Cogent Prolog'),nl,nl,
  write('For more information contact:'),nl,
  write('Amzi! inc.'),nl,
  write('40 Samuel Prescott Dr.'),nl,
  write('Stow, MA 01775 USA'),nl,
  write('Tel 508/897-7332, FAX 508/897-2784'),nl,
  write('e-mail amzi@world.std.com'),nl,nl.

% These predicates initialize the state to the goal state (ghoul),
% and allow you to enter single moves.  They are intended to demonstrate the
% effects of the various sequences used by the solve routines.

% They are also called by the solve routine if manual scrambling 
% is requested

manual:-
  pristine(G),                            % Start with the goal state
  retractif(state(_)),
  assert(state(G)),
  cube_print(G),                       % Display it
  disp_moves,                          % List the possible moves
  repeat,                              % Start repeat-fail loop
  rdfield(move,M),                     % Get a move
  (M==q, nl, !                         % If '', clear and end
   ;
   state(S),
   man_move(M,S,S2),                   % Apply move to it
   retract(state(_)),
   assert(state(S2)),
   cube_print(S2),fail).               % Print it and fail back

man_move(M,S,S2):-
  movel(M,S,S2),!.
man_move(M,S,S2):-               % Pop a + in front of an unsigned move
  movel(pp(M),S,S2),!.
man_move(M,_,_):-
  error('Unknown move'-M),!,fail. 

disp_moves:-                  % List the three types of moves
  wrfield(moves,''),          % Heading
  move(X,_,_),                % Will backtrack through all moves
  write(X),tab(1),            % Write move
  fail.                       % Go back for the next one
disp_moves:-                  
  nl,
  wrfield(rotations,''),      % No more moves, do the same for rots
  rot(X,_,_),
  write(X),tab(1),
  fail.
disp_moves:-                  % And again for seqs
  nl,
  wrfield(sequences,''),
  seq(X,_),
  write(X),tab(1),
  fail.
disp_moves:-                 % Got em all, end
  nl,
  wrfield(end_disp,'').

% This is the main body of the program which actually solves the cube.
% See rubdoc1 and rubdoc2 for the big picture

solve:-
  m_disp(solve),              % solve submenu
  m_choose(solve,X),
  rdchar(stepmode,SM),
  (SM==0'y , set_flag(stepmode,on)   % check for a y (scan code 21)
   ;
   set_flag(stepmode,off)),
  solve(X).                   % call solve w/ arity one with menu choice

solve(X):-
  init_solve(X),              % initialize all the stuff
%%RB  T1 is cputime,
  retractif(stage(_)),
  assert(stage(1)),           % the first stage will call the others
  stages,
%%RB  T is cputime - T1,
  state(S),
  cube_print(S),
  write('Done  time = '),
%%RB  write(T),
  nl, nl.
solve(X):-
  error('failing to solve'),
  halt.             % something wrong, back to main

init_solve(X):-
  wrfield(prob,X),
  initialize(X),               % getting closer to the real work
  state(S),
  !.

initialize(X):-
  pristine(G),
  retractif(ghoul(_)),
  assert(ghoul(G)),
  init_crit(Crit),           % set up the initial criteria (all variables
  retractif(crit(_)),
  assert(crit(Crit)),
  !,initial(X).              % get specific start state in the database

initial(random):-                 % create a new random cube
  random_cube(Cube),
  retractif(state(_)),
  assert(state(Cube)), !.
initial(edit):-                   % edit your own
  redit(Cube),
  retractif(state(_)),
  assert(state(Cube)),
  new_colors(Cube), !.
initial(manual):-                 % scramble your own
  manual,
  state(Cube),
  new_colors(Cube),!.

stages:-
  repeat,
  retract(stage(N)),
  init_stage(N,Plan),        % Set the stage, get the plan
  state(S),
  cube_print(S),
  build_plan(Plan),
  improve(N,Plan),                % Put the pieces in the plan in place
  vw(N,V),                   % undo the stage view (done by init_stage)
  undo_view(V),
  N2 is N + 1,               % next stage
  assert(stage(N2)),
  N2 >= 7.

build_plan([]) :- !.
build_plan([H|T]) :-
  assert(impplan(H)),
  build_plan(T).

% init_stage goes to rubdata to get the table entries which define
% the heuristics for the stage

init_stage(N,Plan):-         % return list of target pieces for this stage
  wrfield(stage,N),
  cnd(N,Cands),              % set up candidate moves used by search
  build_cand(Cands),
  vw(N,V),                   % set up preferred view for stage
  set_view(V),
  pln(N,Plan),!.             % get list of target pieces

% improve - works through the list of target pieces for the stage.
%           it first checks to see if its already in place

improve(Stage,[]) :- !.
improve(Stage,[Piece|Rest]) :-
  impro(Stage,Piece),
  !, improve(Stage,Rest).

/*RB
improve(Stage):-
  impplan(Piece),
  impro(Stage,Piece).
*/

impro(Stage,Piece) :-
  add_criteria(Piece,Crit),                % Add new piece to criteria
  target_loc(Piece,Pos,Orient),            % Where is it
  impr(Orient,Stage,Pos,Piece),
  !.

impr(0,_,_,_) :- !.                        % In place and oriented
impr(_,Stage,Pos,Piece) :- imp(Stage,Pos,Piece).

% imp - getting into the real work

imp(Stage,Pos,Piece):-
  color_piece(PieceC,Piece),         % translate side notation to
  wrfield(target,PieceC),            %   color notation for display
  heuristics(Stage,Pos),             % See if special help is needed.
  orientation(Piece, View),          % Preferred view for this piece.
  set_view(View),
  crit(Crit),
  state(State),
%%RB  cntr_set(4,0),						% to limit wild searches
  gc,
  rotate(Moves,State,Crit),      % Search for moves which transform
  retract(state(_)),
  assert(state(Crit)),
  wrfield(rot,Moves),
  add_history(Moves),
  undo_view(View),!.

heuristics(Stage,Pos):-
  (shift_right_1(Stage,Pos);
   shift_right_2(Stage,Pos)),!.
heuristics(_,_):-true.

% The shift_right heuristics are used to avoid the situations where
% the piece is in one of the target positions for the stage, but the
% wrong one, or mis-oriented.  By blindly moving it to the right the
% search is reduced since it doesn't have to search to move it both 
% out of a critical target position and back into the correct one.

shift_right_1(1,Pos):-
  smember('L',Pos),            % Is the target piece already on the left?
  s_r(Pos,Moves),              % If so get the canned moves to move it 
  change(Moves),               % right for easy search.
  !.

shift_right_2(Stage,Pos):-
  Stage < 4,                   % If the target piece is not on the right
  notsmember('R',Pos),         % side, get the canned moves to put it
  s_r(Pos,Moves),              % there to allow easier search
  change(Moves),
  !.

% rotate - the real guts of the solution, all the rest of the code provides
%          support for these six lines.

% These lines illustrate the power and obscurity of Prolog.
% Prolog can be very expressive when the main information is carried
% in the predicate.  However, sometimes the work is being done by
% unification, and it is not at all apparent by reading the code.
% Furthermore, since Prolog predicates often work backwards and
% forwards, it is not clear in a given case what is intended to be
% be input, and what is the output, and, as in this case, what might
% be an in-out.

% The input and output states of rotate are:

% Input: Moves - unbound
%        State - bound to the cube structure for the current state
%        Crit  - partially bound cube structure.  the bound portions
%                represent the pieces in place + the current goal piece

% Output: Moves - a list of moves
%         State - same as input
%         Crit  - fully bound to the new state

% rotate does a breadth first search by recursively calling itself
% before it calls get_move which trys new moves.  it does not save the
% search trees as most breadth first algorithms do, but rather recalculates
% the moves since they can be executed so fast.

% get_move fails when called with the partially bound Crit, unless
% it is a move which reaches the desired state.  The failure causes
% backtracking.  However when rotate calls itself, it gives it a
% fully unbound variable NextState.  This call to rotate succeeds and
% keeps adding new moves generated by get_move on backtracking.

% eventually get_move finds a match and rotate succeeds.

rotate([], State, State).            % start with a no move
rotate(Moves, State, Crit):-         % nothing didnt work, get serious
  rotate(PriorMoves, State, NextState), % get something to build on
%  cntr_inc(4,N4),
%  check_prog(N4),
  get_move(ThisMove, NextState, Crit),  % generate possible moves
  append(PriorMoves, [ThisMove], Moves).   % build up the list

/*RB
check_prog(N) :- N < 250, !.
check_prog(_) :-
  error('not converging'),
  halt.
*/

% The following predicates all perform various useful services
% for the main predicates above.  Some are declared export as well
% and are used by other modules

% add_criteria puts a new piece on the criteria structure.  it works
% by creating two piece format lists, one of the goal state, and the
% other of the current criteria.  It then walks through the two lists
% simultaneously looking for the target piece in the goal state.
% when it finds it it adds it to the criteria.  Crit is unbound on entry

add_criteria(Piece,Crit):-
  crit(OldCrit),
  pieces(OldCrit, OldCritP),
  ghoul(Ghoul),
  pieces(Ghoul, GhoulP),
  add_crit(OldCritP, GhoulP, NewCritP, Piece),
  pieces(Crit, NewCritP),
  retract(crit(_)),
  assert(crit(Crit)), !.

add_crit([V1|V2], [V3|V4], [V3|V2], V5):-
  matches(V3, V5),!.
add_crit([V1|V2], [V3|V4], [V1|V5], V6):-
  !,add_crit(V2, V4, V5, V6).
add_crit(V1, V2, V3, V4):-
  error('something wrong with add_crit'),!.

% The center tiles dont move on the cube.  Sooo if someone enters a cube
% with different color sides then we must find the new center tiles
% and map the new colors to the sides accordingly

new_colors(Cube):- 
  rewrite(ColorCube,Cube),
  get_color(ColorCube),
  rewrite(ColorCube,NewCube),
  retract(state(_)),
  assert(state(NewCube)).

% Set up the initial mapping of sides to colors

init_color:-
  side_color(SC),
  retractall(sidecolor(_)),
  ini_col(SC).

ini_col([]):- !.
ini_col([cc(S,C)|T]):-
  assert(sidecolor(cc(S,C))),
  ini_col(T).

% translate a piece in piece notation to color notation

color_piece(PieceC,Piece):-
  Piece=..[p|Args],
  col_p(ArgsC,Args),
  PieceC=..[p|ArgsC].

col_p([],[]):- !.
col_p([PC|RestC],[P|Rest]):-
  sidecolor(cc(P,PC)),
  col_p(RestC,Rest).

% execute about 50 or 60 random rotations to the goal cube.  due to the
% random function, the random cubes will be the same from run to
% run.  It always starts from the same seed.

random_cube(Cube):-
  ghoul(Start),
  rand_cub(Start,Cube,50).

rand_cub(Cube,Cube,0).
rand_cub(Now,Cube,N):-
  repeat,
  rand_move(M,RN),
  movel(M,Now,Next),
  NN is N - 1,
  !,rand_cub(Next,Cube,NN).

% the classic

member(V1, [V1|V2]):- !.
member(V1, [V2|V3]):-
  member(V1, V3).

% display a list of terms without the list notation

write_list([]):-true.
write_list([H|T]):-
  write(H),tab(1),
  write_list(T).

% target_loc finds the location of a given piece on the cube.  it can
% also be used to find the piece at a given location.  it returns the
% orientation as well, which is 0 if in place, or 1 if in place but
% twisted

target_loc(Piece, Pos, Orient):-
  ghoul(Gt),
  pieces(Gt, G),
  state(St),
  pieces(St, S),
  find_piece(G, S, Pos, Piece, Orient),!.
target_loc(Piece, _,_):-
  error('Failing to find piece'-Piece),
  fail.

% find_piece does the work for target_loc, walking two lists simultaneously
% looking for either the piece or the position, whichever is bound.

find_piece([Gh|Gt], [Sh|St], Pos, Piece, Orient):-
  matches(Pos, Gh),
  matches(Piece, Sh),
  comp(Gh,Sh,Orient),!.
find_piece([V1|V2], [V3|V4], V5, V6, Orient):-
  !,find_piece(V2, V4, V5, V6, Orient).

matches(V1, V2):-
  comp(V1, V2, V3),
  V3 < 2,!.

% comp returns 0 if direct hit, 1 if in place but twisted, and
% 2 if no match

comp(p(V1), p(V1), 0):- !.
comp(p(V1, V2), p(V1, V2), 0):- !.
comp(p(V1, V2), p(V2, V1), 1):- !.
comp(p(V1, V2, V3), p(V1, V2, V3), 0):- !.
comp(p(V1, V2, V3), p(V1, V3, V2), 1):- !.
comp(p(V1, V2, V3), p(V2, V1, V3), 1):- !.
comp(p(V1, V2, V3), p(V2, V3, V1), 1):- !.
comp(p(V1, V2, V3), p(V3, V1, V2), 1):- !.
comp(p(V1, V2, V3), p(V3, V2, V1), 1):- !.
comp(V1, V2, 2).

% allows easy handling of database entries used as flags

set_flag(Flag,Val):-
  retract(flag(Flag,_)),
  assert(flag(Flag,Val)), !.
set_flag(Flag,Val):-
  assert(flag(Flag,Val)).

get_flag(Flag,Val):-
  flag(Flag,Val).

% get_move is used by rotate to generate moves.  the possible moves
% are stored in the database under the key cand.  backtracking causes
% successive moves to be tried

get_move(pp(V1), V2, V3):-
  cand(V1),
  movep(V1, V2, V3).
get_move(mm(V1), V2, V3):-
  cand(V1),
  movep(V1, V3, V2).

% build_cand creates the database of possible moves for a given stage.
% this is one of the important heuristics for limiting the search

build_cand(V1):-
  retractall(cand(_)),
  retractall(candmove(_)),
  build_cands(V1),!.

build_cands([]):- !.
build_cands([V1|V2]):-
  can_seq(V1),
  assertz(cand(V1)),
  !,build_cands(V2).

can_seq(M):-                      % if the search move is a sequence
  seq(M,S),                       % precompute it, so it isn't constantly
  variable(X),                    % redone during search.
  move_list(S,X,Y),
  assertz(candmove(m(M,X,Y))), !.
can_seq(_).

% another classic

append([], V1, V1).
append([V1|V2], V3, [V1|V4]):-
  append(V2, V3, V4).

% apply a list of moves to a state

move_list([], V1, V1):- !.
move_list([Move|V3], V4, V5):-
  movel(Move, V4, V6),
  !,move_list(V3, V6, V5).

% movel is the basic move predicate called from everywhere

movel(pp(M), V2, V3):-        % distinguish between clockwise
  movep(M, V2, V3),!.
movel(mm(M), V2, V3):-        % and counter clockwise moves
  movep(M, V3, V2),!.

% find the move, be it a simple move, a rotation, or a sequence.
% if its a sequence break it into its simple componenents

movep(M, X, Y):- move(M, X, V3), !, Y = V3.
movep(M, X, Y):- rot(M, X, V3), !, Y = V3.
movep(M, X, Y):- candmove(m(M,X,V3)), !, Y = V3.
movep(V1, V2, V3):-
  seq(V1, V4), !,
  move_list(V4, V2, V3),!.
movep([V1|V2], V3, V4):- move_list([V1|V2], V3, V4),!.

% same as move_list, only print new state when done

move_listp(V1, V2, V3):-
  move_list(V1, V2, V3),
  wrfield(rot,V1).

% change is move_list for keeps.
% it takes the old value changes it, updates it,
% and records the history.  it is called by the heuristic routines

change(ML):-
  retract(state(Old)),
  move_listp(ML,Old,New),
  add_history(ML),
  assert(state(New)), !.

% establish a new view.  this means not just rotating the cube, but also
% rotating the criteria and the goal structures.  this is necessary so
% any predicates working with any of the three winds up comparing
% apples and apples.

set_view([]):- !.
set_view(V):-
  retract(state(S1)),
  move_list(V, S1, S2),
  assert(state(S2)),
  retract(ghoul(G1)),
  move_list(V, G1, G2),
  assert(ghoul(G2)),
  retract(crit(C1)),
  move_list(V, C1, C2),
  assert(crit(C2)),
  wrfield(rot,V),
  add_history(V),!.

undo_view([]):- !.
undo_view(RV):-
  reverse(RV,V),
  set_view(V),!.


% convert a cube structure to a list of pieces and visa versa

pieces(cube(X1, X2, X3, X4, X5, X6,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, 
  V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, 
  V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, 
  V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
    [p(X1), p(X2), p(X3), p(X4), p(X5), p(X6),
  p(V7, V8, V9), p(V10, V11, V12), p(V13, V14, V15), p(V16, V17, V18), 
  p(V19, V20, V21), p(V22, V23, V24), p(V25, V26, V27), p(V28, V29, V30), 
  p(V31, V32), p(V33, V34), p(V35, V36), p(V37, V38),
  p(V39, V40), p(V41, V42), 
  p(V43, V44), p(V45, V46), p(V47, V48),
  p(V49, V50), p(V51, V52), p(V53, V54)]).

% get an unbound cube

variable(cube(X1, X2, X3, X4, X5, X6,
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, 
  V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, 
  V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, 
  V48, V49, V50, V51, V52, V53, V54)).

% the initial criteria, unbound except for the six center tiles

init_crit(cube('F', 'R', 'U', 'B', 'L', 'D',
  V7, V8, V9, V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, 
  V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, V32, V33, 
  V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, V47, 
  V48, V49, V50, V51, V52, V53, V54)).

notsmember(X,Y):-smember(X,Y),!,fail.
notsmember(X,Y):-true.

% like the classic, but works on a structure instead

smember(X,Y):-
  Y=..[Fun|Args],
  member(X,Args).

% display errors

error(X):-
  wrfield(error,X), nl,
  get0(_).

% reverse a list of moves, and flip the signs along the way

reverse(L, R) :- rever(L, [], R).

rever([], Z, Z).
rever([H|T], X, Z) :-
  flip_sign(H, FH),
  rever(T, [FH|X], Z).

flip_sign(pp(X), mm(X)):- !.
flip_sign(mm(X), pp(X)):- !.

retractif(X) :-
  retract(X),
  !.
retractif(_).

% RUBMOV - copyright (C) 1994, Amzi! inc.

% this file contains the definitions of all of the 
% moves and rotations primitive to Rubik's Cube.

% Both moves and rotations are done using Prologs unification.
% The first argument is the name of the move or rotation, and the
% second and third arguments define transformations of the structure
% which represents the cube.

% By convention the moves are named by a single character which stands
% for the position of the side being turned.  Rotations are used to
% reposition the entire cube (leaving the pieces in the same relative
% positions).  They are named by the side which defines the axis
% of rotation, preceded by the letter r.

% (Why the funny variable names?  This program was originally written
%  in micro-Prolog (one of my favorites) with its parenthetical list
%  notation.  I then acquired Arity Prolog and wrote a translation
%  program converted the micro-Prolog syntax to Edinburgh syntax.
%  It did the dumb thing with variable names, and I've never bothered
%  to fix many of them, such as these.)

% The sides are: u up, d down, l left, r right, f front, b back.

move(u, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, V10, V11, V12, 
         V13, V14, V15, V16, V17, V18, V19, V20, V21, V22, V23, 
         V24, V25, V26, V27, V28, V29, 
         V30, V31, V32, V33, V34, 
         V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, 
         V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V20, V19, V21, V10, V11, 
         V12, V8, V7, V9, V16, V17, V18, V26, V25, V27, V22, 
         V23, V24, V14, V13, V15, V28, V29, V30, V43, V44, 
         V33, V34, V39, V40, V37, V38, V31, V32, V41, V42, 
         V35, V36, V45, V46, V47, V48, V49, V50, V51, V52, 
         V53, V54)).
move(d, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, 
  V18, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, 
  V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9,
  V17, V16, V18, V13, V14, V15, V29, V28, 
  V30, V19, V20, V21, V11, 
  V10, V12, V25, V26, V27, V23, V22, V24, V31, V32, 
  V41, V42, V35, V36, V45, V46, V39, V40, V37, V38, V43, V44, V33, 
  V34, V47, V48, V49, V50, V51, V52, V53, V54)).
move(r, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, 
  V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, V30, V31, 
  V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, 
  V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V12, V11, V10, V24,
  V23, V22, V13, V14, V15, V16, V17, V18, V9, 
  V8, V7, V21, V20, V19, V25, V26, V27, V28, V29, V30, V48, V47, V52, 
  V51, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44, V45, V46, 
  V34, V33, V49, V50, V32, V31, V53, V54)).
move(l, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
  V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V27, V26, V25, V15, V14, V13, V19, V20, V21, V22, V23, V24, 
  V30, V29, V28, V18, V17, V16, V31, V32, V33, V34, V54, V53,
  V50, V49, V39, V40, V41, V42, V43, V44, V45, V46, V47, V48,
  V36, V35, V51, V52, V38, V37)).
move(f, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
  V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V13, V15, V14, V7, V9, V8, V16, V18, V17, 
  V10, V12, V11, V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V49, V50, V47, V48,
  V43, V44, V45, V46, V39, V40, V41, V42, V51, V52, V53, V54)).
move(b, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21,
  V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V22, V24, V23, V28, 
  V30, V29, V19, V21, V20, V25, V27, V26, V31, V32, V33, V34, V35,
  V36, V37, V38, V39, V40, V41, V42, V51, V52, V53, V54, V47,
  V48, V49, V50, V45, V46, V43, V44)).
rot(ru, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20, V21,
  V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42,
  V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X2, X4, X3, X5, X1, X6, V20, V19, V21, V23, V22,
  V24, V8, V7, V9, V11, 
  V10, V12, V26, V25, V27, V29, V28, 
  V30, V14, V13, V15, V17, V16, V18, V43, V44, V45, V46, V39,
  V40, V41, V42, V31, V32, V33, V34, V35, V36, V37, V38, V52,
  V51, V48, V47, V54, V53, V50, V49)).
rot(rr, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
  V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31, V32, V33, V34, V35, V36, V37, V38, V39, V40, V41,
  V42, V43, V44, V45, V46, V47, V48, V49, V50, V51, V52, V53, V54), 
       cube(X6, X2, X1, X3, X5, X4, V12, V11, 
  V10, V24, V23, V22, V18, V17, V16, 
  V30, V29, V28, V9, V8, V7, V21, V20, V19, V15, V14, V13,
  V27, V26, V25, V48, V47, V52, V51, V50, V49, V54, V53, V42,
  V41, V46, V45, V40, V39, V44, V43, V34, V33, V38, V37,
  V32, V31, V36, V35)).
rot(rf, 
       cube(X1, X2, X3, X4, X5, X6, V7, V8, V9, 
  V10, V11, V12, V13, V14, V15, V16, V17, V18,
  V19, V20, V21, V22, V23, V24, V25, V26, V27, V28, V29, 
  V30, V31,
  V32, V33, V34, V35, V36, V37, V38, V39, V40, V41, V42, V43, V44,
  V45, V46, V47, V48, V49, V50, V51, V52, V53, V54),
       cube(X1, X3, X5, X4, X6, X2, V13, V15, V14, V7, V9, V8, V16, V18, V17, 
  V10, V12, V11,
  V25, V27, V26, V19, V21, V20, V28, 
  V30, V29, V22, V24, V23, V36,
  V35, V32, V31, V38, V37, V34, V33, V49, V50, V47, V48, V53, V54,
  V51, V52, V39, V40, V41, V42, V43, V44, V45, V46)).
