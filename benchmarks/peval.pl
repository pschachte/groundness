% CVS: $Id: peval.pl,v 1.3 1998/10/19 06:35:23 pets Exp $
goal :- ground(F), int(F).

% dynamic stuff

label_num(X) :-
  integer(X).

fc_trace(1).

% end dynamic stuff

reverse(X,Y) :-
 sort(X, Y).

/*

Interpreter for flow chart language of Ch. 3 & 4 of Jones, Gomard & Sestoft.


In the following grammar, non-terminals start with upper-case.  The only
meta-chars are ':', '|' and ';'.  Non-terminals starting with the string
"Prolog" are not defined further, but have the meaning intended by their
name.

We fake nice syntax, using the Prolog reader.  The most severe restriction
is that where normal languages allow single statements, we require blocks
enclosed within braces.  Semi-colons are used as statement separators --- in
particular there can never be a semi-colon before a closing brace.  The
then-part and else-part of an if-then-else must also be usually enclosed
within braces --- the only exception is when both the then and else parts
are simple goto statements.

Expressions use normal function/operator syntax.  The only strange operator
is the prefix tilde --- used similarly to the Lisp quote function to prevent
evaluation of its argument.

Labels must be Prolog vars. When programs are read in as terms, along with
other stuff, the same Prolog var should not be used for different purposes.

Pgm			: Block0 .
			;

Block0			: { read NameList ; CmdSeq }
			;
Block			: { CmdSeq }
			;
CmdSeq			: %empty
			| NonEmptyCmdSeq
			;
NonEmptyCmdSeq		: Cmd ';' NonEmptyCmdSeq
			| Cmd
			;
LabelledCmd		: Label ':' LabelledCmd
			| Cmd
			;
Cmd			: Name ':=' Exp
			| goto Label
			| if Exp then Block else Block
			| if Exp then Block
			| if Exp then goto Label 
			| if Exp then goto Label else goto Label
			| do Block while Exp
			| while Exp do Block
			| case Exp of { Cases }
			| return Exp
			;
Cases			: Case ';' Cases
			| Case
			;
Case			: PrologTerm ':' CaseBlock
			| default ':' Block
			;
CaseBlock		: PrologTerm ':' CaseBlock
			| Block
			;
Exp			: Exp RelOp Exp
			| PrologInteger
			| Name
			| ( Exp )
			| ~ PrologTerm	%Quoted (unevaluated) Prolog term.
			| PrologFunctor(ExpList) %Call Prolog pred.
			;
RelOp			: = | \= | < | =< | > | >=
			;
ExpList			: ExpList , Exp
			| Exp
			;
NameList		: NameList , Name
			| Name
			;
Name			: PrologAtom
			;
Label			: PrologVar
			;
	

*/

/*			    OPERATOR DECLARATIONS.			*/
:-op(999, xfy, ':').	%Labels, case values.
:-op(998, xfx, ':=').	%Assignment.
:-op(998, fx, if).
:-op(997, xfy, then).
:-op(997, xfx, else).
:-op(998, fx, do).	%do Block while Exp.
:-op(997, xfx, while).	%do Block while Exp.
:-op(998, fx, while).	%while Exp do Block.
:-op(997, xfx, do).	%while Exp do Block.
:-op(998, fx, case).
:-op(997, xfx, of).
:-op(998, fx, read).
:-op(998, fx, return).
:-op(996, fx, goto).
:-op(199, fx, ~).	%Quote.

/*			TOP-LEVEL INTERPRETER.				*/

%int(PrgFile): Read program from file PrgFile.  Prompt user for values
%for read-variables.  Run program and output result.  Repeat, until user
%enters an invalid list of values.
int(PrgFile):-
  int(PrgFile, user).
int(PrgFile, OutFile):-
  see(PrgFile) , read(RawPrg) , seen ,
  desugar(RawPrg, Prg) , 
  Prg= [read NameList|RestPrg] , length(NameList, L) ,
  store_init(InitStore) , 
  repeat ,
    clear_labels ,
    write('Enter input (file(F) for input from file F; [] to quit): ') ,
    read(Input) ,
    ( Input = []-> 
        true
      ;
      ( Input = file(F)-> 
          see(F) , read(ValList) , seen , length(ValList, L) 
        ; ValList= Input
      ) ,
      store_add(NameList, ValList, InitStore, Store) ,
      statistics(cputime, TBegin) ,
      int(RestPrg, Store, Z) , 
      statistics(cputime, TEnd) ,
      Time is TEnd - TBegin ,
      write('Time = ') , write(Time) , write(' seconds.') , nl ,
      tell(OutFile) , ( Z = prg(OutPrg)-> out_prg(OutPrg) ; write(Z) ) , told ,
      nl , 
      fail
    ).


%int(Prg, Store, Z): Z is the result of interpreting flow chart program Prg
%given initial store Store.
int(Pgm, Store, Z):-
  run(Pgm, Store, Pgm, Z).




/*				DESUGAR.				*/

%desugar(Prg, PrgZ): Translate program from sugared external form
%Prg to internal form PrgZ.  Translate structured constructs.  Translate
%do-while, while-do and case statements into equivalent if and gotos.
%Ensure that for all if statements, then and else parts consist of only
%goto statements.  Linearize outermost block by removing all nested blocks.
%Add an initial label after first read.

t(X):-
  see(X), read(T), seen, desugar(T, TZ) , out_prg(TZ) , fail.

desugar({Read; RestPrg}, [Read, label(init)|RestPrgZ]):-
  translate_cmd_seq(RestPrg, RestPrgX, []) ,
  goto_opt(RestPrgX, RestPrgZ) ,
  make_labels(RestPrgZ).

%translate_block(Prg, PrgZ, PrgZTail): Translate Prg into difference-list
%PrgZ-PrgZTail.
translate_block({}, Prg, Prg).
translate_block({Prg}, PrgZ, PrgZTail):-
  translate_cmd_seq(Prg, PrgZ, PrgZTail).

translate_cmd_seq(CmdSeq, PrgZ, PrgZTail):-
  CmdSeq= (Cmd ; CmdSeq1) ->
    translate_cmd(Cmd, PrgZ, PrgX) , translate_cmd_seq(CmdSeq1, PrgX, PrgZTail)
  ; translate_cmd(CmdSeq, PrgZ, PrgZTail).

%translate_cmd(Cmd, CmdZ, CmdZTail): Translate Cmd into difference-list
%CmdZ-CmdZTail.

translate_cmd(Label:Cmd, [label(Label)|CmdZ], CmdZTail):-
  ! ,
  ( var(Label)-> true ; error(['Label ', Label, ' not a Prolog variable.']) ) ,
  translate_cmd(Cmd, CmdZ, CmdZTail).

translate_cmd(while E do Block, 
	      [label(Loop), 
	       if E then goto Cont else goto Done, label(Cont)|CmdsZ], 
	      CmdsZTail):-
  ! ,
  translate_block(Block, CmdsZ, [goto Loop, label(Done)|CmdsZTail]).

translate_cmd(do Block while E,
	      [label(Loop) | CmdsZ], CmdsZTail):-
  ! ,
  translate_block(Block, CmdsZ, 
	          [if E then goto Loop else goto EndLoop, label(EndLoop)
	           |CmdsZTail]).

translate_cmd(if E then ThenBlock else ElseBlock, 
	      [if E then goto ThenLabel else goto ElseLabel,
	       label(ThenLabel) | ThenCode],
	      CmdsZTail):-
  functor(ThenBlock, {}, _) , functor(ElseBlock, {}, _) ,
  ! ,
  translate_block(ThenBlock, ThenCode, 
	          [goto EndIf, label(ElseLabel)|ElseCode]) ,
  translate_block(ElseBlock, ElseCode, [label(EndIf) | CmdsZTail]).

translate_cmd(if E then ThenBlock, 
	      [if E then goto ThenLabel else goto EndIf,
	       label(ThenLabel) | ThenCode],
	      CmdsZTail):-
  functor(ThenBlock, {}, _) ,
  ! ,
  translate_block(ThenBlock, ThenCode, 
	          [label(EndIf)|CmdsZTail]).


translate_cmd(if E then goto ThenLabel, 
	      [if E then goto ThenLabel else goto EndIf, 
	       label(EndIf)|CmdsZTail],
	      CmdsZTail):-
  !.


translate_cmd(case E of { Cases }, CmdsZ, CmdsZTail):-
  ! ,
  translate_cases(Cases, E, 
		  default(_DefaultCode, _DefaultCodeTail), EndCase,
		  CmdsZ, [label(EndCase)|CmdsZTail]).

translate_cmd(Cmd, [Cmd|Tail], Tail). %Not a structured command.

translate_cases(Cases, E, Default, EndCase, CmdsZ, CmdsZTail):-
  Cases = (Case ; Cases1) ->
    translate_case(Case, E, Default, EndCase, CmdsZ, CmdsX) ,
    translate_cases(Cases1, E, Default, EndCase, CmdsX, CmdsZTail)
  ; translate_case(Cases, E, Default, EndCase, CmdsZ, DefaultCode) ,
    Default = default(DefaultCode, CmdsZTail) ,
    ( var(DefaultCode)-> DefaultCode= CmdsZTail ; true ).


translate_case(default: Block, _E, default(DefaultCode, DefaultCodeTail),
	       _EndCase, CmdsZ, CmdsZ):-
  (functor(Block, {}, _)-> true ; error(['Invalid default case.']) ) ,
  ! ,
  translate_block(Block, DefaultCode, DefaultCodeTail).

translate_case(V : Case, E, _Default, EndCase, 
	       [if (E = V) then goto CaseLabel else goto NextLabel|CmdsZ], 
	       CmdsZTail):-
  translate_case_lo(Case, E, NextLabel, CaseLabel, EndCase, CmdsZ, CmdsZTail).

translate_case_lo(V : Case, E, NextLabel, CaseLabel, EndCase, 
		  [label(NextLabel), 
		   if (E = V) then goto CaseLabel else goto NextLabel1|CmdsZ],
  		  CmdsZTail):-
  ! ,
  translate_case_lo(Case, E, NextLabel1, CaseLabel, EndCase, CmdsZ, CmdsZTail).

translate_case_lo(Block, _E, NextLabel, CaseLabel, EndCase, 
		  [label(CaseLabel)|CmdsZ], CmdsZTail):-
  (functor(Block, {}, _)-> true ; error(['Invalid case block.']) ) ,
  translate_block(Block, CmdsZ, [goto EndCase, label(NextLabel)|CmdsZTail]).
		  


/*			    GOTO OPTIMIZE.				*/

%goto_opt(Prg, PrgZ): Optimize program, removing successive labels by 
%unification.  Also if labelled statement is a goto, then unify goto
%target with the statement label (thus chaining thru gotos).  If a
%goto is preceeded by a control statement (if or goto) and it does not 
%loop to itself then it can be removed, since it becomes redundant 
%after the preceeding optimization.
%We cash in here on using Prolog vars as labels, letting Prolog do all
%the work without explicitly needing some sort of symbol-table.


goto_opt(Prg, PrgZ):-
  goto_opt(Prg, [], [], PrgZ).

%goto_opt(Prg, LastOp, LastLabel, OptPrg):  OptPrg is the optimized version
%of Prg.  LastOp was the last non-label op, and LastLabel is either label(L)
%if labels occurred after the LastOp, [] otherwise.
goto_opt([], _LastOp, _Label, []).
goto_opt([Cmd|Cmds], LastOp, Label, CmdsZ):-
  goto_opt(Cmd, Cmds, LastOp, Label, CmdsZ).

goto_opt(Cmd, Cmds, LastOp, LastLabel, CmdsZ):-
  Cmd = label(L)->
    ( LastLabel = []-> LastLabelX= Cmd ; LastLabel= Cmd , LastLabelX= Cmd ) ,
    ( Cmds = []-> CmdsZ= [Cmd] ; goto_opt(Cmds, LastOp, LastLabelX, CmdsZ) )
  ;
  Cmd = (goto Target)->
    ( (LastLabel = label(L) , L == Target)-> Loop= true ; Loop= false ) ,
    ( LastLabel = label(L) -> Target= L ; true ) ,
    ((is_jmp_op(LastOp) , Loop = false )->
       goto_opt(Cmds, LastOp, [], CmdsZ)  %goto eliminated.
     ; ( Loop = true-> %Need to retain label.
	   CmdsZ= [LastLabel, Cmd|CmdsX] 
	 ; CmdsZ= [Cmd|CmdsX]
       )
    ) ,
    goto_opt(Cmds, Cmd, [], CmdsX)
  ; %Cmd is something besides a goto or a label.
    ( LastLabel = label(L)->
	CmdsZ= [LastLabel, Cmd|CmdsX] ; 
	CmdsZ= [Cmd|CmdsX]
    ) ,
    goto_opt(Cmds, Cmd, [], CmdsX).

is_jmp_op(goto _).
is_jmp_op(if _).
is_jmp_op(return _).


/*			OUTPUT INTERNAL FORM.				*/

out_prg(Prg):-
  write('{') , nl , write('  ') , out_prg_lo(Prg) , write('} .') , nl.

out_prg_lo([]).
out_prg_lo([Cmd|Cmds]):-
  Cmd = label(L) ->
    write(L) , write(': ') , 
    ( Cmds = [] -> write('goto ') , write(L) , nl ; out_prg_lo(Cmds) ) 
  ;
  Cmds = [] -> 
    write(Cmd) , nl 
  ;
    write(Cmd) , write(';') , nl , write('  ') , out_prg_lo(Cmds).


/*				RUN.					*/
%run(Cmds, Store, Prg, Z): Z is the result of interpreting tail Cmds of
%flow-chart program Prg given store Store.
run([Cmd|Cmds], Store, Prg, Z):-
 ( fc_trace(1)-> write(Cmd) , nl ; true ) ,
  run(Cmd, Cmds, Store, Prg, Z).


%run(Cmd, CmdsTail, Store, Prg, Z): Given store Store, Z is the
%result of interpreting command Cmd followed possibly (if Cmd is not a
%transfer) by tail CmdsTail of flow-chart program Prg.

run(goto(N), _CmdsTail, Store, Prg, Z):- 
  ! ,
  ( get_label(Prg, N, NCmds) ->
      run(NCmds, Store, Prg, Z)
    ; error(['Unknown label ', N, '.'])
  ).

run(label(_L), CmdsTail, Store, Prg, Z):-
  ! ,
  run(CmdsTail, Store, Prg, Z).

run(Var:= Exp, CmdsTail, Store, Prg, Z):-
  ! ,
  eval(Exp, Store, ExpVal) , store_update(Store, Var, ExpVal, Store1) ,
  run(CmdsTail, Store1, Prg, Z).

run(if E then goto M else goto N, _, Store, Prg, Z):-
  ! ,
  eval(E, Store, EVal) ,
  ( EVal = 0 ->
      get_label(Prg, N, ElseCmds) , run(ElseCmds, Store, Prg, Z)
    ; get_label(Prg, M, ThenCmds) , run(ThenCmds, Store, Prg, Z)
  ).
	
run(return(E), _CmdsTail, Store, _Prg, Z):-
  ! ,
  eval(E, Store, Z).

run(X, _CmdsTail, _Store, _Prg, _Z):-
  error(['Invalid statement ', X, '.']).


/*			EXPRESSION EVALUATION.				*/	

%eval(E, Store, Z): Z is the result of evaluating expression E given
%store Store.

eval(E, Store, Z):-
  integer(E)->
    Z= E
  ;
  atom(E)->
    store_lookup(E, Store, Z)
  ;
  ( functor(E, F, 2) , is_member(F, [<, =<, >, >=]) ) -> %Relational op.
      arg(1, E, A1) , eval(A1, Store, A1Z) , check_num(A1Z) ,
      arg(2, E, A2) , eval(A2, Store, A2Z) , check_num(A2Z) ,
      T =.. [F, A1Z, A2Z] , 
      (T-> Z= 1 ; Z= 0)
  ;
  ( functor(E, F, 2) , is_member(F, [\=, ==, \==]) ) -> 
      arg(1, E, A1) , eval(A1, Store, A1Z) ,
      arg(2, E, A2) , eval(A2, Store, A2Z) ,
      T =.. [F, A1Z, A2Z] , 
      (T-> Z= 1 ; Z= 0)
  ;
  E = (E1 = E2) ->
    eval(E1, Store, E1Z) , eval(E2, Store, E2Z) ,
    ( \+ \+ (E1Z = E2Z) -> Z= 1 ; Z= 0 ) %Do not bind any vars.
  ;
  E = ~T ->	%Quoted term.
    Z= T
  ;
  %Call Prolog relation with result in last argument.
  functor(E, F, N) , N1 is N + 1 , functor(E1, F, N1) ,	arg(N1, E1, Z) ,
  eval_args(1, N, E, Store, E1) ,
  ( E1-> true ; error(['Could not evaluate ', E, '.']) ).


eval_args(I, N, E, Store, EZ):-
  I =< N ->
    arg(I, E, A) , arg(I, EZ, AZ) , eval(A, Store, AZ) ,
    I1 is I + 1 , eval_args(I1, N, E, Store, EZ)
  ; true.


/*		   SOME PROLOG PREDICATES FOR EVALUATION. 		*/

+(X, Y, Z):-
  check_num(X) , check_num(Y) , Z is X + Y.

-(X, Y, Z):-
  check_num(X) , check_num(Y) , Z is X - Y.

hd([X|_], X).
tl([_|X], X).
tl([], []).
cons(X, Y, [X|Y]).

check_num(N):-
  number(N)->
    true
  ; error([N, ' is not a number.']).

error(MsgList):-
  error_lo(MsgList) ,
  trace , dummy(_).  %Turn on tracing to find out what went wrong.

error_lo([]):-
  nl.
error_lo([T|Ts]):-
  write(T) , error_lo(Ts).


dummy(_).


/*				STORE.					*/

/*
The store is maintained as two parallel lists of Names and Values, with
Names sorted using @<.
*/

%store_init(Store): Store is initialized to an empty store.
store_init(store([], [])).

%store_lookup(Name, Store, NameVal): NameVal is the value of Name in Store.
store_lookup(Name, store(Names, Vals), NameVal):-
  store_lookup(Name, Names, Vals, NameVal).

store_lookup(Name, [], _, _):-
  error(['No value for variable ', Name, ' found in store.']).

store_lookup(Name, [Name1|Names], [NameVal1|NameVals], NameVal):-
  Name = Name1 ->
    NameVal= NameVal1
  ;
  Name @> Name1 ->
    store_lookup(Name, Names, NameVals, NameVal)
  ; fail.

%store_update(Store, Var, Val, StoreZ): StoreZ is the result of updating
%the value of Var to Val in store Store.  The first component of store is
%maintained in a sorted (@<) order.
store_update(store(Names, Vals), Var, Val, store(NamesZ, ValsZ)):-
  store_update(Names, Vals, Var, Val, NamesZ, ValsZ).

store_update([], [], Var, Val, [Var], [Val]).
store_update(NameList, ValList, Var, VarVal, NamesZ, ValsZ):-
  NameList= [Name|Names] , ValList= [Val|Vals] ,
  ( Name = Var ->
      NamesZ= [Name|Names] , ValsZ= [VarVal|Vals]
    ;
    Var @< Name ->
      NamesZ= [Var|NameList] , ValsZ= [VarVal|ValList]
    ; NamesZ= [Name|NamesX] , ValsZ= [Val|ValsX] ,
      store_update(Names, Vals, Var, VarVal, NamesX, ValsX)
  ).

store_add([], [], Store, Store).
store_add([Name|Names], [Val|Vals], Store, StoreZ):-
  store_update(Store, Name, Val, StoreX) ,
  store_add(Names, Vals, StoreX, StoreZ).


/*				LABELS.					*/

%Bind variables used as labels to labnn terms.  This ensures that labels
%don't get accidentally bound during interpretation or program manipulation.
make_labels([]).
make_labels([C|Cs]):-
  ( C = label(L) -> gen_label(L) ; true ) ,
  make_labels(Cs).

%get_label(Prg, Label, LabelledSec): LabelledSec is the section of program
%Prg starting with label Label. 
get_label(Prg, Label, LabelledSec):-
  (Prg = [label(L)|RestPrg] , Label == L) ->
    LabelledSec= RestPrg
  ; Prg= [_|PrgTail] , get_label(PrgTail, Label, LabelledSec).

/* Could use swipl's gensym, but not all Prologs provide them. */
%SWIPL's database (recorded and friends) seems flaky.  Hence use assert
%and friends.

%Uncomment the next line if your Prolog requires dynamic 
%predicates to be declared.
%:-dynamic label_num/1.
label_num(0).

%clear_labels: Call when program starts to start generating labels at 0.
clear_labels:-
  retract(label_num(_)) , asserta(label_num(0)).

%gen_label(L): L is a new label of the form 'labN' where N is an integer.
gen_label(L):-
  retract(label_num(LNum)) ,
  LNum1 is LNum + 1 , asserta(label_num(LNum1)) ,
  name(LNum, LNumName) , conc("lab", LNumName, LName) , name(L, LName).

/*				TRACE.					*/

/* 

Print out each statement as it is executed.  Controlled by
set/reset_fc_trace.  Of limited utility.

*/

fc_trace(0).

set_fc_trace:-
  repeat , \+ retract(fc_trace(_)) , assert(fc_trace(1)).

reset_fc_trace:-
  repeat , \+ retract(fc_trace(_)) , assert(fc_trace(0)).

/*

Prolog utility routines for mix.  The top-level routines are called as
functions from the mix interpreter --- the return value of the function is
the last argument of the corresponding Prolog relation.

*/

%Construct a extended program-point with label A and static store B.
cons_xpp(A, B, xpp(A, B, _Label)).

%Return the extended label associated with an extended program-point.
xlabel(xpp(_, _, L), label(L)).


%Return initial label of program.
initial_pp([_Read, label(L)|_], L).

%Return first basic-block in program.
init_bb([_Read, _Label|BB], BB).

%get_basic_block(L, Program, Z): Z is the basic-block starting after label L
%in Program.
get_basic_block(L, [C|Rest], Z):-
  ( C = label(L1) , L1 == L ) ->
    Z= Rest
  ; get_basic_block(L, Rest, Z).

%pending_pps(Program, Division, Ls): Ls is the list of labels of targets of
%gotos within dynamic ifs in Program.  Division is a list of those Program
%vars which are static.
pending_pps([_Read, label(L)|RestPrg], Division, Ls):-
  get_dynamic_if_targets(RestPrg, Division, [L], LsX) ,
  reverse(LsX, Ls).

%get_dynamic_if_targets(Program, Division, Ls, LsZ): List LsZ is list Ls
%extended with the new targets of gotos within dynamic ifs in Program.  The
%dynamic nature of each if is decided using Division which is a list of the
%static variables in Program.
get_dynamic_if_targets([], _Division, Ls, Ls).
get_dynamic_if_targets([C|Cs], Division, Ls, LsZ):-
  ( ( C = (if Exp then goto Then else goto Else) , 
      \+is_static(Exp, Division) 
    ) ->
      ( is_member(Then, Ls)-> LsX= Ls ; LsX= [Then|Ls] ) ,
      ( is_member(Else, Ls)-> LsY= LsX ; LsY= [Else|LsX] )
    ; LsY= Ls
  ) ,
  get_dynamic_if_targets(Cs, Division, LsY, LsZ).

%next_pp(PP, Program, PPs, PPZ): PPZ is that member of list PPs which is the
%next program-point after PP in Program.
next_pp(PP, [C|Cs], PendingPPs, PPZ):-
  C = label(PP) ->
    next_pp(Cs, PendingPPs, PPZ)
  ; next_pp(PP, Cs, PendingPPs, PPZ).

%next_pp(Program, PPs, PPZ): PPZ is the next program-point in Program
%which is a member of list PPs.
next_pp([C|Cs], PendingPPs, PPZ):-
  ( C = label(L) , is_member(L, PendingPPs) ) ->
    PPZ= L
  ; next_pp(Cs, PendingPPs, PPZ).

%init_code(Read, CodeZ): CodeZ is a difference list containing Read.
init_code(Read, [Read|Code]-Code).

%extend_code(Code, Command, CodeZ):  CodeZ is Code with Command appended.
extend_code(Code-[Command|Rest], Command, Code-Rest).

%complete_code(Code, CodeZ): CodeZ is Code wrapped up.
complete_code(Code - [], prg(Code)).

%read_filter(Program, Division, ReadZ): ReadZ is the read statement which
%is the first statement in Program with the static variables specified by
%list Division removed.
read_filter([read ReadVars|_], Division, read ReadVarsZ):-
  set_difference(ReadVars, Division, ReadVarsZ).


/* 		ACCESSORS & CONSTRUCTORS FOR fc CONSTRUCTS.		*/

cons_assgn(X, Exp, X:= Exp).
assgn_lhs(X:= _Exp, X).   assgn_rhs(_X:= Exp, Exp).


goto_label(goto L, L).

cons_if(Exp, xpp(_, _, ThenLabel), xpp(_, _, ElseLabel), 
	if Exp then goto ThenLabel else goto ElseLabel).
if_test(if Exp then goto _ else goto _, Exp).
then_label(if _Exp then goto ThenLabel else goto _ElseLabel, ThenLabel).
else_label(if _Exp then goto _ThenLabel else goto ElseLabel, ElseLabel).

cons_return(Exp, return Exp).
return_exp(return Exp, Exp).


/* 			XPP LIST MAINTAINENCE.				*/

%add_new_xpp(XPP, Pending, Marked, LiveVarsInfo, Z): Add XPP to Pending
%if it is not in Pending or Marked.  The result is Z.  LiveVarsInfo gives
%the live-variables at each program point used in deciding whether XPP is
%indeed new.
add_new_xpp(XPP, Pending, Marked, LiveVarsInfo, Z):-
  XPP = xpp(L, _, _NewLab) , is_member(L-LiveVars, LiveVarsInfo) ,
  ( ( xpp_member(XPP, Marked, LiveVars) 
      ; xpp_member(XPP, Pending, LiveVars) ) -> 
      Z= Pending 
    ; Z= [XPP|Pending]
  ).

%xpp_member(XPP, XPPs, LiveVars): XPP is a member of XPPs according to 
%the live-variable information specified by LiveVars.
xpp_member(XPP, [xpp(L0, store(Names0, Vals0), NewL0)|XPPs], LiveVars):-
  XPP = xpp(L, store(Names, Vals), NewL) ,
  ( ( L == L0 , equal_vals(LiveVars, Names, Vals, Names0, Vals0) ,
      NewL = NewL0 ) ->
      true
    ; xpp_member(XPP, XPPs, LiveVars)
  ).

%equal_vals(VarNames, Names1, Values1, Names2, Values2): The values in
%Values1 of the variables in VarNames are equal to the values in Values2
%of the variables in VarNames.  Names1 and Names2 gives the variable names
%corresponging to the values in Values1 and Values2.
equal_vals([], _, _, _, _).
equal_vals([V|Vs], N1, V1, N2, V2):-
  get_val(V, N1, V1, VVal1) , get_val(V, N2, V2, VVal2) ,
  VVal1 = VVal2 ,
  equal_vals(Vs, N1, V1, N2, V2).

%get_val(Name, Names, Vals, V): V is the value of Names in the store
%specified by the parallel lists Names and Vals.
get_val(V, [V|_], [VZ|_], VZ).
get_val(V, [N|Ns], [_|Vs], VZ):-
%RB changed, was
%  V \= N , get_val(V, Ns, Vs, VZ).
  V \== N , get_val(V, Ns, Vs, VZ).
  
%is_static(X, Division, Z): Z is 1 if expression X is static according to
%Division (0 otherwise).
is_static(X, Division, Z):-
  is_static(X, Division) ->  Z= 1 ; Z= 0.

%is_static(X, Division): Expression X is static according to Division.
is_static(X, Division):-
  var(X) ->
    error(['Internal error: Prolog var in expression.'])
  ;
  atom(X) -> 
    is_member(X, Division)
  ; 
  ( number(X) ; X = (~ _) )->
    true
  ;
  functor(X, _F, N), is_static_args(1, N, X, Division).

is_static_args(I, N, X, Division):-
  I =< N ->
    arg(I, X, A) , is_static(A, Division) ,
    I1 is I + 1 , is_static_args(I1, N, X, Division)
  ; true.

%Reduce(Exp, Store, Z): Z is the result of reducing expression as much as
%possible using the values of the variables in Store.
reduce(Exp, Store, Z):-
  Store = store(Names, _) ,
  ( Exp = (~ _) ->
      Z= Exp
    ;
    is_static(Exp, Names) ->
      eval(Exp, Store, X) , Z= ~ X
    ; functor(Exp, F, N) , functor(Z, F, N) ,
      reduce_args(1, N, Exp, Store, Z)
  ).

reduce_args(I, N, Exp, Store, Z):-
  I =< N ->
    arg(I, Exp, A) , arg(I, Z, AZ) , reduce(A, Store, AZ) ,
    I1 is I + 1 , reduce_args(I1, N, Exp, Store, Z)
  ; true.


/*		    LIVE-VARIABLE ANALYSIS.			*/

%get_live_vars(Program, Division, PendingPPs, LiveVars): LiveVars is a 
%list of terms of the form L-Vs for each L in list PendingPPs, where Vs is
%the list of static variables in Division which are live in Program at L.
get_live_vars(Program, Division, PendingPPs, LiveVars):-
  live_vars(Program, LiveVarsX) ,
  project_live_vars(LiveVarsX, PendingPPs, Division, LiveVars).


%live_vars(Program, LiveVars): LiveVars is a list of terms of the form 
%bb_info/6 (see below) specifying which variables are live at each label
%in the program.
live_vars([read _|Program], LiveVars):-
  bb_info(Program, BBInfo) ,
  iterate_live_vars(BBInfo, LiveVars).

%bb_info(Program, BBInfoZ):  Collect basic-block information.
%For each basic-block in the program (identified by its 
%entering label), compute bb_info(Label, Succs, Defs, Uses, In, Out), 
%where Label is the label identifying the basic-block, Succs is the labels of
%successive basic-blocks, Defs is the sorted list of vars defined in
%the bb, & Uses are the vars used in the bb, In/Out are []. BBInfoZ is a list 
%of such bb_info structs.
bb_info([label(L)|Prg], [bb_info(L, Succs, Defs, Uses, [], [])|BBInfoZ]):-
  bb_info(Prg, [], [], PrgX, Succs, Defs, Uses) ,
  bb_info(PrgX, BBInfoZ).
bb_info([], []).

bb_info([C|Cs], Defs, Uses, PrgZ, SuccsZ, DefsZ, UsesZ):-
  bb_info(C, Cs, Defs, Uses, PrgZ, SuccsZ, DefsZ, UsesZ). 	

bb_info(label(L), Cs, Defs, Uses, [label(L)|Cs], [L], Defs, Uses):-
  !.
bb_info(if Cond then goto Then else goto Else, Cs, Defs, Uses, 
	Cs, [Then, Else], Defs, UsesZ):-
  ! ,
  update_sets(Cond, Defs, Uses, UsesZ).
bb_info(goto L, Cs, Defs, Uses, Cs, [L], Defs, Uses):-
  !.
bb_info(return Exp, Cs, Defs, Uses, Cs, [], Defs, UsesZ):-
  ! ,
  update_sets(Exp, Defs, Uses, UsesZ).
bb_info(Var:= Exp, Cs, Defs, Uses, CsZ, SuccsZ, DefsZ, UsesZ):-
  update_sets(Exp, Defs, Uses, UsesX) ,
  update_sets(Var, UsesX, Defs, DefsX) ,
  bb_info(Cs, DefsX, UsesX, CsZ, SuccsZ, DefsZ, UsesZ).

%update_sets(Exp, A, B, BZ): BZ is list of atoms B extended by those atoms
%which are in Exp but not in B or list of atoms A.
update_sets(Exp, A, B, BZ):-
  atom(Exp)->
    ( ( is_member(Exp, A) ; is_member(Exp, B) )->
        BZ= B
      ; BZ= [Exp|B]
    )
  ;
  ( Exp = (~ _) ; number(Exp) ) ->
    BZ= B
  ; functor(Exp, _F, N) ,
    update_sets(1, N, Exp, A, B, BZ).

update_sets(I, N, Exp, A, B, BZ):-
  I =< N ->
    arg(I, Exp, Arg) , update_sets(Arg, A, B, BX) ,
    I1 is I + 1 , update_sets(I1, N, Exp, A, BX, BZ)
  ; BZ= B.

%Iteratively compute the in/out sets of the bb_info structs in BBInfo until
%none of the in sets change.
iterate_live_vars(BBInfo, LiveVars):-
  iterate_live_vars(BBInfo, BBInfo, [], false, LiveVars).

%iterate_live_vars(BBInfo, OldBBInfo, AccBBInfo, Change, LiveVars):
%LiveVars is the list of bb_info structs which result by repeatedly
%iterating the in/out set computation of the bb_info structs in BBInfo
%until Change is still false after a complete iteration.
iterate_live_vars([], _AllInfo, AccLiveVars, Change, LiveVars):-
  Change = false ->
    LiveVars= AccLiveVars
  ; iterate_live_vars(AccLiveVars, AccLiveVars, [], false, LiveVars).
iterate_live_vars([LiveVar|LiveVars], AllInfo, AccLiveVars, Change, 
		  LiveVarsZ):-
  bb_live_vars(LiveVar, AllInfo, LiveVarZ, ChangeX) ,
  combine_change(Change, ChangeX, ChangeY) ,
  iterate_live_vars(LiveVars, AllInfo, [LiveVarZ|AccLiveVars], ChangeY,
		    LiveVarsZ).

%bb_live_vars(BB, BBInfos, BBZ, Change): BBZ is the result of performing
%a single update of the in/out sets of bb_info struct BB using the current
%live-variable information in list of bb_info structs BBInfos.  Change is
%true if BB is different from BBZ, false otherwise.
%The computation is out(BB)= U in(BB1) where BB1 is a successor of BB.
%in(BB)= uses(BB) U (out(BB) - defs(BB)).
bb_live_vars(bb_info(L, Succs, Defs, Uses, In, _Out), BBInfo, 
	     bb_info(L, Succs, Defs, Uses, InZ, OutZ), Change):-
  bb_outs(Succs, BBInfo, [], OutZ) ,
  set_difference(OutZ, Defs, X) , set_union(Uses, X, InX) , sort(InX, InZ) ,
  ( InZ = In -> Change = false ; Change = true ).

%bb_outs(Ls, BBInfo, Outs, OutsZ): OutsZ is Outs unioned with the in-sets
%of the basic-blocks corresponding to labels Ls, as specified by BBInfo.
bb_outs([], _BBInfo, Outs, Outs).
bb_outs([L|Ls], BBInfo, Outs, OutsZ):-
  is_member(bb_info(L, _Succs, _Defs, _Uses, In, _Out), BBInfo) ,
  set_union(In, Outs, OutsX) ,
  bb_outs(Ls, BBInfo, OutsX, OutsZ).

%combine_change(ChangeX, ChangeY, ChangeZ): ChangeZ= ChangeX || ChangeY.
combine_change(ChangeX, ChangeY, ChangeZ):-
  ( ChangeX = true ; ChangeY = true ) ->
    ChangeZ= true 
  ; ChangeZ= false.

%project_live_vars(BBInfos, PPs, Div, LiveVars): LiveVars is a list of terms
%of the form L-Vars where for each L in PPs, where Vars are those static
%variables in list Div which are live at L according to the information
%contains in list BBInfos of bb_info structs.
project_live_vars([], _PPs, _Div, []).
project_live_vars([bb_info(L, _Succs, _Defs, _Uses, In, _Out)|LiveInfo],
		  PPs, Div, LiveVars):-
  ( is_member(L, PPs)->
      set_intersection(Div, In, ProjectedIns) , 
      LiveVars= [L-ProjectedIns|LiveVarsX]
    ; LiveVars= LiveVarsX
  ) , 
  project_live_vars(LiveInfo, PPs, Div, LiveVarsX).
  
/*			NORMALIZE					*/

/* 

Normalize fc programs so that they can be compared via unification.
Unify variables followed by ':' with labnn terms.  Replace labxx atoms with
canonical labnn.

*/

normalize(Prog, ProgZ):-
  replace_lab_terms(Prog, 1, N, [], _, ProgX) ,
  replace_var_labels(ProgX, N, _, ProgZ).

%Replace all labnn atoms in the program with labmm terms where mm is
%generated in a systematic way.
replace_lab_terms(Prog, I, N, ReplacedLabels, ReplacedLabelsZ, ProgZ):-
  atom(Prog) ->
    ( name(Prog, [0'l, 0'a, 0'b|_])->
        ( is_member(Prog-ProgZ, ReplacedLabels)->
	    N= I , ReplacedLabelsZ= ReplacedLabels
          ; name(I, IName) , conc("lab", IName, ProgZName) ,
            name(ProgZ, ProgZName) ,
	    N is I + 1 , ReplacedLabelsZ= [Prog-ProgZ|ReplacedLabels]
        )
      ; ProgZ= Prog , N= I , ReplacedLabelsZ= ReplacedLabels
    )
  ;
  ( number(Prog) ; var(Prog) )->
    ProgZ= Prog , N= I , ReplacedLabelsZ= ReplacedLabels
  ;
    functor(Prog, F, M) , functor(ProgZ, F, M) ,
    replace_lab_terms(1, M, Prog, I, N, ReplacedLabels, ReplacedLabelsZ, ProgZ).

replace_lab_terms(J, M, Prog, I, N, ReplacedLabels, ReplacedLabelsZ, ProgZ):-
  J =< M ->
    arg(J, Prog, Arg) , arg(J, ProgZ, ArgZ) ,
    replace_lab_terms(Arg, I, NX, ReplacedLabels, ReplacedLabelsX, ArgZ) ,
    J1 is J + 1 ,
    replace_lab_terms(J1, M, Prog, NX, N, ReplacedLabelsX, 
		      ReplacedLabelsZ, ProgZ)
  ;
    N= I , ReplacedLabelsZ= ReplacedLabels.

%Replace all Prolog variable labels with labnn terms where nn is generated
%in a systematic manner.
replace_var_labels(Prog, I, N, ProgZ):-
  ( atomic(Prog) ; var(Prog) ) ->
     N= I , ProgZ= Prog
  ;
  ( Prog= (L:T) , var(L) )->
      name(I, IName) , conc("lab", IName, LName) , name(L, LName) ,
      ProgZ= (L:TZ) , I1 is I + 1 , replace_var_labels(T, I1, N, TZ)
  ;
    functor(Prog, F, M) , functor(ProgZ, F, M) ,
    replace_var_labels(1, M, Prog, I, N, ProgZ).

replace_var_labels(J, M, Prog, I, N, ProgZ):-
  J =< M ->
    arg(J, Prog, Arg) , arg(J, ProgZ, ArgZ) ,
    replace_var_labels(Arg, I, NX, ArgZ) ,
    J1 is J + 1 , replace_var_labels(J1, M, Prog, NX, N, ProgZ)
  ; N= I.

%fc_compare(FCFile1, FCFile2): Compare FC programs in files FCFile1 & FCFile2.
fc_compare(FCFile1, FCFile2):-
  see(FCFile1) , read(FCPrg1) , seen , normalize(FCPrg1, NormFCPrg1) ,
  see(FCFile2) , read(FCPrg2) , seen , normalize(FCPrg2, NormFCPrg2) ,
  NormFCPrg1 = NormFCPrg2.

/*
This file contains utility Prolog predicates which replace some that
swipl has so that the program can be run with other Prologs which don't
have them.
*/

/*			    SET PREDICATES.				*/

%Sets are represented as unordered lists.


%set_union(X, Y, Z): Z is the union of sets X & Y.
set_union([], Y, Y).
set_union([A|X], Y, Z):-
  ( is_member(A, Y) -> Z= Z1 ; Z= [A|Z1] ) ,
  set_union(X, Y, Z1).

%set_intersection(X, Y, Z): Z is the intersection of sets X & Y.
set_intersection([], _Y, []).
set_intersection([A|X], Y, Z):-
  ( is_member(A, Y) -> Z= [A|Z1] ; Z= Z1 ) ,
  set_intersection(X, Y, Z1).

%set_difference(X, Y, Z): Z is the set X - Y.
set_difference([], _Y, []).
set_difference([A|X], Y, Z):-
  ( is_member(A, Y) -> Z= Z1 ; Z= [A|Z1] ) ,
  set_difference(X, Y, Z1).

%conc(X, Y, Z): Standard append/3.  Different name to avoid clash with
%Prologs which provide it.
conc([], X, X).
conc([A|X], Y, [A|Z]):-
  conc(X, Y, Z).

%is_member(X, Xs): Standard member/2.  Different name to avoid clash with
%Prologs which provide it.
is_member(X, [X|_]).
is_member(X, [_|Xs]):-
  is_member(X, Xs).
