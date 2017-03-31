% CVS: $Id: asm.pl,v 1.3 1998/10/20 03:23:27 pets Exp $
/*-----------------------------------------------------------------------------
Program: SB-Prolog assembler 
Author:  
Date:    

Notes:
1. Entry point:
        ?- asm_PIL(InProgram,InOptions).

-----------------------------------------------------------------------------*/

%:- module(asm,[asm_PIL/2]).
%:- imode asm_PIL(g,g).
%:- qmode(asm_PIL(X,Y),info([],[],([],[]))).
goal :- asm_PIL(I,P).

asm_PIL(Inprog,Opts) :- /* already telling outfile */
	asm_get_index(Inprog, NInprog, Index, Nindex, [], 0),
	asm_dopass1(NInprog, Index, Psctable, Labeltable, Ntext, Npsc), 
	asm_magic(3), 
	asm_putnum(Npsc, 4), asm_putnum(Ntext, 4), asm_putnum(Nindex, 4),
	asm_pass2(NInprog, Index, Psctable, Labeltable,Opts),
	asm_mark_eot.


asm_get_index([],[],[],N,_,N).
asm_get_index([Inst|Rest],Inprog,Index,Nindex,Tail,Ni) :-
	asm_index_inst(Inst,Size) ->
		(N is Ni + Size,
		 Index = [Inst|Rindex],
		 asm_get_index(Rest,Inprog,Rindex,Nindex,Tail,N)
		);
		(Inprog = [Inst|Rprog],
		 asm_get_index(Rest, Rprog, Index, Nindex, Tail, Ni)
		).

/* ---------------------------------------------------------------------- */

asm_getaslist(Insts,Index,Nindex,Tail,Ni) :-
	read(Inst0),
	(Inst0 = end_of_file ->
		(Insts = Tail, 
	  	 Index = Tail, Nindex = Ni) ;
	 	(asm_index_inst(Inst0,Size) ->
		  	(N is Ni + Size,
		  	 Index = [Inst0|Rindex],
	  	  	 asm_getaslist(Insts,Rindex,Nindex,Tail,N)) ;
	 	 	(Insts = [Inst0 | Rinsts],
	  	  	 asm_getaslist(Rinsts,Index,Nindex,Tail,Ni))
		)
	).


/*	asm_pass2 takes as input a program containing symbolic labels, 
structure symbols and constants and the symbol tables and returns
a program in which all symbols have been replaced by their byte
code offsets of PSC indices. Also, for "internal" predicates, i.e.
those which are not exported, static linking is carried out as far as
possible.								*/

asm_pass2(Prog,Index,Csym,Lsym,Opts) :-
	asm_symbol(Csym),
	asm_pass2a(Prog,Csym,Lsym,Opts),
	asm_index(Index, Csym, Lsym).

asm_index([],_,_).
asm_index([Inst|Index],Csym,Lsym) :-
	asm_proc_index(Inst, Csym, Lsym),
	asm_index(Index,Csym,Lsym).

asm_proc_index(pred(Label, Num), Csym, Lsym) :-
	Label = (P, N, _), 
	asm_lookup((P, N, _), Csym, PSC_Index),
	asm_gen(pred(PSC_Index, Num)), !.
asm_proc_index(arglabel(T,Val,Label), Csym, Lsym) :-
	((T = c, asm_lookup((Val,0,_), Csym, Nval));
	 (T = s, Val = (Str, Arity),
	  asm_lookup((Str, Arity, _), Csym, Nval));
	 Nval = Val
	), 
	asm_lookup((Label, L), Lsym, _),
	asm_gen(arglabel(T, Nval, L)), !.

asm_pass2a([],_,_,_).
asm_pass2a([Inst|Prog],Csym,Lsym,Opts) :-
	asm_process_pil_inst(Inst,Csym,Lsym,Opts),
	asm_pass2a(Prog,Csym,Lsym,Opts).

asm_process_pil_inst(label((_,_,_)),_,_,_).
asm_process_pil_inst(call(Pred,N),Csym,Lsym,Opts) :- 
	asm_proc_call(Pred,N,Csym,Lsym,Inst,Opts), asm_gen(Inst).
asm_process_pil_inst(execute(Pred),Csym,Lsym,Opts) :-
	asm_proc_exec(Pred,Csym,Lsym,Inst,Opts), asm_gen(Inst).
asm_process_pil_inst(Inst,Csym,Lsym,Opts) :-
	not(Inst = label(_)),
	not(Inst = call(_,_)),
	not(Inst = execute(_)),
	asm_p2(Inst, NInst, Csym),
	asm_gen(NInst).
asm_process_pil_inst(Inst,Csym,Lsym,Opts) :-
	not(Inst = label(_)),
	not(Inst = call(_,_)),
	not(Inst = execute(_)),
	not(asm_p2(Inst, NInst, Csym)),
	asm_p4(Inst, NInst, Lsym),
	asm_gen(NInst).
asm_process_pil_inst(Inst,Csym,Lsym,Opts) :-
	not(Inst = label(_)),
	not(Inst = call(_,_)),
	not(Inst = execute(_)),
	not(asm_p2(Inst, NInst, Csym)),
	not(asm_p4(Inst, NInst, Lsym)),
	asm_gen(Inst).


asm_proc_call((Pred,Arity),Npars,_,Lsym,calld(EPaddr,Npars),Opts) :-
	not( (member1(t,Opts), Arity >= 0) ),
	asm_lookup0( ((Pred,Arity,_),EPaddr), Lsym, _), !.
asm_proc_call((Pred,Arity),Npars,Csym,_,call(PSC_Index,Npars),Opts) :-
	asm_lookup( (Pred,Arity,_), Csym, PSC_Index).

asm_proc_exec((Pred,Arity),_,Lsym,jump(EPaddr),Opts) :-
	not( (member1(t,Opts), Arity >= 0) ),
	asm_lookup0( ((Pred,Arity,_),EPaddr), Lsym, _), !.
asm_proc_exec((Pred,Arity),Csym,_,execute(PSC_Index),Opts) :-
	asm_lookup( (Pred,Arity,_), Csym, PSC_Index).

asm_magic(N) :-
	asm_putnum(17, 1),
	asm_putnum(18, 1),
	asm_putnum(19, 1),
	asm_putnum(N, 1).

asm_index_inst(pred(_,_),8).
asm_index_inst(arglabel(T,_,_),N) :-
	asm_index_inst1(T,N), !.
asm_index_inst(arglabel(T,_,_),N) :-
	N = 5.

asm_index_inst1(i,9).
asm_index_inst1(c,9).
asm_index_inst1(s,9).

asm_pass1([], _, _, N, N).
asm_pass1([label(X)| Rest], Lsym, Csym, Lc, NewLc) :-
	member1((X, Lc), Lsym),
	asm_pass1(Rest, Lsym, Csym, Lc, NewLc).
asm_pass1([Special| Rest], Lsym, Csym, Lc, NLc) :-
	not(Special = label(_)),
	asmpass1_do(Special, Csym),
	asmpass1_doinst(Special, Nbytes),
	NewLc is Lc + Nbytes,
	asm_pass1(Rest, Lsym, Csym, NewLc, NLc).
asm_pass1([Inst| Rest], Lsym, Csym, Lc, NLc):-
	not(Inst = label(_)),
	not(asmpass1_do(Inst,Csym)),
	asmpass1_doinst(Inst, Nbytes),
	NewLc is Lc + Nbytes,
	asm_pass1(Rest, Lsym, Csym, NewLc, NLc).

asmpass1_do(getcon(Con,_), Csym)	:- 
	member1((Con,0,_), Csym).

asmpass1_do(getstr((Str,Arity), _), Csym) :- 
	member1((Str,Arity,_), Csym).

asmpass1_do(unicon(Con), Csym) :- 
	member1((Con,0,_), Csym).

asmpass1_do(putcon(Con,_), Csym) :- 
	member1((Con,0,_), Csym).

asmpass1_do(putstr((Str,Arity), _), Csym) :- 
	member1((Str,Arity,_), Csym).

asmpass1_do(bldcon(Con), Csym) :- 
	member1((Con,0,_), Csym).

asmpass1_do(putstrv((Str,Arity), _), Csym) :- 
	member1((Str,Arity,_), Csym).

asmpass1_do(getstrv((Str,Arity), _), Csym) :- 
	member1((Str,Arity,_), Csym).

asmpass1_do(call((Pname, Arity), _), Csym) :-
	(Arity >= 0, member1((Pname, Arity, _), Csym)) ; (Arity < 0).

asmpass1_do(calld((Pname, Arity), _), Csym) :-
	(Arity >= 0, member1((Pname, Arity, _), Csym)) ; (Arity < 0).

asmpass1_do(execute((Pname, Arity)), Csym):-
	member1((Pname, Arity, _), Csym).

		
asmpass1_doinst(Inst) :- asmpass1_doinst1(Inst,_).
asmpass1_doinst(Junk,2) :-
	not(asmpass1_doinst1(Junk,_)),
	write('unknown opcode detected in pass1'),tab(3),
	write(Junk),nl.

asmpass1_doinst1(label(X),0).
asmpass1_doinst1(getpvar(V,R),4).
asmpass1_doinst1(getpval(V,R),4).
asmpass1_doinst1(gettval(R,R1),4).
asmpass1_doinst1(getcon(I,R),6).
asmpass1_doinst1(getnumcon(I,R),6).
asmpass1_doinst1(getnil(R),2). 
asmpass1_doinst1(getstr(I,R),6).
asmpass1_doinst1(getlist(R),2).
asmpass1_doinst1(getlist_tvar_tvar(R0,R1,R2),4).
asmpass1_doinst1(getcomma(R),2).
asmpass1_doinst1(getcomma_tvar_tvar(R0,R1,R2),4).
asmpass1_doinst1(unipvar(V),2).
asmpass1_doinst1(unipval(V),2).
asmpass1_doinst1(unitvar(R),2).
asmpass1_doinst1(unitval(R),2).
asmpass1_doinst1(unicon(I),6). 
asmpass1_doinst1(uninumcon(N),6). 
asmpass1_doinst1(uninil,2).
asmpass1_doinst1(putpvar(V,R),4).
asmpass1_doinst1(putpval(V,R),4).
asmpass1_doinst1(puttvar(R,R1),4).
asmpass1_doinst1(putcon(I,R),6).
asmpass1_doinst1(putnumcon(N,R),6).
asmpass1_doinst1(putnil(R),2).
asmpass1_doinst1(putstr(I,R),6).
asmpass1_doinst1(putstrv(I,R),6).
asmpass1_doinst1(getstrv(I,V),6).
asmpass1_doinst1(putlist(R),2).
asmpass1_doinst1(bldpvar(V),2).
asmpass1_doinst1(bldpval(V),2).
asmpass1_doinst1(bldtvar(R),2).
asmpass1_doinst1(bldtval(R),2).
asmpass1_doinst1(bldcon(I),6).
asmpass1_doinst1(bldnumcon(N),6).
asmpass1_doinst1(bldnil,2).
asmpass1_doinst1(trymeelse(L,A),6).
asmpass1_doinst1(retrymeelse(L,A),6).
asmpass1_doinst1(trustmeelsefail(A),2).
asmpass1_doinst1(try(L,A),6).
asmpass1_doinst1(retry(L,A),6).
asmpass1_doinst1(trust(L,A),6).
asmpass1_doinst1(getpbreg(V),2).
asmpass1_doinst1(gettbreg(R),2).
asmpass1_doinst1(putpbreg(V),2).
asmpass1_doinst1(puttbreg(R),2).
asmpass1_doinst1(switchonterm(R,L,L1),10).
asmpass1_doinst1(switchoncon(L),6).
asmpass1_doinst1(switchonstr(L),6).
asmpass1_doinst1(switchonbound(R, L1, L2), 10).
asmpass1_doinst1(addreg(R,R1),4).
asmpass1_doinst1(subreg(R,R1),4).
asmpass1_doinst1(mulreg(R,R1),4).
asmpass1_doinst1(divreg(R,R1),4).
asmpass1_doinst1(and(R,R1),4).
asmpass1_doinst1(or(R,R1),4).
asmpass1_doinst1(logshiftl(R,R1),4).
asmpass1_doinst1(logshiftr(R,R1),4).
asmpass1_doinst1(negate(R),2).
asmpass1_doinst1(movreg(R,R1),4).
asmpass1_doinst1(putdval(V,R),4).
asmpass1_doinst1(putuval(V,R),4).
asmpass1_doinst1(call(I,B),6).
asmpass1_doinst1(calld(L,B),6).
asmpass1_doinst1(allocate,2).
asmpass1_doinst1(deallocate,2).
asmpass1_doinst1(proceed,2).
asmpass1_doinst1(execute(I),6).
asmpass1_doinst1(callv(B),2).
asmpass1_doinst1(executev,2).
asmpass1_doinst1(jump(L),6).
asmpass1_doinst1(jumpz(L,R),6).
asmpass1_doinst1(jumpnz(L,R),6).
asmpass1_doinst1(jumplt(L,R),6).
asmpass1_doinst1(jumple(L,R),6).
asmpass1_doinst1(jumpgt(L,R),6).
asmpass1_doinst1(jumpge(L,R),6).
asmpass1_doinst1(fail,2).
asmpass1_doinst1(noop,2).
asmpass1_doinst1(halt,2).
asmpass1_doinst1(builtin(W),2).


/*	Fill in the values of any symbols which have not been defined
with the value -2. */

asmpass1_setundef([], N, N) :- !.
asmpass1_setundef([ (Pred, Arity, Val) | Rest ], Nout, Nin) :-
	(var(Val), Val = -2; nonvar(Val)),
	name(Pred, Chars), 
	length(Chars, L), 
	Nmed is Nin + L + 6,
	asmpass1_setundef(Rest, Nout, Nmed).

/*	Fill in the values of any predicates which are referenced and 
defined within this module.  Append the names of any predicates which
are defined but not referenced by this module.  Ignore predicates with
arity < 0, these are strictly internal jumps that should not go via the
symbol table.								*/

asmpass1_fillin([], _).
asmpass1_fillin([ (Name, Arity, LcValue) | Rest ], Table) :-
	member1((Name, Arity, LcValue), Table),
	asmpass1_fillin(Rest, Table).

/*	Uniq creates a list (Ulist) containing only the first label
encountered for each predicate-name arity pair and excludes all labels
whose predicate-name arity pair have an arity of -1.  Labels with an
arity of -1 are used as the targets of jump instructions only.	*/

asm_uniq([],_).
asm_uniq([ ((Pname, Arity,_),Lc) | Tail], Ulist) :-
	(Arity =:= -1 ;
	 (Arity =\= -1, member2((Pname, Arity, _), Ulist))
	) -> 
	asm_uniq(Tail, Ulist) ;
	(member1((Pname, Arity, Lc), Ulist),
	 asm_uniq(Tail, Ulist)
	).

asm_dopass1(Program, Index, ConstantSymtab, LabelSymtab, Ntext, Npsc) :-
	asm_pass1(Program,LabelSymtab,ConstantSymtab,0,Ntext),
	asm_index_pass1(Index, ConstantSymtab),
	asm_uniq( LabelSymtab, EntrypointTable ), 
	closetail( LabelSymtab ),
	closetail( EntrypointTable ),
	asmpass1_fillin( EntrypointTable, ConstantSymtab ),
	asmpass1_setundef( ConstantSymtab, Npsc, 0),
	!.

asm_index_pass1([],_).
asm_index_pass1([pred(_,_)|Rest], Csym) :- asm_index_pass1(Rest, Csym).
asm_index_pass1([arglabel(T,Val,Label)|Rest],Csym) :-
	T = c,
	member1((Val, 0, _), Csym),
	asm_index_pass1(Rest, Csym).
asm_index_pass1([arglabel(T,Val,Label)|Rest],Csym) :-
	not(T=c),
	T = s,
	Val = (Str, Ar), member1((Str, Ar, _), Csym),
	asm_index_pass1(Rest, Csym).
asm_index_pass1([arglabel(T,Val,Label)|Rest],Csym) :-
	not(T = c),
	not(T = s),
	asm_index_pass1(Rest, Csym).

/*	pass2 takes as input a program containing symbolic labels, 
structure symbols and constants and the symbol tables and returns
a program in which all symbols have been replaced by their byte
code offsets of PSC indices. Also, for "internal" predicates, i.e.
those which are not exported, static linking is carried out as far as
possible.								*/

asm_p2(getcon(Con,R), getcon(I,R), Csym) :-
	asm_lookup((Con, 0, _), Csym, I). 
asm_p2(getstr((Str,Arity), R),getstr(I,R),Csym):-
	asm_lookup((Str,Arity,_), Csym, I).
asm_p2(getstrv((Str,Arity), R),getstrv(I,R),Csym) :-
	asm_lookup((Str,Arity,_), Csym, I).
asm_p2(unicon(Con), unicon(I), Csym) :- asm_lookup((Con,0,_), Csym, I).
asm_p2(putcon(Con,R), putcon(I,R), Csym) :- asm_lookup((Con,0,_),  Csym, I).
asm_p2(putstr((Str,Arity), R),putstr(I,R),Csym) :-
	asm_lookup((Str,Arity,_), Csym, I).
asm_p2(putstrv((Str,Arity), R),putstrv(I,R),Csym):-
	asm_lookup((Str,Arity,_), Csym, I).
asm_p2(bldcon(Con),bldcon(I),Csym) :- asm_lookup((Con,0,_), Csym, I).
asm_p2(call((Pname, Arity), B), call(I,B), Csym) :-
	asm_lookup((Pname, Arity, _), Csym, I).
asm_p2(execute((Pname, Arity)), execute(I), Csym) :-
	asm_lookup((Pname, Arity, _), Csym, I).

asm_p4(trymeelse(L, A), trymeelse(Val,A), Lsym)	:-
	asm_lookup((L, Val), Lsym, _).
asm_p4(retrymeelse(L, A), retrymeelse(Val,A), Lsym) :-
	asm_lookup((L, Val), Lsym, _).
asm_p4(try(L, A), try(Val,A), Lsym) :- asm_lookup((L, Val), Lsym, _).
asm_p4(retry(L, A), retry(Val,A), Lsym) :- asm_lookup((L, Val), Lsym, _).
asm_p4(trust(L, A), trust(Val,A), Lsym) :- asm_lookup((L, Val), Lsym, _).
asm_p4(jump(L), jump(Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(jumpz(R, L), jumpz(R,Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(jumpnz(R, L), jumpnz(R,Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(jumplt(R, L), jumplt(R,Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(jumple(R, L), jumple(R,Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(jumpgt(R, L), jumpgt(R,Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(jumpge(R, L), jumpge(R,Val), Lsym) :- asm_lookup((L, Val),Lsym,_).
asm_p4(switchonterm(R, L1, L2), switchonterm(R,Val1,Val2), Lsym) :-
	asm_lookup((L1, Val1), Lsym, _),
	asm_lookup((L2, Val2), Lsym, _).
asm_p4(switchoncon(L), switchoncon(Val), Lsym) :-
	asm_lookup((L,Val),Lsym,_).
asm_p4(switchonstr(L), switchonstr(Val), Lsym) :-
	asm_lookup((L,Val),Lsym, _).


/*  "asm_lookup0" is the same as "asm_lookup", except that it doesn't give
     an error  message if asm_lookup fails, but simply fails quietly.	*/

asm_lookup0( ( abs(Value), Value ), _, _).
asm_lookup0( Symbol, Symtab, Index ) :- 
	not(Symbol = (abs(_),_)),
	nthmember1( Symbol, Symtab, Index).

asm_lookup(Symbol,Symtab,Index) :- asm_lookup0(Symbol,Symtab,Index).
asm_lookup(Symbol, _, _) :-
	not(asm_lookup0(Symbol,Symtab,Index)),
	umsg(['error on lookup in pass2: ',Symbol,'... aborting execution']),
	abort.

/* asm_symbol outputs the PSC table in byte file header format */

asm_symbol(Symtab) :- member1(Sym, Symtab), asm_putsym(Sym), fail.
asm_symbol(_).

asm_putsym((String, Arity, Value)) :-
	asm_putnum(Value, 4),
	asm_putnum(Arity, 1),
	name(String,Chars),
	length(Chars,L),
	asm_putnum(L,1),
	writename(String),
	!.

/*	Putnum(Number, Length) will write Number as a binary number
which will be Length bytes long */

asm_putnum(Num,1) :- Num < 256, put(Num).
asm_putnum(Num,Nbytes) :- Num >= 256,
		Byte is Num /\ 255,
		Rest is Num >> 8,
		N is Nbytes - 1,
		asm_putnum(Rest,N),
		put(Byte).


asm_opgen(N) :- asm_putnum(N, 1).
asm_opgen_even(N) :- asm_putnum(N, 1), asm_putnum(0, 1).

asm_strgen(N) :- asm_putnum(N,4).

asm_gen(Inst) :- asm_gen1(Inst).
asm_gen1(Junk) :- not(asm_gen1(Junk)),
		  umsg('unknown opcode detected in pass 2 :'),
		  umsg(Junk).

asm_gen1(label(L)).
asm_gen1(pred(I, N)) :- asm_putnum(I, 4), asm_putnum(N, 4).
asm_gen1(arglabel(T, Val, L)) :-
	writename(T),
	(((T = i; T = c; T = s), asm_putnum(Val,4)); true),
	asm_putnum(L, 4).
asm_gen1(getpvar(V,R)) :-
	asm_opgen_even(0), asm_putnum(V,1), asm_putnum(R,1).
asm_gen1(getpval(V,R)) :-
	asm_opgen_even(1), asm_putnum(V,1), asm_putnum(R,1).
asm_gen1(gettval(R,R1)) :-
	asm_opgen_even(3), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(getcon(I,R)) :-
	asm_opgen(4), asm_putnum(R,1), asm_putnum(I,4).
asm_gen1(getnil(R)) :-
	asm_opgen(5), asm_putnum(R,1).
asm_gen1(getstr(I,R)) :-
	asm_opgen(6), asm_putnum(R,1), asm_strgen(I).
asm_gen1(getstrv(I,V)) :-
	asm_opgen(2), asm_putnum(V,1), asm_strgen(I).
asm_gen1(getlist(R)) :-
	asm_opgen(7), asm_putnum(R,1).

asm_gen1(getlist_tvar_tvar(R0,R1,R2)) :-
	asm_opgen(72), asm_putnum(R0,1), asm_putnum(R1,1),
	asm_putnum(R2,1).
asm_gen1(getcomma(R)) :-
	asm_opgen(73), asm_putnum(R,1).
asm_gen1(getcomma_tvar_tvar(R0,R1,R2)) :-
	asm_opgen(74), asm_putnum(R0,1), asm_putnum(R1,1),
	asm_putnum(R2,1).

asm_gen1(unipvar(V)) :- asm_opgen(8), asm_putnum(V,1).
asm_gen1(unipval(V)) :- asm_opgen(9), asm_putnum(V,1).
asm_gen1(unitvar(R)) :- asm_opgen(10), asm_putnum(R,1).
asm_gen1(unitval(R)) :- asm_opgen(11), asm_putnum(R,1).
asm_gen1(unicon(I)) :- asm_opgen_even(12), asm_putnum(I,4).
asm_gen1(uninil) :- asm_opgen_even(13).

asm_gen1(putpvar(V,R)) :-
	asm_opgen_even(16), asm_putnum(V,1), asm_putnum(R,1).
asm_gen1(putpval(V,R)) :-
	asm_opgen_even(17), asm_putnum(V,1), asm_putnum(R,1).
asm_gen1(puttvar(R,R1)) :-
	asm_opgen_even(18), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(putcon(I,R)) :- asm_opgen(20), asm_putnum(R,1), asm_putnum(I,4).
asm_gen1(putnil(R)) :- asm_opgen(21), asm_putnum(R,1).
asm_gen1(putstr(I,R)) :- asm_opgen(22),  asm_putnum(R,1), asm_strgen(I).
asm_gen1(putstrv(I,V)) :- asm_opgen(19), asm_putnum(V,1), asm_strgen(I).
asm_gen1(putlist(R)) :- asm_opgen(23), asm_putnum(R,1).
asm_gen1(bldpvar(V)) :- asm_opgen(24), asm_putnum(V,1).
asm_gen1(bldpval(V)) :- asm_opgen(25), asm_putnum(V,1).
asm_gen1(bldtvar(R)) :- asm_opgen(26), asm_putnum(R,1).
asm_gen1(bldtval(R)) :- asm_opgen(27), asm_putnum(R,1).
asm_gen1(bldcon(I)) :- asm_opgen_even(28), asm_putnum(I,4).
asm_gen1(bldnil) :- asm_opgen_even(29).

asm_gen1(getnumcon(N,R)) :-
	asm_opgen(14), asm_putnum(R,1), asm_putnum(N,4).
asm_gen1(putnumcon(N,R)) :-
	asm_opgen(15),  asm_putnum(R,1), asm_putnum(N,4).
asm_gen1(uninumcon(I)) :- asm_opgen_even(30), asm_putnum(I,4).
asm_gen1(bldnumcon(N)) :- asm_opgen_even(31), asm_putnum(N,4).
asm_gen1(switchonterm(R,L,L1)) :-
	asm_opgen(176), asm_putnum(R,1),  asm_putnum(L,4),
	asm_putnum(L1,4).
asm_gen1(switchoncon(L)) :- asm_opgen_even(177), asm_putnum(L,4).
asm_gen1(switchonstr(L)) :- asm_opgen_even(178), asm_putnum(L,4).
asm_gen1(switchonbound(R, L, L1)) :-
	asm_opgen(179), asm_putnum(R,1), asm_putnum(L,4),
	asm_putnum(L1, 4).
asm_gen1(negate(R)) :- asm_opgen(210), asm_putnum(R,1).
asm_gen1(and(R,R1)) :-
	asm_opgen_even(211), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(or(R,R1)) :-
	asm_opgen_even(212), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(logshiftl(R,R1)) :-
	asm_opgen_even(213), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(logshiftr(R,R1)) :-
	asm_opgen_even(214), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(addreg(R,R1)) :-
	asm_opgen_even(215), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(subreg(R,R1)) :-
	asm_opgen_even(216), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(mulreg(R,R1)) :-
	asm_opgen_even(217), asm_putnum(R,1), asm_putnum(R1,1).
asm_gen1(divreg(R,R1)) :-
	asm_opgen_even(218), asm_putnum(R,1), asm_putnum(R1,1).

asm_gen1(movreg(R,R1)) :-
	asm_opgen_even(209), asm_putnum(R,1), asm_putnum(R1,1).

asm_gen1(trymeelse(L,A)) :-
	asm_opgen(160), asm_putnum(A,1), asm_putnum(L,4).
asm_gen1(retrymeelse(L,A)) :-
	asm_opgen(161),  asm_putnum(A,1), asm_putnum(L,4).
asm_gen1(trustmeelsefail(A)) :- asm_opgen(162), asm_putnum(A,1).
asm_gen1(try(L,A)) :- asm_opgen(163),  asm_putnum(A,1), asm_putnum(L,4).
asm_gen1(retry(L,A)) :- asm_opgen(164), asm_putnum(A,1), asm_putnum(L,4).
asm_gen1(trust(L,A)) :- asm_opgen(165),  asm_putnum(A,1), asm_putnum(L,4).

asm_gen1(getpbreg(V)) :- asm_opgen(166), asm_putnum(V,1).
asm_gen1(gettbreg(R)) :- asm_opgen(167), asm_putnum(R,1).
asm_gen1(putpbreg(V)) :- asm_opgen(168), asm_putnum(V,1).
asm_gen1(puttbreg(R)) :- asm_opgen(169), asm_putnum(R,1).

asm_gen1(putdval(V,R)) :-
	asm_opgen_even(224), asm_putnum(V,1), asm_putnum(R,1).
asm_gen1(putuval(V,R)) :-
	asm_opgen_even(225), asm_putnum(V,1), asm_putnum(R,1).

asm_gen1(call(I,B)) :- asm_opgen(232), asm_putnum(B,1), asm_putnum(I,4).
asm_gen1(allocate) :- asm_opgen_even(233).
asm_gen1(deallocate) :- asm_opgen_even(234).
asm_gen1(proceed) :- asm_opgen_even(235).
asm_gen1(execute(I)) :- asm_opgen_even(236), asm_putnum(I,4).
asm_gen1(calld(L,B)) :- asm_opgen(239), asm_putnum(B,1), asm_putnum(L,4).

asm_gen1(jump(L)) :- asm_opgen_even(240), asm_putnum(L,4).
asm_gen1(jumpz(R,L)) :- asm_opgen(241), asm_putnum(R,1), asm_putnum(L,4).
asm_gen1(jumpnz(R,L)) :- asm_opgen(242), asm_putnum(R,1), asm_putnum(L,4).
asm_gen1(jumplt(R,L)) :- asm_opgen(243), asm_putnum(R,1), asm_putnum(L,4).
asm_gen1(jumple(R,L)) :- asm_opgen(244), asm_putnum(R,1), asm_putnum(L,4).
asm_gen1(jumpgt(R,L)) :- asm_opgen(245), asm_putnum(R,1), asm_putnum(L,4).
asm_gen1(jumpge(R,L)) :- asm_opgen(246), asm_putnum(R,1), asm_putnum(L,4).

asm_gen1(fail) :- asm_opgen_even(248).
asm_gen1(noop) :- asm_opgen_even(249).
asm_gen1(halt) :- asm_opgen_even(250). 
asm_gen1(builtin(W)) :- asm_opgen(251), asm_putnum(W,1).

asm_mark_eot :- asm_opgen_even(255),asm_putnum(0,4).

umsg(X) :- write(X), nl.

nthmember(X,[X|_],1).
nthmember(X,[_|Rest],N) :- nthmember(X,Rest,N1), nonvar(N1), N is N1 + 1.

nthmember1(X,L,N) :- nthmember1a(X,L,0,N).
nthmember1a(X,[X|_],N,N) :- !.
nthmember1a(X,[_|L],N,N1) :- N2 is N+1, nthmember1a(X,L,N2,N1).

closetail([]).
closetail([_|L]) :- closetail(L).
writename(_).

member1( X, [X|Xs]).
member1( X, [_|Xs]):- member1(X,Xs).

member2(X,L) :- nonvar(L), L = [Y|Z], (X = Y ; member2(X,Z)).
