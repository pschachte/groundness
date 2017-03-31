% CVS: $Id: parser_dcg.pl,v 1.3 1998/10/19 06:35:22 pets Exp $
goal :- program(X,Y,Z,W).

language(functional).
language(imperative).

integer(0).
integer(1).
integer(2).
append([], L, L).
append([H|L1], L2, [H|L3]) :-
    append(L1, L2, L3).

name(pippo, [112,105,112,112,111]).
name(pippi, [112,105,112,112,105]).
name(poppo, [112,111,112,112,111]).


'C'([Y|Z], Y, Z).

/*
**    SLice: modulo `parse'
**
**    Questo modulo contiene il parser di SLice. E` stato realizzato
**    utilizzando le DCG (Definite Clause Grammars), un formalismo
**    per la realizzazione di parser in Prolog.
**
**    Le DCG permettono di realizzare parser in maniera molto veloce
**    e leggibile, una DCG e` leggibile quasi quanto la produzione
**    di una qualunque grammatica libera.
**    Vedi "Clocksin & Mellish" e "Sterling & Shapiro".
**
**    Il parser accetta in ingresso una lista di token prodotta
**    dal modulo `tokenize' e produce l'albero astratto del programma
**    sotto forma di termine Prolog.
**
**    Il parsing viene effettuato in modo top-down CON backtracking,
**    per il solo motivo che viene consentita l'omissione del ramo
**    else nel comando condizionale.
**    Se si decidesse di eliminare tale possibilita` la grammatica
**    sarebbe LL(1) e quindi il parsing deterministico (predictive).
**
**    Predicati definiti in questo modulo:
**
**    afterBegin/3, afterSide/5, alphaNumeric/1, boolConstant/3,
**    compStat/3, decl/3, decl1/3, decl2/3, decl3/3, expr/3,
**    exprList/3, factor/3, factorAfterId/4, forDirection/5,
**    formList/3, formal/3, funcHead/5, id/3, init/3, lexId/1,
**    limit/3, limits/3, maybeElse/3, maybeInit/4, moreDecl/4,
**    moreDecl1/4, moreDecl2/4, moreExpr/4, moreLimits/4, moreNeEL/4,
**    moreNeFL/4, morePrint/3, moreSExpr/4, moreSList/4, moreTerm/4,
**    neExprList/3, neFormList/3, num/3, optionalStat/3, procHead/4,
**    program/4, reserved/1, simpleExpr/3, simpleType/3, stat/3,
**    statAfterId/4, statList/3, term/3, type/3, unsignedNum/3,
**    varAfterId/4. 
*/

/*
**    Tipi semplici.
**    --------------
*/

simpleType(int) --> [integer], !.
simpleType(bool) --> [boolean], !.

/*
**    Tipi in genere, array compresi.
**    -------------------------------
*/

type(T) --> simpleType(T), !.

type(array(Limits, T)) --> {language(imperative)},
    [array], !, ['['], limits(Limits), [']'], [of], simpleType(T).

limit(b(L, H)) --> num(L), ['..'], num(H).

limits(X) --> limit(Y), moreLimits([Y], X).

moreLimits(Y, X) --> [','], !, limit(Z), {append(Y, [Z], W)}, moreLimits(W, X).
moreLimits(X, X) --> [].

/*
**    Identificatori.
**    ---------------
*/

id(X) --> [X], {atom(X), lexId(X), !, \+ reserved(X)}.

lexId(X) :- name(X, [C|L]),            /* Il primo carattere puo` essere: */
               ((C >= 97, C =< 122) ;  /*   una lettera minuscola o       */
                (C >= 65, C =< 90)  ;  /*   una lettera maiuscola o       */
                 C = 95),              /*   il carattere underscore "_".  */
                   alphaNumeric(L).

alphaNumeric([C|T]) :- ((C >= 97, C =< 122) ;
                 (C >= 65, C =< 90)  ;
                 (C >= 48, C =< 57)  ;
                  C = 95),
                    alphaNumeric(T).
alphaNumeric([]).

/*
**    Variabile o riferimento di array (dopo id).
**    -------------------------------------------
*/

varAfterId(X, aref(X, El)) --> {language(imperative)},
    ['['], !, neExprList(El), [']'].

varAfterId(X, i(X)) --> [].

/*
**    Costanti numeriche.
**    -------------------
*/

unsignedNum(X) --> [X], {integer(X)}, !.

num(X) --> unsignedNum(X), !.
num(X) --> [+], !, unsignedNum(X).
num(X) --> [-], !, unsignedNum(Y), {X is -Y}.

/*
**    Costanti booleane.
**    ------------------
*/

boolConstant(tt) --> [true], !.
boolConstant(ff) --> [false], !.

/*
**    Espressioni.
**    ------------
*/

expr(X) --> simpleExpr(Y), moreExpr(Y, X).

/*
**    moreExpr(Y, X)
**        - Y e` l'albero costruito finora in questa sequenza
**            di termini (ereditato);
**        - X e` l'albero finale (sintetizzato).
**
**    La stessa tecnica (che tra l'altro consente di realizzare
**    l'associativita` a sinistra degli operatori) e` utilizzata
**    piu` volte nel seguito.
**    Vedi Aho, Sethi, Ullman:
**         "Compilers: principles, techniques and tools", chapter 5.
*/

moreExpr(Y, X) --> [==], !, simpleExpr(Z), moreExpr(eq(Y, Z), X).
moreExpr(Y, X) --> [<>], !, simpleExpr(Z), moreExpr(ne(Y, Z), X).
moreExpr(Y, X) --> [<],  !, simpleExpr(Z), moreExpr(lt(Y, Z), X).
moreExpr(Y, X) --> [<=], !, simpleExpr(Z), moreExpr(le(Y, Z), X).
moreExpr(Y, X) --> [>=], !, simpleExpr(Z), moreExpr(ge(Y, Z), X).
moreExpr(Y, X) --> [>],  !, simpleExpr(Z), moreExpr(gt(Y, Z), X).
moreExpr(X, X) --> [].

simpleExpr(X) --> term(Y), moreSExpr(Y, X).

moreSExpr(Y, X) --> [+],  !, term(Z), moreSExpr(plus(Y, Z), X).
moreSExpr(Y, X) --> [-],  !, term(Z), moreSExpr(minus(Y, Z), X).
moreSExpr(Y, X) --> [or], !, term(Z), moreSExpr(or(Y, Z), X).
moreSExpr(X, X) --> [].

term(X) --> factor(Y), moreTerm(Y, X).

moreTerm(Y, X) --> [*],   !, factor(Z), moreTerm(prod(Y, Z), X).
moreTerm(Y, X) --> [div], !, factor(Z), moreTerm(div(Y, Z), X).
moreTerm(Y, X) --> [mod], !, factor(Z), moreTerm(mod(Y, Z), X).
moreTerm(Y, X) --> [and], !, factor(Z), moreTerm(and(Y, Z), X).
moreTerm(X, X) --> [].

factor(X) --> unsignedNum(X), !.
factor(X) --> boolConstant(X), !.
factor(X) --> ['('], !, expr(X), [')'].

factor(X) --> id(Y), !, factorAfterId(Y, X).

/*
**    Chiamata di funzione.
**    ---------------------
*/

factorAfterId(Y, funcall(Y, ExprList)) -->
    ['('], !, exprList(ExprList), [')'].

/*
**    Identificatore o riferimento di array.
**    --------------------------------------
*/

factorAfterId(Y, X) --> varAfterId(Y, X), !.

/*
**    Operatori unari.
**    ----------------
*/

factor(not(X)) -->    [not], !, factor(X).
factor(X) -->         [+],   !, factor(X).
factor(uminus(X)) --> [-],   !, factor(X).

/*
**    Blocco delle espressioni.
**    -------------------------
*/

factor(let(D,E)) --> {language(functional)}, [let], !, decl(D), [in], expr(E).

/*
**    Espressione condizionale.
**    -------------------------
*/

factor(if(E,E1,E2)) --> [if], !, expr(E), [then], expr(E1), [else], expr(E2).

/*
**    Espressioni con side-effects.
**    -----------------------------
*/

factor(side(D, C, E)) --> {language(imperative)},
    [expr], !, afterSide(D, C, E).

afterSide(D, C, E)   --> decl(D), !, [;], optionalStat(C), [result], expr(E).
afterSide(nil, C, E) --> optionalStat(C), !, [result], expr(E).

/*
**    Input dall'utente.
**    ------------------
*/

factor(input(X)) --> [input], !, simpleType(X).

/*
**    Lista di espressioni (attuali).
**    -------------------------------
*/

exprList(X) --> neExprList(X).
exprList([]) --> [].

neExprList(X) --> expr(Y), moreNeEL([Y], X).

moreNeEL(Y, X) --> [','], !, expr(Z), {append(Y, [Z], W)}, moreNeEL(W, X).
moreNeEL(X, X) --> [].

/*
**    Dichiarazioni.
**    --------------
*/

decl(X) --> decl1(Y), moreDecl(Y, X).

/*
**    Composizione sequenziale.
**    -------------------------
*/

moreDecl(Y, X) --> [;], decl1(Z), moreDecl(seq(Y, Z), X).
moreDecl(X, X) --> [].

decl1(X) --> decl2(Y), moreDecl1(Y, X).

/*
**    Composizione parallela.
**    -----------------------
*/

moreDecl1(Y, X) --> ['|'], !, decl2(Z), moreDecl1(parallel(Y, Z), X).
moreDecl1(X, X) --> [].

decl2(X) --> decl3(Y), moreDecl2(Y, X).

/*
**    Composizione privata.
**    ---------------------
*/

moreDecl2(Y, X) --> [->], !, decl3(Z), moreDecl2(private(Y, Z), X).
moreDecl2(X, X) --> [].

/*
**    Dichiarazione semplice.
**    -----------------------
*/

decl3(def(X,T,E)) --> {language(functional)},
                          id(X), !, [:], simpleType(T), init(E).

/*
**    Dichiarazione di costante.
**    --------------------------
*/

decl3(con(X,T,E)) --> {language(imperative)},
                          [const], !, id(X), [:], simpleType(T),
                               init(E).

/*
**    Dichiarazione di variabile.
**    ---------------------------
*/

decl3(var(X,T,E)) --> {language(imperative)},
                          [var], !, id(X), [:], type(T),
                              maybeInit(T, E).

/*
**    Parentetizzazione delle dichiarazioni.
**    --------------------------------------
*/

decl3(X) --> ['('], !, decl(X), [')'].

/*
**    Dichiarazione ricorsiva.
**    ------------------------
*/

decl3(rec(X)) --> [rec], !, decl(X).

/*
**    Inizializzazione di variabile (obbligatoria ed opzionale).
**    ----------------------------------------------------------
*/

init(E) --> [=], expr(E).

maybeInit(_, E) --> init(E).
maybeInit(T, X) --> [],
    {((T=array(_, TypeArray), RealType=TypeArray) ; RealType=T),
        ((RealType=int, X=0) ; (RealType=bool, X=ff))}.

/*
**    Dichiarazione di funzione.
**    --------------------------
*/

decl3(fundef(F, Formals, T, E)) -->
    funcHead(F, Formals, T), !, [=], expr(E).

/*
**    Dichiarazione di procedura.
**    ---------------------------
*/

decl3(procdef(P, Formals, C)) --> {language(imperative)},
    procHead(P, Formals), !, compStat(C).

/*
**    Testa della dichiarazione di procedura.
**    ---------------------------------------
*/

procHead(P, Formals) --> [procedure],
    id(P), ['('], formList(Formals), [')'].

/*
**    Testa della dichiarazione di funzione.
**    --------------------------------------
*/

funcHead(F, Formals, T) --> [function],
    id(F), ['('], formList(Formals), [')'], [:], simpleType(T).

/*
**    Parametri formali.
**    ------------------
*/

formList(X) --> neFormList(X).
formList([]) --> [].

neFormList(X) --> formal(Y), moreNeFL([Y], X).

moreNeFL(Y, X) --> [','], !, formal(Z), {append(Y, [Z], W)}, moreNeFL(W, X).
moreNeFL(X, X) --> [].

formal(ival(X,T)) --> {language(imperative)},    /* call by value         */
    id(X), !, [:], simpleType(T).
formal(fval(X,T)) --> {language(functional)},    /* call by value         */ 
    id(X), !, [:], simpleType(T).
formal(name(X,T)) -->                            /* call by name          */
    [name], !, id(X), [:], simpleType(T).
formal(ref(X,T)) --> {language(imperative)},     /* call by reference     */
    [ref], !, id(X), [:], simpleType(T).
formal(const(X,T)) --> {language(imperative)},   /* call by constant      */
    [const], !, id(X), [:], simpleType(T).
formal(copy(X,T)) --> {language(imperative)},    /* call by value-result  */
    [copy], !, id(X), [:], simpleType(T).
formal(funcpar(F, Formals, T)) -->               /* parametro funzionale  */ 
    funcHead(F, Formals, T), !.
formal(procpar(P, Formals)) -->                  /* parametro procedurale */
    {language(imperative)}, 
    procHead(P, Formals), !.

/*
**    Comandi.
**    --------
*/

/*
**    Blocco di comandi o comando composto.
**    -------------------------------------
*/

compStat(X) --> [begin], afterBegin(X).

afterBegin(block(D, C)) --> decl(D), !, [;], optionalStat(C), [end].
afterBegin(C) --> optionalStat(C), !, [end].

/*
**    Sequenza di comandi opzionale.
**    ------------------------------
*/

optionalStat(X) --> statList(X).
optionalStat(nop) --> [].

/*
**    Sequenza di comandi.
**    --------------------
*/

statList(X) --> stat(Y), moreSList(Y, X).

moreSList(Y, X) --> [;], stat(Z), moreSList(seq(Y, Z), X).
moreSList(X, X) --> [].

/*
**    Comandi.
**    --------
*/

/*
**    Chiamata di procedura o assegnamento.
**    -------------------------------------
*/

stat(X) -->
    id(Y), !, statAfterId(Y, X).

/*
**    Chiamata di procedura.
**    ----------------------
*/

statAfterId(Y, proccall(Y, ExprList)) -->
    ['('], !, exprList(ExprList), [')'].

/*
**    Assegnamento.
**    -------------
*/

statAfterId(Y, ass(X, E)) -->
    varAfterId(Y, X), !, [:=], expr(E).

/*
**    Comando composto.
**    -----------------
*/

stat(X) -->
    compStat(X), !.

/*
**    Comando condizionale.
**    ---------------------
*/

stat(if(E, C1, C2)) -->
    [if], expr(E),
    [then], stat(C1),
    maybeElse(C2).

maybeElse(C) --> [else], !, stat(C).
maybeElse(nop) --> [].

/*
**    Comando while.
**    --------------
*/

stat(while(E,C)) -->
    [while], !, expr(E), [do], stat(C).

/*
**    Comando di uscita `print'.
**    --------------------------
*/

stat(print(Format, El)) -->
    [print], !, ['('], [Format], morePrint(El).
morePrint(El) -->
    [','], !, exprList(El), [')'].
morePrint([]) -->
    [')'].

/*
**    Comando for.
**    ------------
**
**    N.B.: Il comando `for' NON ha un corrispondente in sintassi astratta
**          ma viene espanso dal parser in un ciclo `while' come segue:
**
**                for X := E1 to (downto) E2 do C
**
**          viene tradotto in
**
**                X := E1;
**                begin
**                  const $limit : integer = E2;
**                  while X <= (>=) $limit do begin
**                    begin
**                      const X : integer = X;
**                      C
**                    end;
**                    X := X + (-) 1
**                  end
**                end
**
**          Si noti come C (il comando) sia eseguito in un ambiente
**          in cui X (l'indice del for) e` una costante, e quindi
**          non modificabile.
**          Il comando for del linguaggio imperativo di SLice e` dunque
**          sintatticamente e semanticamente identico a quello di
**          Pascal.
*/

stat(seq(ass(i(X),E1),
            block(con('$limit',int,E2),
              while(Comparison,
                seq(block(con(X,int,i(X)),C),ass(i(X),Update))))))
  --> [for], !, id(X), [:=], expr(E1), forDirection(X, Comparison, Update),
          expr(E2), [do], stat(C).

forDirection(X, le(i(X),i('$limit')), plus(i(X),1)) -->  [to], !.
forDirection(X, ge(i(X),i('$limit')), minus(i(X),1)) --> [downto], !.

/*
**    Programma (categoria iniziale per il linguaggio imperativo).
**    ------------------------------------------------------------
*/

program(Name, Command) --> [program], id(Name), compStat(Command).

/*
**    Parole riservate.
**    -----------------
*/

reserved(program) :- language(imperative).
reserved(const) :- language(imperative).
reserved(var) :- language(imperative).
reserved(array) :- language(imperative).
reserved(of) :- language(imperative).
reserved(begin) :- language(imperative).
reserved(end) :- language(imperative).
reserved(expr) :- language(imperative).
reserved(result) :- language(imperative).
reserved(ref) :- language(imperative).
reserved(copy) :- language(imperative).
reserved(while) :- language(imperative).
reserved(do) :- language(imperative).
reserved(for) :- language(imperative).
reserved(to) :- language(imperative).
reserved(downto) :- language(imperative).
reserved(procedure) :- language(imperative).
reserved(print) :- language(imperative).

reserved(let) :- language(functional).
reserved(in) :- language(functional).

reserved(integer).
reserved(boolean).
reserved(input).
reserved(name).
reserved(rec).
reserved(and).
reserved(or).
reserved(mod).
reserved(div).
reserved(if).
reserved(then).
reserved(else).
reserved(true).
reserved(false).
reserved(not).
reserved(function).



