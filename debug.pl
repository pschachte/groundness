%  File     : debug.pl
%  RCS      : $Id: debug.pl,v 1.2 2001/11/19 05:15:24 schachte Exp $
%  Author   : Peter Schachte
%  Origin   : Thu Oct  8 07:36:43 1998
%  Purpose  : Make debugging the analyzer easier
%  Copyright: © 1998 Peter Schachte.  All rights reserved.
%

:- module(debug, [ print_bryant/1, print_bryant/2 ]).

:- use_module(analysis, [anal_print/1]).

:- multifile user:portray/1.

user:portray(X) :-
        integer(X),                             % A really ugly hack:
        ( X > 65536 ; X < -65536),              % assume any number > 64K and
        0 =:= X /\ 3,                           % divisible by 4 is a boolfn
        !,
        anal_print(X).


print_bryant(X) :-
        print_bryant(X, 0).

print_bryant(0, N) :-
        !,
        tab(N),
        write(false),
        nl.
print_bryant(1, N) :-
        !,
        tab(N),
        write(true),
        nl.
print_bryant(Addr, N) :-
        !,
        tab(N),
        N1 is N+4,
        V is integer_at(Addr),
        Tr is address_at(Addr+4),
        Fa is address_at(Addr+8),
        write(V),
        nl,
        print_bryant(Tr, N1),
        print_bryant(Fa, N1).
