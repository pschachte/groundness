%  file    : misc
%  Authors : Peter Schachte
%  Purpose : misc support code for Prolog groundness analyzer

:- module(misc, [
	max/3,
	reverse/2
   ]).

%  max(+X, +Y, -Z)
%  Z is the greater of X and Y.

max(X, Y, Z) :-
	(   X >= Y ->
		Z = X
	;   Z = Y
	).


%  reverse(+L0, -L)
%  reverse(+L0, -L, +Tail)
%  List L has the same elements as L0, but in reverse order.  If Tail is
%  supplied, L is L0 reversed followed by Tail.

reverse(L0, L) :-
	reverse(L0, L, []).

reverse([], L, L).
reverse([U|Us], L, Tail) :-
	reverse(Us, L, [U|Tail]).


