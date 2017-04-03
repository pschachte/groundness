%  file    : misc
%  Authors : Peter Schachte
%  Purpose : misc support code for Prolog groundness analyzer
%  Copyright 1998 Peter Schachte
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


