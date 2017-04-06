%  File     : debug.pl
%  RCS      : $Id: debug.pl,v 1.2 2001/11/19 05:15:24 schachte Exp $
%  Author   : Peter Schachte
%  Origin   : Thu Oct  8 07:36:43 1998
%  Purpose  : Make debugging the analyzer easier
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

:- module(debug, [ print_bryant/1 ]).

:- use_module(analysis, [anz_print/1]).

:- multifile user:portray/1.

user:portray(X) :-
        integer(X),                             % A really ugly hack:
        ( X > 65536 ; X < -65536),              % assume any number > 64K and
        0 =:= X /\ 3,                           % divisible by 4 is a boolfn
        !,
        anz_print_stderr(X).


print_bryant(X) :-
        anz_print(X).
