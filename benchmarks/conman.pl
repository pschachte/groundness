% CVS: $Id: conman.pl,v 1.2 1998/10/19 04:21:16 pets Exp $
% From the book
% PROLOG PROGRAMMING IN DEPTH
% by Michael A. Covington, Donald Nute, and Andre Vellino
% (Prentice Hall, 1997).
% Copyright 1997 Prentice-Hall, Inc.
% For educational use only

% CONMAN.PL
%   A simple expert system shell with uncertainty.

%
% Requires procedure defined in the file GETYESNO.PL
%

%:- ensure_loaded('getyesno.pl').

:- dynamic known/3.

%
% CONMAN user interface
%   CONMAN modifies and extends the standard Prolog infer-
%   ence engine, providing the ability to use confidence
%   factors in reaching conclusions. As a result, we dis-
%   tinguish the procedures in CONMAN that handle communi-
%   cation with the user from the predicates that make up
%   the CONMAN inference engine.
%

%
% conman
%   Starts the consultation, calls the procedures making up
%   the inference engine, reports conclusions to the user, and
%   invokes the explanatory facility.
%

conman :- kb_intro(Statement),
          writeln(Statement),nl,
          kb_threshold(T),
          kb_hypothesis(Hypothesis),
          confidence_in([Hypothesis,yes],CF),
          CF >= T,
          write('Conclusion: '),
          writeln(Hypothesis),
          write('Confidence in hypothesis: '),
          write(CF),
          writeln('%.'),
          explain_conclusion(Hypothesis), fail.

conman :- kb_hypothesis(Hypothesis),
          confirm([Hypothesis]),!,
          writeln('No further conclusions.'),
          nl, finish_conman.

conman :- writeln('Can draw no conclusions.'),
          nl, finish_conman.

%
% finish_conman
%   Ends a consultation and begins another if requested.
%

finish_conman :-
     retractall(known(_,_,_)),
     write('Do you want to conduct another consultation?'),
     yes, nl, nl, !, conman.

finish_conman.

%
% ask_confidence(+Hypothesis,-CF)
%   Asks the user to express his/her confidence in the
%   Hypothesis under consideration. User chooses a descriptive
%   phrase in answer to the request. This phrase is converted 
%   to a confidence factor CF between 0 and 100.
%

ask_confidence(Hypothesis,CF) :-
     kb_can_ask(Hypothesis),
     writeln('Is the following conjecture true? --'),
     write('  '), writeln(Hypothesis),
     writeln(['Possible responses: ',
              '  (y) yes            (n) no',
              '  (l) very likely    (v) very unlikely',
              '  (p) probably       (u) unlikely',
              '  (m) maybe          (d) don''t know.',
              '         (?) why?']),
     write('  Your response --> '),
     get_only([y,l,p,m,n,v,u,d,?],Reply), nl, nl,
     convert_reply_to_confidence(Reply,CF),
     !, Reply \== d,
     ask_confidence_aux(Reply,Hypothesis,CF).

%
% ask_confidence_aux(+Char,+Hypothesis,-CF)
%   If the user asks for a justification for a request
%   for information, this procedure invokes the explantory
%   facility and then repeats the request for information.

ask_confidence_aux(Char,_,_) :- Char \== ?, !.

ask_confidence_aux(_,Hypothesis,CF) :-
     explain_question,
     !, ask_confidence(Hypothesis,CF).
%
% get_only(+List,-Reply)
%   An input routine that requires the user to press
%   one of the keys in the List, which the routine
%   returns as Reply.
%

get_only(List,Reply) :-
     get(Char),name(Value,[Char]),
     member(Value,List),Reply = Value, !.

get_only(List,Reply) :-
     write(' [Invalid response.  Try again.] '),
     !,
     get_only(List,Reply).

%
% convert_reply_to_confidence(+Char,-CF)
%   A table for converting characters to numerical
%   confidence factors.
%

convert_reply_to_confidence(?,_).
convert_reply_to_confidence(d,_).
convert_reply_to_confidence(n,0).
convert_reply_to_confidence(v,5).
convert_reply_to_confidence(u,25).
convert_reply_to_confidence(m,60).
convert_reply_to_confidence(p,80).
convert_reply_to_confidence(l,90).
convert_reply_to_confidence(y,100).

%
% explain_question
%   Justifies a request to the user for information by
%   reporting hypotheses the information will be used to
%   test.
%

explain_question :-
     current_hypothesis(Hypothesis),
     writeln(
'This information is needed to test the following hypothesis:'),
     writeln(Hypothesis), nl,
     writeln('Do you want further explanation?'),
     explain_question_aux,!.

explain_question :-
     writeln('This is a basic hypothesis.'),
     nl, wait.

%
% explain_question_aux
%   Inputs the user's indication whether further explanation
%   for a question is desired and, if so, forces explain_question
%   to backtrack and provide further explanation.
%

explain_question_aux :- \+ yes, nl, nl, !.

explain_question_aux :- nl, nl, fail.

%
% explain_conclusion(+Hypothesis)
%   Where Hypothesis is a conclusion just reported, this
%   routine asks the user whether he/she wants to see how
%   the conclusion was derived and, if so, invokes an
%   auxiliary routine that provides the explanation.
%

explain_conclusion(Hypothesis) :-
     writeln('Do you want an explanation?'),
     yes, nl, nl,
     explain_conclusion_aux(Hypothesis), wait, !.

explain_conclusion(_) :- nl, nl.

%
% explain_conclusion_aux(+Hypothesis)
%   Recursively reports all rules and facts used to
%   derive the Hypothesis.
%

explain_conclusion_aux([]) :- !.

explain_conclusion_aux([Hypothesis,_]) :-
     !, explain_conclusion_aux(Hypothesis).

explain_conclusion_aux([and,[Hypothesis,_],Rest]) :-
     !, explain_conclusion_aux(Hypothesis),
     explain_conclusion_aux(Rest).

explain_conclusion_aux([or,[Hypothesis,_],Rest]) :-
     !, explain_conclusion_aux(Hypothesis),
     explain_conclusion_aux(Rest).

explain_conclusion_aux(Hypothesis) :-
     known(Hypothesis,CF,user),
     kb_threshold(T),CF >= T,
     !, write(Hypothesis),writeln(' -'),
     write('From what you told me, I accepted this with '),
     write(CF),writeln('% confidence.'), nl.

explain_conclusion_aux(Hypothesis) :-
     known(Hypothesis,CF,user),
     !, DisCF is 100 - CF,
     write(Hypothesis),writeln(' -'),
     write('From what you told me, I rejected this with '),
     write(DisCF),writeln('% confidence.'), nl.

explain_conclusion_aux(Hypothesis) :-
     known(Hypothesis,50,no_evidence),
     !, write(Hypothesis),writeln(' -'),
     writeln(
          'Having no evidence, I assumed this was 50-50.'),
      nl.

explain_conclusion_aux(Hypothesis) :-
     !, known(Hypothesis,CF1,[CF,Prerequisites,Conditions]),
     writeln(Hypothesis),write('Accepted with '),
     write(CF1),
     writeln('% confidence on the basis of the following'),
     write('Rule: '),writeln(Hypothesis),
     write('  with confidence of '),
     write(CF),
     writeln('% if'),
     list_prerequisites(Prerequisites),
     list_conditions(Conditions), nl,
     explain_conclusion_aux(Conditions).

%
% list_prerequisites(+List)
%   Part of the explanatory facilty.
%   Reports whether the hypotheses in the List have
%   been confirmed or disconfirmed.
%

list_prerequisites([]) :- !.

list_prerequisites([-,Hypothesis|Rest]) :-
     !, write('  is disconfirmed: '),
     writeln(Hypothesis),
     list_prerequisites(Rest).

list_prerequisites([Hypothesis|Rest]) :-
     write('  is confirmed: '),
     writeln(Hypothesis),
     list_prerequisites(Rest).

%
% list_conditions(+Condition)
%   Part of the explanatory facilty.
%   Formats and displays a possibly complex Condition.
%

list_conditions([]) :- !.

list_conditions([and,Hypothesis,Rest]) :-
     list_conditions(Hypothesis),
     list_conditions(Rest).

list_conditions([or,Hypothesis,Rest]) :-
     writeln(' ['),
     list_conditions(Hypothesis),
     writeln('     or'),
     list_conditions(Rest), writeln(' ]').

list_conditions([Hypothesis,yes]) :-
     write('    to confirm: '),
     writeln(Hypothesis).

list_conditions([Hypothesis,no]) :-
     write('    to disconfirm: '),
     writeln(Hypothesis).

%
% wait
%   Prompts the user to press Return and then waits for
%   a keystroke.
%

wait :- write('Press Return when ready to continue. '),
        get0(_), nl, nl.

%
% CONMAN inference engine
%   The CONMAN inference engine computes the confidence in
%   compound goals and decides which of several rules best
%   support a conclusion. It remembers this information for
%   later use by itself, the main conman procedure, and the
%   explanatory facilities.
%

%
% confidence_in(+Hypothesis,-CF)
%   Computes the confidence factor CF for the possibly
%   complex Hypothesis from the confidence factors for
%   sub-hypotheses. Confidence factors for sub-hypotheses
%   come from the working database, are requested from the
%   user, or are determined by whatever evidence is provided
%   by rules that support each sub-hypothesis.
%

confidence_in([],100) :- !.

confidence_in([Hypothesis,yes],CF) :-
     known(Hypothesis,CF,_), !.

confidence_in([Hypothesis,yes],CF) :-
     ask_confidence(Hypothesis,CF), !,
     assert(known(Hypothesis,CF,user)).

confidence_in([Hypothesis,yes],CF) :-
     asserta(current_hypothesis(Hypothesis)),
     findall(X,evidence_that(Hypothesis,X),List),
     findall(C,member([C,_],List),CFList),
     retract(current_hypothesis(_)),
     CFList \== [],
     !, maximum(CFList,CF),
     member([CF,Explanation],List),
     assert(known(Hypothesis,CF,Explanation)).

confidence_in([Hypothesis,yes],50) :-
     assert(known(Hypothesis,50,no_evidence)), !.

confidence_in([Hypothesis,no],CF) :-
     !, confidence_in([Hypothesis,yes],CF0),
     CF is 100 - CF0.

confidence_in([and,Conjunct1,Conjunct2],CF) :-
     !, confidence_in(Conjunct1,CF1),
     confidence_in(Conjunct2,CF2),
     minimum([CF1,CF2],CF).

confidence_in([or,Disjunct1,Disjunct2],CF) :-
     !, confidence_in(Disjunct1,CF1),
     confidence_in(Disjunct2,CF2),
     maximum([CF1,CF2],CF).

%
% evidence_that(+Hypothesis,-Evidence)
%   Finds rules for Hypothesis whose prerequisites are 
%   confirmed and determines the confidence factor for
%   the Hypothesis that can be derived from the rule.
%   The resulting Evidence consists in the confidence 
%   factor together with information about the rule
%   used in determining the confidence factor.
%

evidence_that(Hypothesis,[CF,[CF1,Prerequisite,Condition]]):-
     c_rule(Hypothesis,CF1,Prerequisite,Condition),
     confirm(Prerequisite),
     confidence_in(Condition,CF2),
     CF is (CF1 * CF2)//100.

%
% confirm(+Hypothesis)
%   Checks to see if the confidence factor for the
%   Hypothesis reaches the threshold set in the
%   knowledge base.
%

confirm([]).

confirm([-,Hypothesis|Rest]) :-
     !, known(Hypothesis,CF,_),
     kb_threshold(T),
     M is 100 - CF, M >= T,
     confirm(Rest).

confirm([Hypothesis|Rest]) :-
     known(Hypothesis,CF,_),
     kb_threshold(T),CF >= T,
     !, confirm(Rest).

%
% minimum(+Values,-Minimum)
%   Returns the smaller value Minimum of the pair of
%   Values.
%

minimum([M,K],M) :- M < K, ! .
minimum([_,M],M).

%
% yes
%   Prompts the user for a response and succeeds if the
%   user enters 'y' or 'Y'.
%

yes :-  write('--> '),
        get_yes_or_no(Response),
        !,
        Response == yes.

%
% maximum(+Values,-Maximum)
%   Returns the largest value Maximum in a list of
%   Values.
%

maximum([],0) :- !.
maximum([M],M) :- !.
maximum([M,K],M) :- M >= K, !.
maximum([M|R],N) :- maximum(R,K), maximum([K,M],N).

%
% member(?X,?Y)
%   X is an element of list Y.
%

member(X,[X|_]).
member(X,[_|Z]) :- member(X,Z).

%
% writeln(+Text)
%   Prints Text consisting of a string or a list of
%   strings, with each string followed by a new line.
%

writeln([]) :- !.

writeln([First|Rest]) :-
        !,
        write(First),
        nl,
        writeln(Rest).

writeln(String) :-
        write(String),
        nl.

% From the book
% PROLOG PROGRAMMING IN DEPTH
% by Michael A. Covington, Donald Nute, and Andre Vellino
% (Prentice Hall, 1997).
% Copyright 1997 Prentice-Hall, Inc.
% For educational use only

% File GETYESNO.PL
% Menu that obtains 'yes' or 'no' answer

get_yes_or_no(Result) :- get(Char),              % read a character
                         get0(_),                % consume the Return after it
                         interpret(Char,Result),
                         !.                      % cut -- see text

get_yes_or_no(Result) :- nl,
                         write('Type Y or N:'),
                         get_yes_or_no(Result).

interpret(89,yes).  % ASCII 89  = 'Y'
interpret(121,yes). % ASCII 121 = 'y'
interpret(78,no).   % ASCII 78  = 'N'
interpret(110,no).  % ASCII 110 = 'n'
