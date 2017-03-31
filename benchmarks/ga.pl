% CVS: $Id: ga.pl,v 1.3 1998/10/21 04:25:57 pets Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		       SIMPLE GENETIC ALGORITHM
%
% This file implements the shell of the simple genetic algorithm,
% which uses only recombination in its search:
% 1. Initialize population of size N (*)
% 2. Compute fitness of each individual (*)
% 3. Sort population by fitness
% 4. If best individual is a solution to the problem, (*)
%    return the individual.
% 5. Otherwise, let the fittest 1/2 N individuals recombine
%    by: (a) select random mate from original pop, (b) perform
%    recombination, yielding 2 new individuals, (c) insert new
%    individuals into pop
% 6. Loop back to step 2 with new population.
%
% Note that:
% (a) selection is simple (ranked on fitness only)
% (b) no mutation
% (c) fixed size population.
%
% The user must specify entries with (*):
% - genome_length(-NumberOfWords/NumberOfBits)    
%          genome has NumberOfWords units of length NumberOfBits.
% - fitness(+Individual,-Fitness)   Fitness is fitness of Individual
%

goal :- test_ga.


test_ga :-
	format(user_output,'number of generations?~n',[]),
	read(Gen), integer(Gen),
	format(user_output,'number of individuals? (must be even)~n',[]),
	read(Pop), integer(Pop),
	test_ga(Gen, Pop).

test_ga(Gen, Pop) :-
	statistics(instr,[SeqA, ParA]),
	statistics(walltime, [T1|_]),
	ga(Gen,Pop,_),
	statistics(walltime, [T2|_]),
	statistics(instr,[SeqB, ParB]),
	Time is T2 - T1,
	Instr is (SeqB + ParB) - (SeqA + ParA),
	format('best solution found in ~w secs~n',[Time]),
	format('map: ~w instructions executed~n',[Instr]).

% ga(+NumberOfGenerations,+PopulationSize,-SolutionIndividual)
%  - number of generations and population size are integers
%  - the returned solution is a "Fitness-Individual" pair.

ga(MaxGen,N,Solution) :-
	HalfN is N//2,
	genome_length(Genome),
	initial_population(N,Genome,Pop),
	ga_iterate(MaxGen,Pop,N,HalfN,Genome,Solution).

:- parallel([initial_population/3]).

initial_population(0,_Genome,Pop) :- !, Pop = [].
initial_population(N,Genome,Pop) :- N > 0,
	individual(Genome,Indiv),
	Pop = [Indiv|RestPop],
	M is N-1,
	initial_population(M,Genome,RestPop).

individual(GenomeWords/GenomeBits,Indiv) :-
	iterate_random_pattern(1,GenomeWords,GenomeBits,GW),
	Indiv =.. ['$'|GW].

iterate_random_pattern(M,N,_GenomeBits,Indiv) :- M > N, !, Indiv = [].
iterate_random_pattern(M,N,GenomeBits,Indiv) :- M =< N,
	M1 is M+1,
	Indiv = [Wd|Rest],
	indiv_word(GenomeBits,Wd),
	iterate_random_pattern(M1,N,GenomeBits,Rest).

indiv_word(GenomeBits,IndivWd) :-
	X is 1 << GenomeBits,
	ran(X,IndivWd).     % random bit pattern of length = genome

ga_iterate(0,Pop,_N,_HalfN,_Genome,Solution) :- !,
	fitness_of_population(Pop,FitnessPop),
	sort_by_fitness(FitnessPop,SortedPop),
	SortedPop = [Solution|_].
ga_iterate(GenLeft,Pop,N,HalfN,Genome,Solution) :- GenLeft > 0,
	NewGenLeft is GenLeft-1,
	fitness_of_population(Pop,FitnessPop),
	sort_by_fitness(FitnessPop,SortedPop),
	show_best_solution(SortedPop),
	recombine_all(HalfN,SortedPop,N,Pop,Genome,NewPop),
	ga_iterate(NewGenLeft,NewPop,N,HalfN,Genome,Solution).

:- parallel([fitness_of_population/2]).

fitness_of_population([],[]).
fitness_of_population([X|Xs],[FX|FXs]) :-
	fitness(X,F),
	FX = F-X,
	fitness_of_population(Xs,FXs).

converged([F-X|_],Soln) :-
	format('best fitness: ~q~n',[F]),
	solution(F), Soln = X.

recombine_all(0,_SortedPop,_N,_Pop,_Genome,NewPop) :- !, NewPop = [].
recombine_all(K,[_F-X|Xs],N,Pop,Genome,NewPop) :- K > 0,
	ran(N,Indiv),
	nth(Indiv,Pop,Mate),
	recombine(X,Mate,Genome,Y1,Y2),
	NewPop = [Y1,Y2|RestPop],
	L is K-1,
	recombine_all(L,Xs,N,Pop,Genome,RestPop).

recombine(X1,X2,GenomeWds/GenomeBits,Y1,Y2) :-
	select_recombination_word(X1,X2,GenomeWds,Wd1,Wd2,NewWd1,NewWd2,Y1,Y2),
	Genome0 is GenomeBits-1,
	ran(Genome0,SplitPoint0),
	SplitPoint is SplitPoint0+1,
	splitting_mask(GenomeBits,SplitPoint,UpperPart,LowerPart),
	NewWd1 is (Wd1 /\ UpperPart) \/ (Wd2 /\ LowerPart),
	NewWd2 is (Wd2 /\ UpperPart) \/ (Wd1 /\ LowerPart).

select_recombination_word(X1,X2,GenomeWds,Wd1,Wd2,NewWd1,NewWd2,Y1,Y2) :-
	ran(GenomeWds,SplitAtWord),
	X1 =.. [_|X1s],
	X2 =.. [_|X2s],
	new_lists(SplitAtWord,X1s,X2s,Y1s,Y2s,Wd1,Wd2,NewWd1,NewWd2),
	Y1 =.. ['$'|Y1s],
	Y2 =.. ['$'|Y2s].

new_lists(0,X1s,X2s,Y1s,Y2s,Wd1,Wd2,NewWd1,NewWd2) :- !,
	X1s = [Wd1|Y2sRest],
	X2s = [Wd2|Y1sRest],
	Y1s = [NewWd1|Y1sRest],
	Y2s = [NewWd2|Y2sRest].
new_lists(N,[X1|X1s],[X2|X2s],[X1|Y1s],[X2|Y2s],Wd1,Wd2,NewWd1,NewWd2) :-
	N > 0,
	M is N-1,
	new_lists(M,X1s,X2s,Y1s,Y2s,Wd1,Wd2,NewWd1,NewWd2).

splitting_mask(Genome,SplitPoint,UpperPart,LowerPart) :-
	LowerPart is (1 << SplitPoint)-1,
	UpperPart is ((1 << (Genome - SplitPoint)) - 1) << SplitPoint.


reverse([],[]).
reverse([X|Xs],Ys) :-
	reverse(Xs,[X],Ys).

reverse([],Ys,Ys).
reverse([X|Xs],Ys,Zs) :-
	reverse(Xs,[X|Ys],Zs).

nth(0,Lst,Elt) :- !,
	Lst = [Elt|_].
nth(N,[_|Xs],Elt) :- N > 0,
	M is N-1,
	nth(M,Xs,Elt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SICStus
%
% :- use_module(library(random)).
%
% random(High,N) :- random(0,High,N).
%
% ran(High,N) :- random(High,N).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reform Prolog
%
% Randomizer is one-based rather than zero-based.

ran(High,N) :- random(High,N1), N is N1-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extremely simple test: the 'bigger' elements win.
%
%genome_length(8).
%solution(X) :- (X =:= 255 ; X =:= 0).
%fitness(X,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%       SOLVING TRAVELLING SALESPERSON WITH A GENETIC ALGORITHM
%
% We represent 2^k cities with k bits. The genome is a (2^k) long
% sequence of k-bit segments which represents the order of visits.
%
% The cities are represented as integer coordinates on a grid. They
% are represented as city(Number,X_coord,Y_coord).
%
% In the example, we randomly choose one city to start at. Then, the
% sequence of visits is done. When the genome runs out, there may be
% unvisited cities. These are visited in some order, which is
% from lowest to highest numbered here.
%
% The genome is viewed as a circular list -- we choose one word, then
% one city in that word and perform the visit by running to the end, then
% starting from the beginning and visiting until the city prior to
% the first one.
%
% Note that we use the single-point crossover operation. If we used some
% other method, we might get different results.

% Note that 'lower fitness is better'.

fitness(Individual,Fitness) :-
	Individual =.. [_|Xs],
	city(0,X,Y),              % always start at city 0
	tour(Xs,X,Y,NewX,NewY,[0],Visited,0,Dist),
	visit_the_unvisited(Visited,NewX,NewY,Dist,Fitness,
	                    _CompleteVisit,Visited).

tour([],X,Y,X,Y,Vis,Vis,Dist,Dist).
tour([C|Cs],X,Y,NewX,NewY,Visited,NewVisited,Dist,NewDist) :-
	visit_cities(C,X,Y,TmpX,TmpY,Visited,TmpVisited,Dist,TmpDist),
	tour(Cs,TmpX,TmpY,NewX,NewY,TmpVisited,NewVisited,TmpDist,NewDist).

visit_cities(C,X,Y,NewX,NewY,Visited,NewVisited,Dist,NewDist) :-
	select_cities(C,C1,C2,C3),
	city(C1,C1X,C1Y),
	city(C2,C2X,C2Y),
	city(C3,NewX,NewY),
	distance(X,Y,C1X,C1Y,Dist,D0),
	distance(C1X,C1Y,C2X,C2Y,D0,D1),
	distance(C2X,C2Y,NewX,NewY,D1,NewDist),
	visit([C3,C2,C1],Visited,NewVisited).

% Visited cities will appear in reverse order!

visit(X,Y,Z) :- append(X,Y,Z).

% dummy right now.

visit_the_unvisited(Xs,NewX,NewY,D0,D1,V0,V1) :-
	sort(Xs,SortedXs),
	visit_unvisited(0,127,NewX,NewY,SortedXs,D0,D1,V0,V1).

visit_unvisited(M,N,_,_,_ ,D0,D1,V0,V1) :- M > N,!, D0 = D1, V0 = V1.
visit_unvisited(M,N,X,Y,Xs,D0,D2,V0,V2) :- M =< N,
	( Xs = [] ->
	  Ys = [],
	  V0 = [M|V1],
	  city(M,TmpX,TmpY),
	  distance(X,Y,TmpX,TmpY,D0,D1)
	; Xs = [C|Cs] ->
	  ( C =:= M ->
	    V0 = V1,
	    D1 = D0, Ys = Cs
	  ; Ys = [X|Xs],
	    V0 = [M|V1],
	    city(M,TmpX,TmpY),
	    distance(X,Y,TmpX,TmpY,D0,D1)
	  )
	),
	M1 is N+1,
	visit_unvisited(M1,N,TmpX,TmpY,Ys,D1,D2,V1,V2).

% The algorithm is hard-coded for 128 cities: it uses 43 words where
% 3 cities of 7 bits each are stored. Parts of the last word are unused.
% City1 is found as bits 0 to 6
% City2                  7    13
% City3                  14   20

select_cities(Word,City1,City2,City3) :-
	City1Mask is (1 << 7)-1,
	City1 is Word /\ City1Mask,
	City2Mask is ((1 << 14)-1) /\ \(City1Mask),
	City2 is (Word /\ City2Mask) >> 7,
	City3Mask is ((1 << 21)-1) /\ \(City2Mask \/ City1Mask),
	City3 is (Word /\ City3Mask) >> 14.

distance(X,Y,NewX,NewY,D0,D1) :-
	X0 is (NewX-X), Y0 is NewY-Y,
	D1 is sqrt(X0*X0 + Y0*Y0)+D0.

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).
%
/*
:- dynamic city/3.

random_cities(0,_,_) :- !.

random_cities(N,X_lim,Y_lim) :- N > 0,
	ran(X_lim,X_pos),
	ran(Y_lim,Y_pos),
	( city(_,X_pos,Y_pos) -> % discard if exists
	  random_cities(N,X_lim,Y_lim)
	; M is N-1,
	  assert(city(M,X_pos,Y_pos)),
	  random_cities(M,X_lim,Y_lim)
	).

*/
city(127, 520, 509).
city(126, 663, 864).
city(125, 173, 523).
city(124, 565, 932).
city(123, 464, 66).
city(122, 609, 680).
city(121, 805, 633).
city(120, 745, 263).
city(119, 11, 700).
city(118, 457, 993).
city(117, 803, 357).
city(116, 107, 343).
city(115, 908, 568).
city(114, 325, 454).
city(113, 60, 366).
city(112, 155, 453).
city(111, 840, 773).
city(110, 784, 908).
city(109, 91, 455).
city(108, 594, 81).
city(107, 189, 453).
city(106, 563, 463).
city(105, 145, 621).
city(104, 73, 345).
city(103, 797, 186).
city(102, 899, 702).
city(101, 686, 838).
city(100, 606, 480).
city(99, 227, 691).
city(98, 875, 343).
city(97, 110, 937).
city(96, 226, 457).
city(95, 292, 360).
city(94, 138, 632).
city(93, 89, 849).
city(92, 314, 900).
city(91, 406, 330).
city(90, 984, 785).
city(89, 858, 786).
city(88, 571, 246).
city(87, 623, 451).
city(86, 697, 903).
city(85, 513, 205).
city(84, 604, 777).
city(83, 388, 263).
city(82, 221, 477).
city(81, 551, 992).
city(80, 653, 963).
city(79, 598, 689).
city(78, 945, 499).
city(77, 260, 923).
city(76, 822, 307).
city(75, 414, 549).
city(74, 815, 235).
city(73, 838, 442).
city(72, 386, 499).
city(71, 778, 723).
city(70, 97, 627).
city(69, 477, 147).
city(68, 374, 517).
city(67, 615, 333).
city(66, 199, 269).
city(65, 698, 564).
city(64, 953, 731).
city(63, 994, 32).
city(62, 677, 655).
city(61, 62, 208).
city(60, 632, 743).
city(59, 661, 117).
city(58, 755, 89).
city(57, 427, 552).
city(56, 707, 492).
city(55, 756, 239).
city(54, 254, 884).
city(53, 313, 655).
city(52, 29, 563).
city(51, 506, 807).
city(50, 911, 477).
city(49, 629, 741).
city(48, 25, 631).
city(47, 622, 725).
city(46, 210, 413).
city(45, 268, 143).
city(44, 879, 886).
city(43, 499, 280).
city(42, 961, 593).
city(41, 425, 295).
city(40, 242, 515).
city(39, 393, 665).
city(38, 77, 28).
city(37, 615, 413).
city(36, 462, 503).
city(35, 873, 645).
city(34, 559, 449).
city(33, 103, 669).
city(32, 888, 190).
city(31, 501, 557).
city(30, 793, 900).
city(29, 334, 450).
city(28, 560, 339).
city(27, 381, 603).
city(26, 390, 422).
city(25, 992, 167).
city(24, 766, 176).
city(23, 12, 584).
city(22, 140, 59).
city(21, 935, 304).
city(20, 456, 437).
city(19, 135, 631).
city(18, 121, 823).
city(17, 38, 6).
city(16, 499, 346).
city(15, 500, 368).
city(14, 747, 78).
city(13, 35, 671).
city(12, 651, 658).
city(11, 432, 299).
city(10, 981, 444).
city(9, 954, 595).
city(8, 483, 525).
city(7, 723, 213).
city(6, 967, 244).
city(5, 194, 765).
city(4, 561, 744).
city(3, 567, 360).
city(2, 877, 38).
city(1, 303, 36).
city(0, 185, 721).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_by_fitness(FitPop,TmpPop) :-
	keysort(FitPop,TmpPop).

genome_length(43/21).

show_best_solution([F-_X|_]) :-
%	format('best solution: distance ~q~n',[F]),
	true.

