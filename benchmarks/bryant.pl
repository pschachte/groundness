% CVS: $Id: bryant.pl,v 1.3 1998/10/19 06:35:06 pets Exp $
%  File   : bryant
%  Authors: Peter Schachte
%  Purpose: Manipulate ordered binary decision diagrams
%
%				Abstract
%
%  This code manipulates ordered binary decision graphs using variations on
%  Bryant's algorithms.  I've concentrated on being efficient here, at the
%  price of using some non-logical Prolog features.  In particular, I have
%  several var/1 tests.  These are necessary to avoid much structure copying.



/*************************************************************************

				 Introduction

This code manipulates ordered binary decision graphs, also known as Bryant
graphs, using variations on Bryant's algorithms.  We represent nodes in a
Bryant graph as one of:

++itemize
    'true'
    'false'
    ite(*Nodenum*, *Variable*, *TrueChild*, *FalseChild*)
--itemize

where

++description
    *Nodenum*		is an integer uniquely identifying this node
    *Variable*		is the number of the variable this node decides upon
    *TrueChild*		is the bryant graph to use if Variable is true
    *FalseChild* 	is the bryant graph to use if Variable is false
--description

Note that 'ite' means "if-then-else," which is what the nodes of a decision
tree mean.

In this code, I've folded the reduction of the Bryant graph into the code for
applying an operation on two graphs.  I've also folded variable restriction
(i.e., "existentially quantifying away" variables) into the code for
conjoining two graphs.  This avoids building a lot of graph that we're just
going to throw away anyway.

All the code in this files works on reduced ordered binary decision graphs.
If the inputs are not reduced, the outputs may not be.

*************************************************************************/

/*************************************************************************

				 Disjunction

This code computes the reduced disjunction of two Bryant graphs in a single
step.  The reduction of the graph is folded into the construction of the
graph, which is implemented by compose_node/7.

*************************************************************************/


%  goal
%  to make this useful as a benchmark, we add an initial goal that calls much
%  of the code in this file.  This code does the work of one of the steps in
%  analyzing append/3.

goal :-
        ground([I1,I2,I3,I4,I5,I6,I7,I8,I9]),
        iff_conj(I1, I2, G1),
        iff_conj(I3, I4, G2),
        bryant_and(G1, G2, I5, G3),
        iff_conj(I6, I7, G4),
        bryant_and(I8, G4, G5),
        bryant_and(G3, G5, I9, G6),
        bryant_or(G5, G6, G),
        write(G).

/*RB
goal :-
	iff_conj(1, [6,5], G1),
	iff_conj(3, [6,4], G2),
	bryant_and(G1, G2, 5, G3),
	iff_conj(2, [3], G4),
	bryant_and(ite(1,1,true,false), G4, G5),
	bryant_and(G3, G5, 3, G6),
	bryant_or(G5, G6, G),
	write(G).
*/

%  bryant_or(+Graph1, +Graph2, -Graph)
%  bryant_or(+Graph1, +Graph2, -Graph, +Id0, -Id, +-Generated, +-Done)
%  Graph is the disjunction of Graph1 and Graph2.  Id0 is the first node
%  number to use, and Id is the lowest node number not used in Graph.
%  Generated stores the nodes we've generated so far in constructing Graph,
%  indexed by the node numbers of their two children.  This lets us avoid
%  generating the same node twice.  Done stores the pairs of nodes of Graph1
%  and Graph2 we've already disjoined, and the disjunction we've computed for
%  them.  This lets avoid redoing work.  Both Generated and Done can be viewed
%  as cheap memoization tables.  Both are hash tables represented as terms of
%  arity 127, with each argument of the term a list of entries that hash to
%  the same hash key.  In the case of Generated, the elements of each hash
%  bucket are simply nodes; for Done, the elements are result(N1, N2, N)
%  terms, where the result of disjoining node N1 with node N2 was node N.  
%  Both Generated and Done are incomplete structures, ie, the tails of the
%  lists representing hash buckets are unbound.  This isn't very clean, but is
%  much more efficient, in the absence of copy avoidance, than copying large
%  terms on each update.

bryant_or(G1, G2, G) :-
	functor(Generated, generated, 127),
	functor(Done, done, 127),
	bryant_or(G1, G2, G, 2, _, Generated, Done).


bryant_or(true, _, G, Id, Id, _, _) :-
	!,
	G = true.
bryant_or(false, G2, G, Id0, Id, Generated, _) :-
	!,
	copy_graph(G2, G, Id0, Id, Generated).
bryant_or(G1, G2, G, Id0, Id, Generated, Done) :-
	bryant_or_1(G2, G1, G, Id0, Id, Generated, Done).


%  bryant_or_1(+Graph2, +Graph1, -Graph, +Id0, -Id, +-Generated, +-Done)
%  Same as bryant_or/7, except that first two args are swapped for indexing.

bryant_or_1(true, _, G, Id, Id, _, _) :-
	!,
	G = true.
bryant_or_1(false, G1, G, Id0, Id, Generated, _) :-
	!,
	copy_graph(G1, G, Id0, Id, Generated).
bryant_or_1(G2, G1, G, Id0, Id, Generated, Done) :-
	G1 = ite(N1,_,_,_),
	G2 = ite(N2,_,_,_),
	hash_key(N1, N2, Key),
	arg(Key, Done, DoneBucket),
	member(result(N1,N2,G0), DoneBucket),	% lookup or add node
	!,					% consider no alternative nodes
	(   nonvar(G0) ->			% memoization hit:  all done
		G = G0,
		Id = Id0
	;   G = G0,
	    or_nodes(G1, G2, G, Id0, Id, Generated, Done)
						% memo miss:  do the work
	).


%  or_nodes(+G1, +G2, -G, +Id0, -Id, +-Generated, +-Done)
%  Arguments are as for bryant_or/7.  We actually disjoin two nodes.  If the
%  two nodes represent the same variable, we recursively disjoin their
%  respective 'then' and 'else' nodes and construct a node from them.
%  Otherwise we recursively disjoin the 'then' and 'else' nodes of the node
%  with the earlier variable with the other node, and construct a node from
%  them.  This is Bryant's algorithm.

or_nodes(G1, G2, G, Id0, Id, Generated, Done) :-
	G1 = ite(_,V1,T1,F1),
	G2 = ite(_,V2,T2,F2),
	(   V1 < V2 ->
		V = V1,
		bryant_or(T1, G2, Gt, Id0, Id1, Generated, Done),
		bryant_or(F1, G2, Gf, Id1, Id2, Generated, Done)
	;   V1 > V2 ->
		V = V2,
		bryant_or(G1, T2, Gt, Id0, Id1, Generated, Done),
		bryant_or(G1, F2, Gf, Id1, Id2, Generated, Done)
	;					% else V1 =:= V2
	    V = V1,
	    bryant_or(T1, T2, Gt, Id0, Id1, Generated, Done),
	    bryant_or(F1, F2, Gf, Id1, Id2, Generated, Done)
	),
	compose_node(V, Gt, Gf, G, Id2, Id, Generated).



/*************************************************************************

				 Conjunction

This code computes the reduced conjunction of two Bryant graphs, possibly with
some variables restricted away, in a single step.

*************************************************************************/


%  bryant_and(+Graph1, +Graph2, -Graph)
%  bryant_and(+Graph1, +Graph2, +Restriction, -Graph)
%  bryant_and(+Graph1, +Graph2, +Mapping, +Restriction, -Graph)
%  bryant_and(+Graph1, +Graph2, +Mapping, +Restriction, -Graph, +Id0, -Id,
%	      +-Generated, +-Done)
%  Graph is the conjunction of Graph1 and Graph2.  Restriction is the highest
%  variable number to include in the resulting graph; higher variables are
%  extentially quantified away.  This is done by replacing any subgraph that
%  is not false with true.  Mapping is a term whose arguments specify the
%  "real" node numbers to use for nodes in Graph1.  Where Graph1 has a node
%  with variable number N, instead consider the variable to be the Nth
%  argument of Mapping.  The arguments of Mapping must appear in strictly
%  increasing order.  Mapping may also be the atom 'identity', signifying that
%  no argument mapping is to be performed.  Other arguments are as for
%  bryant_or/[3,7].

bryant_and(G1, G2, G) :-
	functor(Generated, generated, 127),
	functor(Done, done, 127),
	bryant_and(G1, G2, identity, 999999, G, 2, _, Generated, Done).


bryant_and(G1, G2, Restriction, G) :-
	functor(Generated, generated, 127),
	functor(Done, done, 127),
	bryant_and(G1, G2, identity, Restriction, G, 2, _, Generated, Done).


bryant_and(G1, G2, Mapping, Restriction, G) :-
	functor(Generated, generated, 127),
	functor(Done, done, 127),
	bryant_and(G1, G2, Mapping, Restriction, G, 2, _, Generated, Done).


bryant_and(true, G2, _, Restriction, G, Id0, Id, Generated, _) :-
	!,
	restrict_graph(G2, identity, Restriction, G, Id0, Id, Generated).
bryant_and(false, _, _, _, G, Id, Id, _, _) :-
	!,
	G = false.
bryant_and(G1, G2, Mapping, Restriction, G, Id0, Id, Generated, Done) :-
	bryant_and_1(G2, G1, Mapping, Restriction, G, Id0, Id, Generated, Done).


%  bryant_and_1(+Graph2, +Graph1, +Mapping, +Restriction, -Graph, +Id0, -Id,
%		+-Generated, +-Done)
%  Same as bryant_and/9, except that first two args are swapped for indexing.

bryant_and_1(true, G1, Mapping, Restriction, G, Id0, Id, Generated, _) :-
	!,
	restrict_graph(G1, Mapping, Restriction, G, Id0, Id, Generated).
bryant_and_1(false, _, _, _, G, Id, Id, _, _) :-
	!,
	G = false.
bryant_and_1(G2, G1, Mapping, Restriction, G, Id0, Id, Generated, Done) :-
	G1 = ite(N1,_,_,_),
	G2 = ite(N2,_,_,_),
	hash_key(N1, N2, Key),
	arg(Key, Done, DoneBucket),
	member(result(N1,N2,G0), DoneBucket),	% lookup or add node
	!,					% consider no alternative nodes
	(   nonvar(G0) ->			% memoization hit:  we've
						% already anded these nodes:
						% just reuse previous result.
		G = G0,
		Id = Id0
	;   G = G0,
	    and_nodes(G1, G2, Mapping, Restriction, G, Id0, Id, Generated, Done)
						% memo miss:  do the work
	).


%  and_nodes(+G1, +G2, +Mapping, +Restriction, -G, +Id0, -Id, +-Generated,
%	     +-Done)
%  Arguments are as for bryant_and/9.  We actually conjoin two nodes.  If the
%  two nodes represent the same variable, we recursively conjoin their
%  respective 'then' and 'else' nodes and construct a node from them.
%  Otherwise we recursively conjoin the 'then' and 'else' nodes of the node
%  with the earlier variable with the other node, and construct a node from
%  them.  This is Bryant's algorithm.

and_nodes(G1, G2, Mapping, Restriction, G, Id0, Id, Gen, Done) :-
	G1 = ite(_,V1a,T1,F1),
	G2 = ite(_,V2,T2,F2),
	(   Mapping == identity ->
		V1 = V1a
	;   arg(V1a, Mapping, V1)
	),
	(   V1 > Restriction, V2 >  Restriction ->
		Id = Id0,
		restricted_and(G1, G2, Mapping, G, Done)
						% don't build any nodes
	;   (   V1 < V2 ->
		    V = V1,
		    bryant_and(T1, G2, Mapping, Restriction, Gt, Id0, Id1,
			       Gen, Done),
		    bryant_and(F1, G2, Mapping, Restriction, Gf, Id1, Id2,
			       Gen, Done)
	    ;   V1 > V2 ->
		    V = V2,
		    bryant_and(G1, T2, Mapping, Restriction, Gt, Id0, Id1,
			       Gen, Done),
		    bryant_and(G1, F2, Mapping, Restriction, Gf, Id1, Id2,
			       Gen, Done)
	    ;					% else V1 =:= V2
		V = V1,
		bryant_and(T1, T2, Mapping, Restriction, Gt, Id0, Id1, Gen,
			   Done),
		bryant_and(F1, F2, Mapping, Restriction, Gf, Id1, Id2, Gen,
			   Done)
	    ),
	    compose_node(V, Gt, Gf, G, Id2, Id, Gen)
	).



%  restricted_and(+G1, +G2, +Mapping, -G, +-Done)
%  restricted_and_1(+G2, +G1, +Mapping, -G, +-Done)
%  G is true if G1 & G2 is satisifiable, else false.  There are two predicates
%  in order to get indexing on both G1 and G2.  Done and Mapping are as above.

restricted_and(true, G2, _, G, _) :-
	!,
	(   G2 == false ->
		G = false
	;   G = true
	).
restricted_and(false, _, _, G, _) :-
	!,
	G = false.
restricted_and(G1, G2, Mapping, G, Done) :-
	restricted_and_1(G2, G1, Mapping, G, Done).


restricted_and_1(true, _, _, G, _) :-
	!,
	G = true.
restricted_and_1(false, _, _, G, _) :-
	!,
	G = false.
restricted_and_1(G2, G1, Mapping, G, Done) :-
	G1 = ite(N1,_,_,_),
	G2 = ite(N2,_,_,_),
	hash_key(N1, N2, Key),
	arg(Key, Done, DoneBucket),
	member(result(N1,N2,G0), DoneBucket),	% lookup or add node
	!,					% consider no alternative nodes
	(   nonvar(G0) ->			% memoization hit:  we've
		G = G0				% already anded these nodes:
						% just reuse previous result.
	;   G = G0,
	    restricted_and_nodes(G1, G2, Mapping, G, Done)
						% memo miss:  do the work
	).



%  restricted_and_nodes(+G1, +G2, +Mapping, -G, +-Done)
%  Just like and_nodes/8, except that G is always either 'true' or 'false'.
%  Therefore, no *Id* argument pair is needed, nor the *Generated* table.
%  Also, since we know we're restricting away all arguments in G1 and G2, we
%  don't need the *Restriction* limit variable.

restricted_and_nodes(G1, G2, Mapping, G, Done) :-
	G1 = ite(_,V1a,T1,F1),
	G2 = ite(_,V2,T2,F2),
	(   Mapping == identity ->
		V1 = V1a
	;   arg(V1a, Mapping, V1)
	),
	(   V1 < V2 ->
		V = V1,
		restricted_and_pair(T1, G2, F1, G2, Mapping, G, Done)
	;   V1 > V2 ->
		V = V2,
		restricted_and_pair(G1, T2, G1, F2, Mapping, G, Done)
	;					% else V1 =:= V2
	    V = V1,
	    restricted_and_pair(T1, T2, F1, F2, Mapping, G, Done)
	).


%  restricted_and_pair(+G1a, +G2a, +G1b, +G2b, +Mapping, -G, +-Done)
%  G is true iff there exists some truth value assignment for the variables in
%  G1a, G2a, G1b, and G2b such that (G1a & G2a) or (G1b & G2b) is true.  Other
%  args are as above.

restricted_and_pair(G1a, G2a, G1b, G2b, Mapping, G, Done) :-
	restricted_and(G1a, G2a, Mapping, G0, Done),
	(   G0 == true ->	
		G = true			% don't need to evaluate G1b &
						% G2b
	;   restricted_and(G1b, G2b, Mapping, G, Done)
	).



/*************************************************************************

				Graph Building

This code is responsible for building reduced Bryant graphs.

*************************************************************************/

%  compose_node(+V, +Gt, +Gf, -G, +Id0, -Id, +-Generated)
%  G is the node with variable V, then node Gt, and else node Gf.  Id0 is the
%  first node number to use in G, and Id is the next node number after
%  constructing node G.  If Gt and Gf are the same, then we just return it as
%  the node, since the value of V at this node is immaterial.  Generated is
%  our memoization table of nodes already generated.

compose_node(V, Gt, Gf, G, Id0, Id, Generated) :-
	(   same_node(Gt, Gf) ->		% graph doesn't depend on
						% value of V:  elide node
		G = Gt,
		Id = Id0
	;   new_node(V, Gt, Gf, G, Id0, Id, Generated)
	).


%  new_node(+V, +Gt, +Gf, -G, +Id0, -Id, +-Generated)
%  Arguments are the same as for compose_node/7.  Here we check our
%  memoization table Generated to see if we've already generated a node for
%  variable V with Gt and Gf as its then and else subnodes.  If so, then we
%  just return that node, else we must make a new one.


new_node(V, Gt, Gf, G, Id0, Id, Generated) :-
	node_id(Gt, Tid),
	node_id(Gf, Fid),
	hash_node(V, Tid, Fid, Hash),
	arg(Hash, Generated, Bucket),
	member(G, Bucket),
	G = ite(N,V,Gt,Gf),			% after member call to avoid
						% creating term unless needed
	!,					% consider no alternative nodes
	(   nonvar(N) ->
		Id = Id0			% reuse existing node
	;   N = Id0,
	    Id is Id0+1
	).



%  copy_graph(+G0, -G, +Id0, -Id, +-Generated)
%  G is a copy of graph G0, with nodes numbered from Id0 through Id - 1,
%  reusing nodes memoized in Generated, and memoizing all new nodes.

copy_graph(true, true, Id, Id, _).
copy_graph(false, false, Id, Id, _).
copy_graph(ite(_,V,T,F), G, Id0, Id, Generated) :-
	copy_graph(T, Gt, Id0, Id1, Generated),
	copy_graph(F, Gf, Id1, Id2, Generated),
	% since input graph is minimized, T and F can't be the same, so we
	% call new_node/7 instead of compose_node/7.
	new_node(V, Gt, Gf, G, Id2, Id, Generated).




%  restrict_graph(+G0, +Restriction, -G)
%  restrict_graph(+G0, +Mapping, +Restriction, -G)
%  restrict_graph(+G0, +Mapping, +Restriction, -G, +Id0, -Id, +-Generated)
%  G is a copy of graph G0 restricted to variables =< Restriction, with nodes
%  numbered from Id0 through Id - 1, and reusing nodes memoized in Generated.
%  Mapping is as above.

restrict_graph(G0, Restriction, G) :-
	functor(Generated, generated, 127),
	restrict_graph(G0, identity, Restriction, G, 2, _, Generated).


restrict_graph(G0, Mapping, Restriction, G) :-
	functor(Generated, generated, 127),
	restrict_graph(G0, Mapping, Restriction, G, 2, _, Generated).


restrict_graph(true, _, _, true, Id, Id, _).
restrict_graph(false, _, _, false, Id, Id, _).
restrict_graph(ite(_,V0,T,F), Mapping, Restriction, G, Id0, Id, Generated) :-
	(   Mapping == identity ->
		V = V0
	;   arg(V0, Mapping, V)
	),
	(   V > Restriction ->
		% input graph is reduced, so if it's not false, it must have
		% at least one true leaf.
		G = true,
		Id = Id0
	;   restrict_graph(T, Mapping, Restriction, Gt, Id0, Id1, Generated),
	    restrict_graph(F, Mapping, Restriction, Gf, Id1, Id2, Generated),
	    compose_node(V, Gt, Gf, G, Id2, Id, Generated)
	).


%  iff_conj(+V1, +Vs, -G)
%  iff_conj(+V1, +Vs, -G, +Id)
%  G is the reduced ordered binary decision graph representing V1 if and only
%  if the conjunction of the variables on the list Vs.  V1 must not be on the
%  list Vs.  Id is the lowest node number used in G.  This code is very
%  specialized, and is optimized to generate this type of graph which we will
%  need often.

iff_conj(V1, Vs, G) :-
	iff_conj(V1, Vs, G, 2).

iff_conj(V1, Vs, G, Id0) :-
	sort(Vs, Sorted),			% this removes duplicates
	iff_conj_1(Sorted, V1, G, Id0, []).

%  iff_conj_1(+Vs, +V1, -G, +Id0, +Node)
%  Like iff_conj/4, but Vs is known to be sorted.  Node is either [] or a Node
%  is a node whose meaning is ~V1 (i.e., ite(_,V1,false,true)).  This is the
%  only node which can be shared; by explicitly handling this, we avoid a lot
%  of the overhead of the usual new_node/7 code.

iff_conj_1([], V1, ite(Id,V1,true,false), Id, _).
iff_conj_1([V|Vs], V1, G, Id0, Node) :-
	(   V < V1 ->
		(   Node == [] ->
			Node1 = ite(Id0,V1,false,true),
			Id1 is Id0 + 1
		;   Node1 = Node,
		    Id1 = Id0
		),
		G = ite(Id1,V,G1,Node1),
		Id2 is Id1 + 1,
		iff_conj_1(Vs, V1, G1, Id2, Node1)
	;   G = ite(Id0,V1,Trues,Falses),	% V > V1
	    Id1 is Id0 + 1,
	    build_and_chain([V|Vs], true, false, Id1, Id2, Trues),
	    build_and_chain([V|Vs], false, true, Id2, _, Falses)
	).


%  build_and_chain(+Vs, +Pos, +Neg, +Id0, -Id, -Conj)
%  Conj is a reduced bryant graph representing the conjunction, or the
%  negation of the conjunction, of the variables on Vs.  Pos and Neg determine
%  whether it is the conjunction or its negation:  Pos is the boolean value for
%  when the conjunction is true, and Neg is the value when it is false.  They
%  each must be 'true' or 'false', and must not be the same.  Id0 is the
%  lowest node number used in G, and Id is 1 + the greatest.

build_and_chain([], Pos, _, Id, Id, Pos).
build_and_chain([V|Vs], Pos, Neg, Id0, Id, ite(Id0,V,Conj,Neg)) :-
	Id1 is Id0 + 1,
	build_and_chain(Vs, Pos, Neg, Id1, Id, Conj).



/*************************************************************************

			       Comparing Graphs

*************************************************************************/



%  identical_graphs(+G1, +G2)
%  identical_graphs(+G1, +G2, +-Compared)
%  Graphs G1 and G2 represent the same boolean function.  Compared is a
%  memoization table of nodes we've already compared and know to be identical.
%  Since the input graphs are known to be acyclical, we don't need to worry
%  about loops.

identical_graphs(G1, G2) :-
	functor(Compared, compared, 127),
	identical_graphs(G1, G2, Compared).

identical_graphs(true, true, _).
identical_graphs(false, false, _).
identical_graphs(ite(N1,V,T1,F1), ite(N2,V,T2,F2), Compared) :-
	hash_key(N1, N2, Hash),
	arg(Hash, Compared, Bucket),
	member(N1=X, Bucket),			% delay binding X to see if
	!,					% N1=N2 was already in bucket
	(   nonvar(X) ->			% it was:
		X=N2				% X must be N2
	;   X=N2,				% it wasn't:  add it, and
	    identical_graphs(T1, T2, Compared),	% make sure T1=T2 and
	    identical_graphs(F1, F2, Compared)	% F1=F2.
	).



%  same_node(+N1, +N2)
%  Nodes N1 and N2 are the same node.  This only works if they're nodes of the
%  same graph, and assumes that identical graphs have the same root node ids.
%  This, of course, is the invariant we are working to enforce.

same_node(true, true).
same_node(false, false).
same_node(ite(N,_,_,_), ite(N,_,_,_)).



/*************************************************************************

			  Graph Handling Primitives

*************************************************************************/

%  node_id(+Node, -Id)
%  Id is the integer id of Node.  true and false are automatically numbered 1
%  and 0, respectively.

node_id(true, 1).
node_id(false, 0).
node_id(ite(Id,_,_,_), Id).


%  hash_key(+N1, +N2, -Hash)
%  Hash is a numeric hash key constructed from node ids N1 and N2.

hash_key(N1, N2, Hash) :-
	Hash is 1+(((N1+17)*(N2+19)) mod 127).


%  hash_node(+V, +N1, +N2, -Hash)
%  Hash is a numeric hash key constructed from variable number V and node ids
%  N1 and N2.

hash_node(V, N1, N2, Hash) :-
	Hash is 1+(((V+7)*(N1+13)*(N2+17)) mod 127).


%  member(+X, +-Y)
%  X is a member of list Y.

member(X, [X|_]).
member(X, [_|Y]) :-
	member(X, Y).


/*************************************************************************

				  Test Cases

The test case from Bryant's paper:

++verbatim
?- bryant_or(ite(2,0,ite(3,2,false,true),true),
	     ite(2,1,ite(3,2,true,false),false), G).

G = ite(4,0,ite(3,1,true,ite(2,2,false,true)),true)
--verbatim

Other tests:

    (X & (Y<->Z)) | ((X&Y) <-> Z) ==> ((X&Y) <-> Z)

++verbatim
?- NOTZ = ite(5,2,false,true),
   bryant_or(ite(2,0,ite(3,1,ite(4,2,true,false),ite(5,2,false,true)),false),
	     ite(2,0,ite(3,1,ite(4,2,true,false),NOTZ),NOTZ),
	     G).

NOTZ = ite(5,2,false,true),
G = ite(5,0,ite(4,1,ite(2,2,true,false),ite(3,2,false,true)),ite(3,2,false,true))
--verbatim


    (X<->~Y) & Z ==> 
++verbatim
?- bryant_and(ite(2,0,ite(3,1,false,true),ite(4,1,true,false)),
	      ite(2,2,true,false), G).

G = ite(5,0,ite(3,1,false,ite(2,2,true,false)),ite(4,1,ite(2,2,true,false),false))
--verbatim

Big test:

++verbatim
?- iff_conj(0,[3,4],G1), iff_conj(2,[3,5],G2),
   bryant_and(G1,G2,G3),
   G4 = ite(6,1,ite(3,4,ite(2,5,true,false),false),ite(5,4,ite(4,5,false,true),false)),
   bryant_and(G3,G4,2,G).
--verbatim

*************************************************************************/
