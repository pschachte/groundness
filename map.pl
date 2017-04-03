%  File     : map.pl
%  RCS      : $Id: map.pl,v 1.1.2.27 2005/10/17 07:07:17 schachte Exp $
%  Author   : Peter Schachte
%  Origin   : Mon Aug 11 17:09:41 2003
%  Purpose  : Implement finite maps using balanced trees
%  Copyright: (c) 2004 Peter Schachte.  All rights reserved.


:- module(map, [ map_empty/1,		% the empty map
		 map_cardinality/2,	% the number of mappings in a map
		 map_member/3,		% backtrack over mappings
		 map_search/3,		% find a mapping with key == to the
					% the key being sought
		 map_fetch/3,		% find a mapping = to the given key
					% is (semi)det when key is given
		 map_store/4,		% insert into map, replacing mapping
		 map_remove/4,		% remove mapping
		 map_remove/3,		% remove mapping
		 map_delete/3,		% remove mapping if it's there
		 map_replace/4,		% replace an existing mapping, or fail
		 map_replace/5,		% replace an existing mapping, or fail
		 map_replace/6,		% replace an existing mapping or insert
		 map_remove_largest/4,	% remove and return largest key
		 map_remove_smallest/4,	% remove and return smallest key
		 map_iterator/2,	% term allowing iteration over map
		 map_next/4,		% next mapping in iteration over map
		 map_map/3,		% apply an operation to each mapping
		 map_union/4,		% combine two mappings
		 map_intersection/4,	% combine two mappings
		 list_map/2,		% interconvert list <-> map
		 sorted_list_to_map/2,	% convert sorted assoc list to map
		 print_map/1,		% print a map legibly

	% Implementation of sets as maps
		 set_empty/1,		% the empty set
		 set_cardinality/2,	% the number of elements in a set
		 set_search/2,		% membership test using ==
		 set_member/2,		% backtrack over set members
		 set_insert/3,		% add element to set
		 set_iterator/2,	% term allowing iteration over a set
		 set_next/3		% next element of set
	       ]).


:- module_transparent(map_map/3).
:- module_transparent(map_union/4).
:- module_transparent(map_intersection/4).

/****************************************************************
			       Data Structure

Maps are represented as:

	empty_map

or

	map(Balance, L, K, V, R)

where:

	Balance	is height(L) -  height(R)
	L	is the left subtree
	K	is a key
	V	is the value associated with K
	R	is the right subtree

We maintain the invariant -1 =< Balance =< 1.

****************************************************************/



/****************************************************************
				 Empty Maps
****************************************************************/

%  map_empty(?Empty)
%  Empty is an empty map.

map_empty(empty_map).


/****************************************************************
			      Map Cardinality
****************************************************************/

%  map_cardinality(+Map, -Cardinality)
%  Cardinality is the number of key->value pairs in Map.

map_cardinality(Map, Cardinality) :-
        map_cardinality(Map, 0, Cardinality).

map_cardinality(empty_map, Cardinality, Cardinality).
map_cardinality(map(_, L, _, _, R), Cardinality0, Cardinality) :-
        Cardinality1 is Cardinality0 + 1,
        map_cardinality(L, Cardinality1, Cardinality2),
        map_cardinality(R, Cardinality2, Cardinality).


/****************************************************************
			     Finding Map Members

Searching a balanced tree is the same as searching any binary search tree, so
this code is quite simple.

****************************************************************/

%  map_member(+Map, ?Key, ?Value)
%  Map associates Value with Key.  Backtracks over mappings in order.

map_member(map(_,L,_,_,_), Key, Value) :- map_member(L, Key, Value).
map_member(map(_,_,Key,Value,_), Key, Value).
map_member(map(_,_,_,_,R), Key, Value) :- map_member(R, Key, Value).

%  map_search(+Map, +Key, -Value)
%  Map associates Value with Key.  Key must be == to a key in Map.

map_search(map(_,L,K,V,R), Key, Value) :-
	compare(C, Key, K),
	map_search1(C, L, V, R, Key, Value).

map_search1((<), L, _, _, Key, Value) :-
	map_search(L, Key, Value).
map_search1((=), _, Value, _, _, Value).
map_search1((>), _, _, R, Key, Value) :-
	map_search(R, Key, Value).

%  map_fetch(?Key, +Map, ?Value)
%  map_fetch(+Key, +Map, -Value)
%  Map associates Value with Key.  Can backtrack over mappings, but is
%  (semi)deterministic if Key is bound.

map_fetch(K, T, V) :-
	(   nonvar(K) ->
		map_search(T, K, V)
	;   map_member(T, K, V)
	).


/****************************************************************
			     Finding Map Members

Our one tool to maintain balance is rotation.  To rotate a tree, we shift one
subtree up, shifting one of its subtrees to become a child of the other
subtree.  Pictorially, we replace a tree like this:

		   N
		  / \
		 /   \
		M     Z
	       / \   / \
	      /   \  ---
	     X     Y
	    / \	  / \
	    ---   ---

with one like this:

	   M'
	  / \
	 /   \
	X     N'
       / \   / \
       ---  Y   Z
	   / \ / \
	   --- ---

This is a right rotation; left rotation is the mirror (reverse) transformation.

We denote by N the height of the tree rooted at N, and by bal(N) the
difference between the heights of N's left and right subtrees.  In the first
diagram above, N = 1 + max(M, Z) and bal(N) = M - Z.

For rotation to work as desired, we must have the following preconditions:

    1 =< bal(N) =< 2 and 0 =< bal(M) =< 2 and bal(M) =< bal(N)

Given this, and that X, Y, and Z are balanced, we can guarantee that M' and
N' will be balanced after the rotation.

Because of the lower bound on bal(M), we know that max(X,Y) = X.  So we know:

	bal(M) = X - Y
	bal(N) = M - Z
	       = 1 + max(X,Y) - Z
	       = X - Z + 1

now we can calculate bal(N') and bal(M'):

	bal(N') = Y - Z
		= (X - Z) - (X - Y)
		= (X - Z + 1) - (X - Y) - 1
		= bal(N) - bal(M) - 1

	bal(M') = X - N'
		= X - max(Y,Z) - 1
		= min(X-Y, X-Z) - 1
		= min(bal(M), bal(N)-1) - 1

To see that these are in the required range, consider the specified
preconditions.  Since 0 <= bal(N) - bal(M) =< 2, bal(N') must be in range.
Also, the minimum and maximum values of min(bal(M), bal(N)-1) will be 0 and
2, respectively, so the bounds for bal(M') are satisfied, and the resulting
tree is balanced.  Note that the allowable range for bal(M) is 0 to 2; ie,
the left child of a node to be rotated right need not be balanced!

Also consider what happens when bal(M) = -1, out of its required range, while
bal(N) is still in range.  Then max(X,Y) = Y, so

	bal(N) = M - Z
	       = 1 + max(X,Y) - Z
	       = Y - Z + 1

	bal(N') = Y - Z
		= bal(N) - 1 (between 0 and 1)

	bal(M') = X - N'
		= X - max(Y,Z) - 1
		= min(X-Y, X-Z) - 1
		= min(bal(M), bal(N)-1) - 1
		= -2

The analysis for left rotation is similar.  We replace a tree like this

	   M
	  / \
	 /   \
	X     N
       / \   / \
       ---  Y   Z
	   / \ / \
	   --- ---

with one like this:

		   N'
		  / \
		 /   \
		M'    Z
	       / \   / \
	      /   \  ---
	     X     Y
	    / \	  / \
	    ---   ---

For this case we require the preconditions

    -2 =< bal(M) =< -1 and -2 =< bal(N) =< 0 and -2 <= bal(M) - bal(N) =< 0

In this case we wind up with:

	bal(M') = bal(M) - bal(N) + 1
	bal(N') = max(bal(M)+1, bal(N)) + 1

Again, for the case bal(M) = 1, bal(N') = 2, while bal(M') is in range.  if
bal(M) = 1, then after a left rotation, bal(N') is *in* the range for the
left child of a node to be *right* rotated.  And similarly for taking the
result of a right rotation as the right child of a node to be left rotated.

This means that if we are to rotate a node right and the balance of its left
child is negative, we can rotate the child right without worrying about the
balances of *its* children.  And similarly for right rotating the right child
of a node to be left rotated.

Note that when we contemplate rotating a tree, we have just inserted
into a subtree.  To avoid building this subtree, only to have to
rebuild it later, we introduce the concept of a bias.  When inserting into a
left subtree whose growth would require us to rotate right, we add a bias of
1 to the lower allowable balance for that subtree, ensuring that its balance
winds up between 0 and 2, rather than -1 and 1.  This means we never need
to rotate a child before we can rotate the parent.

It is important to tell whether or not an insertion caused growth in the
height of the tree.  If it did not, there is no need to rotate the tree.
Insertion into an empty tree always causes growth.  A single insertion into a
non-empty tree tree causes growth iff the insertion into the left or right
subtree caused growth and the balance of the new tree is not zero.

****************************************************************/

%  map_store(+Key, +Value, +Map0, -Map)
%  Map associates Value with Key; otherwise, it is identical to Map0.
%  Map0 may or may not associate a value with Key.

map_store(Key, Value, T0, map(B, L, K, V, R)) :-
	map_store1(T0, Key, Value, 0, _, B, L, K, V, R).


%  map_store1(T0, Key, Value, Bias, Growth, B, L, K, V, R)
%  The node map(B,L,K,V,R) is the result of inserting Key and Value
%  into tree T0.  The change in height of this node relative to T0 is Growth.
%  We ensure that Bias-1 =< B =< Bias+1.

map_store1(empty_map, Key, Value, _, 1, 0, empty_map, Key, Value, empty_map).
map_store1(map(B0,L0,K0,V0,R0), Key, Value, Bias, G, B, L, K, V, R) :-
	compare(C, Key, K0),
	map_store2(C, Key, Value, Bias, B0, L0, K0, V0, R0, G, B, L, K, V, R).

%  map_store2(C, Key, Value, Bias, B0, L0, K0, V0, R0, G, B, L, K, V, R)
%  The node map(B,L,K,V,R) is the result of inserting Key and Value
%  into tree map(B0,L0,K0,V0,R0), where C is the comparison of Key to K0.
%  The change in height due to this insertion is G.

map_store2((<), Key, Value, Bias, B0, L0, K0, V0, R0, G, B, L, K, V, R) :-
	%% Bias1 is 1 if growth on left would make us rotate, otherwise 0
	Bias1 is max(-Bias, B0),
	map_store1(L0, Key, Value, Bias1, G1, B1, L1, K1, V1, R1),
	(   G1 =\= 0, B0 > min(0,Bias) ->
	    %% Balance is out of range:  must rotate right
	    %% -1 =< Bias =< 1, so B0 >= 0
	    G is 1 - B0,
	    K = K1, V = V1, L = L1,
	    B is min(B1, B0) - 1,
	    B3 is B0 - B1,
	    node(B3, R1, K0, V0, R0, R)
%	    R = map(B3, R1, K0, V0, R0)
	;   %% Balance is still in range:  no rotation
	    B is B0 + G1,
	    growth(B, G1, G),
	    K = K0, V = V0, R = R0,
	    node(B1, L1, K1, V1, R1, L)
%	    L = map(B1, L1, K1, V1, R1)
	).
map_store2((=), K, V, _, B, L, _, _, R, 0, B, L, K, V, R).
map_store2((>), Key, Value, Bias, B0, L0, K0, V0, R0, G, B, L, K, V, R) :-
	%% Bias1 is -1 if growth on right would make us rotate, otherwise 0
	Bias1 is min(-Bias, B0),
	map_store1(R0, Key, Value, Bias1, G1, B1, L1, K1, V1, R1),
	(   G1 =\= 0, B0 < max(0,Bias) ->
	    %% Balance is out of range:  must rotate left
	    %% -1 =< Bias =< 1, so B0 =< 0
	    G is B0 + 1,
	    B is max(B1, B0) + 1,
	    K = K1, V = V1, R = R1,
	    B3 is B0 - B1,
	    node(B3, L0, K0, V0, L1, L)
%	    L = map(B3, L0, K0, V0, L1)
	;   %% Balance is still in range:  no rotation
	    B is B0 - G1,
	    growth(B, G1, G),
	    K = K0, L = L0, V = V0,
	    node(B1, L1, K1, V1, R1, R)
%	    R = map(B1, L1, K1, V1, R1)
	).


growth(0, _, 0) :- !.
growth(_, Grow, Grow).


node(B, L, K, V, R, Tree) :-
	Tree = map(B,L,K,V,R).
	%% XXX sanity checking code:  get rid of it when debugged
%	check_balanced(Tree).

check_balanced(Tree) :-
	(   balanced_tree(Tree)
	->  true
	;   throw(not_bbtree(Tree))
	).
	

balanced_tree(Tree) :- balanced_tree(Tree, _).

balanced_tree(0) :- !, fail.	% catch unbound variables
balanced_tree(empty_map, 0).
balanced_tree(map(B,L,K,_,R), Depth) :-
	abs(B) =< 1,
	(   root_key(L, LK)
	->  LK @< K
	;   true
	),
	(   root_key(R, RK)
	->  K @< RK
	;   true
	),
	balanced_tree(L, Ld),
	balanced_tree(R, Rd),
	B =:= Ld - Rd,
	Depth is 1 + max(Ld, Rd).


depth(empty_map, 0).
depth(map(_,L,_,_,R), D) :-
	depth(L, LD),
	depth(R, RD),
	D is 1 + max(LD, RD).

root_key(map(_,_,K,_,_), K).



/****************************************************************
			  Removing Values From Maps

As for insertion, removal must maintain balance by sometimes rotating trees.
In this case, when a subtree shrinks to make the parent tree unbalanced, we
rotate *into* that subtree.  One key difference here from insertion is that
insertion always inserts a new leaf node, whereas removal may remove any
node.  When removing the root of a (sub)tree, we must promote either the
leftmost leaf of the right subtree or the rightmost leaf of the left to be
the new root.  We choose to take a leaf from the deeper child, as that one is
guaranteed to have at least one key.  If the tree is balanced, we check for
the possibility that it is a leaf node itself, in which case removal leaves
the empty tree.  If not we take from the right child.

Once again we avoid building nodes that will need to be rotated, this time by
having removal return a "shrinkage" value (0 or 1) indicating how much height
this tree lost due to node removal.  Unfortunately removal does not work as
neatly as insertion.  The subtree that was recursively processed is the
subtree to be rotated *into*, rather than the one to be rotated *out of*, as
is the case for insertion.  Therefore, having a "bias" is not helpful.  Once
we see the shrinkage resulting from an insertion, and how that affects the
balance of the tree, we decide whether or not to rotate.

The predicate rotate_if_needed/12 decides, based on the new balance of the
tree after the shrinkage of one subtree is considered, whether or not to
rotate.  However, as in insertion, rotation after node removal requires
care.  Suppose in the following example, the heights of Y and Z are the same,
and X is one smaller.

		   N
		  / \
		 /   \
		M     Z
	       / \   / \
	      /   \  ---
	     X     Y
	    / \	  / \
	    ---   ---

Then bal(N) = 1.  Suppose removing a key from Z decreases its height by 1;
now bal(N) = 2, so we must rotate, leaving this:

	   M'
	  / \
	 /   \
	X     N'
       / \   / \
       ---  Y   Z
	   / \ / \
	   --- ---

But now, bal(M') = X - (1 + max(Y,Z)) = X - (1 + Y) = X - (2 + X) = -2.
To avoid this problem, we ensure that 0 <= bal(M) <= 2, before rotating.
That is before a right rotation, we ensure the left subtree is not
right-heavy, and conversely before a left rotation, we ensure the right
subtree is not left-heavy.  This fills the purpose of the bias for
insertion.

****************************************************************/

%  map_remove(+Key, +Map0, -Map)
%  map_remove(+Key, -Value, +Map0, -Map)
%  Map associates Value with Key; otherwise, it is identical to Map0.
%  Map0 associates Value with Key; otherwise, it is identical to Map.

map_remove(Key, Value, map(B0, L0, K0, V0, R0), Map) :-
	compare(C, Key, K0),
	map_remove1(C, Key, Value, B0, L0, K0, V0, R0, Map, _).

map_remove(Key, map(B0, L0, K0, V0, R0), Map) :-
	compare(C, Key, K0),
	map_remove1(C, Key, _, B0, L0, K0, V0, R0, Map, _).

%  map_delete(+Key, +Map0, -Map)

map_delete(Key, map(B0, L0, K0, V0, R0), Map) :-
	compare(C, Key, K0),
	map_remove1(C, Key, _, B0, L0, K0, V0, R0, Map, _),
	!.
map_delete(_, Map, Map).


%  map_remove1(C, Key, Value, B0, L0, K0, V0, R0, Map, Shrinkage)
%  The node map(B,L,K,V,R) is the result of removing Key and Value
%  from tree map(B0,L0,K0,V0,R0), where C is the comparison of Key to K0.
%  The reduction in height due to this removal is Shrinkage.

map_remove1((<), Key, Value, B0, map(B1, L1, K1, V1, R1), K0, V0, R0, M, S) :-
%  check_balanced(map(B0,map(B1, L1, K1, V1, R1),K0,V0,R0)),
	compare(C, Key, K1),
	map_remove1(C, Key, Value, B1, L1, K1, V1, R1, M1, S1),
	B2 is B0 - S1,
	rotate_if_needed(B2, M1, K0, V0, R0, B, L, K, V, R, S1, S),
	node(B, L, K, V, R, M).
%	M = map(B, L, K, V, R).
map_remove1((=), _, Value, B0, L0, _K0, Value, R0, M, S) :-
%  check_balanced(map(B0,L0,K0,Value,R0)),
	map_remove_root(B0, L0, R0, M, S).
map_remove1((>), Key, Value, B0, L0, K0, V0, map(B1, L1, K1, V1, R1), M, S) :-
%  check_balanced(map(B0,L0,K0,V0,map(B1, L1, K1, V1, R1))),
	compare(C, Key, K1),
	map_remove1(C, Key, Value, B1, L1, K1, V1, R1, M1, S1),
	B2 is B0 + S1,
	rotate_if_needed(B2, L0, K0, V0, M1, B, L, K, V, R, S1, S),
	node(B, L, K, V, R, M).


%  rotate_if_needed(B0, L0, K0, V0, R0, B, L, K, V, R, S0, S)
%  B, L, K, V, R comprise a node with the same content as B0, L0, K0, V0, R0,
%  except that the former is balanced, providing the latter is unbalanced by
%  at most 1 (ie, B0 ranges between -2 and 2).  S is the amount by which the
%  height of the node decreases, and S0 is the amount the child shrunk.

rotate_if_needed( 2, map(B1, L1, K1, V1, R1), K0, V0, R0,
		      B, L, K, V, R, _, S) :-
	    ensure_left(B1, L1, K1, V1, R1, B2, L, K, V, R2, S),
	    B3 is 1 - B2,
	    B is min(B2-1, 0),
	    node(B3, R2, K0, V0, R0, R).
%	    R = map(B?, L0, K0, V0, L2).
rotate_if_needed( 1, L, K, V, R,  1, L, K, V, R, _, 0).
rotate_if_needed( 0, L, K, V, R,  0, L, K, V, R, S, S).
rotate_if_needed(-1, L, K, V, R, -1, L, K, V, R, _, 0).
rotate_if_needed(-2, L0, K0, V0, map(B1, L1, K1, V1, R1),
		      B, L, K, V, R, _, S) :-
	    ensure_right(B1, L1, K1, V1, R1, B2, L2, K, V, R, S),
	    B3 is -1 - B2,
	    B is max(0, B2+1),
	    node(B3, L0, K0, V0, L2, L).
%	    L = map(B?, L0, K0, V0, L2).


ensure_right(-1, L, K, V, R, -1, L, K, V, R, 1).
ensure_right( 0, L, K, V, R,  0, L, K, V, R, 0).
ensure_right( 1, map(B1, L1, K1, V1, R1), K0, V0, R0, B, L1, K1, V1, R, 1) :-
	B is min(0, B1) - 1,
	B2 is -max(0, B1),
	node(B2, R1, K0, V0, R0, R).
%	R = map(B2, R1, K0, V0, R0),


ensure_left( 1, L, K, V, R,  1, L, K, V, R, 1).
ensure_left( 0, L, K, V, R,  0, L, K, V, R, 0).
ensure_left(-1, L0, K0, V0, map(B1, L1, K1, V1, R1), B, L, K1, V1, R1, 1) :-
	B is max(0,B1) + 1,
	B2 is -min(0,B1),
	node(B2, L0, K0, V0, L1, L).
%	L = map(B2, L0, K0, V0, L1).


map_remove_root( 1, map(B1, L1, K1, V1, R1), R0, M, S) :-
	!,
	remove_largest1(R1, L1, B1, K1, V1, K, V, L, S),
	B is 1 - S,
	node(B, L, K, V, R0, M).
%	M = map(B, L, K, V, R0).
map_remove_root( 0, L0, R0, M, S) :-
	remove_root_balanced(R0, L0, M, S).
map_remove_root(-1, L0, map(B1, L1, K1, V1, R1), M, S) :-
	remove_smallest1(L1, B1, K1, V1, R1, K, V, R, S),
	B is S - 1,
	node(B, L0, K, V, R, M).
%	M = map(B, L0, K, V, R).

remove_root_balanced(empty_map, M, M, 1).
remove_root_balanced(map(B1, L1, K1, V1, R1), L0, M, 0) :-
	remove_smallest1(L1, B1, K1, V1, R1, K, V, R, S),
	node(S, L0, K, V, R, M).
%	M = map(B, L0, K, V, R).

/****************************************************************
			  Removing Extreme Elements
****************************************************************/

map_remove_smallest(map(B1, L1, K1, V1, R1), K, V, Map) :-
	remove_smallest1(L1, B1, K1, V1, R1, K, V, Map, _).

remove_smallest1(empty_map, _, K, V, R, K, V, R, 1).
remove_smallest1(map(B0,L0,K0,V0,R0), B1, K1, V1, R1, K, V, M, S) :-
	remove_smallest1(L0, B0, K0, V0, R0, K, V, M1, S1),
	BS is B1 - S1,
	rotate_if_needed(BS, M1, K1, V1, R1, B2, L2, K2, V2, R2, S1, S),
	node(B2, L2, K2, V2, R2, M).


map_remove_largest(map(B1, L1, K1, V1, R1), K, V, Map) :-
	remove_largest1(R1, L1, B1, K1, V1, K, V, Map, _).

remove_largest1(empty_map, L, _, K, V, K, V, L, 1).
remove_largest1(map(B0,L0,K0,V0,R0), L1, B1, K1, V1, K, V, M, S) :-
	remove_largest1(R0, L0, B0, K0, V0, K, V, M1, S1),
	BS is B1 + S1,
	rotate_if_needed(BS, L1, K1, V1, M1, B2, L2, K2, V2, R2, S1, S),
	node(B2, L2, K2, V2, R2, M).


/****************************************************************
		       Interconverting Lists and Maps
****************************************************************/

list_map(List, Map) :-
	(   var(Map) ->
	    length(List, Len),
	    keysort(List, List1),
	    sorted_list_to_map(Len, List1, [], _, Map)
	;   map_to_list(Map, List, [])
	).

sorted_list_to_map(List, Map) :-
	length(List, Len),
	sorted_list_to_map(Len, List, [], _, Map).

sorted_list_to_map(0, List, List, 0, empty_map) :- !.
sorted_list_to_map(1, [K-V|List], List, 0, map(0,empty_map,K,V,empty_map)) :-
	!.
sorted_list_to_map(2, [K0-V0,K1-V1|List], List, 1,
		   map(1,map(0,empty_map,K0,V0,empty_map),
		       K1,V1,empty_map)) :- !.
sorted_list_to_map(Len, List, List0, B, Map) :-
%	    map(B,L,K,V,R)) :-
	Left is Len >> 1,
	sorted_list_to_map(Left, List, [K-V|Tail], LB, L),
	Right is Len-Left-1,
	sorted_list_to_map(Right, Tail, List0, RB, R),
	B is LB /\ (1-RB),	% Balance=1 iff LB is 1 and RB is 0
	node(B, L, K, V, R, Map).


map_to_list(empty_map, List, List).
map_to_list(map(_,L,K,V,R), List, List0) :-
	map_to_list(L, List, [K-V|List1]),
	map_to_list(R, List1, List0).



add_list_map([], Map, Map).
add_list_map([K-V|Es], M0, M) :-
	map_store(K, V, M0, M1),
	add_list_map(Es, M1, M).

/****************************************************************
			  Replacing Values In Maps
****************************************************************/

%  map_replace(+Key, +Value, +Map0, -Map)
%  map_replace(+Key, -Oldvalue, +Value, +Map0, -Map)
%  map_replace(+Key, +Defaultoldvalue, -Oldvalue, +Value, +Map0, -Map)
%  Map associates Value with Key, while Map0 associates Oldvalue with Key;
%  otherwise Map0 and Map are identical.  This is more efficient than
%  map_store, as it does not need to rebalance any trees.  If Key has no
%  mapping in Map0, then map_replace/4,5 fail, whereas map_replace/6
%  succeeds with Oldvalue bound to Defaultoldvalue and with Key associated
%  with Value in Map. 

map_replace(Key, Value, T0, T) :-
	map_replace1(T0, Key, _, Value, T).


map_replace(Key, Value0, Value, T0, T) :-
	map_replace1(T0, Key, Value0, Value, T).


map_replace(Key, Default, Value0, Value, T0, T) :-
	(   map_replace1(T0, Key, V, Value, T) ->
	    Value0 = V
	;   Value0 = Default,
	    map_store(Key, Value, T0, T)
	).


map_replace1(map(S0,L0,K0,V0,R0), Key, Value0, Value, map(S0,L,K0,V,R)) :-
	compare(C, Key, K0),
	map_replace2(C, Key, Value0, Value, L0, L, V0, V, R0, R).

map_replace2((<), Key, Value0, Value, L0, L, V, V, R, R) :-
	map_replace1(L0, Key, Value0, Value, L).
map_replace2((=), _, V0, V, L, L, V0, V, R, R).
map_replace2((>), Key, Value0, Value, L, L, V, V, R0, R) :-
	map_replace1(R0, Key, Value0, Value, R).


/****************************************************************
			       Traversing Maps
****************************************************************/

%  map_iterator(Map, Iter)
%  Iter is a term that can be used to traverse Map.

map_iterator(Map, iter1(Map,done)).


%  map_next(+Iter, -Key, -Value, -Iter1)
%  Key is the first key specified by Iter, Value is the associated value,
%  and Iter1 is a iter that will traverse the rest of Iter after Key.

map_next(iter1(Map,Iter0), K, V, Iter) :-
	map_next1(Map, Map, Iter0, K, V, Iter).
map_next(iter2(map(_,_,K,V,R),Iter0), K, V, iter1(R,Iter0)).


map_next1(empty_map, _, Iter0, K, V, Iter) :-
	map_next(Iter0, K, V, Iter).
map_next1(map(_,L,_,_,_), Map0, Iter0, K, V, Iter) :-
	map_next1(L, L, iter2(Map0,Iter0), K, V, Iter).



%  map_map(+Map0, +Closure, -Map)
%  Map is Map0 with Closure(Key, Val0, Val) called for every Key -> Val0
%  mapping in Map0 and Val0 then replaced with Val.
:- meta_predicate map:map_map(*,3,*).

map_map(empty_map, _, empty_map).
map_map(map(B,L0,K,V0,R0), Closure, map(B,L,K,V,R)) :-
	call(Closure, K, V0, V),
	map_map(L0, Closure, L),
	map_map(R0, Closure, R).


%  map_union(+Map0, +Map1, +Closure, -Map)
%  Map maps every element of Map0 not mapped by Map1 as Map0 maps it, and
%  similarly for elements mapped by Map1 but not Map0.  Elements mapped by
%  both are mapped to V such that Closure(K, V0, V1, V).
%
%  The best way I can see to do this is to turn the two trees into lists,
%  merge them, and turn the result into a map.
:- meta_predicate map:map_union(*,*,4,*).

map_union(Map0, Map1, Closure, Map) :-
	map_to_list(Map0, List0, []),
	map_to_list(Map1, List1, []),
	union_alists(List0, List1, Closure, List),
	length(List, Len),
	sorted_list_to_map(Len, List, [], _, Map).


:- meta_predicate map:union_alists(*,*,4,*).
union_alists([], List, _, List).
union_alists([K-V|List0], List1, Closure, List) :-
	union_alists1(List1, K, V, List0, Closure, List).

:- meta_predicate map:union_alists1(*,?,?,*,4,*).
union_alists1([], K0, V0, List0, _, [K0-V0|List0]).
union_alists1([K1-V1|List1], K0, V0, List0, Closure, List) :-
	compare(C, K0, K1),
	union_alists2(C, K0, K1, V0, V1, List0, List1, Closure, List).

:- meta_predicate map:union_alists2(*,?,*,?,?,*,*,4,*).
union_alists2((<), K0, K1, V0, V1, List0, List1, Closure, [K0-V0|List]) :-
	union_alists1(List0, K1, V1, List1, Closure, List).
union_alists2((=), K, _, V0, V1, List0, List1, Closure, [K-V|List]) :-
	call(Closure, K, V0, V1, V),
	union_alists(List0, List1, Closure, List).
union_alists2((>), K0, K1, V0, V1, List0, List1, Closure, [K1-V1|List]) :-
	union_alists1(List1, K0, V0, List0, Closure, List).


%  map_intersection(+Map0, +Map1, +Closure, -Map)
%  Map maps every element of Map0 not mapped by Map1 as Map0 maps it, and
%  similarly for elements mapped by Map1 but not Map0.  Elements mapped by
%  both are mapped to V such that Closure(K, V0, V1, V).
%
%  The best way I can see to do this is to turn the two trees into lists,
%  merge them, and turn the result into a map.

:- meta_predicate map:map_intersection(*,*,4,*).
map_intersection(Map0, Map1, Closure, Map) :-
	map_to_list(Map0, List0, []),
	map_to_list(Map1, List1, []),
	intersect_alists(List0, List1, Closure, List),
	length(List, Len),
	sorted_list_to_map(Len, List, [], _, Map).

:- meta_predicate map:intersect_alists(*,*,4,*).
intersect_alists([], _, _, []).
intersect_alists([K-V|List0], List1, Closure, List) :-
	intersect_alists1(List1, K, V, List0, Closure, List).

:- meta_predicate map:intersect_alists1(*,?,?,*,4,*).
intersect_alists1([], _, _, _, _, []).
intersect_alists1([K1-V1|List1], K0, V0, List0, Closure, List) :-
	compare(C, K0, K1),
	intersect_alists2(C, K0, K1, V0, V1, List0, List1, Closure, List).

:- meta_predicate map:intersect_alists2(*,?,*,?,?,*,*,4,*).
intersect_alists2((<), _K0, K1, _V0, V1, List0, List1, Closure, List) :-
	intersect_alists1(List0, K1, V1, List1, Closure, List).
intersect_alists2((=), K, _, V0, V1, List0, List1, Closure, [K-V|List]) :-
	call(Closure, K, V0, V1, V),
	intersect_alists(List0, List1, Closure, List).
intersect_alists2((>), K0, _K1, V0, _V1, List0, List1, Closure, List) :-
	intersect_alists1(List1, K0, V0, List0, Closure, List).

/****************************************************************
			    Printing Maps Legibly
****************************************************************/

print_map(empty_map) :-
	!,
	write('MAP{}').
print_map(Map) :-
	map_iterator(Map, Iter),
	print_map_iterator(Iter, 'MAP{'),
	write('}').
% print_map(_) :-
% 	write('<MAP>').


print_map_iterator(Iter, Sep) :-
	(   map_next(Iter, K1, V1, Iter1) ->
	    write(Sep),
	    print(K1),
	    (	V1 \== '' ->
		write('->'),
		print(V1)
	    ;	true
	    ),
	    print_map_iterator(Iter1, ', ')
	;   true
	).


:- multifile user:portray/1.

user:portray(empty_map) :-
	!,
	print_map(empty_map).
user:portray(map(B,L,K,V,R)) :-
	!,
	print_map(map(B,L,K,V,R)).


/****************************************************************
				Sets as Maps
****************************************************************/

set_empty(Set) :-
	map_empty(Set).

set_cardinality(Set, Cardinality) :-
        map_cardinality(Set, Cardinality).

set_search(Set, Elt) :-
	map_search(Set, Elt, _).

set_member(Elt, Set) :-
	(   nonvar(Elt) ->
		map_search(Set, Elt, _)
	;   map_member(Set, Elt, _)
	).


set_insert(Elt, Set0, Set) :-
	map_store(Elt, '', Set0, Set).

set_iterator(Set, Iter) :-
	map_iterator(Set, Iter).

set_next(Iter0, Elt, Iter) :-
	map_next(Iter0, Elt, _, Iter).
