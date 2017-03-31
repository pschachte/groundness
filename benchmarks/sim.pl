% CVS: $Id: sim.pl,v 1.3 1998/10/21 04:26:16 pets Exp $
/* A simulator for OR-parallel Prolog. It explicitely generates the
   search tree
   hacked for simulator -- Kish Shen 11 Sept. 89
*/
:-op(100,fx,#). /* # is used here for indicating variables in the
                   clauses that are passed to the simulator 
                */
:-op(100, fx, ?). 
:-op(100,fx,@). /* @ is used to indicate local or-parallel points */

:-op(100,fx,^^). /* ^^ is used to indicate `or-under' */

:-op(100,fx,\). /* \ is used to indicate `condor' */

:-op(100,fx,\&). /* \& is `or-under' with itself not parallelised */

goal :- do([query(#1),write(#1),nl],1).

do(Query, NQ) :-
   functor(Vars, $, NQ),
   rename_vars(Query, 0, Vars, Query1),
   /* rename the variables in query from the form ?I to #(I1) */
   ( (procedure_for(Query1, Goal, Rest, Procedure, Type, normal, ParCon, 0), 
   /* Picks up a procedure from the test database corresponding to the
      first predicate of Query1 in name, and indicate if or-parallel
      The second last term specifies if we're at a or-under condition when the procedure
      is to be picked. Last term is the condition after.
   */        
       !, Chain = chain(1,*), /* chain is the structure of or-parallel nodes in a branch */ 
       execute(Type, Procedure, 0, Backtrack_Con, pack(
            stat(0,0),stat(0,0),Last,nodes(Node,Nodes)), 
            pack(NQ,Goal,Rest,1,0,Total_Branches,env(0,$),Stat,ParCon)),
       ( (nonvar(Node), !, /* Node is nonvar ==> meet orpar nodes */
             gen_data(Last, Stat, Data_up),
             Node = branch(_,Data_up,_), /* slot in Data_up */
             ( (Type = orpar, !, 
                   append(Nodes, [Node], Nodes1),
                   Tree = branch(stat(0,0),stat(0,0),node(*,1,Nodes1,*))
                   /* no Data_down (2'nd arg.), top of tree */
               )
                  ;Tree = branch(stat(0,0),stat(0,0),node(*,1,[Node],*))
                  /* if the root node is a normal node, treat as single parallel or-node */
             ),
             write_stat(-1, Total_Branches, Stat)
         )
           ;(gen_data(stat(0,0), Stat, Data_up),
             /* no or-parallel branches: all variable references must be private,
                therefore 3'rd arg is uninstantiated.
             */
             write_stat(-1, Total_Branches, Stat)
            )
       ) 
   )
   ;write(failure),nl /* could not find predicate */ 
  ).



/* write_stat prints out the general satistics on this try */
write_stat(Start_Br, Total_Branches, stat(Success,Failure)):-
   nl, write('***********************************************************'),
   nl,
   Br is Total_Branches - Start_Br,
   write('Total number of main branches: '),write(Br),nl,
   write('Total number of successful resolutions: '),write(Success),nl,
   write('Total number of failed resolutions: '),write(Failure),nl.



/* procedure_for picks up the procedures corresponding to 1st argument */

procedure_for(Query, Goal, Rest, Procedure, orpar, ParCon, ParCon,_) :-
/* This is the or parallel case on the query, marked by @ */
   head_and_body(Query, @(Goal), Rest), !,
   functor(Goal, Predicate, Arity),
   procedure(Predicate, Arity,_, Procedure).
   /* Type is not important here */
procedure_for(Query, Goal, Rest, Procedure, orpar, orunder(Gen), orunder(Gen),_) :-
/* This is the case where we meet another `orunder' while an `orunder' is already in effect.
   Since earlier `orunder' must be earlier in generation (otherwise it would have been canceled)
   The earlier generation is kept */
   head_and_body(Query, ^^(Goal), Rest), !,
   functor(Goal, Predicate, Arity), 
   procedure(Predicate, Arity,_, Procedure).
procedure_for(Query, Goal, Rest, Procedure, orpar, normal, orunder(Gen), Gen) :-
/* This is the case where we meet an `orunder' */
   head_and_body(Query, ^^(Goal), Rest), !,
   functor(Goal, Predicate, Arity), 
   procedure(Predicate, Arity,_, Procedure).
procedure_for(Query, Goal, Rest, Procedure, normal, orunder(Gen), orunder(Gen),_) :-
/* There is two types of `orunder': one that include itself as or-parallel, (^^), and one 
   exclude itself (\&) 
   This is the case where we meet another `orunder' while an `orunder' is already in effect.
   Since earlier `orunder' must be earlier in generation (otherwise it would have been canceled)
   The earlier generation is kept */
   head_and_body(Query, \&(Goal), Rest), !,
   functor(Goal, Predicate, Arity), 
   procedure(Predicate, Arity,_, Procedure).
procedure_for(Query, Goal, Rest, Procedure, normal, normal, orunder(Gen), Gen) :-
/* This is the case where we meet an `orunder' */
   head_and_body(Query, \&(Goal), Rest), !,
   functor(Goal, Predicate, Arity), 
   procedure(Predicate, Arity,_, Procedure).
procedure_for(Query, Goal, Rest, Procedure, orpar, orunder(Gen), orunder(Gen),_) :-
/* this is the case for meeting a condor while in orunder */
   head_and_body(Query, \(Goal), Rest), !,
   functor(Goal, Predicate, Arity),
   procedure(Predicate, Arity,_, Procedure).
procedure_for(Query, Goal, Rest, Procedure, Type, normal, normal,_) :-
/* Case for meeting condor with no orunder in effect */
   head_and_body(Query, \(Goal), Rest), !,
   functor(Goal, Predicate, Arity),
   procedure(Predicate, Arity, Type, Procedure).
procedure_for(Query, Goal, Rest, Procedure, Type, ParCon, ParCon,_) :-
   head_and_body(Query, Goal, Rest), functor(Goal, Predicate, Arity),
   /* pick up first predicate from the query list */
   procedure(Predicate, Arity, Type, Procedure). 
   /* This picks up the Procedures corresponding to the name Predicate */


head_and_body([X|L], X, L).


/* execute unifies the predicate given to it in the 3rd argument (Goal) with
   the clauses given to it in the 1st arguement (Procedure), and then executing
   any remaining goals in the 4th argument (Rest)
*/



execute(buildin, buildin, Branch, Backtrack_Con, pack(stat(S,F), 
    Last,Current,nodes(Node,[])), pack(NQ,Goal,Rest,Depth,Generation,Branch1,
    Env0,Stat,ParCon)) :-
/* deal with buidin predicates */
    Depth1 is Depth + 1,
    call_buildin(Goal, Env0, Env, NQ, NQ1, Condition),
    ( (Condition = success, !, /* if build-in predicate had suceeded */
         Generation1 is Generation + 1,
         S1 is S + 1, 
         nextbranch([##], Generation1, Rest, pack(pack(NQ1, Branch, Depth1, Branch1, Env, Backtrack_Con,
                    stat(S1,F), Stat, Last, Current, Node), ParCon))
      /* If there is a cut, must be from a sibling goal (i.e. same generation),
         therefore, pass it up without question
      */
      )
     ;(F1 is F + 1,
       Stat = stat(S,F1),
       Branch1 is Branch,
       Backtrack_Con = normal,
       Current = Last
      )
    ), !.
execute(cut, [!], Branch, cut(Cut_Generation), pack(stat(S,F),Last,
        Current,nodes(Node,[])), pack(NQ,!,Rest,Depth,Generation,Branch1,
        Env,Stat,ParCon)):-
/* Special case of cut as goal. Need special treatment on backtracking */
    Generation1 is Generation + 1,
    Depth1 is Depth + 1,
    S1 is S + 1,
    nextbranch([##], Generation1, Rest, pack(pack(NQ, Branch, Depth1, Branch1, Env, Backtrack_Con1,
               stat(S1,F), Stat, Last, Current, Node), ParCon)),
    ( (Backtrack_Con1 = cut(Cut_Generation), !,
      /* Is there a 'cut' from deeper down the tree? If so, use its generation */
       true)
      ;Cut_Generation is Generation /* if not, use current generation */
    ),          
    !.
execute(normal, Procedure, Branch, Backtrack_Con, pack(S,Last, 
        Current,nodes(Node,[])), Package) :-
    Package =  pack(NQ,Goal,Rest,Depth,Generation,Branch1,Env,S1,ParCon),
    Depth1 is Depth + 1,
    match(Procedure, First_alternative_body, Rest_of_rule, NQ, NQ1, Goal, Env, Env1,
          Condition1, S, S2),
     ( (Condition1 = failure, !,
          Backtrack_Con = normal,
          Branch1 is Branch, /* normal node, so no change */
          S = S1,
          Last = Current
      )
        ;(Generation1 is Generation + 1,
          nextbranch(First_alternative_body, Generation1, Rest, pack(pack(NQ1, Branch, Depth1,
                     Branch3, Env1, Backtrack_Con2, S2, S3, Last, Current1, 
                     Node2), ParCon)),
          backtrack(normal, Rest_of_rule, Branch3, Backtrack_Con2, Backtrack_Con,
          pack(S3,Current1,Current,nodes(Node1,_)), Package),
          /* if both Node and Node1/Nodes1 are or-parallel nodes, we've
             embedded or-nodes
         */
          ( (nonvar(Node1), nonvar(Node2), !, /* has or-parallel been meet on both sides ? */
                 /* note: Node2 is the left-node, Node1 is the right-node */
                 Node2 = branch(_,_, node(*,_,_,Ex_Node)),
                 Node1 = branch(Data,_,Node_R),
                 insert_ex_node(Node_R, Data, Ex_Node),
                 /* insert Node_R (the right-node's data) into Ex_Node of the left-node */
                 Node = Node2 /* pass the left-node up */
            )
               ;pass_up(Node1, Node2, Node) /* pass up the Node that is or-parallel */
          )
         )
    ).
execute(orpar, Procedure, Branch, Backtrack_Con, pack(S,Last, 
    Current,nodes(Node,Nodes)), Package) :-
    Package = pack(NQ,Goal,Rest,Depth,Generation,Branch1,Env,S1,ParCon),
    Depth1 is Depth + 1,
    match(Procedure, First_alternative_body, Rest_of_rule, NQ, NQ1, Goal, Env,
          Env1, Condition1, S, S2),
    /* Matches Goal against the rule-heads in Procedure, returning the first
       one that matches. The body of the clause is First_alternative_body,
       the rest of the alternatives are Rest_of_rule. Condition1 indicates success
       or failure to match anything
    */
    ( (Condition1 = failure,
         !, 
         Backtrack_Con = normal, 
         Branch1 is Branch,
         S1 = S,
         /* the Goal failed to match the clauses in the database, so this
            branch has failed
         */
         Nodes = [],
         Current = Last
      )
       ;( (/*true =>  /* independently executable */
	  do_left_branch(S2, Generation, First_alternative_body, Rest, NQ1,
	      Branch, Depth1, Branch3, Env1, Backtrack_Con2, S2, stat(SuBase,
	      FaBase), Current1, Child, Node1, ParCon) ,
         backtrack(orpar, Rest_of_rule, 0, normal, Backtrack_Con1,
	 			/* need normal backtrack condition */
                   pack(stat(0,0), stat(0,0), Rt_Child, nodes(Node, Nodes2)), 
		   pack(NQ,Goal,Rest,Depth,Generation,Delta_Branch,Env,stat(DSu,DFa),ParCon))
	  ),
		   /* DSu, DFa and Delta_Branch are the changes in these from
		      start of backtrack
		   */
	 NewSu is SuBase + DSu, NewFa is FaBase + DFa,
	 S1 = stat(NewSu,NewFa),
	 Branch1 is Branch3 + Delta_Branch,
	 set_condition(Generation, Backtrack_Con2, Backtrack_Con1, Backtrack_Con),

	 /* set_condition moved up for IAP */
         ( (nonvar(Node), !, /* note Node is the rightmost node */ 
         /* if Node is not a variable ==> this is not the backtrack that hits no alternatives */
                [Node1|Nodes2] = Nodes
                /* glue the branch due to nextbranch to those due to backtracking */
           )
          ;(Node1 = Node, /* if Node was uninstan. ==> branch due to nextbranch is right-most */
            Nodes = []
           )
         ), 
         Current = S1
        )
    ).


/* fill fills in the value of the Ex_Node of a node in the shape tree with
   '*' if it is a variable (so that the shape tree generated will not 
   contain any variables), or leave it if it is already instantiated to '*'
   (i.e. there is at least one extra node )
*/
fill(Ex_Node):- Ex_Node == *, !. /* already set to '*', leave it */
fill(*). /* instantiate ex-node slot to '*'  if it is a variable */
fill(node(_,_,_,Ex_Node)):- fill(Ex_Node). /* go down to next level */

/* use for allowing IAP */
do_left_branch(S2, Generation, First_alternative_body, Rest, NQ1,
	Branch, Depth1, Branch3, Env1, Backtrack_Con2, S2, S3,
        Current1, Child, Node1, ParCon) :-
   S2 = stat(Su,Fa), Su1 is Su - 1,
   /* exclude the sucessful unifcation in match */
   Current1 = stat(Su1, Fa),
   Generation1 is Generation + 1,
   nextbranch(First_alternative_body, Generation1, Rest, pack(pack(NQ1, Branch, Depth1, 
             Branch3, Env1, Backtrack_Con2, S2, S3, Current1, Child,
             Node1), ParCon)),
        /* Try the body of the successful head (if any), and then the other
           outstanding goals, Child will hold the conditions on backtracking
           to the last or-parallel node
        */
             gen_data(Child, S3, Data_up), /* get the data_up stats */
   ( (var(Node1), !, /* if Node1 is a variable ==> have not meet or-parallel
                        node, i.e. we're at a tip of the shape tree 
                     */
           Node1 = branch(Data_up, stat(0,0), tip)
           /* set node as tip - put Data_up in Data_down's slot */
     )
         ;(Node1 = branch(_,Data_up,node(_,_,_,Ex_Node)), /* instan. the Data_up slot */
           fill(Ex_Node)  /* fill in the value of Ex_Node properly */
          )
   ).
	 

/* insert_ex_node inserts the node of the 1'st arguement into the first 
   free extra-node slot of the 3'rd arguement */

insert_ex_node(node(_,Dep,Li,Ex), Data, Main):-
    var(Main), !, /* we have found the slot */
    Main = node(Data, Dep, Li, Ex). /* put Data in its rightful place */
insert_ex_node(Node, Data, node(_,_,_,Main)):-
    insert_ex_node(Node, Data, Main). /* try next-level down */

/* pass_up is used for passing up an or-parallel node through the normal nodes */

pass_up(Node1, Node2, Node):- /* if any side contain an or-parallel node, pass it up */
    (nonvar(Node1), !, Node = Node1)
   ;(nonvar(Node2), !, Node = Node2), !.
pass_up(_,_,_). /* neither side is defined, so no defined Node yet */


/* gen_data generates the statistical data that is to be stored in nodes of the shade tree */

gen_data(stat(S1,F1), stat(S2, F2), stat(S,F)):-
    S is S2 - S1,
    F is F2 - F1.

/* fill_list instantiates the unistantiated elements of a list to 0 */

fill_list([]):- !.
fill_list([E|L]):-
    var(E), !, E = 0, fill_list(L).
fill_list([E|L]):- fill_list(L).

 
/* call_buildin executes the buildin procedures */
call_buildin(put(T1), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T2, Env), 
     ( (evaluate(T2, T3), !,
           Condition = success,
           put(T3)
       )
      ;(write('Prolog engine error: non-arithematic expression as argument for "put"'),
        nl,
        Condition = failure
       )
      ).
call_buildin(=\=(T1, T2), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env), 
     copy_term(T2, T4, Env),
     ( (evaluate(T3, T5), evaluate(T4, T6), !,
        ( (=\=(T5, T6), !,
             Condition = success
          )
         ;Condition = failure
        )
       ;(write('Prolog engine error: non-number found in arithematic expression'),
         write(' for =\= predicate.'),
         nl, /* evaluate has failed: must be non-number */
         Condition = failure
        )
       )
     ).
call_buildin(=:=(T1, T2), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env), 
     copy_term(T2, T4, Env),
     ( (evaluate(T3, T5), evaluate(T4, T6), !,
        ( (=:=(T5, T6), !,
             Condition = success
          )
         ;Condition = failure
        )
       ;(write('Prolog engine error: non-number found in arithematic expression'),
         write(' for =:= predicate.'),
         nl, /* evaluate has failed: must be non-number */
         Condition = failure
        )
       )
     ).
call_buildin(>(T1, T2), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env), 
     copy_term(T2, T4, Env),
     ( (evaluate(T3, T5), evaluate(T4, T6), !,
        ( (>(T5, T6), !,
             Condition = success
          )
         ;Condition = failure
        )
       ;(write('Prolog engine error: non-number found in arithematic expression'),
         write(' for > predicate.'),
         nl, /* evaluate has failed: must be non-number */
         Condition = failure
        )
       )
     ).
call_buildin(>=(T1, T2), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env), 
     copy_term(T2, T4, Env),
     ( (evaluate(T3, T5), evaluate(T4, T6), !,
        ( (>=(T5, T6), !,
             Condition = success
          )
         ;Condition = failure
        )
       ;(write('Prolog engine error: non-number found in arithematic expression'),
         write(' for > predicate.'),
         nl, /* evaluate has failed: must be non-number */
         Condition = failure
        )
       )
     ).
call_buildin(<(T1, T2), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env), 
     copy_term(T2, T4, Env),
     ( (evaluate(T3, T5), evaluate(T4, T6), !,
        ( (<(T5, T6), !,
             Condition = success
          )
         ;Condition = failure
        )
       ;(write('Prolog engine error: non-number found in arithematic expression'),
         write(' for > predicate.'),
         nl, /* evaluate has failed: must be non-number */
         Condition = failure
        )
       )
     ).
call_buildin(=<(T1, T2), Env, Env, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env), 
     copy_term(T2, T4, Env),
     ( (evaluate(T3, T5), evaluate(T4, T6), !,
        ( (=<(T5, T6), !,
             Condition = success
          )
         ;Condition = failure
        )
       ;(write('Prolog engine error: non-number found in arithematic expression'),
         write(' for > predicate.'),
         nl, /* evaluate has failed: must be non-number */
         Condition = failure
        )
       )
     ).
call_buildin(=(T1, T2), Env, Env1, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env),
     copy_term(T2, T4, Env),
     ( (unify(T3, T4, Env, Env1), !,
       /* the `=' predicate may be reguarded as unification */
            Condition = success
       )
      ;(Condition = failure,
        Env1 = Env /* set the correct values */
       )
     ).
call_buildin(\=(T1, T2), Env, Env1, NQ, NQ, Condition):-
     copy_term(T1, T3,  Env),
     copy_term(T2, T4, Env),
     ( (unify(T3, T4, Env, Env1), !,
       /* the `\=' predicate may be reguarded as succeding when unification fails */
            Condition = failure
       )
      ;(Condition = success,
        Env1 = Env /* set the correct values */
       )
     ).
call_buildin(integer(T), Env, Env, NQ, NQ, Condition):-
     copy_term(T, T1, Env),
     ( (integer(T1), !,
           Condition = success
       )
      ;Condition = failure
     ), !.
call_buildin(number(T), Env, Env, NQ, NQ, Condition):-
     copy_term(T, T1, Env),
     ( (number(T1), !,
           Condition = success
       )
      ;Condition = failure
     ), !.
call_buildin(atom(T), Env, Env, NQ, NQ, Condition):-
     copy_term(T, T1, Env),
     ( (atom(T1), !,
           Condition = success
       )
      ;Condition = failure
     ), !.
call_buildin(atomic(T), Env, Env, NQ, NQ, Condition):-
     copy_term(T, T1, Env),
     ( (atomic(T1), !,
           Condition = success
       )
      ;Condition = failure
     ), !.
call_buildin(is(T1, T2), Env, Env1, NQ, NQ, Condition):-
    copy_term(T1, T3, Env),
    copy_term(T2, T4, Env),
    ( (evaluate(T4, T5), !,
         ( (unify(T3, T5, Env, Env1), !,
           /* only evaluate RHS of `is', as in Prolog */
               Condition = success
           )
          ;(Condition = failure,
            Env1 = Env
           )
         )
      )
     ;(Condition = failure,
       write('Prolog engine error: non-number found in arithematic expression'),
       write(' for "is" predicate:'), nl,
       write(T2), nl,
       Env1 = Env
      )
    ).
call_buildin(var(T), Env, Env, NQ, NQ, Condition):-
    copy_term(T, T1, Env),
    ( (T1 = #(_), /* representation of a variable in this engine */
          Condition = success
      )
     ;Condition = failure
    ).
call_buildin(nonvar(T), Env, Env, NQ, NQ, Condition):-
    copy_term(T, T1, Env),
    ( (\+(T1 = #(_)), /* representation of a variable in this engine */
          Condition = success
      )
     ;Condition = failure
    ).
call_buildin(functor(T1,T2,T3), Env, Env1, NQ, NQ1, Condition):-
    copy_term(T1, Te1, Env),
    copy_term(T2, Te2, Env),
    copy_term(T3, Te3, Env),
    ( (Te1 = #(_), /* if the first term is a variable, then other two terms must be instantiated */
           ( (\+(Te2 = #(_)), \+(Te3 = #(_)),
              functor(St, Te2, Te3), /* St is the structure constructed by Te2 and Te3 */
              !, /* if any of the above three conditions fails, then the simulator's functor will fail */
              fill_in(Te3, St, NQ), /* Convert the variables in St from underlying Prolog format to the #(N) format */ 
              /* Te3 is the arity of St */
              NQ1 is NQ + Te3,
              ( (unify(Te1, St, Env, Env1), !,
                     Condition = success
                )
               ;(write('Internal error message -- functor not functioning....'), nl)
              )
             )
            ;(Condition = failure, /* simulator's functor has failed */
               Env = Env1, NQ1 is NQ
             )
            )
      )
     ;(/* The first term is not a variable */
           (do_functor(Te1, Te2, Te3, T2, T3, Env, Env1), !,
                  Condtion = success, NQ1 is NQ
            )
           ;(Condtion = failure,
             Env = Env1, NQ1 is NQ
            )
      )
    ).
call_buildin(arg(T1,T2,T3), Env, Env1, NQ, NQ, Condition):-
     copy_term(T1, Te1, Env),
     copy_term(T2, Te2, Env),
     /* T1 and T2 needs to be instantiated */
     ( (\+(Te1 = #(_)), \+(Te2 = #(_)), !,
                 copy_term(T3, Te3, Env),
                 ( (arg(Te1, Te2, X), /* X is the Te1'th argument of Te2 */
                    unify(Te3, X, Env, Env1), !,
                         Condition = success
                   )
                  ;(Condition = failure, /* arg has failed, or T3 failed to unify with X */
                    Env = Env1
                   )
                 )
       )
      ;(Condition = failure, /* Te1 or Te2 are not variables */
        Env = Env1
       )
     ).
call_buildin(numbervars(T,N0,N1), Env, Env1, NQ, NQ, Condition):-
     copy_term(T, T1, Env),
     copy_term(N0, N, Env),
     ( (integer(N), !,
             number_vars(T, T1, N, Nu, Env, Env2),
             /* recursively number variables in T, Nu is the next number value */
             ( (unify(N1, Nu, Env2, Env1), !,
                     Condition = success
               )
              ;(Condition = failure, /* unification failed, something wrong with third argument */
                Env2 = Env1
               )
             )
       )
      ;(write('Prolog engine error -- 2nd argument of numbervars not integer'),nl,
        Condition = failure,
        Env = Env1
       )
     ).
call_buildin(fail, Env, Env, NQ, NQ, failure).
call_buildin(false, Env, Env, NQ, NQ, failure).
call_buildin(true, Env, Env, NQ, NQ, success).
call_buildin(write(T1), Env, Env, NQ, NQ, success):-
/* write always suceeds */
     copy_term(T1, T2, Env), write(T2).
/* for write or any other buildin predicates with argument, use copy_term */
call_buildin(engine_debug, Env, Env, NQ, NQ, success) :-
     write('Prolog engine debugging breakpoint'), nl,
     read(_). /* just a dummy term to allow programmer a breakpoint */
call_buildin(display(T1), Env, Env, NQ, NQ, success):-
     copy_term(T1, T2, Env), display(T2).
call_buildin(Goal,Env,Env,NQ,NQ,success):- Goal. /* general no argument case */

number_vars(T, #(C), N, Nu, Env, Env1):-
/* structure is a variable, unify it with $VAR(I) */
      unify(T, '$VAR'(N), Env, Env1),
      Nu is N + 1. 
number_vars(T, T1, N, Nu, Env, Env1):-
/* T is the simulator variable associated with T1, the structure */
      functor(T1,_,N1), /* N1 is the arity of T1 */
      number_vars(0, N1, T, T1, N, Nu, Env, Env1).

number_vars(Max,Max,_,_,N,N,Env,Env). /* finish numbering structure */
number_vars(I, Max, T, T1, N, Nu, Env, Env1):-
     I1 is I + 1, /* number (if needed) the I'th + 1 argument for structure T1 */
     arg(I1,T1,A), /* get the I'th + 1 argument */
     copy_term(A, A1, Env),
     number_vars(A, A1, N, Nu1, Env, Env2),
     /* recursively number sub-terms */
     number_vars(I1, Max, T, T1, Nu1, Nu, Env2, Env1).
    /* go for next term in T */

fill_in(0,_,_).
fill_in(N, St, NQ):-
    arg(N, St, X), N1 is NQ + N,
    X = #(N1), N2 is N - 1, /* rename the variables */
    fill_in(N2, St, NQ).

do_functor(T, #(I1), #(I2), T2, T3, Env, Env1):-
    !, functor(T, F, N), /* get F and N */
    ( (unify(T2, F, Env, Env2),
       unify(T3, N, Env2, Env1)
      )
     ;(write('Internal error in functor...point 2'), nl)
    ).
do_functor(T, #(I1), N, T2,_, Env, Env1):-
/* N is not a variable */
   !, functor(T, F, N), /* get F */
   ( (unify(T2, F, Env, Env1) )
    ;(write('Internal error in functor...point 3'), nl)
   ).
do_functor(T, F, #(I2),_, T3, Env, Env1):-
/* N is not a variable */
   functor(T, F, N), /* get N */
   ( (unify(T3, N, Env, Env1) )
    ;(write('Internal error in functor...point 4'), nl)
   ).
   
/* evaluate evaluates an arithematic expression */
evaluate(T1 + T2, T3):- !,
    evaluate(T1, N1), 
    evaluate(T2, N2), 
    T3 is N1 + N2.
evaluate(T1 - T2, T3):- !,
    evaluate(T1, N1), 
    evaluate(T2, N2), 
    T3 is N1 - N2.
evaluate(T1 * T2, T3):- !,
    evaluate(T1, N1), 
    evaluate(T2, N2), 
    T3 is N1 * N2.
evaluate(T1 / T2, T3):- !,
    evaluate(T1, N1), 
    evaluate(T2, N2), 
    ( (N2 = 0, !, 
         write('Prolog engine error- division by zero: answer set to zero'),
         nl, T3 is 0
      )
      ;(T3 is N1 / N2)
    ).
evaluate(T, T):- number(T).


/* copy_term dereferences a term and all its variables and copies its contents to its 2'nd argument */

copy_term(T1, T3, Env):- 
    dereference(T1, T2, Env), 
    copy_term0(T2, T3, Env).
copy_term0(#(N), #(N), _):- !.
copy_term0(T, T, _):- atomic(T), !.
copy_term0(T1, T2, Env):-
    functor(T1, F, N), functor(T2, F, N), copy_terms(N, T1, T2, Env).
copy_terms(0,_,_,_):- !.
copy_terms(N, T1, T2, Env):-
    arg(N, T1, X1), arg(N, T2, X2), copy_term(X1, X2, Env),
    N1 is N - 1, copy_terms(N1, T1, T2, Env).


/* nextbranch explores a branch of the tree from point of call */

nextbranch([##], Generation, Rest, Package) :-
/* the body of the current predicate has been exhausted; expand Rest */
    reset_generation(Rest, Rest1, Generation, Generation1, State), 
    grow_branch(State, Generation1, Rest1, Package).
nextbranch(Body, Generation, Rest, pack(InnerPackage,ParCon)) :-
/* There are body goals, try them */
    update_parcon(ParCon, ParCon1, Generation),
    find_and_execute(Generation, Body, Rest, InnerPackage, ParCon1).

/* grow_branch is used by nextbranch to decide if a branch should be grown,
   and if so, grow it 
*/
grow_branch(not_empty, Generation, Rest, pack(InnerPackage,ParCon)) :-
     Generation1 is Generation - 1, /* for this '##' */
     update_parcon(ParCon, ParCon1, Generation1),
     find_and_execute(Generation1, Rest, InnerPackage, ParCon1).
grow_branch(empty,_,_,pack(pack(_,Branch,Depth,Branch,_,normal,Stat,Stat,Current,
            Current,_),_)) :-
/* Have exhausted Rest, i.e. reached the end of a branch */
     write('Success'), nl, Depth1 is Depth - 1, /* report correct depth */
     write('Depth of branch is '), write(Depth1), nl, nl.

/* find_and_execute finds the next goal, and executes it. */
find_and_execute(Generation, Rest, InnerPackage, ParCon) :-
/* for the nextbranch([##],... case */
     procedure_for(Rest, Goal, Rest1, Procedure, Type, ParCon, ParCon1, Generation), !,
     /* if we find a Goal, then... */
     InnerPackage = pack(NQ, Branch, Depth, Branch1, Env, Backtrack_Con, S, S1, Last, Current, Node),
     execute(Type, Procedure, Branch, Backtrack_Con, pack(S, 
             Last, Current1, Nodes1), pack(NQ, Goal, Rest1, Depth, Generation, Branch1, 
             Env, S2, ParCon1)),
     make_or_node(Type, InnerPackage, pack(S2,Current1,Nodes1)).
find_and_execute(_,_,pack(_,Branch,_,Branch,_,normal,stat(S,F),stat(S,F1),
                 Current,Current,_),_) :-
/* Failed to find goal, one more failure */
     F1 is F + 1.

find_and_execute(Generation, Body, Rest, InnerPackage, ParCon) :-
/* for the other nextbranch */
     procedure_for(Body, Goal, Rest1, Procedure, Type, ParCon, ParCon1, Generation), !,
     /* if we find a Goal, then... */
     append(Rest1, Rest, Rest2),
     InnerPackage = pack(NQ, Branch, Depth, Branch1, Env, Backtrack_Con, S, S1, Last, Current, Node),
     execute(Type, Procedure, Branch, Backtrack_Con, pack(S, 
             Last, Current1, Nodes1),
             pack(NQ, Goal, Rest2, Depth, Generation, Branch1, Env, S2, ParCon1)),
     make_or_node(Type, InnerPackage, pack(S2,Current1,Nodes1)).
find_and_execute(_,_,_,pack(_,Branch,_,Branch,_,normal,stat(S,F),stat(S,F1),
                 Current,Current,_),_) :-
/* Failed to find goal, one more failure */
     F1 is F + 1.
    
/* made_or_node creates an or-node if Type is orpar */
make_or_node(normal, pack(_,_,_,_,_,_,S,S1,_,Current,Node), pack(S2,Current,nodes(Node,_))) :-
    (S == S2 -> /* Complete failure for node? */ 
        (S = stat(Su, Fa), Fa1 is Fa + 1, S1 = stat(Su, Fa1) )
       ;S1 = S2
    ).
make_or_node(buildin, pack(_,_,_,_,_,_,S,S1,_,Current,Node), pack(S2,Current,nodes(Node,_))) :-
/* deal with buildins */
    (S == S2 -> /* Complete failure for node? */ 
        (S = stat(Su, Fa), Fa1 is Fa + 1, S1 = stat(Su, Fa1) )
       ;S1 = S2
    ).
make_or_node(orpar, pack(_,_,Depth,_,_,_,S,S1,Last,Current,Node), Extra_Info) :-
/* or-parallel node, so we want to make an or-node */
    gen_data(Last, S, Data_down), /* creat the data entry */
    make_or_node(pack(S1,Current,Node,Data_down,Depth), Extra_Info).

make_or_node(Package, pack(S2,_,nodes(Node1,_))) :-
    var(Node1), !, /* there was a complete failure */
    Package = pack(Current,Current,Node,Data_down,Depth),
    /* set Current to S1, the state of data at this point */
    S2 = stat(Su,Fa),
    Fa1 is Fa + 1, /* One more failure */
    Current = stat(Su,Fa1),
    Node = branch(Data_down,_, node(*, Depth, [branch(stat(0,1),
           stat(0,0),tip)],_)).
make_or_node(pack(S,Current,Node,Data_down,Depth), pack(S,Current,nodes(Node1,Nodes))) :-
/* successful or-node, so must glue the branches together.
   Current is set to Current1, S to S2 (what is returned by execute)
*/
    append(Nodes, [Node1], Nodes1),
    Node = branch(Data_down,_,node(*,Depth,Nodes1,_)).

/* update_parcon updates the Parcon variable before a new unification */
update_parcon(normal, normal,_). /* if it is normal to start off with, then no problem */
update_parcon(orunder(Gen), orunder(Gen), Gen1) :- Gen1 > Gen.
/* ParCon is propagated as long as the clause's is of a younger generation */
update_parcon(orunder(_), normal,_).
 
/* reset_generation resets the Generation at the end of a clause's body */
reset_generation([],_,G,G,empty):- !. /* at end of query */
reset_generation([##|R], R1, G, G1, State):- reset_generation(R,R1,G,G2,State), G1 is G2 - 1, !.
/* ## indicates one more level has been entered, thus remove it. R1 is
   the new Rest with all '##' (level markers) removed
 */
reset_generation(L, L, G, G, not_empty). /* exhausted levels, return */


/* backtrack handles 'backtracking'- both when a branch succeeds, or when it fails */
backtrack(_,[],Branch,Back_Con,Back_Con,pack(S,C,C,nodes(_,[])),pack(_,_,_,_,_,Branch,_,S,_)).
backtrack(orpar, Procedure, Branch, _, Backtrack_Con, 
          Package1, Package2) :-
/* node is a or-parallel node */
     Package2 = pack(_,_,_,_,Generation,_,_,_,_), /* Get Generation */
     ( (Procedure = [], !, Branch2 is Branch) /* keep same branch if no more or-parallel alternatives */
                          ;Branch2 is Branch + 1 
     ),
     execute(orpar, Procedure, Branch2, Backtrack_Con, Package1, Package2).
     /* try next alternative regardless of conditions */
backtrack(normal,_,Branch,cut(Generation1),Backtrack_Con,pack(S,C,C,nodes(_,[])),
          pack(_,_,_,_,Generation,Branch,_,S,_)) :-
/* normal node with cut passed up from lower down the branch */
    filter(Generation, Generation1, Backtrack_Con), !.
    /* if cut occured in same generation as node, then pass it up */
backtrack(normal, Proc, Branch, Back_in, Backtrack_Con, Package1, Package2) :-
/*  normal node */
   Package2 = pack(_,_,_,_,Gen,_,_,_,_), /* get Generation */
   execute(normal, Proc, Branch, Backtrack_Con1, Package1, Package2),
    /* try next alternative since there are no cuts */
   set_condition(Gen,Back_in,Backtrack_Con1,Backtrack_Con).


/* set_condition sets the condition to be returned to the parent node of a or-parallel node */

set_condition(G, cut(G1), cut(G2), Backtrack_Con):- /* use smaller generation */
    ( (G2 < G1, !, filter(G, G2, Backtrack_Con) )
                  ;filter(G, G1, Backtrack_Con)
    ), !.
set_condition(G, cut(G1),_, Backtrack_Con):- filter(G, G1, Backtrack_Con), !.
set_condition(G,_, cut(G1), Backtrack_Con) :- filter(G, G1, Backtrack_Con), !.
/* if the branch, or any on it's right have beened cuted- filter the cut */
set_condition(_,_,_, normal).


/* filter is used to filter a 'cut' condition- if cut occurs in a brother predicate
   (i.e. the cut and the current predicate are in same generation), then pass cut up
*/
filter(Generation, Generation, cut(Generation)):- !.
filter(Gen,Cut_Gen, cut(Cut_Gen)):- Gen > Cut_Gen, !. /* if 'cut' is in an older gen. */
filter(_,_, normal). /* reached its parent, so reset cut */


/* match attemps to unify Goal (6th argument) with the alternatives in Procedure (1st
   argument), returning the new Environement (7th arg.) and success/failure (10th arg.)
*/

match([],[],[], NQ, NQ,_, Env, Env, failure, Stat, Stat).
/* all alternatives exhausted */
match([NC-Clause|Clauses], Body, Clauses1, NQ, NQ1, Goal, Env, Env1, Condition,
      stat(Successes, Failures), stat(Successes1, Failures) ):-
    functor(Vars,$,NC), /* this is used to enforce sharing for the same var. */
    rename_vars(Clause, NQ, Vars, [Head|Body1]), 
    ( (unify(Goal, Head, Env, Env2), !, 
          append(Body1,[##], Body), /* add in a new level */
          Clauses1 = Clauses, Condition = success,
          NQ1 is NQ + NC, Env1 = Env2,
          Successes1 is Successes + 1, Failures1 is Failures
      )
        ;(match(Clauses, Body, Clauses1, NQ, NQ1, Goal, Env, Env1, Condition, 
                stat(Successes, Failures), stat(Successes1, Failures))
          /* try other alternatives if first fails */
         )
    ).


/* rename_vars renames the variable from the form ?I to #(I1) for later
   to avoid duplication of variable names
*/

rename_vars(#I, NQ, Vars, #(I1)):-
     !, I1 is I + NQ.
    /* rename variables from I to I1 to avoid duplication */
rename_vars([X|L], NQ, Vars, [X1|L1]) :-
    !, rename_vars(X, NQ, Vars, X1), rename_vars(L, NQ, Vars, L1).
rename_vars(T,_,_,T):- atomic(T), !.
rename_vars(T, NQ, Vars, T1):- /* T is a clause here */
    functor(T, F, N), functor(T1, F, N), rename_vars(N, T, NQ, Vars, T1).
    /* rename each argument of T in turn */


rename_vars(0,_,_,_,_):- !.
rename_vars(N, T, NQ, Vars, T1):-
    arg(N, T, X), arg(N, T1, X1), rename_vars(X, NQ, Vars, X1),
    /* rename the N'th arg. of T to the N'th arg. of T1 */
    N1 is N-1, rename_vars(N1, T, NQ, Vars, T1).


/* unify attemps to unify a goal (1'st arg.) to an alternative (2'nd arg.) in 
   the database. The last arg. indicates if the attempt is successful or not
*/

unify(#(I), #(I), Env, Env):- !.
unify(#(I1), #(I2), Env, Env1):- !, 
/* two different variables */
    ( (I1 < I2, !, bind(#(I2), #(I1), Env, Env1))
                 ;(bind(#(I1), #(I2), Env, Env1))
    ).
    /* binds the later variable to the earlier one */
unify(#(I), T, Env, Env1):-
    !, bind(#(I), T, Env, Env1).
unify(T, #(I), Env, Env1):- 
    !, bind(#(I), T, Env, Env1).
/* bind the non-var term to the variable */
unify(T1, T2, Env, Env):- atomic(T1), T1 = T2, !.
unify(T1, T2, Env, Env1):- 
    functor(T1, F, N), functor(T2, F, N),
    unify(0, N, T1, T2, Env, Env1).


unify(N,N,_,_,Env,Env):- !.
unify(I,N,T1,T2,Env0,Env):-
    I < N, I1 is I + 1,
    arg(I1, T1, X1), arg(I1, T2, X2),
    dereference(X1, Y1, Env0),
    dereference(X2, Y2, Env0),
    unify(Y1, Y2, Env0, Env1), 
    unify(I1, N, T1, T2, Env1, Env).


/* dereference gets the 'value' of a variable by look-up of Array and
   dereferencing through the reference chain
*/

dereference(#(I), Y, env(Size, Array)):-
    I < 1<<Size, 
    N is Size-2, Subindex is I>>N /\ 3, 
    array_item(Subindex, N, I, Array, X), !,
    dereference(X, Y, env(Size, Array)).
dereference(X,X,_).

/* bind binds two terms together */

bind(#(I), X, env(Size, Array), env(Size1, Array1)):-
    enlarge_array(I, Size, Array, Size1, Array0), !,
    update_array_item(Size1, I, Array0, X, Array1). /* put in value */

enlarge_array(I, Size, Array, Size, Array):- I < 1<<Size, !.
/* variable-cell has been added to Array, no need to expand */
enlarge_array(I, Size0, Array0, Size, Array):-
    Size1 is Size0 + 2,
    Array1 = $(Array0, $,$, $), !,
    enlarge_array(I, Size1, Array1, Size, Array).


array_item(0, 0, Index, $(Item,_,_,_), Item):- !, not_undef(Item).
array_item(0, N, Index, $(Array,_,_,_), Item):-
    N1 is N - 2,
    Subindex is Index >> N1 /\ 3,
    array_item(Subindex, N1, Index, Array, Item).
array_item(1, 0, Index, $(_,Item,_,_), Item):- !, not_undef(Item).
array_item(1, N, Index, $(_, Array,_,_), Item):-
    N1 is N - 2,
    Subindex is Index >> N1 /\ 3,
    array_item(Subindex, N1, Index, Array, Item).
array_item(2, 0, Index, $(_,_,Item,_), Item):- !, not_undef(Item).
array_item(2, N, Index, $(_,_,Array,_), Item):-
    N1 is N - 2,
    Subindex is Index >> N1 /\ 3,
    array_item(Subindex, N1, Index, Array, Item).    
array_item(3, 0, Index, $(_,_,_,Item), Item):- !, not_undef(Item).
array_item(3, N, Index, $(_,_,_,Array), Item):- 
    N1 is N - 2,
    Subindex is Index >> N1 /\ 3,
    array_item(Subindex, N1, Index, Array, Item).


not_undef($):- !, fail. /* $ is the symbol used for undefined in array */
not_undef(_).


update_array_item(0, Index, Item, NewItem, NewItem):- !.
update_array_item(N, Index, Array, NewItem, NewArray):-
    N1 is N - 2,
    Subindex is Index >> N1 /\ 3,
    update_subarray(Subindex, Array, Array1, NewArray1, NewArray),
    update_array_item(N1, Index, Array1, NewItem, NewArray1).


update_subarray(I, '$', X, X1, Array):- !,
    update_subarray(I, $($,$,$,$), X, X1, Array).
update_subarray(0, $(W,X,Y,Z), W, W1, $(W1,X,Y,Z)).
update_subarray(1, $(W,X,Y,Z), X, X1, $(W,X1,Y,Z)).
update_subarray(2, $(W,X,Y,Z), Y, Y1, $(W,X,Y1,Z)).
update_subarray(3, $(W,X,Y,Z), Z, Z1, $(W,X,Y,Z1)).


append([], L, L).
append([X|L1],L2,[X|L3]):- append(L1,L2,L3).


%procedure(query,1,normal,[4 - [query([# 1,# 2,# 3,# 4]),and_begin,true,density(# 1,# 2),density(# 3,# 4),and_end,# 2 > # 4,20 * # 2 < 21 * # 4]]).


procedure(query,1,normal,[4 - [query([# 1,# 2,# 3,# 4]),density(# 1,# 2),density(# 3,# 4),# 2 > # 4,20 * # 2 < 21 * # 4]]).

procedure(density,2,normal,[4 - [density(# 1,# 2),pop(# 1,# 3),area(# 1,# 4),# 2 is # 3 * 100 / # 4]]).


procedure(pop,2, orpar ,[0 - [pop(china,8250)],0 - [pop(india,5863)],0 - [pop(ussr,2521)],0 - [pop(usa,2119)],0 - [pop(indonesia,1276)],0 - [pop(japan,1097)],0 - [pop(brazil,1042)],0 - [pop(bangladesh,750)],0 - [pop(w_germany,620)],0 - [pop(nigeria,613)],0 - [pop(mexico,581)],0 - [pop(uk,559)],0 - [pop(italy,554)],0 - [pop(france,525)],0 - [pop(philippines,415)],0 - [pop(thailand,410)],0 - [pop(turkey,383)],0 - [pop(egypt,364)],0 - [pop(spain,352)],0 - [pop(poland,337)],0 - [pop(s_korea,335)],0 - [pop(iran,320)],0 - [pop(ethiopia,272)],0 - [pop(argentina,251)]]).

procedure(area,2, orpar ,[0 - [area(china,3380)],0 - [area(india,1139)],0 - [area(ussr,8708)],0 - [area(usa,3609)],0 - [area(indonesia,570)],0 - [area(japan,148)],0 - [area(brazil,3288)],0 - [area(bangladesh,55)],0 - [area(parkistan,311)],0 - [area(w_germany,96)],0 - [area(nigeria,373)],0 - [area(mexico,764)],0 - [area(uk,86)],0 - [area(italy,116)],0 - [area(france,213)],0 - [area(philippines,90)],0 - [area(thailand,200)],0 - [area(turkey,296)],0 - [area(egypt,386)],0 - [area(spain,190)],0 - [area(poland,121)],0 - [area(s_korea,37)],0 - [area(iran,628)], 0 - [area(ethiopia,350)],0 - [area(argentina,1080)]]).

/* reduced database
procedure(pop,2, orpar ,[0 - [pop(china,8250)],0 - [pop(india,5863)],0 - [pop(ussr,2521)],0 - [pop(usa,2119)],0 - [pop(indonesia,1276)],0 - [pop(japan,1097)],0 - [pop(brazil,1042)],0 - [pop(bangladesh,750)],0 - [pop(w_germany,620)],0 - [pop(nigeria,613)],0 - [pop(mexico,581)],0 - [pop(uk,559)],0 - [pop(italy,554)],0 - [pop(france,525)],0 - [pop(philippines,415)],0 - [pop(thailand,410)],0 - [pop(turkey,383)],0 - [pop(egypt,364)],0 - [pop(spain,352)]]).

procedure(area,2, orpar ,[0 - [area(china,3380)],0 - [area(india,1139)],0 - [area(ussr,8708)],0 - [area(usa,3609)],0 - [area(indonesia,570)],0 - [area(japan,148)],0 - [area(brazil,3288)],0 - [area(bangladesh,55)],0 - [area(parkistan,311)],0 - [area(w_germany,96)],0 - [area(nigeria,373)],0 - [area(mexico,764)],0 - [area(uk,86)],0 - [area(italy,116)],0 - [area(france,213)],0 - [area(philippines,90)],0 - [area(thailand,200)],0 - [area(turkey,296)],0 - [area(egypt,386)],0 - [area(spain,190)]]).
*/
procedure(and_begin, 0, and_begin, and_begin).

procedure(and_end, 0, and_end, and_end).

procedure(put, 1, buildin, buildin).

procedure(write, 1, buildin, buildin).

procedure(display, 1, buildin, buildin).

procedure(nl, 0, buildin, buildin).

procedure(!, 0, cut, [!]).

procedure(=\=, 2, buildin, buildin).

procedure(=:=, 2, buildin, buildin).

procedure(>, 2, buildin, buildin).

procedure(>=, 2, buildin, buildin).

procedure(<, 2, buildin, buildin).

procedure(=<, 2, buildin, buildin).

procedure(integer, 1, buildin, buildin).

procedure(number, 1, buildin, buildin).

procedure(atom, 1, buildin, buildin).

procedure(atomic, 1, buildin, buildin).

procedure(var, 1, buildin, buildin).

procedure(nonvar, 1, buildin, buildin).

procedure(fail, 0, buildin, buildin).

procedure(false, 0, buildin, buildin).

procedure(true, 0, buildin, buildin).

procedure(=, 2, buildin, buildin).

procedure(\=, 2, buildin, buildin).

procedure(is, 2, buildin, buildin).

procedure(functor, 3, buildin, buildin).

procedure(arg, 3, buildin, buildin).

procedure(numbervars, 3, buildin, buildin).

procedure(\==, 2, buildin, buildin).

procedure(==, 2, buildin, buildin).

procedure(=.., 2, buildin, buildin).

procedure(seen, 0, buildin, buildin).

procedure(told, 0, buildin, buildin).

procedure(see, 1, buildin, buildin).

procedure(tell, 1, buildin, buildin).

procedure(read, 1, buildin, buildin).

procedure(call, 1, call, call).

procedure(#(_), 1, call, call).



