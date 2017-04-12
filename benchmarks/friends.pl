goal :- friendship(john, amanda).

friends(john,jake).
friends(mike,hans).
friends(hans,robert).
friends(robert,angela).

mutual_friendship(X,Y):-
    friends(X,Y);
    friends(Y,X).

friendship(X,Y):-
    mutual_friendship(X,Y),!;
    mutual_friendship(Y,Z), friendship(Z,X).
