%% Simple naive reverse

goal :- nrev([a,b,c], L).

app([], Z, Z).
app([U|X], Y, [U|Z]) :-
        app(X, Y, Z).

nrev([], []).
nrev([U|X], Z) :-
        nrev(X, Y),
        app(Y, [U], Z).
