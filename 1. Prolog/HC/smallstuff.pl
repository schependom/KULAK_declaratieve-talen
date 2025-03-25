max(X, Y, Max) :- X >= Y, !, Max = X.
max(_, Y, Y).

/*
    ?- max(1,2,X).
        X = 2.

    ?- max(-10,10,5).
        false.

    ?- max(-10,10,10).
        true.

    ?- max(10,-10,5).
        false.
*/