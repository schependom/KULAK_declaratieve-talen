/*
    OEFENING 1
*/

peano_plus(nul, X, X).                              % X plus nul is X
peano_plus(s(X), Y, s(Z)) :- peano_plus(X, Y, Z).   % (X+1) + Y = SOM + 1 als X + Y = SOM

%peano_min(X, nul, X).                              % X min nul is X
%peano_min(X, s(Y), Z) :- peano_min(X, Y, s(Z)).

peano_min(X, nul, X).
peano_min(s(X), s(Y), Z) :- peano_min(X, Y, Z).     % (X+1) - (Y+1) = X - Y 

% Geen negatieve waarden!!
peano_groter_dan(s(_), nul).    % Variabele geen naam geven want niet gebruikt! Als hij in het hoofd voorkomt, moet hij ook in de body voorkomen
peano_groter_dan(s(X), s(Y)) :- peano_groter_dan(X, Y).

peano_maximum(X, nul, X).
peano_maximum(nul, X, X).
peano_maximum(s(X), s(Y), s(Z)) :- peano_maximum(X, Y, Z).

/*
    OEFENING 2
*/

% nil stelt de lege boom voor.
% een knoop stellen we voor door node(Ldb, Value, Rdb).
diepte(nil, 0).                 % Diepte van de lege boom is nul.
diepte(node(L, _, R), D) :-     % Niet-lege boom met LDB L, RDB R en waarde die er niet toe doet.
    diepte(L, D1),              % Hoogte van LDB in D1
    diepte(R, D2),              % Hoogte van RDB in D2
    D is max(D1, D2) + 1.

/*
    OEFENING 3 en 4
*/

% Base case: Evaluating a number just returns the number itself.
eval(number(N), N) :- number(N).

% Evaluating arithmetic expressions

eval(plus(A, B), Result) :-
    eval(A, Av),
    eval(B, Bv),
    Result is Av + Bv.

eval(min(A, B), Result) :-
    eval(A, Av),
    eval(B, Bv),
    Result is Av - Bv.

eval(neg(A), Result) :-
    eval(A, Av),
    Result is -Av.

% Evaluating boolean expressions

% Equality
eval(=(A, B), tru) :-
    eval(A, Av),
    eval(B, Bv),
    Av = Bv.
eval(=(A, B), fal) :-
    eval(A, Av),
    eval(B, Bv),
    Av \= Bv.

% AND operator
eval(and(A, B), tru) :-
    eval(A, tru),
    eval(B, tru).
eval(and(_, _), fal).

% OR operator
eval(or(A, _), tru) :-
    eval(A, tru).
eval(or(_, B), tru) :-
    eval(B, tru).
eval(or(_, _), fal).

% NOT operator
eval(not(A), tru) :-
    eval(A, fal).
eval(not(A), fal) :-
    eval(A, tru).

/*
    number(5)   => 5
    plus(A,B)   => A + B
    minus(A,B)  => A - B
    neg(A)      => -A
    eval(number(5), X) => X = 5
    eval(plus(number(5), number(3)), X) => X = 8
*/


/*
    COMMANDS

    ?- eval(and(or(not(tru),tru),fal),X).
            X = fal.

    ?- eval(plus(number(3),number(4)),X).
            X = 7.

    ?- eval(and(=(plus(number(3),number(4)),number(5)),not(or(fal,fal))),X).
            X = fal.