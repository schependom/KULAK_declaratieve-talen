/*
    1. PEANO
*/


%%%%
% Peano plus

% Basisgeval
peano_plus(nul, X, X).                              % X plus nul is X
% Recursief geval
peano_plus(s(X), Y, s(Z)) :- peano_plus(X, Y, Z).   % (X+1) + Y = SOM + 1 als X + Y = SOM


%%%%
% Peano min

peano_min(X, nul, X).
peano_min(s(X), s(Y), Z) :- peano_min(X, Y, Z).     % (X+1) - (Y+1) = X - Y 

peano_min_bis(X, Y, Z) :- peano_plus(Y, Z, X).      % Y + Z = X => X - Y = Z

/*
    ?- peano_min(s(s(zero)),s(s(s(zero))),Z).       % 2 - 3 = Z <=> Z = -1 = niet voor te stellen met s(), vertrekkend vanaf nul!
        false.

    ?- peano_min(s(s(s(zero))),Y,s(s(zero))).       % 3 - Y = 2 <=> Y = 1 = s(nul).
        Y = s(nul).

    ?- peano_min(A, B, C).
        A = C,          B = nul         ;
        A = s(C),       B = s(nul)      ;
        A = s(s(C)),    B = s(s(nul))   ;
        ...    
*/


%%%%
% Groter dan

% Geen negatieve waarden!!
peano_groter_dan(s(_), nul).                                % Variabele geen naam geven want niet gebruikt! Als hij in het hoofd voorkomt, moet hij ook in de body voorkomen.
peano_groter_dan(s(X), s(Y)) :- peano_groter_dan(X, Y).

/*
    ?- peano_groter_dan( s(s(s(nul))), Y ).                % 3 > Y
        Y = nul         ;
        Y = s(nul)      ;
        Y = s(s(nul))   .
    
    ?- peano_groter_dan(X,nul).
        X = s(_).

    ?- peano_groter_dan(X,X).
        ! error -> stack limit
        ! geen OCCURS CHECK in Prolog !
        Oplossing? Zie hieronder!

    ?- peano_groter_dan_occurs(X,X).
        false.
*/

peano_groter_dan_occurs(s(_), nul).         % Basisgeval: zelfde als hiervoor
peano_groter_dan_occurs(s(X), s(Y)) :-
    X \== Y,                                % ! Expliciete OCCURS CHECK!
    peano_groter_dan_occurs(X, Y).          %   ... geconjugeerd met vorige body (zonder occurs)


%%%%
% Maximum

peano_maximum(X, nul, X).                                   % X = max(X,0)
peano_maximum(nul, X, X).                                   % X = max(0,X)
peano_maximum(s(X), s(Y), s(Z)) :- peano_maximum(X, Y, Z).  % naar beneden op de recursieladder

maximum(X,Y,X) :- peano_groter_dan_occurs(X,Y).             % X > Y => max(X,Y)=X
maximum(X,Y,Y) :- peano_groter_dan_occurs(Y,X).             % Y > X => max(X,Y)=Y
maximum(X,X,X).                                             % X = X => max(X,X)=X

/*
    ?- peano_maximum(s(s(s(nul))),s(nul),X).
        X = s(s(s(nul))).

    ?- maximum(s(s(s(nul))),s(nul),X).
        X = s(s(s(nul))).

    ?- maximum(nul, X, s(s(nul))).
        X = s(s(nul)) .

    ?- peano_maximum(nul, X, s(s(nul))).
        X = s(s(nul)).
*/


%%%%
% Deling
% (X/Y = D) -> "Hoeveel keer kan je Y aftrekken van X om R te bekomen?" -> 'teller' D bijhouden

peano_div(X, Y, s(D), R) :-         % X / Y = (D+1) + R         => verhoogde 'teller' D
	peano_plus(Z,Y,X),              % Z + Y = X                 => Z = X - Y
	peano_div(Z,Y,D,R).             % Recursieve oproep         => Z/Y = D + R (verlaagde 'teller' D)

peano_div(s(R), Y, zero, s(R)) :-
	groter_dan(Y, s(R)).            % Y > (R+1) => (R+1)/Y = R+1

peano_div(X, X, s(zero), zero).     % X/X = 1 + 0


/*
    2. DIEPTE
*/

% Een lege boom stellen we voor door    nil.
% Een knoop stellen we voor door        node(Ldb, Value, Rdb).

diepte(nil, 0).                 % Diepte van de lege boom is nul.
diepte(node(L, _, R), D) :-     % Niet-lege boom met LDB L, RDB R en waarde die er niet toe doet.
    diepte(L, D1),              % Hoogte van LDB = D1
    diepte(R, D2),              % Hoogte van RDB = D2
    D is max(D1, D2) + 1.       % Neem het maximum en tel er 1 bij op.

/*
    ?- diepte(node(node(nil, 2, nil), 1, nil), D).
        D = 2.

    ?- diepte(node(nil, 1, node(node(nil, 3, nil), 2, node(nil, 1, nil))), D).
        D = 3.
*/


/*
    3. BOOLEAANSE UITDRUKKINGEN
*/

eval(tru, tru).     % zodat ?- eval(tru, X) => X = tru.
eval(fal, fal).     % zodat ?- eval(fal, X) => X = fal.

% AND operator
eval(and(A, B), tru) :-
    eval(A, tru),
    eval(B, tru), !.
eval(and(_, _), fal).

% OR operator
eval(or(A, _), tru) :-
    eval(A, tru), !.
eval(or(_, B), tru) :-
    eval(B, tru), !.
eval(or(_, _), fal).

% NOT operator
eval(not(A), tru) :-
    eval(A, fal).
eval(not(A), fal) :-
    eval(A, tru).


/*
    4. CALCULATOR
    
    number(5)                           => 5
    plus(A,B)                           => A + B
    minus(A,B)                          => A - B
    neg(A)                              => -A
    eval(number(5), X)                  => X = 5
    eval(plus(number(5), number(3)), X) => X = 8
*/

eval(number(N), N).

eval(plus(A, B), Result) :-
    eval(A, AR),
    eval(B, BR),
    Result is AR + BR.

eval(min(A, B), Result) :-
    eval(A, AR),
    eval(B, BR),
    Result is AR - BR.

eval(neg(A), Result) :-
    eval(A, AR),
    Result is -AR.

eval(=(A, B), tru) :-
    eval(A, Res),
    eval(B, Res).

eval(=(A, B), fal) :-
    eval(A, AR),
    eval(B, BR),
    AR \== BR.

/*
    ?- eval(and(or(not(tru),tru),fal),X).
            X = fal.

    ?- eval(plus(number(3),number(4)),X).
            X = 7.

    ?- eval(and(=(plus(number(3),number(4)),number(5)),not(or(fal,fal))),X).
            X = fal.
*/