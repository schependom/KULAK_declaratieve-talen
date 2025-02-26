/*
    1. LIJSTEN
*/

%%%%
% Gewone recursie

listlength([], 0).
listlength([_|T], Length) :-
    listlength(T, LengthTail),
    Length is LengthTail + 1.

%%%%
% Staartrecursie

% listlengthAcc/2 -> initialiseer accumulator van listlengthAcc/3 op nul
listlengthAcc(List, Length) :- listlengthAcc(List, 0, Length). 

listlengthAcc([_|T], Acc, Length) :-
    NewAcc is Acc + 1,
    listlengthAcc(T, NewAcc, Length).
listlengthAcc([], Acc, Acc).            % resultaat pas helemaal op het eind berekend

% Laatst/2 geeft het laatste element van een lijst terug
laatst([X], X).
laatst([_|T], Laatst) :- laatst(T, Laatst).

/*  VB.
    ?- laatst([1,2,3],X).
        X = 3 ;
*/

%%%%
% Een predicaat opeenvolgend/3 dat nagaat of een gegeven element onmiddellijk op een ander element volgt
% in een gegeven lijst.

% Basisgeval
opeenvolgend(X, Y, [X, Y | _ ] ).                           % X komt voor Y als lijst = [X, Y, ...].
% Alternatief
opeenvolgend(X, Y, [X | [Y | _]] ).
% Recursief geval
opeenvolgend(X, Y, [_ | T]) :- opeenvolgend(X, Y, T).       % Anders checken we de staart.

/*  VB.
    ?- opeenvolgend(3,6,[1,2,3,6]).
        true.

    ?- opeenvolgend(2,4,[1,2,3,4,5]).
        false.

    ?- opeenvolgend(2,3,[1,2,3,4,5]).
        true ;
*/

%%%%
% Een predicaat vectorsom/3 dat nagaat of een lijst de som van de elementen bevat van twee andere lijsten.
% Veronderstel dat alle lijsten even lang zijn. Wat gebeurt er als alle lijsten leeg zijn? Wat gebeurt er als de
% lijsten oneindig zijn?

vectorsom([],[],[]).
%vectorsom([X], [Y], [Z]) :- Z is X + Y.
vectorsom([H1 | T1], [H2 | T2], [H3 | T3]) :-
    H3 is H1 + H2,
    vectorsom(T1, T2, T3).

/* VB.
    ?- vectorsom([1,2,3],[3,2,1],[4,4,4]).
        true.

    ?- vectorsom([1,2,3],[1,2,3],[4,4,4]).
        false.

    ?- vectorsom([1,2,3],[3,2,1],X).
        X = [4, 4, 4].

    ?- vectorsom([1,2,3],X,[4,4,4]).
        ! kan niet
        De reden is dat hij het volgende doet:
            5 is 1 + _24326
        En de interne variabele is niet geïnstantieerd, dus die kan niet geëvalueerd worden.

*/

%%%%
% een predicaat zoekwaarde/3 dat de waarde teruggeeft die overeenkomt met de waarde in een lijst pair(key,value) waarden

/* VB.
    ?- zoekwaarde( [ pair(1,a), pair(2,b), pair(4,d), pair(3,c) ], 2, V )
        V = b.
*/

% Basisgeval -> head bevat de gezochte key
zoekwaarde( [ pair(Key, Value) | _ ], Key, Value ).                             % Tail doet er niet toe
zoekwaarde( [ _ | Tail ], Key, Value ) :- zoekwaarde( Tail, Key, Value ).       % Head doet er niet toe

/* Het predicaat \+ gaat na of iets niet kan bewezen worden.

    ?- \+ member(4, [1, 2, 3]).
        true.

    ?- \+ member(X, [1, 2, 3]).
        false.

    Verschil tussen onderstaande queries?

    ?- X=3, \+ member(X, [1,2]).
        X = 3.
    ?- \+ member(X, [1,2]), X=3.
        false.
*/



/*
    5. PRIEMGETALLEN
    Getal per getal de veelvouden verwijderen
*/