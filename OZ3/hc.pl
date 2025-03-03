append([], L, L).
append([H | Rest], L2, [H | RestPlusL2]) :-
    append(Rest, L2, RestPlusL2).

/*
    ?- append(A, [b], [1,b]).
        A = [1].

    ?- append([X], [Y], [a|Z]).
        X = a, Y = Z.

    ?- append(A, B, [a,b,c]).
        A = [],         B = [a, b, c]   ;
        A = [a],        B = [b, c]      ;
        A = [a, b],     B = [c]         ;
        A = [a, b, c],  B = []          ;
*/

member(X, [X | _]).
member(X, [_ | R]) :- member(X, R).

delete(H, [H | Resultaat], Resultaat).          % basis: delete head
delete(X, [H | Tail], [H | NieuweTail]) :-      % nieuwe tail is oude min X
    delete(X, Tail, NieuweTail).

/*
    ?- delete(4, [2,4,8], L).
        L = [2, 8].

    ?- delete(4, L, [2,3]).
        L = [4, 2, 3] ;
        L = [2, 4, 3] ;
        L = [2, 3, 4] ;
*/

% Naïeve sort

permuteSort(Lijst, Gesorteerd) :-
    permute(Lijst, Gesorteerd),
    sorted(Gesorteerd).

% Basisgeval
permute([], []).

% Recursie:
%   -> Uit welke lijsten moeten we de head verwijderen om de gepermuteerde tail te verkrijgen?
permute([H | Tail], Result) :-
    permute(Tail, PermutedTail),
    delete(H, Result, PermutedTail).

% Basisgevallen

% BG1: hier kom je nooit in door recursie op sorted alleen
sorted([]).
% BG2: dit is het effectieve basisgeval van de recursie van sorted alleen
sorted([_]).
sorted([El1, El2 | Rest]) :-
    El1 < El2, 
    sorted([El2 | Rest]).

/*
    QUICKSORT
*/

quickSort([], []).
quickSort([Spil | Rest], Resultaat) :-
    partition(Spil, Rest, Kleiner, Groter),
    quickSort(Kleiner, KleinerSorted),
    quickSort(Groter, GroterSorted),
    append(KleinerSorted, [Spil | GroterSorted], Resultaat).

% partition/4 met argumenten Spil, Input (zonder Spil!), Kleiner, Groter
partition(_Spil, [], [], []).
partition(Spil, [H | T], Kleiner, Groter) :-
    (
    H < Spil ->                                 % Als... dan...
        Kleiner = [H | RestKleiner],
        partition(Spil, T, RestKleiner, Groter)
    ;
        Groter = [H | RestGroter],              % Anders
        partition(Spil, T, Kleiner, RestGroter)
    ).

/*
    ?- partition(3,[1,2,3,4,5],Groter,Kleiner).
        Groter = [1, 2],
        Kleiner = [3, 4, 5] .

    ?- quickSort([5,3,2,9,10,-2],X).
        X = [-2, 2, 3, 5, 9, 10] .
*/