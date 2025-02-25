% De linkerlijst bevat alleen a's en de rechterlijst alleen b's.
% Het aantal a's en b's is gelijk.

a2b([],[]).
a2b([a|Ta],[b|Tb]) :- a2b(Ta,Tb).

/*

    SIMPELE CHECK

    ?- a2b([a,a,a],[b,b,b]).
        true.
    ?- a2b([a,a,a],[b,b,b,b]).
        false.

    'TRANSLATOR'

    ?- a2b([a,a,a],X).
        X = [b,b,b].

    ?- a2b(X,[b,b,b]).
        X = [a,a,a].

*/

/*
    NIET tail recursive versie van lenList/2 
        ->  berekent de lengte van een lijst in zijn eerste argument en geeft die terug in zijn tweede argument
        ->  bepaalde doelen op een bepaald recursieniveau moeten wachten op het antwoord op een lager recursieniveau
            vóórdat ze kunnen geëvalueerd worden!
*/

% Basisgeval
lenList([], 0).

% Recursief geval
lenList([_|T], Length) :-
    lenList(T, TailLength),
    Length is TailLength + 1.   % evaluatie moet wachten op completie van lager recursieniveau!!

/*
    TAIL RECURSIVE VERSIE
*/

% Dit predicaat lenList/2 met ariteit 2 roept het tail-recursive predicaat lenAcc/3 met ariteit 3 aan.
lenAcc(List, Len) :-
    lenAcc(List, 0, Len).  % initialiseer de accumulator op 0.

% Recursief geval
lenAcc([_|T], Acc, Len) :- % head doet er niet toe
    NewAcc is Acc + 1,      % is -> evaluatie!
    lenAcc(T, NewAcc, Len).

% Basisgeval
lenAcc([], Acc, Acc).

/*
    Tail recursive manier om het maximum te vinden in een lijst van 
    integers. Merk op dat de integers negatief kunnen zijn, dus dat we
    in het ge-overloadede maxList/2 predicaat de accumulator niet mogen
    initialiseren met 0, want dan zouden we nooit een negatief getal
    kunnen vinden. We initialiseren hem dus met de eerste waarde in de lijst.
*/

maxList(List, Max) :-
    [H|_] = List,
    maxList(List, H, Max). % accumulator geïnitialiseerd met eerste waarde in lijst

% Recursieve regel 1
%   Head is strikt groter dan Acc (= huidige maximum van de overlopen heads)
maxList([H|T], Acc, M) :-
    H > Acc,
    maxList(T, H, M).

% Recursieve regel 2
%   Head is kleiner dan of gelijk aan Acc
maxList([H|T], Acc, M) :-
    H =< Acc,               % opgelet met =< en >= !
    maxList(T, Acc, M).

% Basisgeval op het einde
maxList([],M,M).

/*
    ?- lenList([-8,3,4,0,-10,20],X).
        X = 6.

    ?- lenAcc([-8,3,4,0,-10,20],X).
        X = 6.

    ?- maxList([-8,3,4,0,-10,20],X).
        X = 20.
*/