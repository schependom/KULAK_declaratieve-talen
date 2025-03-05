/*
    ONE-PASS TRANSLATION
*/

/*

    ?- vertaal([def(a), gebr(a), gebr(b), gebr(c), def(c), def(b)], L).
        L = [stel(a,1), gebr(1), gebr(3), gebr(2), stel(c, 2), stel(b, 3)].

    TIP2

    ?- member(1-X, [_ | _]).
        true.

    TIP3

    ?- L=[_ | _],member(1-X,L),X=a.
        L = [1-a|_],
        X = a ;
        L = [_, 1-a|_],
        X = a ;
        L = [_, _, 1-a|_],
        X = a ;
        L = [_, _, _, 1-a|_],
        X = a ;
        L = [_, _, _, _, 1-a|_],

    ?- L=[_ | _],member(1-X,L).
        L = [1-X|_] ;
        L = [_, 1-X|_] .
*/

% Overload
vertaal(Invoer, L) :-
    vertaal(Invoer, L, 1, [_ | _]).

vertaal([], _, _, _).

% gebr(x) na def(x)
%   => index zit al in de symbooltabel
%   => voeg gebr(x) toe aan de uitvoer
vertaal([gebr(Head) | Tail], [gebr(Index) | L], Counter, SymboolTabel) :-
    member(Index-Head, SymboolTabel), !,            % uitroepteken zorgt ervoor dat we niet naar het choice point gaan voor vertaal/3
    vertaal(Tail, L, Counter, SymboolTabel).        %   --> de keuze voor dit hoofd ligt nu vast en we gaan niet daar de regel hieronder.

% gebr(x) vóór def(x)
vertaal([gebr(Head) | Tail], [ gebr(TBD_Index) | L ], Counter, SymboolTabel) :-
    \+ member(Counter-Head, SymboolTabel), !,       % check of deze index nog niet in de symbooltabel zit
    member(TBD_Index-Head, SymboolTabel), !,        % voeg _INTERNE_VARIABELE-x toe aan de symbooltabel
    vertaal(Tail, L, Counter, SymboolTabel).     %   => deze variabele wordt later ge-unificeerd met N wanneer def(x) wordt gezien

% def() geeft geen probleem.
vertaal([def(Head) | Tail], [stel(Head, Counter) | L], Counter, SymboolTabel) :-
    member(Counter-Head, SymboolTabel), !,          % voeg een entry toe aan de symbooltabel
    succ(Counter, NewCounter),                      % NewCounter is Counter + 1
    vertaal(Tail, L, NewCounter, SymboolTabel).

