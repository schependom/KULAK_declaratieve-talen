/*
    1. ONE-PASS TRANSLATION (werkt nu, bedankt.)
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


/*
    2. VAKANTIELICHT
*/

/*
    - N plaatsen (1, ..., N)
    - M verbindingen = snelwegen (1, ..., M) met elk een kleur
    - Elke snelweg 1 keer gebruiken
    - Vertrekken bij plaats 1
    - Terugkeren bij plaats 1
    - Nooit 2 keer dezelfde kleur na elkaar

    Vooraf, check:
        1) Knopen hebben even aantal verbindingen
        2) Knoop heeft X verb met kleur => minstens X verb met andere kleur
*/

punt(X) :- snelweg(_, X, _). % voorkom oneindige lussen met de cut (!)
punt(X) :- snelweg(X, _, _), !.

% Basisgeval
count_connections_from_list([], _, Acc, Acc).
% Recursief geval: voeg nog niet geteld punt toe aan Geteld
count_connections_from_list([Head | Tail], Geteld, Acc, Result) :-
    \+ member(Head, Geteld),
    succ(Acc, NewAcc),
    count_connections_from_list(Tail, [Head | Geteld], NewAcc, Result).

% Tel aantal verbonden punten met bepaalde kleur
count_connections_with_color(Punt, Result, Kleur) :-
    findall(VerbPunt, ( snelweg(Punt,VerbPunt,Kleur) ; snelweg(VerbPunt,Punt,Kleur) ), Lijst),
    count_connections_from_list(Lijst, [], 0, Result).

% Kleur doet er niet toe
count_connections(P, R) :- count_connections_with_color(P, R, _).

% Check 1: even aantal verbonden knopen
check1 :-
    punt(X),
    count_connections(X, Result),
    Result mod 2 =\= 0, !.          % als voldaan -> \+check1 is false -> stop met zoeken

check2 :-
    punt(X),
    snelweg(X, _, Kleur),
    snelweg(X, _, AndereKleur),
    Kleur \== AndereKleur,
    count_connections_with_color(X, Result, Kleur),
    count_connections_with_color(X, AnderResult, AndereKleur),
    AnderResult < Result.

check2 :-
    punt(X),
    snelweg(_, X, Kleur),
    snelweg(_, X, AndereKleur),
    Kleur \== AndereKleur,
    count_connections_with_color(X, Result, Kleur),
    count_connections_with_color(X, AnderResult, AndereKleur),
    AnderResult < Result.

check :-
    \+ check1,     % er bestaat geen knoop die een oneven aantal verbonden knopen heeft
    \+ check2.     % er bestaat geen knoop die meer verbindingen heeft met een kleur dan met de andere kleur

/*
    snelweg(1, 2, geel).
    snelweg(2, 3, blauw).
    snelweg(1, 3, geel).
    snelweg(1, 5, rood).        % -> 1 is oneven

    NIET IN ORDE

    ?- \+ check1.
        false.

    snelweg(1, 2, geel).
    snelweg(2, 3, blauw).
    snelweg(1, 3, geel).

    WEL IN ORDE

    ?- \+ check1.
        true.

    snelweg(1, 2, geel).
    snelweg(1, 3, geel).
    snelweg(1, 4, blauw).

    ?- count_connections_with_color(1,Result,rood).
        Result = 0.

    ?- count_connections_with_color(1,Result,geel).
        Result = 2.

    ?- count_connections_with_color(1,Result,blauw).
        Result = 1.

    ?- \+ check2.
        false.

*/

snelweg(1, 2, geel).
snelweg(1, 3, geel).
snelweg(2, 4, blauw).
snelweg(3, 4, groen).

% Resultaat is een pad vanaf punt 1 en
% is van de vorm [2-yellow, 3-blue, 1-yellow]

tour(Resultaat) :-
    check,
    % TODO

snelweg(1, 2, c).
snelweg(2, 3, a).
snelweg(1, 3, b).
snelweg(3, 5, a).
snelweg(3, 4, c).
snelweg(5, 4, d).


% Geoverload predicaat
% toer(Bezocht, LaatsteKleur, )