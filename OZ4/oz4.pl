/*
    1. ONE-PASS TRANSLATION
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
vertaal(Invoer, Resultaat) :-
    vertaal(Invoer, Resultaat, 1, [_ | _]). % initialiseer symbooltabel op [_ | _] -> onvolledige lijst

% vertaal(Invoer, Resultaat, Counter, SymboolTabel)
vertaal([], [], _, _).

% def() geeft geen probleem.
vertaal([def(Head) | Tail], [stel(Head, Counter) | RestResultaat], Counter, SymboolTabel) :-
    member(Counter-Head, SymboolTabel), !,          % voeg een entry toe aan de symbooltabel
    succ(Counter, NewCounter),                      % NewCounter is Counter + 1
    vertaal(Tail, RestResultaat, NewCounter, SymboolTabel).

% Bij gebr(X) 2 gevallen:
%   -> gebr(X) kan NA def(X) komen
%       =>  In dit geval zit CounterX-X al in symbooltabel
%       =>  Voeg gebr(CounterX) toe aan output.
%   -> gebr(x) kan VÓÓR def(x) komen
%       =>  In dit geval zit X nog niet in de symbooltabel
%       =>  Zet TijdelijkeCounter-X in symbooltabel
%       =>  TijdelijkeCounter is een _INTERNE variabele die later zal geünificeerd worden
%           met de effectieve Counter op het moment dat def(X) wordt gezien
vertaal([gebr(X) | Tail], [gebr(Index) | RestResultaat], Counter, SymboolTabel) :-
    member(Index-X, SymboolTabel), !, 
    vertaal(Tail, RestResultaat, Counter, SymboolTabel).

% member(Index-X,SymboolTabel),! slaagt altijd juist 1 x.
%        en Index-X unificeert met een term die voorkomt in de SymboolTabel

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

snelweg(1,2,c).
snelweg(2,3,a).
snelweg(1,3,b).
snelweg(3,5,a).
snelweg(3,4,c).
snelweg(5,4,d).
snelweg(5,3,d).

punt(X) :- snelweg(_, X, _).
punt(X) :- snelweg(X, _, _).

weg(X, Y-Kleur) :-
    snelweg(X, Y, Kleur); snelweg(Y, X, Kleur).       % als voorgaande regel n.v.t. is.

check :-
    punt(X),                        % Leg een (keuze)punt vast -> mogelijkheid tot backtracken voorzien.
    \+ oneven(punt(X)),             % Als die knoop een oneven aantal verbonden knopen heeft, faalt oneven/0.
    \+ nietInBalans(punt(X)), !.    % ALs die knoop x wegen heeft met een bepaalde kleur en minder dan x wegen met een andere kleur, faalt nietInBalans/0.

% Bepaal het aantal verbonden knopen = graad.
graad(punt(X), Graad) :-
    findall(Y, weg(X, Y-_), VerbondenKnopen),
    length(VerbondenKnopen, Graad).

% punt(X) heeft een oneven graad.
oneven(punt(X)) :-
    graad(X, Graad),
    Graad mod 2 =:= 1, !.

% Niet in balans    <=>     punt(X) heeft meer verbonden knopen via weg met bepaalde kleur dan via wegen met andere kleuren
%                   <=>     graad(X) >= 2 * AantalVerbMetKleur !!
nietInBalans(punt(X)) :-
    weg(X, _-Kleur),                                                        % Leg een kleur vast -> mogelijkheid tot backtracking
    findall(VerbKnoop, weg(X, VerbKnoop-Kleur), VerbondenKnopenMetKleur),   % Zoek alle knopen die verb zijn met Kleur.
    length(VerbondenKnopenMetKleur, AantalVerbMetKleur),                    % Bepaal het aantal van deze verb. knopen
    AantalMaal2 is AantalVerbMetKleur * 2,                                  % Equivalente voorwaarde
    graad(X, GraadX),
    AantalMaal2 =< GraadX, !.                                               % Equivalente voorwaarde

% Overloading
tour([Stad-Kleur | Tour]) :- 
    check,
    % Alle wegen die vanuit stad 1 vertrekken
    findall(Stad-Kleur, weg(1,Stad-Kleur), WegenVanuit1),
    length(WegenVanuit1, AantalWegenVanuit1),
    succ(AantalWegenVanuit1Min1, AantalWegenVanuit1),    % minus 1
    % Sorteer de wegen, waarbij de stad met het laagste nummer eerst komt.
    sort(WegenVanuit1, SortedWegenVanuit1),
    member(Stad-Kleur, SortedWegenVanuit1),
    % tour(Resterend, Gebruikt, Vervolg)
    tour(AantalWegenVanuit1Min1, [1-Stad, Stad-1], [Stad-Kleur | Tour]), !.

% tour(Resterend, Gebruikt, Vervolg)
% Resterend = aantal nog beschikbare wegen vanuit 1
tour(Resterend, Gebruikt, [LaatsteStad-LaatsteKleur, Stad-Kleur | Tour]) :-
    % Vind alle wegen vanuit X
    findall(Stad-Kleur, (               % voorwaarden findall
        weg(LaatsteStad, Stad-Kleur),
        (Resterend > 1 ; Stad \= 1),     % niet te vroeg stoppen
        Kleur \= LaatsteKleur,           % andere kleur dan waarmee werd toegekomen
        \+ member(LaatsteStad-Kleur,Gebruikt)  
    ), Routes),
    sort(Routes, SortedRoutes),
    member(Stad-Kleur, SortedRoutes),
    (
        Stad == 1 ->                            % Mogelijks meerdere keren door stad 1 !!
            succ(ResterendMin1, Resterend),     % -> verlaag Resterend
            tour(ResterendMin1, [LaatsteStad-Stad, Stad-LaatsteStad | Gebruikt], [Stad-Kleur | Tour])  
        ;
            tour(Resterend, [LaatsteStad-Stad, Stad-LaatsteStad | Gebruikt], [Stad-Kleur | Tour])  
    ), !. % 1 tour is genoeg.

% We moeten nog terugkeren naar stad 1.
% Resterend is nu gelijk aan 1.
tour(1, Gebruikt, [LaatsteStad-LaatsteKleur, 1-Kleur]) :-
    findall(1-Kleur, (
        weg(LaatsteStad, 1-Kleur),          % leg een stad en een kleur vast
        \+ member(LaatsteStad-1, Gebruikt), % pad mag nog niet gebruikt zijn
        Kleur \= LaatsteKleur               % kleuren moeten verschillen
    ), [1-Kleur]), !.                       % 1 element in lijst (zie hieronder) !!

/*
    MERK OP:
        findall kan zijn resultaat (lijst) unificeren met een single-element
        lijst als er ook effectief maar 1 element in het resultaat zit.

    VB:

    weg(test, 1-a).

    ?- findall(1-Kleur, weg(test,1-Kleur), [1-Kleur]).
        Kleur = a.
*/

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