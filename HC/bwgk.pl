%% insert
%
%   ! We sorteren zodat de eindtoestand (ook gesorteerd) gedetecteerd kan worden!
insert( Kand,L,Sorted):- sort([Kand|L], Sorted).


%% Start- en eindtoestand
%
%   ! Opletten met de volgorde!
starttoestand(toestand(links, [geit, kool, wolf], [])).
eindtoestand(toestand(rechts, [], [geit, kool, wolf])).



%% solve_dfs(+HuidigeToestand, ?ToestandenHistory, -StappenLeft)
%   
%   Dit predicaat slaagt als vanuit de huidige toestand HuidigeToestand, 
%   de eindtoestand toestand(rechts, [], [wolf, geit, kool]) kan bereikt worden,
%   waarbij dezelfde toestand vermeden wordt mbv ToestandenHistory

% ! Is de history een input (+) of een output (-)??

% Basisgeval
%   - geen stappen meer over
%   - test of T een eindttoestand is
solve_dfs(T, _, []) :-
    eindtoestand(T), !.

% Solve vanuit een niet-eindtoestand
solve_dfs(HuidigeToestand, History, [Stap | RestStappen]) :-
    overgang(HuidigeToestand, Stap, NieuweToestand),
    geldigeToestand(NieuweToestand),
    \+ member(NieuweToestand, History),
    write(NieuweToestand), nl,
    solve_dfs(                          % los recursief verder op
        NieuweToestand, 
        [NieuweToestand | History],     % voeg nieuwe toestand toe aan history
        RestStappen                     % 1 stap genomen -> ga verder met tail
    ).



%% geldigeToestand(+Toestand)
%
%   Dit predicaat slaagt als Toestand een geldige toestand is.

geldigeToestand(toestand(links, _, Rechts)) :- \+ gevaar(Rechts).
geldigeToestand(toestand(rechts, Links, _)) :- \+ gevaar(Links).



%% gevaar(+Kant)
%
%   Dit predicaat slaagt als Kant (de lijst die de rechter-/linkerkant voorstelt)
%   gevaar bevat (wolf-geit, geit-kool).

gevaar(Kant) :-
    member(wolf, Kant),
    member(geit, Kant).
gevaar(Kant) :-
    member(geit, Kant),
    member(kool, Kant).



%% overgang(+Toestand, -Stap, -NieuweToestand)
%
%   Dit predicaat slaagt als NieuweToestand het resultaat is
%   van vanuit Toestand de actie Stap (linksNaarRechts/1, rechtsNaarLinks/1 
%   of alleen/0) te nemen.
%
%   linksNaarRechts/1 heeft 1 argument: het object dat de boer meepakt op de boot.

overgang(toestand(links, L1, R1), linksNaarRechts(Voorwerp), toestand(rechts, L2, R2)) :-
    select(Voorwerp, L1, L2),   % ! Voorwerp verwijderen (mbv select/3!) uit L1 resulteert in L2
    insert(Voorwerp, R1, R2).
overgang(toestand(rechts, L1, R1), rechtsNaarLinks(Voorwerp), toestand(links, L2, R2)) :-
    select(Voorwerp, R1, R2),
    insert(Voorwerp, L1, L2).
overgang(toestand(Kant1, L, R), alleen, toestand(Kant2, L, R)) :-
    omgekeerd(Kant1, Kant2).



%% omgekeerd(Kant1, Kant2)
%
%   Dit predicaat slaagt als Kant1 de omgekeerde kant is van Kant2.

omgekeerd(links, rechts).
omgekeerd(rechts, links).



%% Depth-first oplossing

bwgk(Stappen) :- 
    starttoestand(BeginToestand), 
    write(BeginToestand), nl,
    solve_dfs(BeginToestand,[BeginToestand],Stappen).



%% Iterative deepening oplossing

bwgk(Stappen) :- 
    starttoestand(BeginToestand), 
    write(BeginToestand), nl,
    list(Stappen), % !!                                 lengte 0, 1, 2, ... -> stappen beperken
    solve_dfs(BeginToestand,[BeginToestand],Stappen).



%% list(?Lijst)
%
%   (? = +)
%   Dit predicaat slaagt wanneer Lijst een lijst is.
%
%   (? = -)
%   Het kan ook gebruikt worden als generator.
%       -> Genereert lijsten met _INT_VAR's van lengten 0, 1, 3, ...
%       -> Nodig voor ID search (stappen beperken)

list([]).
list([_ | T]) :- list(T).