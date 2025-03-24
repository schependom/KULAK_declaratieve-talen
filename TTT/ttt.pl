
%%%%%%%%
% 1. PAD

size(4).
goto(4, 1, final). % eindpunt

goto(1,4,right).
goto(2,3,right).
goto(1,2,up).
goto(1,1,up).
goto(2,4,right).
goto(3,3,down).
goto(2,2,down).
goto(2,1,down).
goto(3,4,left).
goto(4,3,up).
goto(4,2,up).
goto(3,1,right).
goto(4,4,up).




% 1.

%% volgende(+Direction, +Xvorig, +Yvorig, -Xnew, -Ynew)
volgende(up, Xvorig, Yvorig, Xvorig, Ynew) :- succ(Yvorig, Ynew).
volgende(down, Xvorig, Yvorig, Xvorig, Ynew) :- succ(Ynew, Yvorig).
volgende(right, Xvorig, Yvorig, Xnew, Yvorig) :- succ(Xvorig, Xnew).
volgende(left, Xvorig, Yvorig, Xnew, Yvorig) :- succ(Xnew, Xvorig).

/*
    ?- volgende(up, 1, 1, Xnew, Ynew).
        Xnew = 1,
        Ynew = 2.

    ?- volgende(left,3,3,Xnew,Ynew).
        Xnew = 2,
        Ynew = 3.
*/





% 2.

op_het_bord(X, Y) :-
    size(Size), % Leg size vast
    X > 0,
    X =< Size,
    Y > 0,
    Y =< Size.

%% bad_move(+X, +Y, +Visited)

bad_move(X, Y, _) :-
    \+ op_het_bord(X,Y), !.     % slechte zet -> niet meer verderzoeken

bad_move(X, Y, Visited) :-
    member((X, Y), Visited).

/*

    ?- bad_move(1,4,[]).
    false.

    ?- bad_move(0,1,[]).
    true.

    ?- bad_move(5,4,[]).
    true.

    ?- bad_move(1,4,[(1,1),(1,2),(1,3),(1,4)]).
    true.
*/






% 3.

/*
%% path(-Path) voor een gewoon pad (niet gesaboteerd)

path(Path) :- path( (1,1), [(1,1)], Path ).

% Eindpositie bereikt
path((CurrX, CurrY), Bezocht, FinalPath) :-
    goto(CurrX, CurrY, final),
    sort(Bezocht, FinalPath).                   % output sorteren

% path(CurrentPos, Bezocht, FinalPath)
path((CurrX, CurrY), Bezocht, FinalPath) :-
    goto(CurrX, CurrY, dir),                    % leg richting vast
    volgende(dir, CurrX, CurryY, NextX, NextY), % leg volgende coordinaten vast
    \+ bad_move(NextX, NextY, Bezocht),         % geldige move
    path((NextX, NextY), [(NextX, NextY) | Bezocht], FinalPath).
*/


%% path(-Path) voor een gesaboteerd pad

path(Path) :- path( (1,1), [(1,1)], Path ).

% Eindpositie bereikt
path((CurrX, CurrY), Bezocht, FinalPath) :-
    goto(CurrX, CurrY, final),                  % eindpunt bereikt
    reverse(Bezocht, FinalPath).

% Geen goto/3 info -> leeg vakje -> alle richtingen proberen
path((CurrX, CurrY), Bezocht, FinalPath) :-
    \+ goto(CurrX, CurrY, _),                   % leeg vakje, want geen goto/3 info
    findall( 
        X-Y-Dir,                                % vind alle mogelijke nieuwe vakjes
        (                                       % voorwaarden findall
            volgende(Dir, CurrX, CurrY, X, Y),  %   1. aangrenzend vakje
            goto(X, Y, Dir),                    %   2. het volgende vakje moet info bevatten
            \+ bad_move(X, Y, Bezocht)          %   3. het volgende vakje mag geen slechte move zijn
        ), 
        BurenOmTeProberen 
    ),
    member(NextX-NextY-Dir, BurenOmTeProberen), % kies buur om te proberen -> mogelijkheid tot backtracking
    path((NextX, NextY), [(NextX, NextY) | Bezocht], FinalPath).

% Vakje met informatie
path((CurrX, CurrY), Bezocht, FinalPath) :-
    goto(CurrX, CurrY, Dir),                    % leg richting vast
    volgende(Dir, CurrX, CurrY, NextX, NextY),  % leg volgende coordinaten vast
    \+ bad_move(NextX, NextY, Bezocht),         % geldige move
    path((NextX, NextY), [(NextX, NextY) | Bezocht], FinalPath).






%%%%%%%%%%
% 2. LOOPS

arrow(a,b).
arrow(b,c).
arrow(c,c).
arrow(a,d).
arrow(d,a).


% 1. 


%% delete_node(+ArrowList, +Node, -Result)

delete_node(ArrowList, Node, Result) :-
    delete_node(ArrowList, [], Node, Result).   % Initialiseer Acc op lege lijst



%% delete_node(ArrowsToCheck, Acc, Node, Result)
%
%   Gebruikt een Accumulator om telkens de head van ArrowsToCheck
%   toe te voegen aan de output wanneer deze head de Node NIET bevat.

% Basisgeval -> unificeer Acc met Result
delete_node([], Result, _, Result).

% De pijl bevat de node niet
delete_node([arrow(N1, N2) | Tail], Acc, Node, Result) :-
    N1 \== Node,
    N2 \== Node,
    delete_node(Tail, [arrow(N1, N2) | Acc], Node, Result).     % voeg pijl toe aan output (acc)

% De pijl bevat de node links
delete_node([arrow(Node, _) | Tail], Acc, Node, Result) :-
    delete_node(Tail, Acc, Node, Result).

% De pijl bevat de node rechts
delete_node([arrow(_, Node) | Tail], Acc, Node, Result) :-
    delete_node(Tail, Acc, Node, Result).


/* VB.
    AllArrows = [arrow(a,b), arrow(b,c), arrow(c,c), arrow(a,d), arrow(d,a)].

    ?- delete_node([arrow(a,b), arrow(b,c), arrow(c,c), arrow(a,d), arrow(d,a)], a, Result).
        Result = [arrow(c, c), arrow(b, c)] .
*/



% 2.

knooppunt(X, ArrowList) :- member(arrow(X, _), ArrowList).
knooppunt(X, ArrowList) :- member(arrow(_, X), ArrowList).



%% path(+Start, +Current, +ArrowList, +Visited, -Loop)
    
% Recursief geval
path(Start, Current, ArrowList, Visited, Loop) :-
    findall( % zoek alle volgende punten die we nog niet bezocht hebben en waarvoor een boog bestaat
        Next,
        (
            member(arrow(Current, Next), ArrowList),
            \+ member(Next, Visited)
        ),
        NextNodes
    ),
    member(NextNode, NextNodes),    % leg volgende punt vast
    (
        (NextNode == Start)         % loop gevonden?
            ->
        (Loop = [Start | Visited])  % Ja -> unificeer de accumulator Visited met het resultaat Loop, met het startpunt geprepend
            ;
        path(Start, NextNode, ArrowList, [NextNode | Visited], Loop) % Neen -> zoek verder
    ).

/*
    ?- path(a, a, [arrow(a,b), arrow(b,c), arrow(c,c), arrow(a,d), arrow(d,a)], [], Output).
        [d, a].
*/


%% loops(-Loops)
%       Dit predicaat vindt alle loops in de variabele AllArrows.

loops(Loops) :-
    findall(arrow(A,B), arrow(A,B), AllArrows),
    findloops(AllArrows, [], Loops).


%% findloops(+ArrowList, +AccLoops, -AllLoops)
%
%       De output is een lijst = (permutatie van) alle cycli

% Basisgeval: geen arrows meer over
findloops([], Acc, Acc).

% Recursief geval: voer procedure uit
findloops(ArrowList, AccLoops, AllLoops) :- 
    findall(Knooppunt, knooppunt(Knooppunt, ArrowList), Knooppunten),
    sort(Knooppunten, KnooppuntenZonderDuplicates),                 % vindt alle knooppunten (zonder duplicates)
    member(Knooppunt, KnooppuntenZonderDuplicates),                 % leg een knooppunt vast -> mogelijkheid tot backtracking voorzien
    path(Knooppunt, Knooppunt, ArrowList, [], Loop),                % zoek een loop vanuit het vastgelegde knooppunt
    delete_node(ArrowList, Knooppunt, NieuweArrowList),             % verwijder de bogen die Knooppunt bevatten
    findloops(NieuweArrowList, [Loop | AccLoops], AllLoops).        % start te procedure opnieuw (met de nieuwe arrows en de loop geprepend bij Acc)

/*
    ?- loops(Loops).
    Loops = [[c], [a, d]] ;
    Loops = [[c], [a, d]] ;
    Loops = [[a, d], [c]] ;
    Loops = [[a, d], [c]] ;
*/






%%%%%%%%%%%
% 3. DOMINO



% 1.

%% solve_domino(+Dominos, -Chain)

solve_domino(Dominos, Chain) :- 
    member((A,B), Dominos),                 % selecteer een domino
    select((A,B), Dominos, Resterend),      % verwijder die uit de lijst
    solve_domino(Resterend, [(A,B)], Chain).% probeer te leggen

%% solve_domino(+DominosToBeUsed, +ChainAcc, -Chain)

% Basisgeval:
%   geen steentjes meer over
%   check of eerste gelijk is aan laatste
solve_domino([], [(FirstA, FirstB) | Rest], [(FirstA, FirstB) | Rest]) :-
    last(Rest, (_LastA,LastB)),     % haal laatste op
    \+ (FirstA \== LastB).          % eerste mag niet verschillend zijn van de laatste

/* VB.
    ?- solve_domino([],[(3,2),(2,1),(1,3)], Chain).
        Chain = [(3, _), (2, 1), (1, 3)].
*/

% Recursief geval
% Eerste steentje matcht met vorig gelegd steentje
solve_domino([(A, B) | Tail], [(B, C) | RestAcc], Chain) :-
    solve_domino(Tail, [(A,B), (B, C) | RestAcc], Chain).
solve_domino([(B, A) | Tail], [(B, C) | RestAcc], Chain) :-
    solve_domino(Tail, [(A, B), (B, C) | RestAcc], Chain).


/*
    ?- solve_domino([(2,1),(2,3),(1,3)],Chain).
    Chain = [(1, 3), (3, 2), (2, 1)] ;
    Chain = [(3, 1), (1, 2), (2, 3)] ;
    Chain = [(3, 2), (2, 1), (1, 3)] .
*/







% 2.


%% Sublijsten van domino's genereren

subset_own([], []).
subset_own([E | Tail], [E | NTail]) :-
    subset_own(Tail, NTail).
subset_own([_ | Tail], NTail) :-
    subset_own(Tail, NTail).

/* 
    ?- subset_own([(1,2),(2,3),(3,1)],R).
    R = [(1, 2), (2, 3), (3, 1)] ;
    R = [(1, 2), (2, 3)] ;
    R = [(1, 2), (3, 1)] ;
    R = [(1, 2)] ;
    R = [(2, 3), (3, 1)] ;
    R = [(2, 3)] ;
    R = [(3, 1)] ;
    R = [].
*/


%% longest_domino_chain(+Dominos, -Chain)
longest_domino_chain(Dominos, Chain) :-
    findall(
        L-Subset,
        (
            subset_own(Dominos, Subset),
            length(Subset, L)
        ),
        Subsets
    ), % zoek alle subsets, bepaal hun lengte en stop beiden in een lijst Subsets
    sort(Subsets, AscSortedSubsets),                % sorteer (kleinste subsets eerst want asc)
    reverse(AscSortedSubsets, DescSortedSubsets),   % reverse (nu staan de grootste subsets eerst)
    first_domino_chain(DescSortedSubsets, Chain).   % zoek de eerste subset die een opl heeft -> dit is de langste opl!


% De eerste (en grootste) subset heeft een oplossing
%   -> de lengte van die oplossing is sws de lengte van die subset
%   -> unificeer de oplossing met de chain uit longest_domino_chain/2
first_domino_chain([(_-FirstSubset) | _], Solution) :-
    findall(Oplossing, solve_domino(FirstSubset, Oplossing), Oplossingen),
    member(Solution, Oplossingen).

% Met de eerste (langste) subset kan geen oplossing gevonden worden
first_domino_chain([_-FirstSubset| RestSubsets], Chain) :-
    \+ (
        solve_domino(FirstSubset, Solution),
        Solution \== []
    ),
    first_domino_chain(RestSubsets, Chain).


/*
    ?- longest_domino_chain([(1,2),(2,3),(3,1),(4,5)],Chain).
    Chain = [(3, 1), (1, 2), (2, 3)] ;

    ?- longest_domino_chain([(2,1),(2,3),(3,4),(4,1),(5,2)],Chain).
    Chain = [(1, 4), (4, 3), (3, 2), (2, 1)] ;
*/