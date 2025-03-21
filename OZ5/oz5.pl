house(5,4). % B=5, H=4

wall(v, 2, 3, 4). % verticale muur van y = 3 naar 4 met x-coordinaat 2
wall(v, 3, 0, 1). % verticale muur van y = 0 naar 1 met x-coordinaat 3
wall(v, 4, 0, 1).
wall(h, 2, 3, 5).

color(1, 1, green). % linker onderhoek
color(1, 2, red).
color(3, 1, red).
color(4, 0, blue).

/*
    1. wall_in_part( part(XCo1, XCo2, XCo1, XCo2), Wall )

            - slaagt als Wall een muur is binnen dit deel
            - Co1 variabelen de links-beneden, en de Co2 variabelen de rechts-boven hoek
*/

wall_in_part(part(XCo1, YCo1, XCo2, YCo2), wall(h, WallYCo, WallXCo1, WallXCo2)) :-
    wall(h, WallYCo, WallXCo1, WallXCo2),
    WallXCo1 >= XCo1,
    WallXCo1 =< XCo2,
    WallXCo2 >= XCo1,
    WallXCo2 =< XCo2,
    WallYCo > YCo1,
    WallYCo < YCo2.

wall_in_part(part(XCo1, YCo1, XCo2, YCo2), wall(v, WallXCo, WallYCo1, WallYCo2)) :-
    wall(v, WallXCo, WallYCo1, WallYCo2),
    WallYCo1 >= YCo1,
    WallYCo1 =< YCo2,
    WallYCo2 >= YCo1,
    WallYCo2 =< YCo2,
    WallXCo > XCo1,
    WallXCo < XCo2.

/*
    ?- wall_in_part(part(0, 0, 5, 4), X).
        X = wall(h, 2, 3, 5) ;
        X = wall(v, 2, 3, 4) ;
        X = wall(v, 3, 0, 1) ;
        X = wall(v, 4, 0, 1).
*/

/*
    2. color_in_part(part(XCo1, YCo1, XCo2, YCo2), Color) unificeert Color met een kleur in het gegeven part
*/

color_in_part(part(XCo1, YCo1, XCo2, YCo2), Color) :-
    color(X, Y, Color),
    X >= XCo1,
    X < XCo2,
    Y >= YCo1,
    Y < YCo2.

/*
    ?- color_in_part(part(0, 0, 5, 4), X).
        X = green ;
        X = red ;
        X = red ;
        X = blue.
*/

/*
    3. 
        - single_color_present(Color) slaagt als er hoogstens 1 kleur is aangewezen.
        - part_has_wall(Part) slaagt als er minstens 1 wall in Part aanwezig is.

    SORT VERWIJDERT DUPLICATES!

    ?- sort([blue,green,green],Sorted), length(Sorted, X).
        Sorted = [blue, green],
        X = 2.
*/

single_color_present(Part) :-
    findall(X, color_in_part(Part, color(_,_,X)), Colors),      % let op, niet gewoon X, maar color(_,_,X) !
    sort(Colors, ColorsWithoutDuplicates),                      % verwijder duplicates
    length(ColorsWithoutDuplicates, LengthWithoutDuplicates),
    LengthWithoutDuplicates =< 1.

part_has_wall(Part) :-
    wall_in_part(Part, _), !.   % cut zodat hij stopt met zoeken!!

/*
    ?- single_color_present(part(0, 0, 5, 4)).
        true.

    ?- part_has_wall(part(0,0,5,4)).
        true.
*/

/*
    4. cut_plan(CuttingPlan, Parts) genereert een versnijdingsplan.

        - vb. CuttingPlan = [cut(h,2,0,5), cut(v,3,0,2), cut(v,4,0,2), cut(v,2,2,4)]
        - Na verticale cut eerst links, dan rechts.
        - Na horizontale cut eerst onderaan, dan bovenaan.
        - Elk part hoogstens 1 kleur keuze
        - Elk part geen muren
*/


% Initiele call
cut_plan(CuttingPlan, Parts) :-
    house(Breedte, Hoogte),                                             % Leg de Breedte en Hoogte van het huis vast
    cut_plan(CuttingPlan, [], [part(0, 0, Breedte, Hoogte)], [], Parts).



% cut_plan(UiteindelijkCuttingPlan, CutsAccumulator, [HuidigPart | RestParts])

% Basisgeval: geen RestParts meer   -> unificeer CutsAccumulator met UiteindelijkCuttingPlan
%                                   -> unificieer PartsAccumulator met Parts
cut_plan(CuttingPlan, CutsAcc, [], PartsAcc, Parts) :-
    reverse(CutsAcc, CuttingPlan),
    sort(PartsAcc, Parts).
    
% CUT MOGELIJK: Horizontaal
cut_plan(CuttingPlan, CutsAcc, [part(X1, Y1, X2, Y2) | RestParts], PartsAcc, Parts) :-
    wall_in_part(part(X1, Y1, X2, Y2), wall(h, CutY, _, _)),     % Neem de Y-coordinaat CutY van de eerste horizontale muur
    cut_plan(CuttingPlan, [cut(h, CutY, X1, X2) | CutsAcc], [part(X1, Y1, X2, CutY), part(X1, CutY, X2, Y2) | RestParts], PartsAcc, Parts).

% CUT MOGELIJK: Verticaal
cut_plan(CuttingPlan, CutsAcc, [part(X1, Y1, X2, Y2) | RestParts], PartsAcc, Parts) :-
    wall_in_part(part(X1, Y1, X2, Y2), wall(v, CutX, _, _)),     % Neem de X-coordinaat CutX van de eerste verticale muur
    cut_plan(CuttingPlan, [cut(v, CutX, Y1, Y2) | CutsAcc], [part(X1, Y1, CutX, Y2), part(CutX, Y1, X2, Y2) | RestParts], PartsAcc, Parts).

% GEEN CUT MOGELIJK
% part(X1, Y1, X2, Y2) mag ook gewoon Part zijn! -> de argumenten worden nergens meer gebruikt
cut_plan(CuttingPlan, CutsAcc, [part(X1, Y1, X2, Y2) | RestParts], PartAcc, Parts) :-
    \+ part_has_wall(part(X1, Y1, X2, Y2)),
    single_color_present(part(X1, Y1, X2, Y2)),
    % Geen cuts toevoegen aan de accumulator en part minder om te beschouwen.
    % Voeg ook het 'finale' part toe aan de parts accumulator.
    cut_plan(CuttingPlan, CutsAcc, RestParts, [part(X1, Y1, X2, Y2) | PartAcc], Parts).


/*

    Klein voorbeeldje:

    part(0, 0, 5, 4)

                horizontale cut, ga onderaan verder

         |||||
    part(0, 0, 5, 2), part(0, 2, 5, 4)

                verticale cut, ga links verder
    
         |||||
    part(0, 0, 3, 1), part(3, 0, 5, 2),     part(0, 2, 5, 4)

                uncuttable, ga rechts verder

                        |||||
    part(0, 0, 3, 1), part(3, 0, 5, 2),     part(0, 2, 5, 4)

                verticale cut, ga links verder

                                |||||
    part(0, 0, 3, 1),       part(3, 0, 4, 2), part(4, 0, 5, 2),     part(0, 2, 5, 4)

                uncuttable, ga rechts verder -> ook uncuttable -> ga verder bovenaan

                                                                        |||||
    part(0, 0, 3, 1),       part(3, 0, 4, 2), part(4, 0, 5, 2),     part(0, 2, 5, 4)

                verticale cut
*/

/*
    QUERY:

        ?- cut_plan(Cuts, Parts).
            Cuts = [cut(h, 2, 0, 5), cut(v, 3, 0, 2), cut(v, 4, 0, 2), cut(v, 2, 2, 4)],
            Parts = [part(0, 0, 3, 2), part(0, 2, 2, 4), part(2, 2, 5, 4), part(3, 0, 4, 2), part(4, 0, 5, 2)] ;
            Cuts = [cut(h, 2, 0, 5), cut(v, 4, 0, 2), cut(v, 3, 0, 2), cut(v, 2, 2, 4)],
            Parts = [part(0, 0, 3, 2), part(0, 2, 2, 4), part(2, 2, 5, 4), part(3, 0, 4, 2), part(4, 0, 5, 2)] ;
            Cuts = [cut(v, 2, 0, 4), cut(h, 2, 2, 5), cut(v, 3, 0, 2), cut(v, 4, 0, 2)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 2), part(2, 2, 5, 4), part(3, 0, 4, 2), part(4, 0, 5, 2)] ;
            Cuts = [cut(v, 2, 0, 4), cut(h, 2, 2, 5), cut(v, 4, 0, 2), cut(v, 3, 0, 2)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 2), part(2, 2, 5, 4), part(3, 0, 4, 2), part(4, 0, 5, 2)] ;
            Cuts = [cut(v, 2, 0, 4), cut(v, 3, 0, 4), cut(h, 2, 3, 5), cut(v, 4, 0, 2)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 2), part(3, 2, 5, 4), part(4, 0, 5, 2)] ;
            Cuts = [cut(v, 2, 0, 4), cut(v, 3, 0, 4), cut(v, 4, 0, 4)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 4), part(4, 0, 5, 4)] ;
            Cuts = [cut(v, 2, 0, 4), cut(v, 4, 0, 4), cut(v, 3, 0, 4)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 4), part(4, 0, 5, 4)] ;
            Cuts = [cut(v, 3, 0, 4), cut(v, 2, 0, 4), cut(h, 2, 3, 5), cut(v, 4, 0, 2)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 2), part(3, 2, 5, 4), part(4, 0, 5, 2)] ;
            Cuts = [cut(v, 3, 0, 4), cut(v, 2, 0, 4), cut(v, 4, 0, 4)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 4), part(4, 0, 5, 4)] ;
            Cuts = [cut(v, 4, 0, 4), cut(v, 2, 0, 4), cut(v, 3, 0, 4)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 4), part(4, 0, 5, 4)] ;
            Cuts = [cut(v, 4, 0, 4), cut(v, 3, 0, 4), cut(v, 2, 0, 4)],
            Parts = [part(0, 0, 2, 4), part(2, 0, 3, 4), part(3, 0, 4, 4), part(4, 0, 5, 4)] ;
*/

/*
    In commentaar:
        + inputvariabele
        - outputvariabele
        ? kan ofwel input of output zijn

    Examen 
        -> Niet mondeling, enkel schriftelijk
        -> Oefeningen Haskell en Prolog met theorievragen erbij
        -> Slides, eigen oplossingen, voorbeeldoplossingen, documentatie SWI Prolog
        -> Wat is unificatie? Hoe kunnen we hiermee zoeken in Prolog?
        -> Hoe zoekt Prolog? Uitvoeringsboom, backtracking -> volgorde predicaten en locatie cuts!
        -> Accumulatoren, open lijsten, staartstructuren,... zijn beter dan append!
                => MAAR doe dit pas op het eind, als je de rest hebt geïmplementeerd en alles werkt

    Waar op letten?
        - Cuts (op juiste locatie!)
        - Findall!
        - Negatie \+ : wanneer wel/niet gebruiken? Wat is het juist?

    Maandag:
        - 3 oefeningen
        - Telkens met subpredicaten
        - Moeilijkheidsgraad vglb. met laatste oefening laatste OZ.
        - Geen oefeningen over zaken die we niet gezien hebben

    LET OP
        - delete/3 is select/3 in SWIPL!!
*/