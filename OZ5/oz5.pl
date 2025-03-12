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
    wall_in_part(Part, _), !.   % cut zodat hij stopt met zoeken

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

cut_plan(CuttingPlan, Parts):
    