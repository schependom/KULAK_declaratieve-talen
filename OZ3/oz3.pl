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
    2. GRAFEN
*/

% Gegeven een symmetrische graaf met als vertices {a, b, c, d} en verbindingen tussen (a, b), (b, c), (b, d) en (c, d).

%   a. Bedenk een voorstelling van de graaf in Prolog.
%   -> edge/2
edge(a, b).
edge(b, c).
edge(b, d).
edge(c, d).

%   b. Implementeer het predicaat buur/2 dat slaagt als twee vertices onmiddellijk met elkaar verbonden zijn.
buur(X, Y) :- edge(X, Y).
buur(X, Y) :- edge(Y, X).

%   c. Implementeer een predicaat pad/2 dat slaagt telkens twee vertices door een pad in de graaf verbonden.
%   -> Let op voor oneindige lussen

% pad/2: slaagt als er een pad is tussen twee nodes
pad(X, Y) :- pad(X, Y, []).

pad(X, Y, _) :- buur(X, Y). % Basisgeval: directe verbinding
pad(X, Y, _) :- buur(X, Z), pad(Z, Y, [X]), !. % Voorkomt onnodige zoekopdrachten


/*
    3. FIBONACCI

    fib/2 slaagt wanneer voor fib(N,G) geldt dat G het N-de fibonaccigetal is.
*/

%%%%
% Naief

fib(0, 0).
fib(1, 1).

fib(N, G) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, G1),
    fib(N2, G2),
    G is G1 + G2.


%%%%
% Staartrecursie

% fib_acc/3 -> initialiseer accumulator van fib_acc/4 op 0 en 1
fib_acc(N, G) :- fib_acc(N, 0, 1, G).

% fib_acc/4 -> slaagt wanneer voor fib_acc(N, Acc1, Acc2, G) geldt dat G het N-de fibonaccigetal is.
fib_acc(0, Acc1, _, Acc1).
fib_acc(N, Acc1, Acc2, G) :-
    N > 0,
    LoweredN is N - 1,
    NewAcc is Acc1 + Acc2,
    fib_acc(LoweredN, Acc2, NewAcc, G).

/*
    ?- fib_acc(5,X).
        X = 5 .

    ?- fib_acc(10,X).
        X = 55 ;
*/


/*
    4. GEBALANCEERDE BOMEN

    Gebruik als repesentatie voor een boom:
        Boom := leeg|node(Boom, Waarde, Boom)

    Een boom is gebalanceerd als in elke knoop de diepte van linker subboom en de diepte van de rechter subboom
    niet meer dan 1 verschillen.      
*/

% Vorige oefenzitting:
diepte(nil, 0).                % lege boom heeft diepte nul
diepte(node(L, _, R), D) :-     % waarde van de wortel doet er niet toe
    diepte(L, DL),              % DL bevat diepte van linker subboom
    diepte(R, DR),              % DR bevat diepte van rechter subboom
    D is max(DL, DR) + 1.       % diepte van de boom is max van dieptes linker en rechter subboom + 1


%%%%
% a. Definieer het predicaat inbalans/1 dat slaagt als de gegeven boom gebalanceerd is.

inbalans(nil).                  % Een lege boom is gebalanceerd
inbalans(node(L, _, R)) :-
    diepte(L, DL),              % Bereken diepte linker subboom
    diepte(R, DR),              % Bereken diepte rechter subboom
    abs(DL - DR) =< 1,          % Controleer of de boom gebalanceerd is

/*
    ?- inbalans(nil).
        true.

    ?- inbalans(node(node(nil, 1, node(nil, 2, node(nil, 3, nil))), 4, node(nil, 5, nil))).
        true.

    ?- inbalans(node(node(nil, 1, nil), 2, node(nil, 3, nil))).
        true.
*/

%%%%
% Definieer het predicaat voegtoe/3 dat een element toevoegt aan een gebalanceerde boom op een zodanige manier dat de boom gebalanceerd blijft. Veronderstel dat er geen volgorde is tussen de elementen.
%   -> voegtoe(W, Boom, NieuweBoom)

% Basisgeval
voegtoe(W, nil, node(nil, W, nil)).

% Voeg toe aan de deelboom met de kleinste diepte
voegtoe(W, node(L, V, R), node(NL, V, NR)) :-
    diepte(L, DL),
    diepte(R, DR),
    (DL=<DR
    ->  voegtoe(W, L, NL),
        NR = R
    ;   voegtoe(W, R, NR),
        NL = L
    ).


%%%%
% Extraatje: Verander de definitie van een knoop zodanig dat ook de diepte van de boom opgeslagen blijft.
% Maak nieuwe versies van de hierboven gevraagde predicaten. Gebruik dit nieuwe element om de efficientie van voegtoe/3 te verhogen.

% Nieuwe definitie van een knoop
%   -> node(Left, Value, Right, Depth)
%   -> Depth = diepte van de boom met worden vanuit deze knoop

% Diepte bepalen
newdiepte(nil, 0).
newdiepte(node(_, _, _, D), D).

% Voeg een waarde W toe 
%   -> voegtoe(W, Boom, NieuweBoom)

newvoegtoe(W, nil, node(nil, W, nil, 1)).           % Basisgeval: lege boom
newvoegtoe(W, node(L, V, R, _), NieuweBoom) :-      % Recursief geval
    newdiepte(L, DL), newdiepte(R, DR),             % Bepaal diepte linker en rechter subboom
    (DL =< DR ->                                    % IF            (diepte linker subboom <= diepte rechter subboom)
        newvoegtoe(W, L, NL),                       %   ->  THEN:   Voeg toe aan linker subboom
        newdiepte(NL, NDL),                         %               Bepaal diepte van nieuwe linker subboom
        NieuweDiepte is max(NDL, DR) + 1,           %               Bepaal diepte van nieuwe boom
        NieuweBoom = node(NL, V, R, NieuweDiepte) ; %               Nieuwe boom met nieuwe linker subboom
        newvoegtoe(W, R, NR),                       %   ->  ELSE:   Voeg toe aan rechter subboom
        newdiepte(NR, NDR),                         %               Bepaal diepte van nieuwe rechter subboom
        NieuweDiepte is max(DL, NDR) + 1,           %               Bepaal diepte van nieuwe boom
        NieuweBoom = node(L, V, NR, NieuweDiepte)   %               Nieuwe boom met nieuwe rechter subboom
    ).

/*
    ?- newvoegtoe(4,nil,X).
        X = node(nil, 4, nil, 1).

    ?- newvoegtoe(7,node(nil, 4, nil, 1),X).
        X = node(node(nil, 7, nil, 0), 4, nil, 1).

    ?- newvoegtoe(23, node(node(nil, 7, nil, 0), 4, nil, 1), X).
        X = node(node(node(nil, 23, nil, 0), 7, nil, 1), 4, nil, 2).

    BOOM:
            4
           / \
          7   nil
         /
        23

    ?- newvoegtoe(1, node(node(node(nil, 23, nil, 0), 7, nil, 0), 4, nil, 0), X).
        X = node(node(node(nil, 23, nil, 0), 7, nil, 1), 4, node(nil, 1, nil, 0), 2)

    BOOM:  
            4
           / \
          7   1
         /
        23

    KLOPT NOG NIET!!
         
*/


/*
    5. PRIEMGETALLEN
    Getal per getal de veelvouden verwijderen

    allepriem(X, K) slaagt als X een priemgetal is kleiner dan K.

        - Bereken eerst alle getallen van 1 tot en met K
        - Begin bij het begin en verwijder alle veelvouden
        - Ga zo verder tot enkel de priemgetallen overschieten
*/

allegetallen()

allepriem()