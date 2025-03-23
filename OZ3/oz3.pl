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

listlengthAcc([], Acc, Acc).
listlengthAcc([_|T], Acc, Length) :-
    NewAcc is Acc + 1,
    listlengthAcc(T, NewAcc, Length).

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
% opeenvolgend(X, Y, [X | [Y | _]] ).
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

/* Het predicaat \+ gaat na of iets NIET kan bewezen worden.

    NEGATION BY FAILURE

    Definitie van dit logisch predicaat:
    !   \+ (Goal) :- call(Goal), !, fail.
    !   \+ (_).

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
%       -> we gebruiken het predicaat v(x) dat slaagt als x een vertex is in de graaf
%       -> we gebruiken het predicaat e(x,y) om een verbinding in een grafe voor te stellen

v(a).
v(b).
v(c).
v(d).
v(e).

e(a,b).
e(b,c).
e(b,d).
e(c,d).

%   b. Implementeer het predicaat buur/2 dat slaagt als twee vertices onmiddellijk met elkaar verbonden zijn.

buur(X,Y) :-
	e(X,Y), !.
buur(X,Y) :-
	e(Y,X).

%   c. Implementeer een predicaat pad/2 dat slaagt telkens twee vertices door een pad in de graaf verbonden.
%   -> Let op voor oneindige lussen

% Naieve versie
pad_n(X,Y) :-
	buur(X,Y).
pad_n(X,Y) :-
	buur(X,Z),
	pad_n(Z,Y).

% Verbeterde versie die bijhoudt welke nodes al bezocht zijn
pad(X,Y) :- pad(X,[X],Y).

pad(X,Bezocht,Y) :-
	buur(X,Y),
	\+ member(Y,Bezocht).                   % Y werd nog niet bezocht
pad(X,Bezocht,Y) :-
	buur(X,Z),
	\+ member(Z,Bezocht),                   % Z werd nog niet bezocht
	%append(Bezocht,[Z],BezochtNieuw),
	pad(Z,[Z | Bezocht],Y).                 % Voeg Z toe aan de lijst van bezochte nodes

/*
    ?- pad(a, d).
        true.

    ?- pad(a,X).
        X = b ;
        X = c ;
        X = d ;
        X = d ;
        X = c ;
*/


/*
    3. FIBONACCI

    fib/2 slaagt wanneer voor fib(N,G) geldt dat G het N-de fibonaccigetal is.
*/

%%%%
% Naief

fib_naief(0, 0).
fib_naief(1, 1).

% N: 0, 1, 2, 3, 4, 5, 6,  7,  8,  9, ...
% G: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
fib_naief(N, G) :-
    N > 1,          % OF:
    N1 is N - 1,    % succ(N1, N)
    N2 is N - 2,    % succ(N2, NM1)
    fib_naief(N1, G1),
    fib_naief(N2, G2),
    G is G1 + G2.

/*
    ?- fib_naief(5,X).
        X = 5 .

    ?- fib_naief(9,X).
        X = 34 ;
*/


%%%%
% Staartrecursie

% fib_acc/3 -> initialiseer accumulatoren van fib_acc/4 op 0 en 1
fib_acc(N, G) :- fib_acc(N, 0, 1, G).

% fib_acc/4 -> slaagt wanneer voor fib_acc(N, S1, S2, G) geldt dat G het N-de fibonaccigetal is.
% 	-> S1 is een fibonaccigetal
% 	-> S2 is het fibonaccigetal volgend op S1 na 1 stap
% 	-> X is het fibonaccigetal volgend op S1 na N stappen
fib_acc(0, S1, _, S1).
fib_acc(1, _, S2, S2).
fib_acc(N, S1, S2, G) :-
    N > 1,
    LoweredN is N - 1,                  % of succ(LoweredN, N)
    S is S1 + S2,
    fib_acc(LoweredN, S2, S, G).

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
    (   DL =< DR ->
        voegtoe(W, L, NL),
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
extra_diepte(nil, 0).
extra_diepte(node(_, _, _, D), D).

% Voeg een waarde W toe 
%   -> voegtoe(W, Boom, NieuweBoom)

extra_voegtoe(W, nil, node(nil, W, nil, 1)).           % Basisgeval: lege boom

% MODELOPLOSSING: aparte regel voor elk geval (DL < DR, DL > DR, DL = DR)

% DL < DR
%                                   ↓ nieuwe boom ↓  
extra_voegtoe(W, node(L, V, R, D), node(NL, V, R, D)) :- 
    extra_diepte(L, DL), 
    extra_diepte(R, DR),
    DL < DR,
    extra_voegtoe(W, L, NL).    % voeg toe aan LDB

% DL > DR
%                                   ↓ nieuwe boom ↓
extra_voegtoe(W, node(L, V, R, D), node(L, V, NR, D)) :- 
    extra_diepte(L, DL), 
    extra_diepte(R, DR),
    DL > DR,
    extra_voegtoe(W, R, NR).    % voeg toe aan RDB

% DL = DR
% Nu verandert de diepte wel!
extra_voegtoe(W, node(L, V, R, D), node(NL, V, R, NieuweDiepte)) :- 
    extra_diepte(L, DL), 
    extra_diepte(R, DR),
    DL = DR,
    succ(D, NieuweDiepte),      % Nieuwe diepte is D + 1
    extra_voegtoe(W, L, NL).    % voeg toe aan LDB

% EIGEN VERSIE
extra_voegtoe(W, node(L, V, R, _), NieuweBoom) :-      % Recursief geval
    extra_diepte(L, DL), extra_diepte(R, DR),          % Bepaal diepte linker en rechter subboom
    (DL =< DR ->                                       % IF            (diepte linker subboom <= diepte rechter subboom)
        extra_voegtoe(W, L, NL),                       %   ->  THEN:   Voeg toe aan linker subboom
        NieuweDiepte is DR + 1,                        %               Bepaal diepte van nieuwe boom
        NieuweBoom = node(NL, V, R, NieuweDiepte) ;    %               Nieuwe boom met nieuwe linker subboom
        extra_voegtoe(W, R, NR),                       %   ->  ELSE:   Voeg toe aan rechter subboom
        NieuweDiepte is DL + 1,                        %               Bepaal diepte van nieuwe boom
        NieuweBoom = node(L, V, NR, NieuweDiepte)      %               Nieuwe boom met nieuwe rechter subboom
    ).

/*
    ?- newvoegtoe(4,nil,X).
        X = node(nil, 4, nil, 1).

    ?- newvoegtoe(7,node(nil, 4, nil, 1),X).
        X = node(node(nil, 7, nil, 1), 4, nil, 2).

    ?- newvoegtoe(23, node(node(nil, 7, nil, 1), 4, nil, 2), X).
        X = node(node(nil, 7, nil, 1), 4, node(nil, 23, nil, 1), 2)

    BOOM:
            4
           / \
          7   23

    ?- newvoegtoe(1, node(node(nil, 7, nil, 1), 4, node(nil, 23, nil, 1), 2), X).
        X = node(node(node(nil, 1, nil, 1), 7, nil, 1), 4, node(nil, 23, nil, 1), 2) .

    BOOM:
            4
           / \
          7   23
         /
        1
*/


/*
    5. PRIEMGETALLEN
    Getal per getal de veelvouden verwijderen

    allepriem(X, K) slaagt als X een priemgetal is kleiner dan K.

        - Bereken eerst alle getallen van 1 tot en met K
        - Begin bij het begin en verwijder alle veelvouden
        - Ga zo verder tot enkel de priemgetallen overschieten
*/

% Genereer een lijst met getallen van Low to High (exclusief High)
range(N, N, []).                    % Low = High -> stop recursie
range(Low, High, [Low | Rest]) :- 
    Low < High,
    Next is Low + 1,
    range(Next, High, Rest).

/*
    ?- range(2, 10, X).
        X = [2, 3, 4, 5, 6, 7, 8, 9].
*/

% Genereer een lijst met getallen van 2 (!!) tot N (exclusief N)
getallen(1, []).                % Basisgeval
getallen(N, G) :-
    N > 1,
    range(2, N, G).

/*
    ?- getallen(10,X).
        X = [2, 3, 4, 5, 6, 7, 8, 9].
*/

% Verwijder veelvouden van een getal uit een lijst
verwijder_veelvouden(_, [], []).

% Veelvoud -> verwijder head en ga verder met de tail
verwijder_veelvouden(N, [H | T], Rest) :-
    H mod N =:= 0,
    verwijder_veelvouden(N, T, Rest).

% Geen veelvoud -> hou head (append bij resultaat recursieve call)
verwijder_veelvouden(N, [H | T], [H | RestT]) :-
    H mod N =\= 0,
    verwijder_veelvouden(N, T, RestT).

/*
    ?- verwijder_veelvouden(2, [1,2,3,4,5,6], X).
        X = [1, 3, 5].
    ?- verwijder_veelvouden(3, [1,2,3,4,5,6], X).
        X = [1, 2, 4, 5].
*/

% Finale regel
allepriem(N,Resultaat) :-
    getallen(N,Getallen),
    allepriem([],Getallen,RevResultaat),
    reverse(RevResultaat, Resultaat).       % hoeft niet als je append gebruikt in recursief geval

% Basisgeval: de tail is leeg en we hebben enkel priemgetallen over
allepriem(Res, [], Res).

% Recursief geval: verwijder veelvouden van head uit tail
allepriem(Acc, [Head | Tail], Resultaat) :-
    % append(H,[P],HP),                     % uit modeloplossing
    verwijder_veelvouden(Head,Tail,NieuweTail),
    % Voeg head toe aan accumulator
    allepriem([Head | Acc], NieuweTail, Resultaat).

/*
    ?- allepriem(10,X).
        X = [7, 5, 3, 2].
*/


/*
    6. UITDRUKKING
*/

eval(int(X), _, X).

eval(var(X), [pair(X, Value) | _], Value).
eval(var(X), [pair(Y, _) | Rest], Value) :- 
    X \== Y,
    eval(var(X), Rest, Value).

eval(plus(E1, E2), Lijst, Value) :-
    eval(E1, Lijst, Value1),
    eval(E2, Lijst, Value2),
    Value is Value1 + Value2.

eval(times(E1, E2), Lijst, Value) :-
    eval(E1, Lijst, Value1),
    eval(E2, Lijst, Value2),
    Value is Value1 * Value2.

eval(pow(E1, E2), Lijst, Value) :-
    eval(E1, Lijst, Value1),
    eval(E2, Lijst, Value2),
    Value is Value1 ** Value2.

eval(min(E), Lijst, Value) :-
    eval(E, Lijst, Value1),
    Value is -Value1.

/*
    ?- eval(plus(var(x), int(2)),[pair(x,3)], Value).
        Value = 5.
    ?- eval(plus(var(x), times(int(2), var(y))), [pair(x, 2), pair(y, 3)], Value).
        Value = 8.
*/