%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% VOORBEELD ZOEKPROBLEEM %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connection(london, paris).
connection(london, dublin).
connection(paris, rome).
connection(paris, berlin).
connection(rome, athens).
connection(berlin, warsaw).
connection(dublin, edinburgh).
connection(edinburgh, london).

get_edges(Edges) :-
    findall((S1, S2), connection(S1, S2), Edges).





%%%%%%%%%%%%%%%%%%%%
%%% DIEPTE-EERST %%%
%%%%%%%%%%%%%%%%%%%%

%% Dfs(+StartNode, +EndNode, +Edges, -Paths)
% Vindt een pad van StartNode naar GoalNode gebruikmakend van Depth-First Search.
% Paths is de output en is een lijst van lijsten van knopen van StartNode naar GoalNode.
% Edges is een lijst van (niet-)gerichte bogen, voorgesteld als een lijst van tupels, bv. [(a,b), (b,c)].

% Edges is een lijst van GERICHTE!! bogen
dfsDirectioned(Start, Goal, Edges, [Start | Path]) :- dfsRecursiveDirectioned(Start, Goal, Edges, [], Path).
% Edges is een lijst van NIET-GERICHTE!! bogen.
dfsUndirectioned(Start, Goal, Edges, [Start | Path]) :- dfsRecursiveUndirectioned(Start, Goal, Edges, [], Path).


% Basisgeval
dfsRecursiveDirectioned(Goal, Goal, _, Visited, Result) :- 
  Visited \= [], % als we een loop zoeken, dan is Start==Begin en Visited==[Goal]
  reverse(Visited, Result).

% Recursief geval
dfsRecursiveDirectioned(Current, Goal, Edges, Visited, Path) :-
  member((Current, Next), Edges), % kies een volgende knoop
  (
    \+ member(Next, Visited)
  ), % volgende knoop is ofwel het eind, ofwel nog niet bezocht
  dfsRecursiveDirectioned(Next, Goal, Edges, [Next | Visited], Path).


% Basisgeval
dfsRecursiveUndirectioned(Goal, Goal, _, Visited, Result) :- 
  Visited \= [], % als we een loop zoeken, dan is Start==Begin en Visited==[Goal]
  reverse(Visited, Result).

% Recursief geval
dfsRecursiveUndirectioned(Current, Goal, Edges, Visited, Path) :-
  (
    member((Current, Next), Edges)
      ;
    member((Next, Current), Edges)
  ), % kies een volgende knoop
  (
    \+ member(Next, Visited)
  ),
  dfsRecursiveUndirectioned(Next, Goal, Edges, [Next | Visited], Path).


/*

?- get_edges(Edges), dfsUndirectioned(london, athens, Edges, Path).

    Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
    Path = [london, paris, rome, athens] ;

    Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
    Path = [london, dublin, edinburgh, london, paris, rome, athens] ;

    Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
    Path = [london, dublin, london, paris, rome, athens] ;

    Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
    Path = [london, edinburgh, london, paris, rome, athens] ;

    Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
    Path = [london, edinburgh, dublin, london, paris, rome, athens] ;

*/


% Zoek ALLE paden via findall
dfsAllDirectioned(Start, Goal, Edges, Paths) :-
    findall(Path, dfsRecursiveDirectioned(Start, Goal, Edges, [], Path), Paths).
dfsAllUndirectioned(Start, Goal, Edges, Paths) :-
    findall(Path, dfsRecursiveUndirectioned(Start, Goal, Edges, [], Path), Paths).

/*
  ?- get_edges(Edges), dfsAllUndirectioned(london, london, Edges, Paths).
  Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
  Paths = [[london, paris, london], [london, dublin, edinburgh, london], [london, dublin, london], [london, edinburgh, london], [london, edinburgh, dublin, london]].

  ?- get_edges(Edges), dfsAllDirectioned(london, london, Edges, Paths).
  Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
  Paths = [[london, dublin, edinburgh, london]].
*/







%%%%%%%%%%%%%%%%%%%%%
%%% BREEDTE-EERST %%%
%%%%%%%%%%%%%%%%%%%%%


bfsDirectioned(Start, Goal, Edges, Path) :- 
  Queue = [[Start] | Qtail] - Qtail,
  bfsDirectionedRecursive(Queue, Goal, Edges, [], Path).


% Basisgeval: doel bereikt
bfsDirectionedRecursive([[Goal | RestOfPath] | _] - _, Goal, _, _, Path) :-
  reverse([Goal | RestOfPath], Path),
  !. % stop als je het kortste pad hebt gevonden

bfsDirectionedRecursive([CurrentPath | OtherPaths] - Qtail, Goal, Edges, Visited, Path) :-

  % Haal de huidige node van de front van de queue
  CurrentPath = [CurrentNode | _],

  % Update de bezochte paden
  NewVisited = [CurrentNode | Visited],

  % Vind alle mogelijke onbezochte knopen
  findall(
      [NextNode | CurrentPath],
      (
          member((CurrentNode, NextNode), Edges),
          \+ member(NextNode, NewVisited)
      ),
      NewPaths
  ),

  %% Voeg de nieuwe paden toe aan het einde van de queue
  
  % Onderstaande werkt niet, aangezien NewPaths een lijst van lijsten is!
  % Qtail = [ NewPaths | NewQtail],

  % Daarom moeten we de lijst van lijsten eerst afvlakken....
  append(NewPaths, NewQtail, Qtail),

  % Nieuwe queue: 
  % (1) huidig pad is gepopt
  % (2) nieuwe paden zijn gepusht vanachter (zie hierboven)
  NewQ = OtherPaths - NewQtail,

  % Recursive call with the updated queue and visited nodes
  bfsDirectionedRecursive(NewQ, Goal, Edges, NewVisited, Path).




/* VOORBEELD
?- get_edges(Edges), bfsDirectioned(london, berlin, Edges, Path).
Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
Path = [london, paris, berlin].
*/




/*

TEST: wat gebeurt er als je van/op de queue popt/pusht

Q = [[1,2,3],[3,4] | Tail]-Tail, 
[CurrentPath | OtherPaths] - Qtail = Q, 
CurrentPath = [CurrentNode | _], 
Qtail = [[[6 | CurrentPath], [9 | CurrentPath]] | NewTail], 
NewQ = OtherPaths - NewTail.

?- Q = [[1,2,3],[3,4] | Tail]-Tail, 
|    [CurrentPath | OtherPaths] - Qtail = Q, 
|    CurrentPath = [CurrentNode | _], 
|    Qtail = [[[6 | CurrentPath], [9 | CurrentPath]] | NewTail], 
|    NewQ = OtherPaths - NewTail.

Q = [[1, 2, 3], [3, 4], [[6, 1, 2, 3], [9, 1, 2|...]]|NewTail]-[[[6, 1, 2, 3], [9, 1, 2, 3]]|NewTail],
Tail = Qtail, Qtail = [[[6, 1, 2, 3], [9, 1, 2, 3]]|NewTail],
CurrentPath = [1, 2, 3],
OtherPaths = [[3, 4], [[6, 1, 2, 3], [9, 1, 2, 3]]|NewTail],
CurrentNode = 1,
NewQ = [[3, 4], [[6, 1, 2, 3], [9, 1, 2, 3]]|NewTail]-NewTail.

*/