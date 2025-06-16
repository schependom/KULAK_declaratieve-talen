%% Dfs(+StartNode, +EndNode, +Edges, -Paths)
% Vindt een pad van StartNode naar GoalNode gebruikmakend van Depth-First Search.
% Paths is de output en is een lijst van lijsten van knopen van StartNode naar GoalNode.
% Edges is een lijst van (niet-)gerichte bogen, voorgesteld als een lijst van tupels, bv. [(a,b), (b,c)].

% Edges is een lijst van GERICHTE!! bogen
dfsDirected(Start, Goal, Edges, [Start | Path]) :- dfsRecursiveDirected(Start, Goal, Edges, [], Path).
% Edges is een lijst van NIET-GERICHTE!! bogen.
dfsUndirected(Start, Goal, Edges, [Start | Path]) :- dfsRecursiveUndirected(Start, Goal, Edges, [], Path).

% Basisgeval
dfsRecursiveDirected(Goal, Goal, _, Visited, Result) :- 
  Visited \= [], % als we een loop zoeken, dan is Start==Begin en Visited==[Goal]
  reverse(Visited, Result).

% Recursief geval
dfsRecursiveDirected(Current, Goal, Edges, Visited, Path) :-
  member((Current, Next), Edges), % kies een volgende knoop
  (
    \+ member(Next, Visited)
  ), % volgende knoop is ofwel het eind, ofwel nog niet bezocht
  dfsRecursiveDirected(Next, Goal, Edges, [Next | Visited], Path).

% Basisgeval
dfsRecursiveUndirected(Goal, Goal, _, Visited, Result) :- 
  Visited \= [], % als we een loop zoeken, dan is Start==Begin en Visited==[Goal]
  reverse(Visited, Result).

% Recursief geval
dfsRecursiveUndirected(Current, Goal, Edges, Visited, Path) :-
  (
    member((Current, Next), Edges)
      ;
    member((Next, Current), Edges)
  ), % kies een volgende knoop
  (
    \+ member(Next, Visited)
  ),
  dfsRecursiveUndirected(Next, Goal, Edges, [Next | Visited], Path).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% VOORBEELD ZOEKPROBLEEM

% Define the city connections as facts (undirected for simplicity)
connection(london, paris).
connection(london, dublin).
connection(paris, rome).
connection(paris, berlin).
connection(rome, athens).
connection(berlin, warsaw).
connection(dublin, edinburgh).
connection(edinburgh, london). % A cycle!

% Helper to get all edges for the graph predicates
get_edges(Edges) :-
    findall((C1, C2), connection(C1, C2), Edges).

/*

?- get_edges(Edges), dfsUndirected(london, athens, Edges, Path).

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



% Entry point for directed DFS, finds all possible paths.
% Paths might contain duplicates if different traversal sequences lead to the same node sequence.
dfsAllDirected(Start, Goal, Edges, Paths) :-
    findall(Path, dfsRecursiveDirected(Start, Goal, Edges, [], Path), Paths).

% Entry point for undirected DFS, finds all possible paths.
% Paths might contain duplicates if different traversal sequences lead to the same node sequence.
dfsAllUndirected(Start, Goal, Edges, Paths) :-
    findall(Path, dfsRecursiveUndirected(Start, Goal, Edges, [], Path), Paths).

/*
  ?- get_edges(Edges), dfsAllUndirected(london, london, Edges, Paths).
  Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
  Paths = [[london, paris, london], [london, dublin, edinburgh, london], [london, dublin, london], [london, edinburgh, london], [london, edinburgh, dublin, london]].

  ?- get_edges(Edges), dfsAllDirected(london, london, Edges, Paths).
  Edges = [(london, paris), (london, dublin), (paris, rome), (paris, berlin), (rome, athens), (berlin, warsaw), (dublin, edinburgh), (edinburgh, london)],
  Paths = [[london, dublin, edinburgh, london]].
*/



bfsDirectioned(Start, Goal, Edges, Path) :- 
  Queue = [[Start] | Qtail] - Qtail,
  bfsDirectionedRecursive(Queue, Goal, Edges, [], Path).

% Basisgeval: doel bereikt
bfsDirectionedRecursive([[Goal | RestOfPath] | _] - _, Goal, _, _, Path) :-
  reverse([Goal | RestOfPath], Path).

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
          (
            (\+ member(NextNode, NewVisited)) ;
            NextNode == Goal
          )
      ),
      NewPaths
  ),

  % Voeg de nieuwe paden toe aan het einde van de queue
  Qtail = [ NewPaths | NewQtail],
  % Nieuwe queue: 
  % (1) huidig pad is gepopt
  % (2) nieuwe paden zijn gepusht vanachter (zie hierboven)
  NewQ = OtherPaths - NewQtail,

  % Recursive call with the updated queue and visited nodes
  bfsDirectionedRecursive(NewQ, Goal, Edges, NewVisited, Path).




/*

TEST

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