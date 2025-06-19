% ham(+Edges, -Cycle)
% Finds a Hamiltonian cycle in a graph represented by Edges.
% Edges are represented as b(Node1, Node2).
ham(Graaf, Resultaat) :-
  get_all_vertices(Graaf, Nodes),
  Nodes = [Start | _],                 % Choose the smallest named node as start
  length(Nodes, NumNodes),             % Get the total number of nodes
  % Call hamRec to find the path. We need to know the number of nodes
  % and ensure we visit all of them.
  hamRec(Start, Start, [Start], Graaf, NumNodes, RevRes),
  reverse(RevRes, Resultaat).

% hamRec(+StartNode, +CurrentNode, +PathSoFar, +Edges, +NumNodes, -FullPath)
% Base Case: We have visited all nodes and can return to the start node
hamRec(Start, Current, Path, Bogen, NumNodes, Path) :-
  length(Path, NumNodes),                 % Check if we have visited all nodes
  member(b(Current, Start), Bogen).       % Check if there's an edge from Current back to Start

% Recursive Case: Extend the path
hamRec(Start, Current, PathSoFar, Bogen, NumNodes, Resultaat) :-
  length(PathSoFar, LenPathSoFar),
  NumNodes > LenPathSoFar,           % Ensure we haven't visited all nodes yet
  member(b(Current, Next), Bogen),
  \+ member(Next, PathSoFar),             % Ensure Next has not been visited yet
  hamRec(Start, Next, [Next | PathSoFar], Bogen, NumNodes, Resultaat).




get_all_vertices(Edges, Vertices) :-
  findall(
    V,
    (
      member(b(V,_), Edges) ;
      member(b(_,V), Edges)  
    ),
    VerticesDuplicates
  ),
  sort(VerticesDuplicates, Vertices).





ham_bis(Graaf, Resultaat) :-
  get_all_vertices(Graaf, Vertices),
  Vertices = [Start | Rest],
  permutation(Rest, RestPerm),
  Resultaat = [Start | RestPerm],
  % Voorwaarden pas gecheckt als permutatie gegenereerd is.
  is_hamiltonian(Start, Resultaat, Graaf).

is_hamiltonian(Start, [Last], Bogen) :-
  member(b(Last, Start), Bogen).
is_hamiltonian(Start, [Current, Next | Rest], Bogen) :-
  member(b(Current, Next), Bogen),
  is_hamiltonian(Start, [Next | Rest], Bogen).



%% freeze(X, doel(X,[],Y))
%
% voer doel(X,[],Y) uit zodra X niet meer vrij is

ham2(Graaf, Pad) :-
  get_all_vertices(Graaf, AlleVertices),
  AlleVertices = [StartVertex | RestVertices],    % Pad begint met StartVertex

  % Check on the go of de (gedeeltelijke!) permutaties de 
  % condities niet schaadt.
  is_hamiltonian_on_the_go(StartVertex, PermutatieRest, Graaf),

  % Genereer permutatie van de resterende vertices en unificeer met Pad
  permutation(RestVertices, PermutatieRest),
  Pad = [StartVertex | PermutatieRest].


is_hamiltonian_on_the_go(Start, [Last], Bogen) :-
  member(b(Last, Start), Bogen).
is_hamiltonian_on_the_go(Start, [Current, Next | Rest], Bogen) :-
  % Voer check uit wanneer Next gebonden is
  freeze(
    Next,
    % De check:
    (
      member(b(Current, Next), Bogen),                      % dit wordt direct uitgevoerd
      is_hamiltonian_on_the_go(Start, [Next | Rest], Bogen) % hier gaan we opnieuw wachten tot de volgende (na next) is gebonden!)
    )
  ).




