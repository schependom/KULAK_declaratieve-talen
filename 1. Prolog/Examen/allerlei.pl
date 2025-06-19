% newline
% nl






:- dynamic lemma_predicaat/2.

% Cut niet vergeten!
predicaat(X,Y) :- lemma_predicaat(X,Y), !.
predicaat(X,Y) :-
  print("De rest"),
  assert(lemma_predicaat(X,Y)).

% Bij elke oproep:
%   - Controleer of het lemma bestaat
%   - Als het bestaat, gebruik het EN CUT!!







dfs(Start, Goal, Edges, [Start | Path]) :- dfsRecursive(Start, Goal, Edges, [], Path).

% Basisgeval
dfsRecursive(Goal, Goal, _, Visited, Result) :- 
  Visited \= [], % als we een loop zoeken, dan is Start==Begin en Visited==[Goal]
  reverse(Visited, Result).

% Recursief geval
dfsRecursive(Current, Goal, Edges, Visited, Path) :-
  member((Current, Next), Edges), % kies een volgende knoop
  \+ member(Next, Visited),
  dfsRecursive(Next, Goal, Edges, [Next | Visited], Path).





%%%%%%%%%%%%%%%%%%%
%% STEADFASTNESS %%
%%%%%%%%%%%%%%%%%%%

/*
Steadfastness wil zeggen dat je een predicaat niet fout pad
op kan sturen door de output argumenten verkeerd in te vullen.
*/

%% myMax(X, Y, Maximum)
myMax(X, Y, X) :- X >= Y, !.
myMax(_, Y, Y).

myMaxSteadfast(X, Y, Maximum) :-
  X >= Y,
  !,
  Maximum = X.
myMaxSteadfast(_, Max, Max).

/*
?- myMax(100,10,X).
X = 100.

?- myMax(100,1000,X).
X = 1000.

?- myMax(100,1,1).
! true.
! KLOPT NIET!

?- myMaxSteadfast(100, 1, 1).
false.
! want nu wordt dit wél geunificeerd met de eerste
! clause en daarna wordt er gecut (kan niet naar tweede clause).
*/