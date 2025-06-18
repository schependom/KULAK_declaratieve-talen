## Prolog

```prolog
% newline
nl
```

DFS

```prolog
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
```

Lemma's

```prolog
:- dynamic lemma_collatz/2.

% Bij elke oproep:
%   - Controleer of het lemma bestaat
%   - Als het bestaat, gebruik het EN CUT!!
```

Difference list

```prolog
% T is een variabele
present(N, L-T) :- 
  var(T), !,          % als de tail een veranderlijke is, dan cutten we
  member(N, L),!,     % als N voorkomt in de lijst L, of als L een variabele is, dan cutten we
  var(T).             % ! Als N niet aanwezig is in de veranderlijke L, zal member als generator werken en N toevoegen aan T (dan komt N dus niet voor in het verschil) -> T is dan geen vrije veranderlijke meer

% Er zijn elementen aanwezig in T
present(N, L-[THead|TTail]) :- 
  THead \= N, % N mag niet voorkomen in de tail
  present(N, L-TTail).

% Als huidig in DL zit, unificeer DL met resultaat
collatzLike(_, Huidig, Resultaat, Resultaat) :-
  present(Huidig, Resultaat),
  !.

% Als huidig NIET in DL zit, zoek volgende
collatzLike(P, Huidig, Head-OldTail, Resultaat) :-
  call(P, Huidig, Volgend),
  % Voeg Volgend toe aan het eind van de lijst
  OldTail = [Volgend | NewTail],
  collatzLike(P, Volgend, Head-NewTail, Resultaat).
```