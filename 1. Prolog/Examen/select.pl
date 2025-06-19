% select/3
mySelect(X, [X|R], R).
mySelect(X, [Y|R], [Y|S]) :-
  mySelect(X, R, S).

% Met cut enkel het eerste voorkomen weg
myDeleteFirst(X, [X|R], R) :- !.
myDeleteFirst(X, [Y|R], [Y|S]) :-
  myDeleteFirst(X, R, S).

% delete/3
myDeleteAll(_, [], []).
myDeleteAll(X, [X|XS], RestDel) :-
  myDeleteAll(X, XS, RestDel).
myDeleteAll(X, [Y|XS], [Y|RestDel]) :-
  X \= Y,
  myDeleteAll(X, XS, RestDel).
