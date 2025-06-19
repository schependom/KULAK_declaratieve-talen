val(X) :-
  atom(X).

uitdrukking(val(_)).
uitdrukking(plus(X,Y)) :-
  uitdrukking(X),
  uitdrukking(Y).
uitdrukking(maal(X,Y)) :-
  uitdrukking(X),
  uitdrukking(Y).

infix(val(X), [X]).
infix(maal(X,Y), Res) :-
  infix(X, ResX),
  infix(Y, ResY),
  append(ResX, ["x" | ResY], Res).
infix(plus(X,Y), Res) :-
  infix(X, ResX),
  infix(Y, ResY),
  append(ResX, ["+" | ResY], Res).

infixprior(val(X), [X]).
infixprior(maal(X,Y), Res) :-
  infixprior(X, ResX),
  infixprior(Y, ResY),
  append(ResY, [")"], YMetHaakje),
  append(ResX, ["x", "(" | YMetHaakje], Res).
infixprior(plus(X,Y), Res) :-
  infixprior(X, ResX),
  infixprior(Y, ResY),
  append(ResX, ["+" | ResY], Res).




infixaccu(Expression, Result) :-
    infixaccu(Expression, [], Result).
infixaccu(val(X), Acc, [X | Acc]).
infixaccu(maal(X, Y), Acc, Result) :-
    infixaccu(Y, Acc, Acc1),
    infixaccu(X, ["x" | Acc1], Result).
infixaccu(plus(X, Y), Acc, Result) :-
    infixaccu(Y, Acc, Acc1),
    infixaccu(X, ["+" | Acc1], Result).




infixprioraccu(Expression, Result) :-
    infixprioraccu(Expression, [], Result).
infixprioraccu(val(X), Acc, [X | Acc]).
infixprioraccu(maal(X, Y), Acc, Result) :-
    infixprioraccu(Y, [")" | Acc], Acc1),
    infixprioraccu(X, ["x", "(" | Acc1], Result).
infixprioraccu(plus(X, Y), Acc, Result) :-
    infixprioraccu(Y, Acc, Acc1),
    infixprioraccu(X, ["+" | Acc1], Result).



myAppend([], L, L).
myAppend([X | Rest], L2, [X | AppendRestL2]) :-
  myAppend(Rest, L2, AppendRestL2).


permute([], []).
permute([X|Rest], L) :-
    permute(Rest, PermutedRest),
    select(X, L, PermutedRest).

% select/3
mySelect(X, [X|R], R).
mySelect(X, [Y|R], [Y|S]) :-
  mySelect(X, R, S).

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
