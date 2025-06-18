:- dynamic collatz/2.

collatz(N, L):-
    collatz(N, [], L),
    asserta(collatz(N, L)).  % We voegen het aan de start toe zodat dit eerst wordt gecheckt
collatz(N, CurrentList, List):-
    % N komt al voor in CurrentList
    member(N, CurrentList),
    % Stop en return het resultaat
    append(CurrentList, [N], List),
    !. % We cutten hier, deze voorwaarde moeten we dus niet meer checken bij de rest
collatz(N, CurrentList, List):-
    append(CurrentList, [N], NewCurrentList),
    X is N mod 2,
    ((X =:= 0,
    NewN is N div 2, !);
    (X \= 0,
    NewN is (3 * N) + 1)),
    collatz(NewN, NewCurrentList, List).


% Modulo47Step(+N, -Result)
modulo47Step(N, Result):-
    X is N mod 47,
    ((X =:= 0,
    N > 0,
    Result is N - 46, !);
    (X \= 0, 
    Result is N + 1)).

:- dynamic collatzAchtig/3.

collatzAchtig(P, X, T):-
    collatzAchtig(P, X, [], T),
    asserta(collatzAchtig(P, X, T)). % We voegen het aan de start toe zodat dit eerst wordt gecheckt
collatzAchtig(_, X, Acc, NewAcc):-
    member(X, Acc),
    append(Acc, [X], NewAcc),
    !. % We cutten hier, deze voorwaarde moeten we dus niet meer checken bij de rest
collatzAchtig(P, X, Acc, List):-
    append(Acc, [X], NewAcc),
    call(P, X, Y),
    collatzAchtig(P, Y, NewAcc, List).



present(N, R-T):- % T is een variabele
    var(T), !,
    member(N, R), !,
    var(T). % Als N niet aanwezig is in R, zal dit aan T toegevoegd worden -> T is dan geen var meer
present(N, R-[H|T]):- % Er zijn elementen aanwezig in T
    H \= N,
    present(N, R-T).


collatzLikeArne(P, X, Result):-
    collatzLike(P, X, L-L, Result).

collatzLikeArne(_, X, Head-CurTail, Head-ResTail):-
    present(X, Head-CurTail),
    CurTail = [X|ResTail],
    !. 

collatzLikeArne(P, X, Head-[Thead|Ttail], Result):-
    call(P, Thead, Z),
    collatzLike(P, Z, Head-Ttail, Result).