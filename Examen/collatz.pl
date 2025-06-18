%%%%%%%%%
%%% 1 %%%
%%%%%%%%%

%% collatz(+X, -CR)
%
% X is een natuurlijk getal
%
% CR is de rij natuurlijke getallen beginnende bij X
% en eindigende bij het eerste element dat een tweede keer
% voorkomt.

% natuurlijk getal
natuurlijkStrikt(X) :- integer(X), X>0.

collatz(X, CR) :- lemma_collatz(X,CR), !.

collatz(X, CR) :-
  natuurlijkStrikt(X),
  collatzRec([X], CR),
  assert(lemma_collatz(X,CR)). % onthou de rij

%% collatzRec(+AccRes, +AlGehad, -Resultaat)
collatzRec([H | T], Res) :-
  opvolger(H, Opvolger),
  (
    % Als getal al in de rij voorkomt ...
    member(Opvolger, [H | T]) ->
      reverse([Opvolger, H | T], Res) % ... is de rij af.
    ;
      collatzRec([Opvolger, H | T], Res)
  ).

even(X) :- X mod 2 =:= 0.

%% opvolger(+Y, -Opvolger)
% Even
opvolger(Y, Z) :-
  even(Y),
  Z is Y div 2, 
  !. % Als Y even is, is Y niet oneven.
% Oneven
opvolger(Y, Z) :-
  Z is 3 * Y + 1,
  !. % Als Y oneven is, is Y niet even.



%%%%%%%%%
%%% 2 %%%
%%%%%%%%%

modulo47Step(Y, Z) :- 
  (Y mod 47 =:= 0 ->
  Z is Y - 46 ;
  succ(Y, Z)).

%% collatzAchtig(+P, +X, -L)
%
% P is een predicaat P(+Y, -Z) (Collatz-achtige regel)

collatzAchtig(P, X, L) :- lemma_collatz_achtig(P,X,L), !.

collatzAchtig(P, X, L) :-
  natuurlijkStrikt(X),
  collatzAchtigRec(P, [X], L),
  assert(lemma_collatz_achtig(P,X,L)). % onthou de rij

collatzAchtigRec(P, [H | T], Res) :-
  BerekenOpvolger =.. [P, H, Opvolger],
  call(BerekenOpvolger),
  (
    % Als getal al in de rij voorkomt ...
    member(Opvolger, [H | T]) ->
      reverse([Opvolger, H | T], Res) % ... is de rij af.
    ;
      collatzAchtigRec(P, [Opvolger, H | T], Res)
  ).




%%%%%%%%%
%%% 3 %%%
%%%%%%%%%

:- dynamic lemma_collatz/2.
:- dynamic lemma_collatz_achtig/3.

% Bij elke oproep aan collatz/2 of collatzAchtig/3:
%   - Controleer of het lemma bestaat
%   - Als het bestaat, gebruik het EN CUT!!



%% present(+X, ?Lijst)
% True if X occurs in List-Tail, but not in Tail.
present(X, List-Tail) :-
  \+ occurs_in_tail(X, Tail),
  occurs_in_diff_list(X, List, Tail).

%% occurs_in_diff_list(+X, +List, +Tail)
% True if X syntactically occurs in List before reaching Tail.
occurs_in_diff_list(X, [H|_], _) :-
  X == H, !.
occurs_in_diff_list(X, [H|Rest], Tail) :-
  [H|Rest] \== Tail,
  occurs_in_diff_list(X, Rest, Tail).

%% occurs_in_tail(+X, +Tail)
% True if X syntactically occurs in the tail.
occurs_in_tail(_, Tail) :-
  var(Tail), !, fail.       % Open tail — assume X not in it
occurs_in_tail(_, []) :- !, fail.
occurs_in_tail(X, [Y|Rest]) :-
  X == Y, !.
occurs_in_tail(X, [_|Rest]) :-
  occurs_in_tail(X, Rest).






collatzLike(P, X, L) :-
  lemma_collatz_achtig(P, X, L), !.

collatzLike(P, X, Res) :-
  natuurlijkStrikt(X),
  DL = [X|Tail]-Tail,
  collatz_rec(P, DL, Res),
  assert(lemma_collatz_achtig(P, X, Res)).

%% collatzLikeRec(Predicaat, DifferenceList, Res)
collatz_rec(P, Head-Tail, Result) :-
  Head = [H|_],
  call(P, H, Next),
  (   present(Next, Head-Tail)
  ->  Tail = [Next|NewTail],
      Result = Head-NewTail
  ;   Tail = [Next|NewTail],
      NextDL = Head-NewTail,
      collatz_rec(P, NextDL, Result)
  ).



