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
natuurlijk(X) :- integer(X), X>=0.

collatz(X, CR) :- lemma_collatz(X,CR), !.

collatz(X, CR) :-
  natuurlijk(X),
  collatzRec([X], [X], CR),
  assert(lemma_collatz(X,CR)). % onthou de rij

%% collatzRec(+AccRes, +AlGehad, -Resultaat)
collatzRec([H | T], AlGehad, Res) :-
  opvolger(H, Opvolger),
  (
    % Als getal al in de rij voorkomt ...
    member(Opvolger, AlGehad) ->
      reverse([Opvolger, H | T], Res) % ... is de rij af.
    ;
      collatzRec([Opvolger, H | T], [Opvolger | AlGehad], Res)
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
  natuurlijk(X),
  collatzAchtigRec(P, [X], [X], L),
  assert(lemma_collatz_achtig(P,X,L)). % onthou de rij

collatzAchtigRec(P, [H | T], AlGehad, Res) :-
  BerekenOpvolger =.. [P, H, Opvolger],
  call(BerekenOpvolger),
  (
    % Als getal al in de rij voorkomt ...
    member(Opvolger, AlGehad) ->
      reverse([Opvolger, H | T], Res) % ... is de rij af.
    ;
      collatzAchtigRec(P, [Opvolger, H | T], [Opvolger | AlGehad], Res)
  ).




%%%%%%%%%
%%% 3 %%%
%%%%%%%%%

:- dynamic lemma_collatz/2.
:- dynamic lemma_collatz_achtig/3.

% Bij elke oproep aan collatz/2 of collatzAchtig/3:
%   - Controleer of het lemma bestaat
%   - Als het bestaat, gebruik het EN CUT!!



%% present(+X, ?Ver)
present(X, L-T) :-
  not_in_tail(X, T),
  in_difference(X, L, T).

% Controleer of X in het verschil zit: loop tot je bij T bent
in_difference(X, [X|_], _) :- !.
in_difference(X, [Y|Rest], T) :-
  [Y|Rest] \== T,           % Stop als we bij de tail T zijn
  in_difference(X, Rest, T).

% Controleer dat X niet in de tail voorkomt
not_in_tail(_, []) :- !.
not_in_tail(X, [Y|Rest]) :-
  X \== Y,
  not_in_tail(X, Rest).