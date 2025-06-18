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

% T is een variabele
present(N, L-T) :- 
  var(T), !,          % als de tail een veranderlijke is, dan cutten we
  member(N, L),!,     % als N voorkomt in de lijst L, of als L een variabele is, dan cutten we
  var(T).             % ! Als N niet aanwezig is in de veranderlijke L, zal member als generator werken en N toevoegen aan T (dan komt N dus niet voor in het verschil) -> T is dan geen vrije veranderlijke meer

% Er zijn elementen aanwezig in T
present(N, L-[THead|TTail]) :- 
  THead \= N, % N mag niet voorkomen in de tail
  present(N, L-TTail).

/*
?- present(10, T-T).
false.

?- present(10, [1,2,3|R]-R).
false.

?- present(10, [1,2,3,10|R]-R).
true.

?- present(10, [1,2,3,10]-[10]).
false.
*/



collatzLike(P, X, L) :-
  lemma_collatz_achtig(P, X, L), !.

collatzLike(P, X, Res) :-
  natuurlijkStrikt(X),
  collatzLike_rec(P, X, [X | T]-T, Res),
  assert(lemma_collatz_achtig(P, X, Res)).



%% collatzLike(Predicaat, Huidig, DifferenceList, Resultaat)

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



/*
?- collatzLike(modulo47Step,100,T-T),print(T-T).
@(S_1-S_1,[S_1=[100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,95,96,97,98,99,100|S_1]])
T = [100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,95,96,97,98,99,100|$VAR(T)].

?- collatzLike(modulo47Step,100,R),print(R).
[100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,95,96,97,98,99,100|_13372]-_13372
R = [100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,95,96,97,98,99,100|_13372]-_13372.
*/


