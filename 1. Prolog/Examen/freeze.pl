% freeze(X, doel(X)) voert doel(X) uit zodra X niet meer vrij is!

% Bereken eerst de volledige permutatie en bekijk daarna of die gesorteerd is
psort(L, Sorted) :-
  myPermute(L, Sorted),
  sorted(Sorted).

sorted([_]).
sorted([X,Y | Rest]) :-
  X =< Y,
  sorted([Y | Rest]).





%% mySelect(X, L, NewL)
mySelect(X, [X | Rest], Rest).
mySelect(X, [Y | Rest], [Y | RestSelect]) :-
  mySelect(X, Rest, RestSelect).

%% myPermute(L, Permuted)
myPermute([], []).
myPermute(Lijst, [X | RestPermuted]) :-
  mySelect(X, Lijst, NewLijst),
  myPermute(NewLijst, RestPermuted).





% Genereer een deel van de permutatie en check along the way of de termen gesorteerd
% zijn -> performatiewinst!!
psortFreeze(L, Sorted) :-
  sortedOnTheGo(Sorted),
  myPermute(L, Sorted).         % genereer permutatie

% Elke keer een nieuwe veranderlijke gebonden wordt in de permutatie,
% wordt het doel in de freeze(X,doel(X)) opgeroepen.

% Zo kunnen we vroegtijdig foutieve permutaties stopzetten.

sortedOnTheGo([_]).
sortedOnTheGo([X,Y | Rest]) :-
  freeze(
    Y, 
    (
      X =< Y,                   % als Y gebonden is, is X dat ook -> check of X =< Y
      sortedOnTheGo([Y | Rest]) % Nu is Y gelijk aan X in de clause head en wachten we tot de volgende (Z) gebonden is!
    )
  ).




/*
?- time(psort([1,2,3,4,5,6,7,8],R)).
% 30 inferences, 0.000 CPU in 0.000 seconds (70% CPU, 909091 Lips)
R = [1,2,3,4,5,6,7,8] ;
% 467,336 inferences, 0.044 CPU in 0.045 seconds (99% CPU, 10538640 Lips)
false.

?- time(psortFreeze([1,2,3,4,5,6,7,8],R)).
% 139 inferences, 0.000 CPU in 0.000 seconds (58% CPU, 1510870 Lips)
R = [1,2,3,4,5,6,7,8] ;
% 10,512 inferences, 0.002 CPU in 0.002 seconds (92% CPU, 5360530 Lips)
false.
*/