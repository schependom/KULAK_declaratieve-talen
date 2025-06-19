% freeze(X, doel(X)) voert doel(X) uit zodra X niet meer vrij is!

% Bereken eerst de volledige permutatie en bekijk daarna of die gesorteerd is
psort(L, Sorted) :-
  permutation(L, Sorted),
  sorted(Sorted).

sorted([_]).
sorted([X,Y | Rest]) :-
  X =< Y,
  sorted([Y | Rest]).





%% mySelect(X, L, NewL)
mySelect(X, [X | Rest], Rest).
mySelect(X, [Y | Rest], [Y | RestSelect]) :-
  mySelect(X, Rest, RestSelect).

%! Onderstaande functie gebruiken om het verschil
%! aan te tonen in time/1 voor de freeze/2 versie van on the fly sorted check!
%
%! X wordt namelijk altijd voor de rest (dus ook Y) geinstantieerd!!
%! aangezien het geprepend wordt.
%
%% myPermute(L, Permuted)
myPermute([], []).
myPermute(Lijst, [X | RestPermuted]) :-
  % Verwijder een element uit de lijst,
  % voeg het toe aan de permutatie...
  mySelect(X, Lijst, NewLijst),
  % ...en permuteer de rest.
  myPermute(NewLijst, RestPermuted).

%% myPermute
%     Beter...
%     indien geen gebruik gemaakt van freeze/2!!
myBetterPermute([], []).
myBetterPermute([X | Rest], Permuted) :-
  myBetterPermute(Rest, RestPermuted),
  mySelect(X, Permuted, RestPermuted).

/*
?- time(findall(R, myBetterPermute([a,b,c,d,e,f,g],R), _)).
% 10,968 inferences, 0.003 CPU in 0.004 seconds (90% CPU, 3411509 Lips)
true.

?- time(findall(R, myPermute([a,b,c,d,e,f,g],R), _)).
% 46,145 inferences, 0.009 CPU in 0.009 seconds (95% CPU, 5376952 Lips)
true.
*/





% Genereer een deel van de permutatie en check along the way of de termen gesorteerd zijn 
% -> performatiewinst!!
psortFreeze(L, Sorted) :-
  sortedOnTheGo(Sorted),
  myPermute(L, Sorted).   % genereer permutatie

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
====================
met myBetterPermute:
====================

?- time(psort([5,1,10,20,-1,4,9,6],R)).
% 98,636 inferences, 0.013 CPU in 0.013 seconds (98% CPU, 7566431 Lips)
R = [-1,1,4,5,6,9,10,20] ;
% 86,170 inferences, 0.013 CPU in 0.013 seconds (99% CPU, 6877644 Lips)
false.

?- time(psortFreeze([5,1,10,20,-1,4,9,6],R)).
% 143,064 inferences, 0.018 CPU in 0.020 seconds (90% CPU, 7915897 Lips)
R = [-1,1,4,5,6,9,10,20] ;
% 112,681 inferences, 0.018 CPU in 0.020 seconds (90% CPU, 6332884 Lips)
false.

==============
met myPermute:
==============

?- time(psort([5,1,10,20,-1,4,9,6],R)).
% 242,893 inferences, 0.025 CPU in 0.026 seconds (98% CPU, 9589522 Lips)
R = [-1,1,4,5,6,9,10,20] ;
% 224,473 inferences, 0.025 CPU in 0.025 seconds (98% CPU, 9025492 Lips)
false.

?- time(psortFreeze([5,1,10,20,-1,4,9,6],R)).
% 5,484 inferences, 0.001 CPU in 0.001 seconds (91% CPU, 5232824 Lips)
R = [-1,1,4,5,6,9,10,20] ;
% 5,164 inferences, 0.001 CPU in 0.001 seconds (89% CPU, 6343980 Lips)
false.

!!! DUS: gebruik myPermute om performatiewinst te hebben met freeze !!!

==================
met permutation/2:
==================

?- time(psort([5,1,10,20,-1,4,9,6],R)).
% 221,787 inferences, 0.019 CPU in 0.020 seconds (99% CPU, 11530987 Lips)
R = [-1,1,4,5,6,9,10,20] ;
% 205,407 inferences, 0.021 CPU in 0.022 seconds (98% CPU, 9692210 Lips)
false.

?- time(psortFreeze([5,1,10,20,-1,4,9,6],R)).
% 5,093 inferences, 0.001 CPU in 0.001 seconds (90% CPU, 6588616 Lips)
R = [-1,1,4,5,6,9,10,20] ;
% 4,802 inferences, 0.001 CPU in 0.001 seconds (85% CPU, 6929293 Lips)
false.

*/