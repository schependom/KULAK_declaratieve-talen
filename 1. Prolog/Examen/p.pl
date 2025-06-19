% De opgave is gewoon...
% een onleesbare versie van reverse/2!
p([], []) :- !.
p([X|RX], R) :- p(RX, L), p(X, L, R).

% Leesbaarder
reverseWeird([],[]).
reverseWeird([H | Tail], Resultaat) :-
  reverseWeird(Tail, ReversedTail),
  insertAtEnd(H, ReversedTail, Resultaat).


%% p(X, Rij, Result)
% Onleesbare hulpfunctie die het volgende doet:
%     Insert element X op het einde Rij
%     om Result te verkrijgen
p(X, [], [X]) :- !.
p(X, [Y|L], [Y|R]) :- p(X, L, R).

% Leesbaarder
insertAtEnd(X, [], [X]) :- !.
insertAtEnd(X, [Y|L], [Y|R]) :- insertAtEnd(X, L, R).




/*
?- time(p([1,2,3,4,5],X)).
% 20 inferences, 0.000 CPU in 0.000 seconds (66% CPU, 2500000 Lips)
X = [5,4,3,2,1].

?- time(reverse([1,2,3,4,5],X)).
% 5 inferences, 0.000 CPU in 0.000 seconds (56% CPU, 217391 Lips)
X = [5,4,3,2,1].
*/






% Eigen permute functie =\= functie hierboven!
permuteOwn([],[]).
permuteOwn([H | Rest], ResultPermutation) :-
  permute(Rest, PermutedRest),                % permuteer de tail
  select(H, ResultPermutation, PermutedRest). % zoek alle mogelijke posities om head te inserten in de permutatie van de tail

permuteVoorFreeze([], []).
permuteVoorFreeze(Lijst, [Kandidaat | KleinerePermutatie]) :-
  select(Kandidaat, Lijst, KleinereLijst),
  permuteVoorFreeze(KleinereLijst, KleinerePermutatie).





