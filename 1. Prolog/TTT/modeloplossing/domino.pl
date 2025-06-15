solve_domino([],_) :- !.
solve_domino([(X, X)],_) :- !.
solve_domino(X,Y) :- solve_domino_helper(X, Y), !, circle(Y).

solve_domino_helper([], []).
solve_domino_helper([A], [A]).
solve_domino_helper(Pieces, [(A, B), (B, C)|Chain]) :- select((A, B), Pieces, Rest), solve_domino_helper(Rest, [(B, C)|Chain]).
solve_domino_helper(Pieces, [(A, B), (B, C)|Chain]) :- select((B, A), Pieces, Rest), solve_domino_helper(Rest, [(B, C)|Chain]).

circle([(A,_)|R]) :- last(R, (_, A)).

subset_own([], []).
subset_own([E|Tail], [E|NTail]):-
  subset_own(Tail, NTail).
subset_own([_|Tail], NTail):-
  subset_own(Tail, NTail).

% Try all subsets and return the longest valid chain
longest_domino_chain(Dominos, LongestChain) :-
    findall(Chain, (subset_own(Dominos, Subset), solve_domino(Subset, Chain)), Chains),
    max_length_chain(Chains, LongestChain),!.

% Find the longest chain from a list of chains
max_length_chain([H|T], Max) :- max_length_chain(T, H, Max).

max_length_chain([], Max, Max).
max_length_chain([H|T], CurrentMax, Max) :-
    length(H, LenH),
    length(CurrentMax, LenMax),
    ( LenH > LenMax -> max_length_chain(T, H, Max)
    ; max_length_chain(T, CurrentMax, Max)
    ).
