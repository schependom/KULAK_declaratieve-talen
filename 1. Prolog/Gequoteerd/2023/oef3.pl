%% !!

matrix(Rows*Cols, Matrix) :-
    make_rows(Rows, Cols, Matrix).

make_rows(0, _, []) :- !.
make_rows(R, Cols, [Row | Rest]) :-
    length(Row, Cols),
    R1 is R - 1,
    make_rows(R1, Cols, Rest).



length_is_zero(List) :-
    length(List, 0).

transpose_matrix([], []).
transpose_matrix(Matrix, []) :-
    Matrix \== [], % Ensure it's not the first base case (empty matrix)
    maplist(length_is_zero, Matrix). % All rows must be empty lists (length 0)

% transposeMatrix(  [1,2],
%                   [3,4],
%                   [5,6],
%                   [7,8],  Transposed)
transpose_matrix(Rijen, [EersteNieuweRij | RestNieuweRijen]) :-
  % De eerste nieuwe rij bestaat uit de eerste kolom van de matrix:
  getHeads(Rijen, EersteNieuweRij),
  % De andere nieuwe rijen bestaan uit de tweede, derde, ... kolom
  % De tails is een lijst van lijsten (opnieuw een matrix)
  % Als we die transponeren, krijgen we de rest van de nieuwe rijen.
  getTails(Rijen, Staarten),
  transpose_matrix(Staarten, RestNieuweRijen).

% getHeads([], Acc, HeadsRes) :- reverse(Acc, HeadsRes).
% getHeads([ [H | _] | Rest ], Acc, Res) :- 
%   getHeads(Rest, [H | Acc], Res).

% getTails([], Acc, TailsRes) :- reverse(Acc, TailsRes).
% getTails([ [_ | T] | Rest ], Acc, Res) :- 
%   getTails(Rest, [T | Acc], Res).

getHeads([], []).
getHeads([ [H | _] | Rest ], [H | RestHeads]) :- 
  getHeads(Rest, RestHeads).

getTails([], []).
getTails([ [_ | T] | Rest ], [T | RestTails]) :- 
  getTails(Rest, RestTails).



show(_, []).
show(X, [HeadRij | RestRijen]) :-
  nonvar(X),
  printRij(HeadRij, X),
  nl,
  show(X, RestRijen).

printRij([], _).
printRij([H | T], X) :-
  (
      var(H) 
    ->
      print(X)
    ;
      print(H)
  ),
  printRij(T, X).