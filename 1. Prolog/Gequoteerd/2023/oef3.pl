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





%% solve_binary(+Matrix)
% Fill variables with 0/1 and enforce row/column constraints
solve_binary(Matrix) :-
  fill_rows(Matrix),
  transpose_matrix(Matrix, Transposed),
  show(x, Transposed),
  all_rows_valid(Matrix),     % rijen checken
  all_rows_valid(Transposed). % kolommen checken

all_rows_valid([]).
all_rows_valid([Row|Rest]) :-
  valid_line(Row),
  all_rows_valid(Rest).

%% fill_rows(+Rows)
fill_rows([]).
fill_rows([Row|Rest]) :-
  fill_row(Row),
  fill_rows(Rest).

%% fill_row(+Row)
fill_row([]).
fill_row([Cell|Rest]) :-
  %var(Cell),
  member(Cell, [0,1]),
  fill_row(Rest).

%% valid_line(+List)
% equal number of 0 and 1, and no three identical in a row
valid_line(List) :-
  length(List, N), 
  N mod 2 =:= 0,
  Half is N // 2,
  count_elem(0, List, 0, Half),
  count_elem(1, List, 0, Half),
  \+ has_three(List).

count_elem(_, [], Count, Count).
count_elem(Elem, [H | T], Acc, Count) :-
  (
      H == Elem
  ->  NewAcc is Acc + 1
  ;   NewAcc = Acc
  ),
  count_elem(Elem, T, NewAcc, Count).

has_three([X,X,X|_]) :- (X==0; X==1), !.
has_three([_|T]) :- has_three(T).

%% Example puzzle 1: 4x4 partial binary
binary_0(Matrix) :-
    matrix(4*4, Matrix),
    Matrix = [ [1,1,_,_] ,
               [1,_,1,_] ,
               [_,_,0,_] ,
               [_,0,_,1] ].

%% Example puzzle 2: 6x6 partial binary
binary_1(Matrix) :-
    matrix(6*6, Matrix),
    Matrix = [ [1,1,_,_,_,_] ,
               [1,_,1,_,_,1] ,
               [_,1,1,_,_,_] ,
               [1,_,_,_,_,1] ,
               [_,_,_,_,_,1] ,
               [_,_,_,_,_,_] ].