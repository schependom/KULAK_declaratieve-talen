% Tail recursieve versie
som_tr(A, B, C) :-
  reverse(A, RevA),
  reverse(B, RevB),
  somBitsMetCarry(RevA, RevB, 0, [], C).




somBitsMetCarry_tr([], [], Carry, Acc, Res) :-
  (
    Carry =:= 1       ->
    Res = [1 | Acc]   ;
    Res = Acc
  ).

  somBitsMetCarry_tr([],[],0,Acc,[0|Rest]) :- somBitsMetCarry_tr([],[],0,Acc,Rest).

somBitsMetCarry_tr([A | RestA], [], Carry, Acc, Res) :-
  somBitsMetCarry_tr([A | RestA], [0], Carry, Acc, Res).
somBitsMetCarry_tr([], [B | RestB], Carry, Acc, Res) :-
  somBitsMetCarry_tr([0], [B | RestB], Carry, Acc, Res).

somBitsMetCarry_tr([A | RestA], [B | RestB], Carry, Acc, Res) :-
  Som is A + B + Carry,
  SomCor is Som mod 2,
  NewCarry is Som // 2,
  somBitsMetCarry_tr(RestA, RestB, NewCarry, [SomCor | Acc], Res).




% Non-tail recursive

som(A, B, C) :-
  reverse(A, RevA),
  reverse(B, RevB),
  somBitsMetCarry(RevA, RevB, 0, RevC),
  reverse(RevC, C).

somBitsMetCarry([], [], 0, []).
somBitsMetCarry([], [], 1, [1]).

somBitsMetCarry([A | RestA], [], Carry, Acc) :-
  somBitsMetCarry([A | RestA], [0], Carry, Acc).
somBitsMetCarry([], [B | RestB], Carry, Acc) :-
  somBitsMetCarry([0], [B | RestB], Carry, Acc).

somBitsMetCarry([A | RestA], [B | RestB], Carry, [SomCor | RestSom]) :-
  Som is A + B + Carry,
  SomCor is Som mod 2,
  NewCarry is Som // 2,
  somBitsMetCarry(RestA, RestB, NewCarry, RestSom).




% Base case: empty list stays empty
revBinary([], Res, Res).

% Skip leading 0s (but not if the whole list is just [0])
revBinary([H | T], [], Res) :-
    nonvar(H), H =:= 0,
    T \= [],
    revBinary(T, [], Res).

% Accept 0 or 1 into the accumulator
revBinary([H | T], Acc, Res) :-
    member(H, [0, 1]),
    revBinary(T, [H | Acc], Res).

somver(A, B, C) :-
  revBinary(A, [], RevA),
  revBinary(B, [], RevB),
  somBitsMetCarry_tr(RevA, RevB, 0, [], C).