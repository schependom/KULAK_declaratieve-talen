striktPositief(X) :-
  number(X),
  X>0.


% Bereken het volgende Collatz getal
volgend(N, Volgend) :-
  N mod 2 =:= 0,
  Volgend is N // 2.
volgend(N, Volgend) :-
  N mod 2 =\= 0,
  Volgend is 3*N + 1.


% Generator for natural numbers
num(1).
num(N) :- num(M), N is M+1.


% collatz_forward(N, M): N is gegeven
collatz_forward(1, 0).
collatz_forward(N, M) :-
  striktPositief(N),
  N \= 1, 
  volgend(N, NextN), 
  collatz_forward(NextN, M_rest), 
  M is M_rest + 1.

% Mode 1: N gegeven, M moet worden berekend.
collatz(N, M) :-
  nonvar(N),       
  var(M),                    
  striktPositief(N),       
  collatz_forward(N, M). 

% Mode 2: M is gegeven, N is te vinden.
collatz(N, M) :-
  nonvar(M),      
  var(N),        
  M >= 0,        
  (   M = 0 -> N = 1 
  ;   num(N),           
      collatz_forward(N, M) 
  ).

% Mode 3: Both N and M are given.
collatz(N, M) :-
  nonvar(N),
  nonvar(M),
  striktPositief(N), 
  M >= 0,          
  collatz_forward(N, M).