% [...|B]-B ++ [...|D]-D = [...|D]-D
% ...A...-B ++ ...C...-D = ...A...-D
dl_append(A-B,C-D,A-D) :- B=C.

/*

?- dl_append([1,2,3|A]-A, [4,5,6|B]-B, Resultaat).
A = [4, 5, 6 | B],
Resultaat = [1, 2, 3, 4, 5, 6 | B]-B.

?- X = [1,2,3 | [3,2,1]].
X = [1, 2, 3, 3, 2, 1].

?- X = [1,2 | [3,4|C]].
X = [1, 2, 3, 4 | C].

*/





test_append :- 
  dl_append([1,2,3|A]-A, [4,5,6|B]-B, ResultaatLijst-_),
  is_list(ResultaatLijst).

/*
[trace]  ?- test_append.
   Call: (12) test_append ? creep
   Call: (13) dl_append([1, 2, 3|_47742]-_47742, [4, 5, 6|_47766]-_47766, _47788-_47790) ? creep
   Call: (14) _47742=[4, 5, 6|_47766] ? creep
   Exit: (14) [4, 5, 6|_47766]=[4, 5, 6|_47766] ? creep
   Exit: (13) dl_append([1, 2, 3, 4, 5, 6|_47766]-[4, 5, 6|_47766], [4, 5, 6|_47766]-_47766, [1, 2, 3, 4, 5, 6|_47766]-_47766) ? creep
   Call: (13) is_list([1, 2, 3, 4, 5, 6|_47766]) ? creep
   ! Fail: (13) is_list([1, 2, 3, 4, 5, 6|_47766]) ? creep
   Fail: (12) test_append ? creep
false.
*/


%%%%%%%%%
% We zien dus dat [1, 2, 3, 4, 5, 6|_47766] GEEN lijst is!
%
% De interne voorstelling is
%     .(1, .(2, .(3, .(4, .(5, .(6, _47766))))))
%
% ! Maar lijsten moeten altijd eindigen op [], niet op een variabele!
% Zie hieronder.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interne implementatie van is_list/1 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Negation by failure
is_list_own(X) :-
  var(X), 
  !, fail.
% Basisgeval
is_list_own([]).
% Recursief Geval
is_list_own([_|T]) :-
  is_list(T).

% Alternatieve notatie negation by failure
is_list_alt(X) :-
  \+ var(X).

/*
?- is_list([1,2,3]).
true.

?- is_list([1,2,3 | Var]).
! false.

?- is_list(Var).
! false.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







%%%%%%%%%%%%%%
%%% QUEUES %%%
%%%%%%%%%%%%%%

q_init(X-X).

% append([1,2|_A]-_A, X, [1,2 | [X|_B]]-_B).

%% Voeg X achteraan de queue toe
% [...|OldTail]-OldTail
% .....Old.....-OldTail
q_push(Lijst-OldTail, X, Lijst-NewTail) :-
  OldTail = [X | NewTail].

%% Verwijder X vooraan de queue
q_pop(Lijst-Tail, X, Rest-Tail) :-
  Lijst = [X | Rest].


/* VOORBEELD

?- q_init(X).
X = _A-_A.

?- q_init(X), q_push(X,1,NewX).
X = [1|_A]-[1|_A],
NewX = [1|_A]-_A.

?- q_init(X), q_push(X,1,NewX), q_push(NewX, 2, NewNewX).
X = [1, 2|_A]-[1, 2|_A],
NewX = [1, 2|_A]-[2|_A],
NewNewX = [1, 2|_A]-_A.

?- q_init(X), q_push(X, 1, NewX), q_push(NewX, 2, NewNewX), q_pop(NewNewX, Y, FinalX).
X = [1, 2|_A]-[1, 2|_A],
NewX = [1, 2|_A]-[2|_A],
NewNewX = [1, 2|_A]-_A,
Y = 1,
FinalX = [2|_A]-_A.

*/