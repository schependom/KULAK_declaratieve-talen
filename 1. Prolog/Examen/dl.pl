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







%%%%%%%%%%%%%%
%%% MEMBER %%%
%%%%%%%%%%%%%%

% T is een variabele
present(N, L-T) :- 
  var(T), !,          % als de tail een veranderlijke is, dan cutten we
  member(N, L),       % als N voorkomt in de lijst L, of als L een variabele is, dan cutten we
  var(T).             % ! Als N niet aanwezig is in de veranderlijke L, zal member als generator werken en N toevoegen aan T (dan komt N dus niet voor in het verschil) -> T is dan geen vrije veranderlijke meer

% Er zijn elementen aanwezig in T
present(N, L-[THead|TTail]) :- 
  THead \= N, % N mag niet voorkomen in de tail
  present(N, L-TTail).


present_dl(X, H-T) :-
    % Clause 1: Base case - X is the head of the current list segment H.
    % We ensure that H is sufficiently instantiated (not a free variable)
    % and that H is not yet the tail T (meaning there are still elements).
    nonvar(H),   % Ensure H is not a completely uninstantiated variable.
                 % This prevents generating infinite lists if H and T are unrelated.
    H \== T,     % Crucial check: H must be distinct from T.
                 % If H is identical to T, it means we've reached the end of the
                 % effective list (the "hole"), and there are no more elements.
    H = [X|_].   % Unify H with a list whose head is X.
                 % If H is already a list, X will be unified with its head.
                 % If H is a variable (after nonvar check, implies a partial list
                 % like [_|Z]), it will be instantiated to [X|_].

present_dl(X, H-T) :-
    % Clause 2: Recursive case - X is in the tail of the current list segment H.
    % Similar to the base case, we ensure H is a list and not yet the tail.
    nonvar(H),   % Ensure H is not a completely uninstantiated variable.
    H \== T,     % H must be distinct from T to continue traversing.
    H = [_|Rest],% Unify H with a list and extract its tail 'Rest'.
                 % This advances the 'current list segment' pointer.
    present_dl(X, Rest-T). % Recursively call present/2 with the new list segment (Rest).





%% collatzLike(Predicaat, HuidigGetal, DifferenceList, Resultaat)

% Als huidig in DL zit, unificeer DL met resultaat
collatzLike(_, Huidig, Resultaat, Resultaat) :-
  present(Huidig, Resultaat),
  !.

% Als huidig NIET in DL zit, zoek volgende
collatzLike(P, Huidig, Head-OldTail, Resultaat) :-
  call(P, Huidig, Volgend),
  % ! Voeg Volgend toe aan het eind van de lijst
  OldTail = [Volgend | NewTail],
  collatzLike(P, Volgend, Head-NewTail, Resultaat).