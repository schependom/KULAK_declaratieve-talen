%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% VRAAG 1

% startpunt ALTIJD (1,1)

% eindpunt
%     goto(1, 4, final).

% grootte vh bord
%     size(4).

% verplaatsen over het bord
%     goto(1, 1, up).
%     goto(1, 3, right).
%     goto(3, 3, down).
%     goto(3, 2, left).

% Feiten van het verminkt bord
goto(1, 4, right).
goto(2, 4, right).
goto(3, 4, left).
goto(4, 4, up).
goto(2, 3, right).
goto(3, 3, down).
goto(4, 3, up).
goto(1, 2, up).
goto(2, 2, down).
goto(4, 2, up).
goto(1, 1, up).
goto(2, 1, down).
goto(3, 1, right).
goto(4, 1, final).
size(4).

%%%%%
% 1 %
%%%%%

%% volgende(+Direction, +xOrig, +yOrig, -xNew, -yNew)
volgende(up, Xor, Yor, Xor, Ynew)     :- Ynew is Yor + 1.
volgende(down, Xor, Yor, Xor, Ynew)   :- Ynew is Yor - 1.
volgende(right, Xor, Yor, Xnew, Yor)  :- Xnew is Xor + 1.
volgende(left, Xor, Yor, Xnew, Yor)   :- Xnew is Xor - 1.

/*
?- volgende(up, 1, 1, Xnew, Ynew).
Xnew = 1,
Ynew = 2.

?- volgende(left, 3, 3, Xnew, Ynew).
Xnew = 2,
Ynew = 3.
*/

%%%%%
% 2 %
%%%%%

%% bad_move(+X, +Y, +Visited)
bad_move(X, _, _) :- X < 1, !.
bad_move(_, Y, _) :- Y < 1, !.
bad_move(X, _, _) :- size(S), X > S, !.
bad_move(_, Y, _) :- size(S), Y > S, !.
bad_move(X, Y, Visited) :- member((X, Y), Visited), !.

% Alternatief
/*
bad_move(X, Y, Visited) :- 
  size(S), 
  ( 
    X < 1, !;
    Y < 1, !;
    X > S, !;
    Y > S, !;
    member((X, Y), Visited), !
  ).
*/

/*
?- bad_move(1, 4, []).
false.

?- bad_move(0, 1, []).
true.

?- bad_move(5, 4, []).
true.

?- bad_move(1, 4, [(1,1),(1,2),(1,3),(1,4)]).
true
*/

%%%%%
% 3 %
%%%%%

%% pad(-Path)
pad(Path) :-
    pad((1,1), [(1,1)], PathRev), % Start bij (1,1)
    reverse(PathRev, Path).

%% pad(+CurrPos, +CurrPath, -ResPath)

% Base case: bestemming bereikt
pad((X,Y), CurrentPath, CurrentPath) :-
    goto(X, Y, final).

% Recursive case 1: er is effectief een goto
pad((X,Y), CurrentPath, FinalPath) :-
    goto(X, Y, Dir),
    volgende(Dir, X, Y, Xnew, Ynew),
    \+ bad_move(Xnew, Ynew, CurrentPath),
    pad((Xnew, Ynew), [(Xnew, Ynew) | CurrentPath], FinalPath).

% Recursive case 2: er is GEEN goto instructie 
% !! DFS !!
pad((X,Y), CurrentPath, FinalPath) :-
    \+ goto(X, Y, _),
    % Zoek alle geldige moves
    (   volgende(_, X, Y, Xnew, Ynew),
        \+ bad_move(Xnew, Ynew, CurrentPath),
        pad((Xnew, Ynew), [(Xnew, Ynew) | CurrentPath], FinalPath)
    ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% VRAAG 2

% Feiten (onconditionele waarheden)
arrow(a, b).
arrow(b, c).
arrow(c, c).
arrow(a, d).
arrow(d, a).

% Begint en eindigt in hetzelfde knooppunt -> leg zo'n knooppunt vast.

%%%%%
% 1 %
%%%%%

% AllArrows = [arrow(a, b), arrow(b, c), arrow(c, c), arrow(a, d), arrow(d, a)]

%% delete_node_filter(+ArrowList, +Node, -Result)
delete_node_filter([], _, []).
delete_node_filter([arrow(X,_) | Rest], X, Result) :- !, delete_node_filter(Rest, X, Result).
delete_node_filter([arrow(_,X) | Rest], X, Result) :- !, delete_node_filter(Rest, X, Result).
delete_node_filter([A | AS], X, [A | Rest])        :- !, delete_node_filter(AS, X, Rest).

%% delete_node(?ArrowListInput, +Node, -Result)
% If ArrowListInput is a variable, it's populated with all arrow/2 facts from the knowledge base (= verzameling facts en regels).
% Then, it filters this list to remove any arrows connected to Node.
% If ArrowListInput is an instantiated list, it filters that list directly.
delete_node(ArrowList, Node, Result) :-
    (   
      var(ArrowList) % !! Is ArrowList een variabele?
      ->  
        % Ja, nog niet gebonden
        findall(arrow(S, D), arrow(S, D), ArrowList),
        delete_node_filter(ArrowList, Node, Result)
      ;   
        % Neen -> al gebonden (nonvar!)
        delete_node_filter(ArrowList, Node, Result)
    ).

/*
?- delete_node(ArrowList, a, R).
ArrowList = [arrow(a, b), arrow(b, c), arrow(c, c), arrow(a, d), arrow(d, a)],
R = [arrow(b, c), arrow(c, c)].
*/

%%%%%
% 2 %
%%%%%

%% path(+Start, +Current, +ArrowList, +Visited, -Loop)

% ?- path(a, a, AllArrows, [], L).
% L = [d,a]

% !! memberchk als je enkel wil checken (efficientie!)

%% path(+Start, +Current, +ArrowList, +Visited, -Loop)

% Base case: found a cycle (Current == Start), but only if we've visited at least one other node.
path(Start, Start, _, [Prev|Rest], [Prev|Rest]) :- !.

% Recursive case: follow an arrow to Next, if Next is not yet visited (except for Start).
path(Start, Current, ArrowList, Visited, Loop) :-
    member(arrow(Current, Next), ArrowList),
    (Next = Start, ! ; \+ memberchk(Next, Visited)),
    path(Start, Next, ArrowList, [Current|Visited], Loop).

% Alternatief met findall in ttt.pl


%%%%%
% 3 %
%%%%%

%% findLoops(+ArrowList, +AccLoops, -AllLoops)
findLoops([], Acc, Acc).
findLoops(ArrowList, AccLoops, AllLoops) :-
  member(arrow(StartPunt,_), ArrowList),
  findall(
    [StartPunt | L],
    path(StartPunt, StartPunt, ArrowList, [], L),
    Loops
  ),
  append(Loops, AccLoops, NewAcc),
  delete_node(ArrowList, StartPunt, NewArrows),
  findLoops(NewArrows, NewAcc, AllLoops).

loops(Loops) :-
  findall(arrow(A,B), arrow(A,B), AllArrows),
  findLoops(AllArrows, [], Loops).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% VRAAG 3

%%%%%
% 1 %
%%%%%

%% solve_domino(+Dominos, -Chain)
solve_domino(Dominos, Chain) :- 
  solve_domino(Dominos, [], Chain).

% Geen dominos meer over
% Check of begin gelijk is aan eind
solve_domino([], [(First,Second) | Rest], [(First,Second) | Rest]) :- 
  last(Rest, (_, First)).

% Symmetrische resultaten als je dit doet!
/*solve_domino(Dominos, [], Chain) :-
  select((A,B), Dominos, NewDominos),
  (
    solve_domino(NewDominos, [(A,B)], Chain);
    solve_domino(NewDominos, [(B,A)], Chain)
  ).
*/

% Om symmetrische resultaten te voorkomen, leg de eerste steen 
% maar in 1 mogelijk orientatie!
solve_domino(Dominos, [], Chain) :-
  select((A,B), Dominos, NewDominos),
  solve_domino(NewDominos, [(A,B)], Chain).

solve_domino(Dominos, [(Acc1, Acc2) | Rest], Chain) :-
  select((Acc1,X), Dominos, NewDominos),
  solve_domino(NewDominos, [(X, Acc1), (Acc1, Acc2) | Rest], Chain).

solve_domino(Dominos, [(Acc1, Acc2) | Rest], Chain) :-
  select((X,Acc1), Dominos, NewDominos),
  solve_domino(NewDominos, [(X, Acc1), (Acc1, Acc2) | Rest], Chain).

/*
?- solve_domino([(2,1),(2,3),(1,3)], Chain).
Chain = [(1, 3), (3, 2), (2, 1)] ;
Chain = [(3, 1), (1, 2), (2, 3)] ;
Chain = [(3, 2), (2, 1), (1, 3)] ;
*/

%%%%%
% 2 %
%%%%%

%% Sublijsten van domino's genereren

%% subset_own(+Lijst, -Acc)

% De lege lijst heeft 1 subset
subset_own([], []).
% Genreer subsets die de head WEL bevatten
subset_own([E | Tail], [E | NTail]) :-
    subset_own(Tail, NTail).
% Genereer subsets die de head NIET bevatten
subset_own([_ | Tail], NTail) :-
    subset_own(Tail, NTail).

%% longest_domino_chain(+Dominos, -Chain)

% Naief: voert voor elk van de 2^N subsets solve_domino() uit!!
longest_domino_chain_naief(Dominos, Chain) :-
  findall(
      L-DominoChain,
      (
        subset_own(Dominos, Subset),
        solve_domino(Subset, DominoChain),
        length(DominoChain,L)
      ),
      Chains 
  ),
  sort(Chains, ChainsVKNG),
  last(ChainsVKNG, (_-Chain)).

% Beter: vind eerst alle subsets, orden, en voer
% pas daarna solve_domino() uit, maar enkel op de grootste!
longest_domino_chain_beter(Dominos, Chain) :-
  findall(
      length(Subset)-Subset,
      subset_own(Dominos, Subset),
      Subsets 
  ),
  sort(Subsets, SubsetsVKNG),
  reverse(SubsetsVKNG, SubsetsVGNK),
  first_all_biggest_chains(SubsetsVGNK, Chain).

% Zoek VAN GROOT NAAR KLEIN
% onder de subsets ALLE oplossingen voor de subset in kwestie.
% Geen oplossingen -> zoek verder in kleinere subset
% Wel oplossingen -> lijst ze ALLEMAAL op!
first_all_biggest_chains([(_-FirstSubset) | RestSubsets], Solution) :-
    findall(Oplossing, solve_domino(FirstSubset, Oplossing), Oplossingen),
    ( Oplossingen == []
      -> 
        % Geen oplossingen => ga naar kleinere subset
        first_all_biggest_chains(RestSubsets, Solution)
      ; 
        % Wel oplossingen => lijst ze ALLEMAAL op!
        member(Solution, Oplossingen)
    ).