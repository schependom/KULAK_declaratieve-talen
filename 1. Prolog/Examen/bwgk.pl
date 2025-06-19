%% Toestandsvoorstelling
% wgk(Positie, [...], [...])
% wgk(Positie, LijstLinks, LijstRechts)

%% Stapvoorstelling
%   - lr(geit/kool/wolf)
%   - rl(geit/kool/wolf)
%   - alleen

% Een clause is ofwel...
%   - een feit
%   - een regel
%   - een query

% Twee feiten (geen regels)
beginToestand(wgk(links, [geit,kool,wolf], [])).
eindToestand(wgk(rechts, [], [geit,kool,wolf])).

andereKant(links, rechts).
andereKant(rechts, links).

%% zetStap(T1, stap, T2)
zetStap(wgk(Positie, L, R), alleen, wgk(NieuwePositie, L, R)) :-
  andereKant(Positie, NieuwePositie).
zetStap(wgk(links, L, R), lr(Voorwerp), wgk(rechts, NieuwLinks, NieuwRechts)) :-
  select(Voorwerp, L, NieuwLinks), % verwijder voorwerp uit de linkerkant
  insert(Voorwerp, R, NieuwRechts).
zetStap(wgk(rechts, L, R), rl(Voorwerp), wgk(links, NieuwLinks, NieuwRechts)) :-
  select(Voorwerp, R, NieuwRechts),
  insert(Voorwerp, L, NieuwLinks).


%% insert(Voorwerp, Kant, NieuweKant)
% ! Nodig om te sorteren !
insert(Voorwerp, Kant, NieuweKantSorted) :- sort([Voorwerp | Kant], NieuweKantSorted).


%% geldigeToestand(Toestand)
geldigeToestand(wgk(rechts, L, _)) :- \+ gevaar(L).
geldigeToestand(wgk(links, _, R)) :- \+ gevaar(R).

%% gevaar(LijstMetVoorwerpenAanEenKant)
gevaar(LijstKant) :- member(wolf, LijstKant), member(geit, LijstKant).
gevaar(LijstKant) :- member(kool, LijstKant), member(geit, LijstKant).



%% solve_dfs(HuidigeToestand, Bezocht, Accumulator, Resultaat)

% Unificeer de Acc met het resultaat
solve_dfs(HuidigeToestand, _, Res, Res) :-
  eindToestand(HuidigeToestand).
solve_dfs(HuidigeToestand, Bezocht, Acc, Res) :-
  zetStap(HuidigeToestand, Stap, NieuweToestand),
  geldigeToestand(NieuweToestand),
  \+ member(NieuweToestand, Bezocht),
  solve_dfs(NieuweToestand, [NieuweToestand | Bezocht], [Stap | Acc], Res).



%% solve_bfs()



% Doe DFS met staartrecursie (accumulator)
testDFS(GezetteStappen) :-
  beginToestand(Start),
  solve_dfs(Start, [Start], [], RevRes),
  reverse(RevRes, GezetteStappen).




% Lijst genereren
list([]).
list([_|T]) :- list(T).









%% bfs_loop(QueueHead-QueueTail, VisitedStates, FinalPath)

% Basisgeval
bfs_loop([(CurrentState, CurrentPath)|_]-_, _, Path) :-
    eindToestand(CurrentState),
    reverse(CurrentPath, Path), !.

% Recursief geval
bfs_loop([CurrentStateAndPath|Q_Head_Rest]-Q_Tail_Current, Visited, Resultaat) :-

    % Dequeue de huidige state en het huidige pad
    CurrentStateAndPath = (CurrentState, CurrentPath),

    % Zet alleen geldige states op de queue!
    findall(
      NextValidStateAndPath,
      (   zetStap(CurrentState, Stap, NextState),
          geldigeToestand(NextState),
          \+ member(NextState, Visited),
          NextValidStateAndPath = (NextState, [Stap|CurrentPath])
      ),
      NewStatesAndPaths
    ),

    % Enqueue de nieuwe staten en krijg de nieuwe staart van de queue
    enqueue_all(NewStatesAndPaths, Q_Tail_Current, NewQ_Tail, ExtraVisitedStates),

    % Update de bezochte statenlijst (nog steeds met append)
    append(Visited, ExtraVisitedStates, NewVisitedStates),

    % Recursieve aanroep met de bijgewerkte queue en bezochte staten
    % Q_Head_Rest is de nieuwe kop van de queue na het verwijderen van CurrentStateAndPath.
    bfs_loop(Q_Head_Rest-NewQ_Tail, NewVisitedStates, Resultaat).

% Geen oplossing (Queue is leeg!)
bfs_loop(EmptyQueueHead-EmptyQueueTail, _, _) :-
    EmptyQueueHead == EmptyQueueTail,
    fail. % No solution found

%% enqueue_all(ListOfElements, CurrentQueueTail, FinalQueueTail, ExtraVisitedStates)
% Voegt een lijst van elementen toe aan een difference list queue.
% CurrentQueueTail is de variabele die de huidige 'lege' staart van de queue representeert.
% FinalQueueTail is de variabele die de 'lege' staart van de queue representeert na het toevoegen van alle elementen.
enqueue_all([], Tail, Tail, []).
enqueue_all([StateEnPad|Rest], CurrentQueueTail, FinalQueueTail, [State|ExtraVisitedStates]) :-
    StateEnPad = (State, _), 
    CurrentQueueTail = [StateEnPad|NextQueueTail], 
    enqueue_all(Rest, NextQueueTail, FinalQueueTail, ExtraVisitedStates).





%% solve_bfs()

% Doe BS met difference lists om de queue te implementeren
testBFS(Path) :-
  beginToestand(StartToestand),
  % Begintoestand met leeg pad
  Queue = [(StartToestand, [])|Q_Tail]-Q_Tail,
  Visited = [StartToestand],
  bfs_loop(Queue, Visited, Path).


% bfs_loop(
%   [
%     (
%       wgk(rechts, [wolf], [geit, kool]), 
%       [lr(kool), alleen, lr(geit)]
%     ), 
%     (
%       wgk(rechts, [kool], [geit, wolf]), 
%       [lr(wolf), alleen, lr(geit)]
%     )
%   |_508]-_508, 
%   [
%     wgk(rechts, [wolf], [geit, kool]), wgk(rechts, [kool], [geit, wolf]), wgk(links, [kool, wolf], [geit]), wgk(rechts, [kool, wolf], [geit]), wgk(links, [geit, kool|...], [])
%   ], 
%   _58
% )

