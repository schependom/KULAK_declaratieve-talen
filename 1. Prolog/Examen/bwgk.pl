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
    reverse(CurrentPath, Path).

% Recursief geval
bfs_loop([CurrentStateAndPath|RestStatesAndPaths]-Q_Tail, Visited, Resultaat) :-
    % Dequeue de huidige state en het huidige pad
    CurrentStateAndPath = (CurrentState, CurrentPath),
    % De queue mag niet leeg zijn
    [CurrentStateAndPath|RestStatesAndPaths] \== Q_Tail,

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
    (   NewStatesAndPaths \== []
    ->  % Nieuwe states gevonden => ENQUEUE
        enqueue_all(NewStatesAndPaths, RestStatesAndPaths, NextQ_Tail, ExtraVisitedStates),
        append(ExtraVisitedStates, Visited, NewVisitedStates),
        bfs_loop(RestStatesAndPaths-NextQ_Tail, NewVisitedStates, Resultaat)
    ;   % Geen nieuwe states gevonden => GA VERDER MET HUIDIGE QUEUE
        bfs_loop(RestStatesAndPaths-Q_Tail, Visited, Resultaat)
    ).

% Geen oplossing (Queue is leeg!)
bfs_loop(EmptyQueueHead-EmptyQueueTail, _, _) :-
    EmptyQueueHead == EmptyQueueTail,
    fail.

%% enqueue_all(ListOfElements, OldQueueTail, NewQueueTail, ExtraVisitedStates)

enqueue_all([], Tail, Tail, []).
enqueue_all([(State, Path)|Rest], [(State, Path)|NextTail], FinalTail, [State|ExtraVisitedStates]) :-
    enqueue_all(Rest, NextTail, FinalTail, ExtraVisitedStates).




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