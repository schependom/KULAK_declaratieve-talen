gebr(_).

vertaal([Head | Tail], L) :-
    vertaal([Head | Tail], L, 1, [_ | _]).

vertaal([], _, _, _).

vertaal([gebr(Head) | Tail], L, Counter, SymboolTabel) :-
    (
        member(Counter-Head, SymboolTabel) ->
        L = [ gebr(Counter) | L ] ;
        L = [ gebr(_) | L ]
    ),
    vertaal(Tail, L, Counter, SymboolTabel).

vertaal([def(Head) | Tail], L, Counter, SymboolTabel) :-
    L = [stel(Head, Counter) | L], !,
    member(Counter-Head, SymboolTabel), !,
    succ(Counter, NewCounter),
    vertaal(Tail, L, NewCounter, SymboolTabel).

