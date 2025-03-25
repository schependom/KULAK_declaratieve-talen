% som/2 slaagt wanneer som/3 slaagt voor initiele accumulatorwaarde van 0
som(Boom, Som) :- som(Boom, 0, Som).

% som/3 slaagt wanneer Acc = Som + sum(Boom)
som(nil, Acc, Acc).
som(node(L,R,W), Acc, Som) :-
    som(L, Acc, Acc1),
    Acc2 is Acc1 + W,
    som(R, Acc2, Som).

/*
    ?- som(node(node(nil,nil,9),node(nil,nil,7),6),Som).
*/