vader(anton,bart).
vader(anton,daan).
vader(anton,elisa).
vader(fabian,anton).
moeder(celine,bart).
moeder(celine,daan).
moeder(celine,gerda).
moeder(gerda,hendrik).

verwante(X,Y):-moeder(M,X),vader(V,X),moeder(M,Y),vader(V,Y), X\==Y.

voorouder(X, Y):-vader(Z, Y),voorouder(X, Z).
voorouder(X, Y):-vader(X, Y).
