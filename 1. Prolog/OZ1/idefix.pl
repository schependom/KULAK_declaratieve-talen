% Comments met procent...
/* Of met slash-sterretje. */

% Facts in de knowledge base,
vader(anton,bart).
vader(anton,daan).
vader(anton,elisa).
vader(fabian,anton).
moeder(celine,bart).
moeder(celine,daan).
moeder(celine,gerda).
moeder(gerda,hendrik).

/*  Y en X moeten dezelfde vader en moeder hebben.
    X mag niet met zichzelf verwant zijn.
        => \== /2 built-in die slaagt indien de twee termen niet (aantoonbaar) gelijk zijn */
verwante(X,Y):- 
    vader(A,X), 
    moeder(Z,X),
    vader(A,Y),
    moeder(Z,Y),
    X \== Y.

/* Voorouder-relatie */
voorouder(X,Y):- vader(X,Y).
voorouder(X,Y):-
    vader(Z,Y),
    voorouder(X,Z).     % volgorde omgedraaid
