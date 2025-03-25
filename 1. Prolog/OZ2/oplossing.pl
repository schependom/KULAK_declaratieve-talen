% 1 Peano
% Prolog ondersteunt standaard numeriek rekenen in de vorm ”X is . . . ” 
% waarbij de puntjes voor een berekening staan. (X is (3+2)**2 + 17)

% In deze oefening gebruiken we een andere voorstelling van de getallen. 
% Het getal 0 wordt voorgesteld door het atoom nul.
% Vertrek van

peano_plus(zero,X,X).
peano_plus(s(X),Y,s(Z)) :- peano_plus(X,Y,Z).

%  Schrijf nu de volgende predicaten:

min(X,zero,X).
min(s(X),s(Y),Z) :- min(X,Y,Z).

% Wat gebeurt er met ?- min(s(s(zero)),s(s(s(zero))),Z). ?
% en met ?- min(s(s(s(zero))),Y,s(s(zero))). ?
% en met ?- min(A,B,C). ?

groter_dan(s(_),zero).
groter_dan(s(X),s(Y)) :-
	groter_dan(X,Y). 

% Stel je analoge vragen als bij min.
% Wat doet ?- groter_dan(s(s(s(zero))),Y). ?
% Wat doet ?- groter_dan(X,zero). ?
% Wat doet ?- groter_dan(Z,Z). ?
% kan je dit nu ook oplossen?

maximum(X,Y,X) :-
	groter_dan(X,Y).
maximum(X,Y,Y) :-
	groter_dan(Y,X).
maximum(X,X,X).

% Extraatje
min_extra(X,Y,Z) :- peano_plus(Y,Z,X).

% Extraatje
div(X,Y,s(D),R) :-
	peano_plus(Z,Y,X), 
	div(Z,Y,D,R).
div(s(R),Y,zero,s(R)) :-
	groter_dan(Y,s(R)).
div(X,X,s(zero),zero).

% 2 Diepte
% Schrijf een predicaat diepte/2 dat de diepte van een boom bepaalt
diepte(nil,0).
diepte(node(L,_,R),D) :-
	diepte(L,DL),
	diepte(R,DR),
	D is max(DR,DL) + 1. 

% 3 Boole
%  Schrijf een predicaat eval{2 die een booleaanse uitdrukking evalueert 
% volgens de regels van de booleaanse algebra.
% Gebruik tru, fal, and, or en not.

eval(tru,tru).
eval(fal,fal).

eval(and(B,C),tru) :-
	eval(B,tru),
	eval(C,tru).
eval(and(B,_),fal) :-
	eval(B,fal).
eval(and(B,C),fal) :-
	eval(B,tru),
	eval(C,fal).

eval(or(B,C),fal) :-
	eval(B,fal),
	eval(C,fal).
eval(or(B,_),tru) :-
	eval(B,tru).
eval(or(B,C),tru) :-
	eval(B,fal),
	eval(C,tru).

eval(not(B),tru) :- eval(B,fal).
eval(not(B),fal) :- eval(B,tru).
	
	
% Stel Prolog vergelijkingen voor met behulp van number/1, plus/2, min/2, neg/1

eval(number(X),X).
eval(plus(A,B),X) :-
	eval(A,XA),
	eval(B,XB),
	X is XA + XB.
eval(min(A,B),X) :-
	eval(A,XA),
	eval(B,XB),
	X is XA - XB.
eval(neg(A),X) :-
	eval(A,XA),
	X is -XA.

eval(=(A,B),tru) :-
	eval(A,X),
	eval(B,X).
eval(=(A,B),fal) :-
	eval(A,XA),
	eval(B,XB),
	XA \== XB.
