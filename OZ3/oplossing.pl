% 1 Lijsten
% Schrijf zelf een alternatief listlength/3 dat gebruik maakt van een accumulator 
% en gebruik de debugger om te onderzoeken wat er gebeurt als je dit toepast op een gegeven lijst. 

listlength([],0).
listlength([_|R],Length) :-
	listlength(R,L),
	Length is L + 1.

listlength_acc([],Acc,Acc).
listlength_acc([_|R],Acc,Length) :-
	Acc1 is Acc + 1,
	listlength_acc(R,Acc1,Length).

% een predicaat laatst{2 dat het laatste element van een lijst teruggeeft.
laatst([X],X).
laatst([_,X|R],Y) :-
	laatst([X|R],Y).
	

% een predicaat opeenvolgend/3 dat nagaat of een gegeven element
% onmiddellijk op een ander element volgt in een gegeven lijst.

opeenvolgend([X,Y|_],X,Y).
opeenvolgend([_|R],X,Y) :-
	opeenvolgend(R,X,Y).
	
% een predicaat vector som/3 

vector_som([],[],[]).
vector_som([X|R],[Y|S],[Z|T]) :-
	Z is X + Y,
	vector_som(R,S,T).

% een predicaat zoekwaarde/3 dat de waarde teruggeeft die overeenkomt met de waarde in een
% lijst pair(key,value) waarden.

zoekwaarde([pair(K,V)|_],K,V).
zoekwaarde([_|R],K,V) :-
	zoekwaarde(R,K,V).

% 2 Grafen
% Gegeven een symmetrische graaf met als vertices {a, b, c, d, e}
% en verbindingen tussen (a, b), (b, c), (b, d) en (c, d).

% we gebruiken het predicaat v(x) dat slaagt als x een vertex is in de graaf
% we gebruiken het predicaat e(x,y) om een verbinding in een grafe voor te stellen

v(a).
v(b).
v(c).
v(d).
v(e).

e(a,b).
e(b,c).
e(b,d).
e(c,d).

buur(X,Y) :-
	e(X,Y).
buur(X,Y) :-
	e(Y,X).

pad_n(X,Y) :-
	buur(X,Z),
	pad_n(Z,Y).
pad_n(X,Y) :-
	buur(X,Y).

pad(X,Y) :- pad(X,[X],Y).

pad(X,H,Y) :-
	buur(X,Y),
	\+ member(Y,H).
pad(X,H,Y) :-
	buur(X,Z),
	\+ member(Z,H),
	append(H,[Z],HH),
	pad(Z,HH,Y).

% Fibonacci
% fib_0 = 0
% fib_1 = 1
% fib_n = fib_{n-2} + fib_{n-1} (n > 1)
% Implementeer naıef en bereken fib25 en fib30.

fib_n(0,0).
fib_n(1,1).
fib_n(N,X) :-
	N > 1,
	succ(NM1,N),
	succ(NM2,NM1),
	fib_n(NM1,X1),
	fib_n(NM2,X2),
	X is X1 + X2.

% Zoek de reden en bedenk een betere.

fib(N,X) :- fib_acc(N,0,1,X).

% fib_acc(N,S1,S2,X)
% 	S1 is een fibonaccigetal
% 	S2 is het fibonaccigetal volgend op S1 na 1 stap
% 	X is het fibonaccigetal volgend op S1 na N stappen
fib_acc(0,S1,_,S1).
fib_acc(1,_,S2,S2).
fib_acc(N,S1,S2,X) :-
	N > 1,
	S is S1 + S2,
	succ(NM1,N), % NM1 is N - 1,
	fib_acc(NM1,S2,S,X).

% 4 Gebalanceerde bomen

% Een boom is gebalanceerd als in elke knoop de diepte van linker 
% subboom en de diepte van de rechter subboom niet meer dan 1 verschillen.

diepte(leeg,0).
diepte(node(L,_,R),X) :-
	diepte(L,XL),
	diepte(R,XR),
	X is max(XL,XR) + 1.

% voor de diepte van een node waarin de diepte is voorgesteld
diepte(node(_,_,D,_),D).

inbalans(leeg).
inbalans(node(L,_,R)) :-
	diepte(L,DL),
	diepte(R,DR),
	abs(DL-DR) < 2.


% voor de diepte van een node waarin de diepte is voorgesteld
inbalans(node(L,_,_,R)) :-
	diepte(L,DL),
	diepte(R,DR),
	abs(DL-DR) < 2.
	

voegtoe(leeg,V,node(leeg,V,leeg)).
voegtoe(node(L,W,R),V,node(LV,W,R)) :-
	diepte(L,DL),
	diepte(R,DR),
	DL < DR,
	voegtoe(L,V,LV).
voegtoe(node(L,W,R),V,node(L,W,RV)) :-
	diepte(L,DL),
	diepte(R,DR),
	DL >= DR,
	voegtoe(R,V,RV).

% Extra

voegtoe_e(leeg,V,node(leeg,V,1,leeg)).
voegtoe_e(node(L,W,D,R),V,node(LV,W,D,R)) :-
	diepte(L,DL),
	diepte(R,DR),
	DL < DR,
	voegtoe_e(L,V,LV).
voegtoe_e(node(L,W,D,R),V,node(L,W,D,RV)) :-
	diepte(L,DL),
	diepte(R,DR),
	DL > DR,
	voegtoe_e(R,V,RV).
voegtoe_e(node(L,W,D,R),V,node(LV,W,DN,R)) :-
	diepte(L,DLR),
	diepte(R,DLR),
	succ(D,DN),
	voegtoe_e(L,V,LV).

% 5 Priemgetallen

allepriem(N,L) :-
	getallen(N,G),
	allepriem([],G,L).
allepriem(H,[],H).
allepriem(H,[P|G],R) :-
	append(H,[P],HP),
	verwijder(P,G,GP),
	allepriem(HP,GP,R).

verwijder(P,[X|G],GR) :-
	X mod P =:= 0,
	verwijder(P,G,GR).
verwijder(P,[X|G],[X|GR]) :-
	X mod P > 0,
	verwijder(P,G,GR).
verwijder(_,[],[]).

getallen(1,[]).
getallen(N,G) :-
	N > 1,
	getallen(N,2,G).
getallen(N,N,[]).
getallen(N,X,[X|R]) :-
	X < N,
	succ(X,XP1),
	getallen(N,XP1,R).
	
	
% 6 Uitdrukking
% Hieronder is een oproep van het predicaat eval{3 dat een uitdrukking evalueert
% (x + 2 * y) = 8 voor x = 2 en y = 3.
% ?- eval(plus(var(x), times(int(2), var(y))), [pair(x, 2), pair(y, 3)], Value).
% Value = 8

eval(var(X),[pair(X,Value)|_],Value).
eval(var(X),[pair(Y,_)|R],Value) :-
	X \== Y,
	eval(var(X),R,Value).

eval(int(X),_,X).

eval(plus(T,S),L,V) :-
	eval(T,L,VT),
	eval(S,L,VS),
	V is VT + VS.

eval(times(T,S),L,V) :-
	eval(T,L,VT),
	eval(S,L,VS),
	V is VT*VS.
	

eval(pow(T,S),L,V) :-
	eval(T,L,VT),
	eval(S,L,VS),
	V is VT**VS.

eval(min(T),L,V) :-
	eval(T,L,VT),
	V is -VT.
