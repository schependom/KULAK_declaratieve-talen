%
% One Pass translate
%
vertaal(Programma,Resultaat) :- vertaal(1,[_|_],Programma,Resultaat).  

%
% vertaal(VolgendeDefinitie,SymbolenTabel,Programma,Resultaat).
% Het verband tussen Programma en Resultaat is volgens de opgave.
% VolgendeDefinitie is een Int: het nummer van de volgende definitie (teller)
% SymbolenTabel is een onvolledige lijst.
% member(X-N,SymbolenTabel),! slaagt altijd juist 1 x.
%        en X-N unificeert met een term die voorkomt in de SymbolenTabel
%
vertaal(_,_,[],[]).
vertaal(VolgendeDefinitie,SymbolenTabel,
		[def(X)|Programma],[stel(X,VolgendeDefinitie)|RL]) :-
	member(X-VolgendeDefinitie,SymbolenTabel),!,
	N1 is VolgendeDefinitie + 1,
	vertaal(N1,SymbolenTabel,Programma,RL).

vertaal(VolgendeDefinitie,SymbolenTabel,[gebr(X)|Programma],[gebr(Xn)|RL]) :-
	member(X-Xn,SymbolenTabel),!,
	vertaal(VolgendeDefinitie,SymbolenTabel,Programma,RL).


% Holiday route

%highway(1,2,red).
%highway(2,3,blue).
%highway(1,3,red).

highway(1,2,c).
highway(2,3,a).
highway(1,3,b).
highway(3,5,a).
highway(3,4,c).
highway(5,4,d).

% Holiday route version 1

%
% weg(X,Y-Color)
% Slaagt als er een weg is van X naar Y of van Y naar X met kleur Color
%
weg(X,Y-Color) :-
	(highway(X,Y,Color) ; highway(Y,X,Color)).
	
%
% checkit/0
% Slaagt als uit elke stad een even aantal wegen vertrekken
% en er geen enkele stad is waar de helft van de vertrekkende wegen
% dezelfde kleur heeft
% 
checkit :-
	\+ onevenOfNietInBalans.

%
% onevenOfNietInBalans/0
% Slaagt als er een stad is waaruit een oneven aantal wegen vertrekken
% of als er een stad is waar meer dan de helft van de vertrekkende wegen
% van dezelfde kleur zijn
%
onevenOfNietInBalans :-
	(highway(X,_,_) ; highway(_,X,_)),
	findall(Y,weg(X,Y-_),LinksX),
	length(LinksX,DegreeX),
	1 is DegreeX mod 2,
	(highway(X,_,C) ; highway(_,X,C)),
	findall(Y,weg(X,Y-C),ColorsXC),
	length(ColorsXC,NumberOfXC),
	NCx2 is NumberOfXC*2,
	DegreeX < NCx2,!.

%
% tour(-Tour) semidet
% Slaagt als checkit/0 slaagt en als
% Tour de eerste(*) lijst is van Y-C termen waarbij
% Y een stad is en elke stad juist 1 keer voorkomt in Tour
% C de kleur van de weg die gevolgd wordt om Y te bereiken
% het het eerste element Y-C bereikt wordt vanuit stad 1 
% het laatste element van de vorm 1-C is
% in elke stad de kleur van de weg bij aankomst
% verschilt van de kleur van de weg bij vertrek
% 
% (*) de eerste lijst is die waarbij de volgende stad
% telkens een zo laag mogelijk nummer Y heeft
%
tour([Y-C|Tour]) :-
	checkit,
	% Vind alle wegen die uit stad 1 vertrekken
	findall(Y-C,weg(1,Y-C),Wegen),
	length(Wegen,NWegen),
	succ(NWegenM1,NWegen),
	% sorteer de wegen, stad met het laagste nummer eerst
	sort(Wegen,SortedWegen),
	member(Y-C,SortedWegen),
	tour(NWegenM1,[1-Y,Y-1],[Y-C|Tour]),!.

%
% tour(N1,Used,Vervolg)
% N1 is het aantal nog beschikbare wegen vanuit 1
% Gebruikt is de verzameling gebruikte wegen
% Vervolg is de eerste rondrit die na aankomst in X
% met een weg van kleur CX
% alle niet gebruikte wegen afreist 
% volgens de beperkingen van tour/2
%
tour(N1,Gebruikt,[X-CX,Y-C|Tour]) :-
	% vind alle wegen die uit stad X 
	% kunnen vertrekken
	% met een kleur verschillend van CX
	findall(Y-C,(weg(X,Y-C),				% zoekt alle mogelijk opties vanuit X, gegeven contraints:
	  (N1 > 1 ; Y \= 1), 					%  niet te vroeg stoppen
	  C \= CX,								%  ander kleur dan waarmee werd toegekomen
	  \+ member(X-Y,Gebruikt)),Routes),		%  niet reeds bezocht
	sort(Routes,SortedRoutes),
	member(Y-C,SortedRoutes),
	(
	Y == 1 *->								% mogelijk meerdere keren door city 1
		succ(N1M1,N1), 
		tour(N1M1,[X-Y,Y-X|Gebruikt],[Y-C|Tour])
	;
		tour(N1,[X-Y,Y-X|Gebruikt],[Y-C|Tour])
	),!.									% cut zorgt er voor dat eerst 
											% gevonden oplossing gebruikt wordt

% laatste stap, terug naar stad nr. 1
%  zorg dat de weg nog niet gebruikt is
%  en dat ander kleur gebruikt wordt

tour(1,Gebruikt,[X-VC,1-C]) :- 				
	findall(1-C,(weg(X,1-C),\+ member(X-1,Gebruikt),C \= VC),[1-C]),!.

% alle steden worden bezocht doordat 1) elke stad even degree heeft, 2) elke weg 1 keer bezocht wordt, 
% 3) verschillende kleuren gebruikt worden en 4) terug naar stad 1 terug keren	
