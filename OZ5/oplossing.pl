%
% house(H,V): de dimensies van het huis
% Slaagt als H de horizontale dimensie en V de vertikale dimensie is.
%
house(5,4).

%
% wall(Direction,Position,From,Till): een muur in het huis
% Slaagt als Direction = v, Position de positie op de horizontale as, 
%    From de begin positie op de vertikale as, 
%    Till de eind positie op de vertikale as
% of Direction = h, Position op de vertikale as
%    From de begin-positio op de horizontale as,
%    Till de eind-positie op de horizontale as.
% Afspraak : From < Till
% (Het predicaat is dynamisch gemaakt om gemakkelijke aanpassingen toe te laten.)
% (Gebruik retract/1 voor de make/0)
% 


wall(v,2,3,4).
wall(v,3,0,1).
wall(v,4,0,1).
wall(h,2,3,5).

% wall(v,1,1,4).
% wall(h,2,2,5).
% wall(v,3,3,4).
% wall(v,4,3,4).

%
% color(XCo,YCo,Color) : de kleur van het vakje
% met links onder (XCo,YCo) en rechtsboven (XCo + 1, YCo + 1)
% ( Het predicaat is dynamisch gemaakt om gemakkelijke aanpassingen toe te laten.)
% (Gebruik retract/1 voor de make/0)
%


color(1,1,green).
color(1,2,red).
color(3,1,red).
color(4,0,blue).

%
% wall_in_part(++Part,-Wall)
% Slaagt als Wall het deel van een muur is dat binnen
% Part gelegen is.
% 
wall_in_part(part(XCo1,YCo1,XCo2,YCo2),wall(v,X,Y1,Y2)) :-
	wall(v,X,LY1,LY2),
	XCo1 < X,
	X < XCo2,
	overlap(interval(YCo1,YCo2),interval(LY1,LY2),interval(Y1,Y2)). % er wordt ook een nieuwe muur teruggegeven
wall_in_part(part(XCo1,YCo1,XCo2,YCo2),wall(h,Y,X1,X2)) :-
	wall(h,Y,LX1,LX2),
	YCo1 < Y,
	Y < YCo2,
	overlap(interval(XCo1,XCo2),interval(LX1,LX2),interval(X1,X2)).

%
% overlap(++Interval1,++Interval2,-Interval3)
% Slaagt als Interval3 de doorsnede is van Interval1 en Interval2
% en deze doorsnede niet ledig en geen singleton is
% 
overlap(interval(L1,R1),interval(L2,R2),interval(L,R)) :-
	L is max(L1,L2),
	R is min(R1,R2),
	L < R.
	
%
% color_in_part(++Part,-Color)
% Slaagt als er een vakje met kleur Color 
% voorkomt als deel van Part
%
color_in_part(part(XCo1,YCo1,XCo2,YCo2),Color) :-
	color(X,Y,Color),
	XCo1 =< X,
	X < XCo2,
	YCo1 =< Y,
	Y < YCo2.

%
% single_color_present(++Part)
% Slaagt als er maximum 1 gekleurd vakje voorkomt
% als deel van Part
%
single_color_present(Part) :-
	color_in_part(Part,Color),
	\+ (color_in_part(Part,OtherColor),
		Color \= OtherColor).
single_color_present(Part) :-
	\+ color_in_part(Part,_).

%
% part_has_wall(++Part)
% Slaagt als er minstens 1 muur
% voorkomt in Part
% 
part_has_wall(Part) :-
	wall_in_part(Part,_),!.

%
% cut_plan(-CuttingPlan,-Parts)
% Slaagt als het volledige huis
% door cuts in de lijst CuttingPlan
% opgedeeld wordt in Parts zonder muren (\+ part_has_wall)
% met hoogstens 1 gekleurd vakje (single_color_present)
% Parts: een gesorteerde lijst delen van het huis (part/4 elementen)
% CuttingPlan: een lijst van cuts (cut/4 elementen)
% die in die volgorde uitgevoerd, resulteren in Parts
%
cut_plan(CuttingPlan,Parts) :-
	house(H,V),
	cut_plan(CuttingPlan,[part(0,0,H,V)],ResultingParts),
	sort(ResultingParts,Parts).

cut_plan([],[],[]).

cut_plan(CuttingPlan,[Part|Parts],[Part|ResultingParts]) :-
	\+ part_has_wall(Part),!,
	single_color_present(Part),
	cut_plan(CuttingPlan,Parts,ResultingParts).

% This clause performs a cut (cut(Direction, Position, From, Till)) on a 
% part of the house if a wall exists within the part. 
% It then divides the part into two smaller parts (Part1 and Part2), 
% and continues with the next cuts.	

cut_plan([cut(Direction,Position,From,Till)|CuttingPlan],[Part|Parts],ResultingParts) :-
	wall_in_part(Part,wall(Direction,Position,_,_)),
	cut_colored_parts(cut(Direction,Position,From,Till),Part,Part1,Part2),
	cut_plan(CuttingPlan, [Part1,Part2|Parts], ResultingParts).

%
% cut_colored_parts(Cut,Part,Part1,Part2)
% Slaagt als bij uitvoeren van  Cut op Part
% de delen Part1 en Part2 ontstaan
% 
cut_colored_parts(cut(v,Position,YCo1,YCo2),part(XCo1,YCo1,XCo2,YCo2),
	part(XCo1,YCo1,Position,YCo2),part(Position,YCo1,XCo2,YCo2)).
cut_colored_parts(cut(h,Position,XCo1,XCo2),part(XCo1,YCo1,XCo2,YCo2),
	part(XCo1,YCo1,XCo2,Position),part(XCo1,Position,XCo2,YCo2)).

