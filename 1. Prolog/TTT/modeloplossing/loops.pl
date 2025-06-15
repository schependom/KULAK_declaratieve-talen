arrow(a,b).
arrow(b,c).
arrow(c,c).
arrow(a,d).
arrow(d,a).

loops(Loops) :-
	findall(arrow(A,B),arrow(A,B),AllArrows),
	findloops(AllArrows,[],Loops) .

findloops([], AllLoops,AllLoops).

findloops(Arrows,AccLoops,AllLoops) :-
	Arrows = [arrow(Start,_)|_],
	findall([Start|Path],
	path(Start,Start,Arrows,[],Path),LoopsFromStart),
	append(LoopsFromStart,AccLoops,NewAccLoops),
	delete_node(Arrows,Start,RestArrows),
	findloops(RestArrows,NewAccLoops,AllLoops) .

path(Start,Current,Arrows,Visited,Loop) :-
	member(arrow(Current,Next),Arrows),
	\+ member(Next,Visited),
	Loop = [Next|RestLoop],
	( Next == Start ->
	RestLoop = []
	;
	path(Start,Next,Arrows,[Next|Visited],RestLoop)
	).

delete_node( [], _, []).
delete_node([A|As],Node,Arrows) :-
	( (A = arrow(Node,_) ; A = arrow(_,Node)) ->
		delete_node(As,Node,Arrows)
	;
		Arrows = [A|RestArrows],
		delete_node(As,Node,RestArrows)
	).