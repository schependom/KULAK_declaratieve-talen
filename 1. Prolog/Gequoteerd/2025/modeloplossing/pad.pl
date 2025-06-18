goto(1,4,right). 
goto(2,4,right). 
goto(3,4,left). 
goto(4,4,up).
goto(2,3,right). 
goto(3,3,down). 
goto(4,3,up).
goto(1,2,up). 
goto(2,2,down). 
goto(4,2,up).
goto(1,1,up). 
goto(2,1,down). 
goto(3,1,right). 
goto(4,1,final).
size(4).

pad([StartPoint|Restpad]) :-
	StartPoint = (1,1),
	Visited = [StartPoint],
	compleet_pad(1,1,Visited,Restpad).

compleet_pad(I,J,_,Resultpad) :-
	goto(I,J,final),
	!,
	Resultpad = [].
compleet_pad(I,J,Visited,Resultpad) :-
	goto(I,J,Direction) ->
	true
	;
	true
	),
	volgende(Direction,I,J,NewI,NewJ),
	\+ bad_move(NewI,NewJ,Visited),
	Resultpad = [(NewI,NewJ)|TailResultpad],
	compleet_pad(NewI,NewJ,[(NewI,NewJ)|Visited],TailResultpad).

bad_move(I,J,Visited) :- member((I,J),Visited).
bad_move(I,J, _) :- (I < 1 ; J < 1).
bad_move(I,J, _) :- size(N), (I > N ; J > N).

volgende( up, I,J, I,NewJ) :- NewJ is J + 1.
volgende( down, I,J, I,NewJ) :- NewJ is J - 1.
volgende( left, I,J, NewI, J) :- NewI is I - 1.
volgende(right, I,J, NewI, J) :- NewI is I + 1.




