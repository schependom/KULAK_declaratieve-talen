vader(paul,koen). 
vader(paul,els). 
vader(koen,eefje). 
vader(koen,marten).
 
moeder(els,vincent).
moeder(els,edith).
moeder(els,pieter).
moeder(denise,koen).

grootvader(X,P) :- vader(X,Z), vader(Z,P).
grootvader(X,P) :- vader(X,Z), moeder(Z,P).
