
find([H|T]):-
	H = 1,!.
	


find([H|T]):-
	find(T).
