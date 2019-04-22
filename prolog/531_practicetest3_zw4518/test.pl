a(X):-
	x == 1,
	X = 1.

test(L):-
	findall(X, a(X), L).
