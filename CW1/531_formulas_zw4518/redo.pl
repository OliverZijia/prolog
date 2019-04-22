% 531 Prolog
% Assessed Exercise 1
% formulas.pl

% Write your answers to the exercise here
:- consult(support).
% Task 1: wff(+F)
% wff(F) holds when F is a (well-formed) formula.
wff(F):-
	ground(F),
	form(F).

form(F):-
	logical_atom(F).
form(F):-
	(F = and(P, Q);
	F = or(P, Q);
	F = imp(P,Q)),
	form(P),
	form(Q).
form(neg(P)):-
	form(P).

% Task 2: cls(+F)
% cls(F) holds when the formula F is a clause; a clause is either a literal or
% a disjunction of literals, and a literal is either an atom or a negated atom.
cls(F):-
	wff(F),
	cl(F).

cl(or(P,Q)):-
	cl(P),
	cl(Q).
cl(F):-
	(F = neg(Q)
	-> logical_atom(Q)
	;  logical_atom(F)).	

% Task 3: ats(+F, -As)
% given the formula F, returns As as a duplicate-free list (in any order) of 
% the atoms in F.
ats(F, As):-
	wff(F),
	atoms(F, A),
	remove_dup(A, [], As).

atoms(F, [A]):-
	logical_atom(F),
	A = F.
atoms(F, A):-
	(F = and(P, Q);
	F = or(P, Q);
	F = imp(P,Q)),
	atoms(P, AP),
	atoms(Q, AQ),
	append(AP, AQ, A).
atoms(neg(P), A):-
	atoms(P, A).

remove_dup([A], Acc, [A|Acc]).
remove_dup([A|Rest], Acc, As):-
	\+ member(A, Rest),
	Acc1 = [A|Acc],
	remove_dup(Rest, Acc1, As),!.
remove_dup([_|Rest], Acc, As):-
	remove_dup(Rest, Acc, As).
	
% Task 4: t_value(+F, +Val, -V)
% Calculates the truth value V of the formula F, given the valuation Val.
t_value(F, Val, V):-
	ground(F),
	ground(Val),
	wff(F),
	logic(Val),
	eval(F, Val, V0),
	(V0 = 1
	-> V = t
	;  V = f).

logic([]).
logic([V|Rest]):-
	logical_atom(V),
	logic(Rest).

eval(P, Val, V):-
	logical_atom(P),
	(member(P, Val)
	-> V = 1
	;  V = 0).
eval(and(P,Q), Val, V):-
	eval(P, Val, VP),
	eval(Q, Val, VQ),
	(2 is VP + VQ
	-> V = 1
	;  V = 0).

eval(or(P,Q), Val, V):-
	eval(P, Val, VP),
	eval(Q, Val, VQ),
	(0 is VP + VQ
	-> V = 0
	;  V = 1).

eval(imp(P,Q), Val, V):-
	eval(neg(P), Val, VP),
	eval(Q, Val, VQ),
	(0 is VP + VQ
	-> V = 0
	;  V = 1).

eval(neg(P), Val, V):-
	eval(P, Val, Vp),
	(Vp = 0
	-> V = 1
	;  V = 0).


