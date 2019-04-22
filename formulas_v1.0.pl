% 531 Prolog
% Assessed Exercise 1
% formulas.pl
:- consult(support).


% Write your answers to the exercise here

% Task 1: wff(+F)
% wff(F) holds when F is a (well-formed) formula.

% query: formula(n,_F), fun_name(_F).
% remember to add cut to improve efficiency!

% P = neg(P,Q);
/*Task1 v1.0	
wff(F):-
	ground(F),
	form(F).

form(P):-
	logical_atom(P).

form(neg(P)):-
	form(P).

form(and(P,Q)):-
	form(P),
	form(Q).

form(or(P,Q)):-
	form(P),
	form(Q).

form(imp(P,Q)):-
	form(P),
	form(Q).	

*/

/*
%Task1 v2.0 use ";" to simplify the code

wff(P):-
	logical_atom(P).

wff(F):-
	ground(F),
	F=or(P,Q),
	wff(P),
	wff(Q);

	ground(F),
	F=and(P,Q),
	wff(P),
	wff(Q);

	ground(F),
	F=imp(P,Q),
	wff(P),
	wff(Q);

	ground(F),
	F=neg(P),
	wff(P).
*/
/*****************************************/
wff(P):-
	logical_atom(P).

wff(or(P,Q)):-
	ground(F),
	wff(P),
	wff(Q).

wff(and(P,Q)):-
	ground(F),
	wff(P),
	wff(Q).

wff(imp(P,Q)):-
	ground(F),
	wff(P),
	wff(Q).

wff(neg(P)):-
	ground(F),
	wff(P).




% Task 2: cls(+F)
% cls(F) holds when the formula F is a clause; a clause is either a literal or
% a disjunction of literals, and a literal is either an atom or a negated atom.
cls(or(P,Q)):-
	wff(or(P,Q)),
	cls(P),
	cls(Q).

cls(P):-
	logical_atom(P);
	P = neg(Q),
	logical_atom(Q).	






% Task 3: ats(+F, -As)
% given the formula F, returns As as a duplicate-free list (in any order) of 
% the atoms in F.

/* Task3 v1.0  use "sort" to remove duplicate elements, but "sort" will change the order of elements in the list

ats(F,As):-
	wff(F),
	get_atom(F,As_dup),
	sort(As_dup,As).

get_atom(F,As):-
	F = and(P,Q),
	get_atom(P,As1),
	get_atom(Q,As2),
	append(As1,As2,As);

	F = or(P,Q),
	get_atom(P,As1),
	get_atom(Q,As2),
	append(As1,As2,As);

	F = imp(P,Q),
	get_atom(P,As1),
	get_atom(Q,As2),
	append(As1,As2,As);

	F = neg(P),
	get_atom(P,As).

get_atom(P,[P]):-
	logical_atom(P).
*/

%Task3 v2.0, use my own remove predicate 
ats(F,As):-
	wff(F),
	get_atom(F,As_dup),
	remove_dup(As_dup,As).

% get the atom in fomulas
get_atom(F,As):-
	F = and(P,Q),
	get_atom(P,As1),
	get_atom(Q,As2),
	append(As1,As2,As);

	F = or(P,Q),
	get_atom(P,As1),
	get_atom(Q,As2),
	append(As1,As2,As);

	F = imp(P,Q),
	get_atom(P,As1),
	get_atom(Q,As2),
	append(As1,As2,As);

	F = neg(P),
	get_atom(P,As).

get_atom(P,[P]):-
	logical_atom(P).

% remove the duplicate elements
remove_dup([],[]).

remove_dup([H|T],List):-
	member(H,T),
	remove_dup(T,List).  %while duplivate

remove_dup([H|T],[H|T1]):-
	\+ member(H,T),
	remove_dup(T,T1).






% Task 4: t_value(+F, +Val, -V)
% Calculates the truth value V of the formula F, given the valuation Val.


t_value(F,Val,V):-
	wff(F),
	ats(F,As),
	sub_list(Val,As),
	t_table(F,Val,Flag),
	Flag = 1,
	V = t,!;
	V = f.


t_table(F,Val,Flag):-  % answer can be directly gotten
	F = and(P,Q),
	logical_atom(P),
	logical_atom(Q),
	member(P,Val),
	member(Q,Val),
	Flag = 1;

	F = or(P,Q),
	logical_atom(P),
	member(P,Val),
	Flag = 1;

	F = or(P,Q),
	logical_atom(Q),
	member(Q,Val),
	Flag = 1;

	F = imp(P,Q),
	logical_atom(Q),
	member(Q,Val),
	Flag = 1;

	F = imp(P,Q),
	logical_atom(P),
	\+ member(P,Val),
	Flag = 1;

	F = neg(P),
	logical_atom(P),
	\+ member(P,Val),
	Flag = 1.
	
t_table(F,Val,Flag):-  %one is fomula
	F = and(P,Q),
	logical_atom(P),
	t_table(Q,Val,Flag1),
	member(P,Val),
	Flag1 = 1,
	Flag = 1;

	F = and(P,Q),
	logical_atom(Q),
	t_table(P,Val,Flag1),
	member(Q,Val),
	Flag1 = 1,
	Flag = 1;

	F = and(P,Q),
	t_table(Q,Val,Flag1),
	t_table(P,Val,Flag2),
	Flag1 + Flag2 =:= 2,
	Flag = 1;

	F = or(P,Q),
	logical_atom(P),
	t_table(Q,Val,Flag);

	F = or(P,Q),
	logical_atom(Q),
	t_table(P,Val,Flag);

	F = or(P,Q),
	t_table(Q,Val,Flag1),
	t_table(P,Val,Flag2),
	Flag1 + Flag2 >= 1,
	Flag = 1;

	F = imp(P,Q),
	logical_atom(Q),
	t_table(P,Val,Flag);

	F = imp(P,Q),
	logical_atom(P),
	t_table(Q,Val,Flag);

	F = neg(P),
	t_table(P,Val,Flag).	

sub_list([],_).
sub_list([X1|XSS],X):-
	member(X1,X),
	sub_list(XSS,X).

