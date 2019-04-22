% 531 Prolog
% Assessed Exercise 1
% formulas.pl

% Write your answers to the exercise here
:- consult(support).
% Task 1: wff(+F)
% wff(F) holds when F is a (well-formed) formula.
wff(F):-				%To avoid running the ground in every iteration, separate the code of judging if F is a well-formed formula as a separated function
	ground(F),
	formed(F).

formed(neg(P)):-			%start testing if P is well-formed formula
	formed(P).
formed(and(P,Q)):-
	formed(P),
	formed(Q).
formed(or(P,Q)):-
	formed(P),
	formed(Q).
formed(imp(P,Q)):-
	formed(P),
	formed(Q).
formed(P):-
	logical_atom(P).

% Task 2: cls(+F)
% cls(F) holds when the formula F is a clause; a clause is either a literal or
% a disjunction of literals, and a literal is either an atom or a negated atom.
cls(F):-
	wff(F),
	or_neg(F).
	
or_neg(P):-				%start testing if F only contains or and neg	
	logical_atom(P);
	P = neg(Q),
	logical_atom(Q).
or_neg(or(P,Q)):-
	or_neg(P),
	or_neg(Q).

% Task 3: ats(+F, -As)
% given the formula F, returns As as a duplicate-free list (in any order) of 
% the atoms in F.

ats(F,As):-
	wff(F),
	get_atom(F,As_dup),
	sort(As_dup,As).		%remove the duplicate atoms

get_atom(P,[P]):-
	logical_atom(P).
get_atom(F,As):-
	F = neg(P),
	get_atom(P,As);

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
	append(As1,As2,As).

% Task 4: t_value(+F, +Val, -V)
% Calculates the truth value V of the formula F, given the valuation Val.
t_value(F,Val,V):-
	wff(F),
	ats(F,As),
	sub_list(Val,As),
	t_table(F,Val,V).

t_table(neg(P),Val,Flag):- 		%start encoding the truth table
	logical_atom(P),
	(member(P,Val)			%if P is an atom, then compare P and Val to get the answer
	-> Flag = f
	;  Flag = t),!;			%if P is an atom, but people want to get another answer, then the t_table(P,Val,Flag1) will be performed, so adding a cut is necessary, other cuts is for the same reason
	t_table(P,Val,Flag1),		%if P is a formula, then continue the recursion, and judge the Flag1 returned by P to get the answer
	(Flag1 == t
	-> Flag = t
	;  Flag = f).
t_table(and(P,Q),Val,Flag):-  
	logical_atom(P),		%if P is an atom and is true, the Flag of and(P,Q) is same as the Flag of Q, otherwise and(P,Q) is false
	(member(P,Val)	
	-> t_table(Q,Val,Flag)
	;  Flag = f),!;

	logical_atom(Q),		%if Q is an atom and is true, the Flag of and(P,Q) is same as the Flag of P, otherwise and(P,Q) is false
	(member(Q,Val)
	-> t_table(P,Val,Flag)
	;  Flag = f),!;

	t_table(Q,Val,Flag1),
	(Flag1 == t
	-> t_table(P,Val,Flag)
	;  Flag = f).
t_table(or(P,Q),Val,Flag):- 
	logical_atom(Q),
	(member(Q,Val)
	-> Flag = t			%if Q is a natom and true, then or(P,Q) is true
	;  t_table(P,Val,Flag)),!;	%else, which is when Q is not a natom or Q is not true, check P, if P is true, then this formula is true, otherwise, it's false.

	logical_atom(P),
	(member(P,Val)
	-> Flag = t			%if P is an atom and true, then or(P,Q) is true
	;  t_table(Q,Val,Flag)),!;	%else, which is when P is not an atom or Q is not true, check Q, if Q is true, then this formula is true, otherwise, it's false.

	t_table(Q,Val,Flag1),		
	(Flag1 == t
	->Flag = t			%because neither of P and Q is an atom, go into next recursion, if Q returns 1(which means true), this formula is true.
	;t_table(P,Val,Flag)).		%if Q returns 0(which means false), check P, and the flag of or(P,Q) is same to the Flag of P.
t_table(imp(P,Q),Val,Flag):-            %= neg(P) V Q
	logical_atom(P),		%if P is an atom, then if P is flase, imp(P,Q)is true, if P is true, the result of imp(P,Q) is same the the result returned by Q
	(\+ member(P,Val)
	-> Flag = t
	;t_table(Q,Val,Flag)),!;  
	
	logical_atom(Q),		%if Q is an atom, then if Q is true, imp(P,Q)is true, if P is false, the result of imp(P,Q) is same the the result returned by neg(P)
	(member(Q,Val)
	-> Flag = t
	;t_table(neg(P),Val,Flag)).
t_table(P,Val,Flag):-  			%if the input is an atom
	logical_atom(P),
	(member(P,Val)
	-> Flag = t
	;  Flag = f).

sub_list([],_).
sub_list([X1|XSS],X):-
	member(X1,X),
	sub_list(XSS,X).
