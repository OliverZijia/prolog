:- consult(support).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Question 1 (50%)                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (a) eval(+E, +Env, -V)

eval(E1 + E2, Env, V):-
	eval(E1, Env, V1),
	eval(E2, Env, V2),
	V is V1 + V2,!.
eval(E1 * E2, Env, V):-
	eval(E1, Env, V1),
	eval(E2, Env, V2),
	V is V1 * V2,!.
eval(-E, Env, V):-
	eval(E, Env, V1),
	V is -V1,!.
eval(E, Env, V):-
	(float(E)
	-> V is E
	;  getV(E, Env, V1),
	   V is V1).


getV(E, [(E, V)|_], V):- !.
getV(E, [], E).
getV(sin(E), [(E, V)|_], sin(V)):- !.
getV(cos(E), [(E, V)|_], cos(V)):- !.
getV(E, [_|Rest], V):-
	getV(E, Rest, V).


% (b) commutes(+E1, +E2)
commutes(sin(E1), sin(E2)):-
	commutes(E1, E2),!.
commutes(cos(E1), cos(E2)):-
	commutes(E1, E2),!.
commutes(E, E):- !.
commutes(E1 + E2, E2 + E1):- !.
commutes(E1 * E2, E2 * E1):- !.

commutes(E1 + E2, E3 + E4):-
	commutes(E1, E3),
	commutes(E2, E4).
commutes(E1 * E2, E3 * E4):-
	commutes(E1, E3),
	commutes(E2, E4).
commutes(-(E1), -(E2)):-
	commutes(E1, E2).

% (c) diff(+E, +V, -D)
diff(E, V, D):-
	dif(E, V, D1),
	simplify(D1, D).
dif(E1 + E2, V, D):-
	dif(E1, V, D1),
	dif(E2, V, D2),
	D = D1 + D2,!.
dif(E1 * E2, V, D):-
	dif(E1, V, D1),
	dif(E2, V, D2),
	D = E1 * D2 + E2 * D1,!.
dif(sin(E), V, D):-
	dif(E, V, D1),
	D = cos(E) * D1,!.
dif(cos(E), V, D):-
	dif(E, V, D1),
	D = -sin(E) * D1,!.	
dif(-E, V, D):-
	dif(E, V, D1),
	D = -D1,!.
dif(V, V, 1.0).
dif(E, V, 0.0):-
	E \= V;
	number(E).



% (d) maclaurin(+E, +X, +N, -V)
maclaurin(E, X, N, V):-
	eval(E, [(x,0.0)], DV),
	add(E, 1, N, DV, Added),
%	Added = Add * x,
	simplify(Added, Sim_Added),
%	write(Added),
%	write(Sim_Added),
%	nl,
	eval(Sim_Added, [(x,X)], V).

add(_, N, N, Acc, Acc).
add(E, Count, N, Acc, Added):-
	Count < N,
	Count1 is Count + 1,
	diff_n(E, x, 1, Count, D),
	eval(D, [(x,0.0)], DN),
	item_n(DN/Count, 1, Count, Dn),
	Acc1 = Acc + Dn,
	add(E, Count1, N, Acc1, Added),!.

diff_n(E, x, N, N, D):-
	diff(E, x, D),!.
diff_n(E, x, Count, N, D):-
	Count < N,
	diff(E, x, D1),
	Count1 is Count + 1,
	diff_n(D1, x, Count1, N, D).

item_n(D, Max, Max, Dn):-
	Dn = (D * x),!.
item_n(D, Count, Max, Dn):-
	D1 = D * x,
	Count1 is Count +1,
	item_n(D1, Count1, Max, Dn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Question 2 (50%)                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% (a) b_to_left(+Pos, -H).
b_to_left(Pos, H):-
	count(Pos, 0, 0, H).

count([], _, H, H).
count([e|Rest], Countb, Counth, N):-
	count(Rest, Countb, Counth, N).
count([w|Rest], Countb, Counth, N):-
	Counth1 is Counth + Countb,
	count(Rest, Countb, Counth1, N). 
count([b|Rest], Countb, Counth, N):-
	Countb1 is Countb + 1,
	count(Rest, Countb1, Counth, N).



% (b) move(+Pos, -NewPos) switch + Count(2 Accumulator) 
move(P, N):-
	pos_e(P, 0, PosE),
% switch(Posm, Acc, Changed(1), PosB(W), PosB(W), N)
	switch(P, [], 0, PosBW, N),
	N \= P,
	D is abs(PosE - PosBW),
	D =< 3. 

switch([w|Rest], Acc, Pos, Pos, New):-
	append(Acc, [e|Rest], N),
	switche(N, w, [], New). 
switch([b|Rest], Acc, Pos, Pos, New):-
	append(Acc, [e|Rest], N),
	switche(N, b, [], New). 
switch([H|Rest], Acc, Count, Pos, New):-
	append(Acc, [H], Acc1),
	Count1 is Count + 1,
	switch(Rest, Acc1, Count1, Pos, New).


switche([e|Rest], I, Acc, New):-
	append(Acc, [I|Rest], New).
switche([H|Rest], I, Acc, New):-
	append(Acc, [H], Acc1),
	switche(Rest, I, Acc1, New).

pos_e([e|_], E, E):- !.
pos_e([_|Rest], Count, E):-
	Count1 is Count + 1,
	pos_e(Rest, Count1, E).
/*
move(F, T):-
%     switch([b,w,e], T) - >  T = [e,w,b]; T = [b,e,w] 
%     switch(From, Pose, Posbw, Flag, Acc, TO)
%     Flag = 0, nothing has changed
%     Flag = 1, b/w has changed to e
%     Flag = 2, e has changed to b/w 
%     basecase: F=1, e; F=2, b/w
%     Flag = 0/2, b/w=e, Flag =1, b/w remain
	switch()
*/
	

% (c) search_agenda(+Agenda, -Visited, -Final)
search_agenda(Pos, Visited, Final):-
	search(Pos, [], Visited, Final, 0).

search([N|_], _, [], N, 0):-
	node_pos(N, Npos),
	check_goal([Npos], Npos),!.
search([n(F,G,Pos,ID,PID)|Rest], Acc, Visited, Final, Count):-
	ID = Count,
	Count1 is Count + 1,	
	append(Acc, [n(F,G,Pos,ID,PID)], Acc1),
	findall(Npos, move(Pos, Npos), NposL),
	((check_goal(NposL, POS),
	b_to_left(POS, H),
	GN is G + 1)
	-> make_node(GN, H, POS, ID, Final),
	   Visited = Acc1
	;  merge(Rest, NposL, GN, ID, [], NAg),
	   search(NAg, Acc1, Visited, Final, Count1)). 

merge(Age, [], _, _, Acc, NAg):-
	append(Acc, Age, NAg).
merge([], [Pos|Rest], G, PID, Acc, NAg):-
	b_to_left(Pos, H),
	make_node(G, H, Pos, PID, N),
	append(Acc, [N], Acc1),
	merge([], Rest, G, PID, Acc1, NAg).

merge([N1|Rest], [Pos|Rest], G, PID, Acc, NAg):-
	b_to_left(Pos, H),
	make_node(G, H, Pos, PID, N2),
	node_f(N1, F1),
	node_f(N2, F2),
	(F1 =< F2
	-> append(Acc, [N1], Acc1),
	   merge(Rest, [Pos|Rest], G, PID, Acc1, NAg)
	;  append(Acc, [N2], Acc1),
	   merge([N1|Rest], Rest, G, PID, Acc1, NAg)).

check_goal([Pos|_], Pos):-
	\+ notgoal(Pos).
check_goal([_|Rest], Pos):-
	check_goal(Rest, Pos).

notgoal([b|Rest]):-
	member(w,Rest),!.
notgoal([_|Rest]):-
	notgoal(Rest).


% (d) trace_moves(+Final, +Visited, -Seq)
trace_moves(F, V, S):-
	moves(F, V, [], S).

moves(F, [], Acc, S):-
	node_pos(F, FPos),
	node_h(F, FH),
	append(Acc, [FPos-FH], S).
moves(F, [N|Rest], Acc, S):-	
	node_pos(N, NPos),
	node_h(N, NH),
	append(Acc, [NPos-NH], Acc1),
	moves(F, Rest, Acc1, S).
	



