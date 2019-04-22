%% File: crossings.pl
%% Name:ZIJIA WANG
%% Date:
%%
%% This program is a solution to Prolog 531 Assessed Exercise 2 'Crossings'
%% The exercise is a version of the classic Farmer-Wolf-Goat-Cabbage Puzzle

%% Step 1 safe(+Bank)
safe(B):-
	member(f, B),!.
safe(B):-
	\+ unsafe(B).

unsafe(B):-
	member(w, B),
	member(g, B).
unsafe(B):-
	member(g, B),
	member(c, B).

%% Step 2 goal(+State)
goal([]-South):-
	length(South, 5),
	member(f, South),
	member(w, South),
	member(g, South),
	member(c, South),
	member(b, South). 

%% Step 3 equiv(+State1, +State2)
equiv(North1-South1, North2-South2):-
	length(North1, N),
	length(North2, N),
	length(South1, S),
	length(South2, S),
	same(North1, North2),
	same(South1, South2).

same([], _).
same(B, B):- !.
same([B1|Rest], B2):-
	member(B1, B2),
	same(Rest, B2). 	  

%% Step 4 visited(+State, +Sequence)
visited(S, [S1|_]):-
	equiv(S, S1),!.
visited(S, [_|Seq]):-
	visited(S, Seq).

%% Step 5 choose(-Items, +Bank) % must the input bank contain f? 
choose(I, B):-
	remove(f, B, [], BRest),
	find(I0,BRest),
	I = [f|I0].
find([],B):-
	safe(B).
find([I0], B):-
	member(I0, B),
	remove(I0, B, [], BRest),
	safe(BRest).

remove(I, [I|Rest], Acc, Br):-
	append(Acc, Rest, Br),!.
remove(I, [B,C|Rest], Acc, Br):-
	Acc1 = [B|Acc],
	remove(I, [C|Rest], Acc1, Br).


%% Step 6 journey(+State1, -State2)
journey(N1-S1, N2-S2):-
	(member(f, N1)
	-> choose(I, N1),
	   remove_items(I, N1, N2),
	   append(I, S1, S2)
	;  choose(I, S1),
	   remove_items(I, S1, S2),
	   append(I, N1, N2)).
	
remove_items([], B, B).
remove_items([I|R], B, New):-
	remove(I, B, [], BRest),
	remove_items(R, BRest, New).

	    
%% Step 7 succeeds(-Sequence)
succeeds(S):-
	extend([f,w,g,c,b]-[], [[f,w,g,c,b]-[]], S).

extend(S, Seq, Seq):-
	goal(S).
extend(S, Acc, Sequence):-
	journey(S, SNew),
	\+ visited(SNew, Acc),
	append(Acc, [SNew], Acc1),
	extend(SNew, Acc1, Sequence).
	

%% Step 8 fee(+State1, +State2, -Fee)
fee(N1-_, N2-_, Fee):-
	length(N1, L1),
	length(N2, L2),
	D is abs(L2 - L1),
	(D = 1
	-> fee(Fee, _)
	;  fee(_, Fee)).
	

fee(1,2).

%% Step 9 cost(-Sequence, -Cost)
cost(Seq, Cost):-
	co(Seq, 0, Cost).

co([S], Acc, Acc):-
	goal(S).
co([S1,S2|Rest], Acc, Cost):-
	fee(S1, S2, Fee),
	Acc1 is Acc + Fee,
	co([S2|Rest], Acc1, Cost).

	
	
