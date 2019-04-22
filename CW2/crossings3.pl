%% File: crossings.pl
%% Name:ZIJIA WANG
%% Date:
%%
%% This program is a solution to Prolog 531 Assessed Exercise 2 'Crossings'
%% The exercise is a version of the classic Farmer-Wolf-Goat-Cabbage Puzzle

%% Step 1 safe(+Bank)
safe(Bank):-				
	member(f,Bank),!;	
	(member(g,Bank)
	-> \+ member(w,Bank),
	   \+ member(c,Bank)
	;  safe([f])).

%% Step 2 goal(+State)
goal([]-South):-
	length(South,5),
	member(c,South),
	member(f,South),
	member(g,South),
	member(f,South),
	member(b,South).


%% Step 3 equiv(+State1, +State2)
equiv(North1-South1, North2-South2):-		
	length(North1,N1),
	length(North2,N2),
	N1 == N2,
	length(South1,S1),
	(N1 < S1
	-> check_eq(North1,North2)
	;  check_eq(South1,South2)). 
check_eq(A,A).
check_eq([A,B],[B,A]).           		  

%% Step 4 visited(+State, +Sequence)
visited(State, [St1|Rest]):-
	equiv(State,St1),!;
	visited(State,Rest).

%% Step 5 choose(-Items, +Bank) % must the input bank contain f? 
choose(Items, Bank):-				% if remainders is safe, farmer brings the first item 
	remove_items([f],Remainder,Bank),
	safe(Remainder),	
	Items = [f];
	remove_items([f],Remainder,Bank),
	finditem(Item,Remainder),
	Items = [Item|[f]].

finditem(Item,Bank):-
	(safe(Bank)				%if remainders are safe, farmer can bring anything
	-> member(Item,Bank)
	;  (member(g,Bank),				%elseif having a goat, must safe without goat, bring the goat
	    Item = g;
	    member(w,Bank),
	    remove_items([w],Remainder,Bank),	%elseif safe without wolf, bring the wolf
	    safe(Remainder),
	    Item = w;
	    member(c,Bank),
	    remove_items([c],Remainder,Bank),	%elseif safe without cabbage, bring the cabbage
	    safe(Remainder),
	    Item = c)).

remove_items([],Remainder,Remainder).		
remove_items([Item1|Item2],Remainder,
		[Item1|Remainder1]):-
	remove_items(Item2,Remainder,
			Remainder1).
remove_items([Item1|Item2],Remainder,
		[Item_2|Remainder1]):-
	Item2 = [Item_2],
	remove_items([Item1],Remainder,
			Remainder1).
remove_items([Item1|Item2],[Item|Remainder],
		[Item|Remainder1]):-
	remove_items([Item1|Item2],Remainder,
			Remainder1).

%% Step 6 journey(+State1, -State2)
journey(North1-South1, North2-South2):- 
	(member(f,North1)			%if f is in North bank, this journey should be from N to S
	-> choose(Items,North1),
	   remove_items(Items,North2,North1),
	   append(Items,South1,South2),
	   safe(South2)
	;  choose(Items,South1),		%else, this journey should be from S to N
	   remove_items(Items,South2,South1),
	   append(Items,North1,North2),
	   safe(North2)).   
	    
%% Step 7 succeeds(-Sequence)
succeeds(Sequence):-              
	extend([[f,w,g,c,b]-[]],Sequence,[]). 	% sequence is the result, and Visited is used for compare.

extend(LastState,LastState,_):-
	LastState = [Laststate],
	goal(Laststate).
extend([Laststate],[Laststate|VisitedSeq],
		Visited):-
	Visited1 = [Laststate|Visited],
%	\+ goal(Laststate),
	journey(Laststate,Newstate),
	\+ visited(Newstate,Visited1),
	extend([Newstate],VisitedSeq, 
			Visited1).


%% Step 8 fee(+State1, +State2, -Fee)
fee(North1-South1, North2-South2, Fee):-
	journey(North1-South1,NewS),
	equiv(North2-South2,NewS),		%judge if the NewS derive from a safe journey
	length(North1,N1),	
	length(North2,N2),
	(N1>N2
	-> I is N1-N2
	;  I is N2-N1),	 
	(I == 1
	-> fees(Fee,_)
	;  fees(_,Fee)).

fees(1,2).

%% Step 9 cost(-Sequence, -Cost)
cost(Sequence, Cost):-
	succeeds(Sequence),
	sum_fee(Sequence,Cost).	

sum_fee([S1,S2],Cost):-
	fee(S1,S2,Cost).
sum_fee([S1,S2|Rest],Cost):-
	fee(S1,S2,Fee1),	
	sum_fee([S2|Rest],Fee2),
	Cost is Fee1 + Fee2.
	
