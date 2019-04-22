/*

    Module 531: Laboratory (Prolog)
    Exercise No.4  (prison)

*/


% May be helpful for testing

% generate_integer(+Min, +Max, -I)
%   I is an integer in the range Min <= I <= Max

generate_integer(Min,Max,Min):-
  Min =< Max.
generate_integer(Min,Max,I) :-
  Min < Max,
  NewMin is Min + 1,
  generate_integer(NewMin,Max,I).
  
  
% Uncomment this line to use the provided database for Problem 2.
% TODO You MUST recomment or remove it from your submitted solution.
% :- include(prisonDb).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     Problem 1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prison_game(+Cells, +Warders, -Escaped)
%   Escaped is a list of cell numbers for prisoners who will escape
%   once all warders have completed their runs.

prison_game(Cells, Warders, Escaped) :-
  integer(Cells), Cells > 0,
  integer(Warders), Warders > 0,
  make_list(Cells, unlocked, Initial),
  run_warders(2, Warders, Initial, Final),
  extract_indices(Final, unlocked, Escaped).
  

% Write your program here.

% Task 1.1 make_list(+N, +Item, -List).
%   Given a (non-negative) integer N and item Item constructs list List of N
%   elements each of which is Item
/*
make_list(1,I,[I]):- !.		
make_list(N, I, [I|Rest]) :-
	N1 is N-1,
	make_list(N1,I,Rest).
*/
make_list(N,I,L):-
	make(N,I,[],L).

make(1,I,L,[I|L]):- !.
make(N,I,L1,L):-
	N1 is N-1,
	make(N1,I,[I|L1],L).



% Task 1.2 extract_indices(+List, +Item, -Indices).
%   Given list List and item Item computes the list Indices of integers N such
%   that Item is the Nth item of the list List

extract_indices(L, I, Indices) :- 	
	findall(Indice,indices(L,I,Indice),Indices). 

indices([I|_], I, 1). 			%find the index of I
indices([_|Rest], I, Index):-
	indices(Rest, I, Index1), 
	Index is Index1+1.



% Task 1.3 run_warders(+N, +W, +Initial, -Final). 
%   Given next warder N and total warders W (both positive integers), and 
%   current door states Initial (a list of the constants locked and unlocked) 
%   returns Final, the list of door states after all warders have completed 
%   their runs.
/*
run_warders(N, W, INI, FIN) :- 
	length(INI,Len),
	change_state(N,W,1,Len,INI,FIN).

change_state(N,W,Index,Len,INI,FIN):-
	(Len >= Index	
	-> is_change(N,W,Index,0,S),
	   (S = 1
	   -> replace(INI,Index,INI1)
	   ;  INI1 = INI),
	   Index1 is Index + 1,
	   change_state(N,W,Index1,Len,INI1,FIN)
	;  FIN = INI).

is_change(N,W,Index,S1,S):-		% S=1:change; S=0:no change
	(W >= N
	-> (0 is mod(Index,N)
	   -> S2 is S1+1
	   ;  S2 is S1),
	   N1 is N + 1,
	   is_change(N1,W,Index,S2,S)
	;  S is mod(S1,2)
	).
	
replace(L, Index, R) :-			%L is original list, R is returns
        Prelen is Index - 1,
	length(Pre, Prelen),
	append(Pre, [S|T], L),
	(S = locked
	-> append(Pre, [unlocked|T], R)
	;  append(Pre, [locked|T], R)).	
*/
run_warders(N, W, INI, FIN) :- 
% change(Len1,Index,N,W,INI,FIN)
	length(INI,Len),
	change(Len,1,N,W,INI,FIN). 

change(_,_,_,_,[],[]).
change(Len,Index,N,W,[I|Rest],FIN):-
	(is_change(N,W,Index,0,0)	
	-> I1 = I
	;  (I = locked
	   -> I1 = unlocked
	   ;  I1 = locked)),
	Index1 is Index + 1,
	change(Len,Index1,N,W,Rest,FIN1),
	FIN = [I1|FIN1].

is_change(N,W,Index,S1,S):-		% S=1:change; S=0:no change
	(W >= N
	-> (0 is mod(Index,N)
	   -> S2 is S1+1
	   ;  S2 is S1),
	   N1 is N + 1,

	   is_change(N1,W,Index,S2,S)

	;  S is mod(S1,2)

	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     Problem 2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write your program here.

% Task 2.1 cell_status(+Cell, +N, ?Status)
%   Succeeds if the status of cell Cell is Status after N warders have made 
%   their runs. If Status is a variable, then the correct value should be 
%   returned.

cell_status(Cell, N, Status) :- 	   % TODO replace this clause
	findall((Surname,FirstName),prisoner(Surname, FirstName,Cell,_, _, _),Pri),
	(is_psy(Pri)			   
	-> Status = locked
	;  ((N > Cell
	    -> N1 = Cell
	    ;  N1 = N),
	   changed(Cell,1,N,0) 	   %changed(Cell,Warder,N,Status)
	   -> Status = unlocked
	   ;  Status = locked)
	).

changed(Cell,W,N,S):-
	(N >= W
	-> (0 is mod(Cell,W)
	   -> S1 is S + 1
	   ;  S1 = S),
	   W1 is W +1,
	   changed(Cell, W1, N, S1)
	;  1 is mod(S,2)). 

is_psy([(Sur,Fir)|Rest]):-
	psychopath(Sur, Fir),!,
	is_psy(Rest).
% Task 2.2 

% escaped(?Surname, ?FirstName)
%   holds when the prisoner with that name escapes (i.e., occupies a cell which 
%   is unlocked after the last warder has made his run, but bearing in mind that
%   prisoners with a year or less left to serve will not escape).

escaped(Sur, Fir) :-      % TODO replace this clause
	warders(N),
	prisoner(Sur, Fir,Cell,_, Sen, Left),
	cell_status(Cell,N,unlocked),
	1 > Sen-Left.

% escapers(-List)
%   List is the list of escaped prisoners. List is a list of terms of the form 
%   (Surname, FirstName), sorted in ascending alphabetical order according to 
%   Surname.

escapers(L) :-      % TODO replace this clause
	setof((Sur,Fir),escaped(Sur,Fir),L).




