% lexis.pl


:-consult('support').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1 (5%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% read_move(-Move) ... [or read_move(-Move, +Input, ?Remainder) as a non-DCG rule]
%   Given an Input string of the form:
%     "xy"        - where x is a valid column and y is a valid row, e.g. "e6"
%   or 
%     "xyd" - where x is a valid column 
%                   y is a valid row
%                   d is either "v" or "h", e.g. "c6h"
%   Returns the corresponding internal representation of the move as Move 

% TODO  "a" = [97], it's a list!!!!!!
read_move((X,Y)) -->
	[X1, Y1],
	{
	member([X1], ["a", "b", "c", "d", "e", "f", "g", "h", "i"]),
	member([Y1], ["1", "2", "3", "4", "5", "6", "7", "8", "9"]),
	X is X1 - "a" + 1,
	Y is Y1 - "0"
	}.

read_move((X,Y,D)) -->
	[X1, Y1, D1],
	{
	member([X1], ["a", "b", "c", "d", "e", "f", "g", "h"]),
	member([Y1], ["1", "2", "3", "4", "5", "6", "7", "8"]),
	X is X1 - "a" + 1,
	Y is Y1 - "0",
	member([D1], ["v", "h"]),
	(D1 == 118
	-> D = v
	;  D = h)
	}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2 (50%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2 (a) in_range(?N,+Min,+Max) 
% :- use_module(library(between)).
% Uncomment the following to skip this part.
/*


in_range(N, Min, Max):-
  between(Min, Max, N).
%%%%%%%%%% method1:  consider as two conditions
in_range(N, R1, R2):-
	(R1 =< R2
	-> in(N, R1, R2)
	;  in(N, R2, R1)).

in(N, Min, Max):-
	N = Min;
	Min < Max,
	Min1 is Min + 1,
	in(N, Min1, Max).
%%%%%%%%%%%
*/
%TODO tip:
%%%%%%%%%%% method2: use a accumulator to record the visited numbers
%%%%%%%%%%%          pay attention to the position of accumulator
%%%%%%%%%%%          make sure all backtrack can use this accumulator,
%%%%%%%%%%%          no backtrack before the accumulator 
in_range(N, R1, R2):-
	in(N, R1, R2, []).

%in(N, N, N, _).
in(N, R1, R2, Acc):-
	N = R1;
	(R1 < R2
	-> R11 is R1 + 1
	;  R1 > R2
	-> R11 is R1 - 1),
	Acc1 = [R1|Acc],
	\+ member(R11, Acc),
	in(N, R11, R2, Acc1).

% 2(b) fence_space(+Fs,?Space)
%   Given the list Fs of existing fences,
%   Space is a fence space, i.e. a new fence Space = (X,Y,Dir)
%   could be added, ignoring the paths open to either pawn.

% Uncomment the following incomplete program to skip this part.
/*
fence_space(_, (1,1,h)).
fence_space(_, (1,1,v)).
*/
fence_space(F, (X,Y,D)):-
	in_range(X, 1, 8),
	in_range(Y, 1, 8),
	(\+ invalid(F, (X,Y,v)),
	    D = v;
	\+ invalid(F, (X,Y,h)),
	    D = h). 

invalid([(X1,Y1,D)|_], (X2,Y2,D)):-
	(D = v
	-> X1 = X2,
	   Diff is abs(Y1 - Y2)
	;  Y1 = Y2,
	   Diff is abs(X1 - X2)),
	Diff =< 1,
	!.
invalid([(X,Y,_)|_], (X,Y,_)):-
	!.
invalid([_|Rest], N):-
	invalid(Rest, N).

/*
test((X, Y)):-
	in_range(X, 1, 9),
	in_range(Y, 1, 9).
*/	
% 2(c) reachable(+From,+Fences,?To)TODO rethink this question!
%   Succeeds iff To = (X1,Y1) is reachable by a pawn at From = (X,Y) 
%   with the given list of Fences, ignoring the position of the other pawn.

% Uncomment the following incomplete program to skip this part.
/*
reachable(_, _, (1,1)).
reachable(_, _, (9,9)).

reachable(From,Fences,(X1, Y1)):-
%	in_range(X1, 1, 9),
%	in_range(Y1, 1, 9),
	reach(From, Fences, [], (X1, Y1)).

reach((X,Y), Fen, Acc, T):-
	next((X,Y), (X1,Y1), Fen, Acc),
	\+ member((X1, Y1), Acc),
	(T = (X1, Y1);
	append(Acc, [(X1, Y1)],Acc1),
	reach((X1, Y1), Fen, Acc1, T)).
	

next((X,Y), (X1,Y1), Fen, Acc):-
	(((X1 is X + 1;
	  X1 is X - 1),
	  Y1 = Y);
	((Y1 is Y + 1;
	  Y1 is Y - 1),
	  X1 = X);
	(X1 = X,
	Y1 = Y)),
	\+ member((X1, Y1), Acc),
	\+ blocked((X,Y), (X1,Y1), Fen),
	X1 > 0,
	X1 =< 9,
	Y1 > 0,
	Y1 =< 9.

blocked((X1,Y1), (X2, Y1), Fen):-
	D is X1 - X2,
	(D = 1
	-> v_fence_at(X2,Y1,Fen)
	;D = -1
	-> v_fence_at(X1, Y1, Fen)).
blocked((X1,Y1), (X1, Y2), Fen):-
	D is Y1 - Y2,
	(D = 1
	-> h_fence_at(X1,Y2,Fen)
	;D = -1
	-> h_fence_at(X1, Y1, Fen)).
*/

reachable(From,Fences,TO):-
%	search(From, Fences, [], [], TO).
% reach(From, Fences, AccVisited, AccUnused, TO)
	reach(From, Fences, [], [], TO).

/*
%search(From, Fences, Vis, [], TO).
search(From, Fences, Vis, Un, TO):-
% reach(From, Fences, AccVisited, Visited, AccUnused, UnusedNew, TO)
	reach(From, Fences, Vis, VisNew, Un, UnN, TO1),
	(TO = TO1;
	UnN = [(X,Y)|UNN],
	search((X,Y), Fences, VisNew, UNN, TO)).
*/
%reach((X,Y), Fen, V, V, U, U, T):-
%	findall((X0,Y0), next((X,Y), (X0,Y0), Fen, V), []).	
reach((X,Y), Fen, AccV, AccU, T):-
	findall((X0,Y0), next((X,Y), (X0,Y0), Fen, AccV), List),
	List = [(X1, Y1)|List1]
	-> (add(List1, AccU, AccU1),
	   remove((X1, Y1), AccU1, [], AccU2),
	   (T = (X1, Y1);
	   append(AccV, [(X1, Y1)], AccV1),
	   reach((X1, Y1), Fen, AccV1, AccU2, T)))
	;  (AccU = [(X1, Y1)| U],
	   (T = (X1, Y1);
	   append(AccV, [(X1, Y1)], AccV1),
	   reach((X1, Y1), Fen, AccV1, U, T))).

remove(_, [], Acc, Acc).
remove(X, [Y|Rest], Acc, AccU2):-
	X \= Y,
	append(Acc, [Y], Acc1),
	remove(X, Rest, Acc1, AccU2).
remove(X, [X|Rest], Acc, AccU2):-
	append(Acc, Rest, AccU2).

add([], U, U).
add([H|T], U, U1):-
	member(H, U),	
	add(T, U, U1),!.
add([H|T], U, U1):-
	U0 = [H|U],
	add(T, U0, U1).	

next((X,Y), (X1,Y1), Fen, Acc):-
	(((X1 is X + 1;
	  X1 is X - 1),
	  Y1 = Y);
	((Y1 is Y + 1;
	  Y1 is Y - 1),
	  X1 = X);
	(X1 = X,
	Y1 = Y)),
	\+ member((X1, Y1), Acc),
	\+ blocked((X,Y), (X1,Y1), Fen),
	X1 > 0,
	X1 =< 9,
	Y1 > 0,
	Y1 =< 9.

blocked((X1,Y1), (X2, Y1), Fen):-
	D is X1 - X2,
	(D = 1
	-> v_fence_at(X2,Y1,Fen)
	;D = -1
	-> v_fence_at(X1, Y1, Fen)).
blocked((X1,Y1), (X1, Y2), Fen):-
	D is Y1 - Y2,
	(D = 1
	-> h_fence_at(X1,Y2,Fen)
	;D = -1
	-> h_fence_at(X1, Y1, Fen)).
	




% 2(d) fence_move(NumFs, Fences, (OppX,OppY), OppTarget, NewF)
%   Given NumFs, the number of fences the player has left to play,
%   the list Fs = [(X,Y,Dir),...] of fences on the board, 
%   (OppX,OppY) the position of the opposing pawn, and
%   OppTarget, the row the opponent is trying to reach,
%   returns a fence = (X,Y,Dir) that can be added according to the rules.

fence_move(NumFs, Fences, (OppX,OppY), OppTarget, NewF):-
	NumFs > 0,
	fence_space(Fences, NewF),
	Fences1 = [NewF|Fences],
	rea((OppX,OppY),Fences1,(_,OppTarget)).
rea((OppX,OppY),Fences1,(_,OppTarget)):-
	reachable((OppX,OppY),Fences1,(_,OppTarget)),!.
% 	reachable((OppX,OppY),Fences1,(OppTarget,_)).   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3 (20%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
% pawn_move(+OpSq,+Fs,+From,?To).
%   Given pairs OpSq = (OppX,OppY) and From = (X,Y) defining the current
%   locations of the two pawns, and a list Fs = [(F1X,F1Y,F1Dir),...], 
%   returns a valid square To = (X1,Y1) that the pawn at (X,Y) can move to 
%   in a single turn.

% Uncomment the following to skip this Question.
/*
pawn_move(_, _, _, (5,2)).
pawn_move(_, _, _, (6,1)).
pawn_move(_, _, _, (4,1)).
*/
% two nodes are not adj
pawn_move((OX,OY), Fs, (X,Y), T):-
	next1((X,Y), (X1,Y1), Fs, []),
	\+ (X1 == OX,
	Y1 == OY),
	T = (X1,Y1).
% two nodes are adj
pawn_move((OX,OY), Fs, (X,Y), T):-	
    	1 is OY - Y,
	Y1 is 2 * OY - Y,
	X1 = OX,
	(\+ v_fence_at(OX, OY, Fs)	%TODO condintional should have bracket
	-> T = (X1,Y1)
	;  lshape((OX,OY), Fs, T, 1));

	-1 is OY - Y,
	Y1 is 2 * OY - Y,
	X1 = OX,
	(\+ v_fence_at(X1, Y1, Fs)
	-> T = (X1,Y1)
	;  lshape((OX,OY), Fs, T, 1));

	1 is OX - X,
	X1 is 2 * OX - X,
	Y1 = OY,
	(\+ h_fence_at(OX, OY, Fs)
	-> T = (X1,Y1)
	;  lshape((OX,OY), Fs, T, 2));

	-1 is OX - X,
	X1 is 2 * OX - X,
	Y1 = OY,
	(\+ h_fence_at(X1, Y1, Fs)
	-> T = (X1,Y1)
	;  lshape((OX,OY), Fs, T, 2)).
% L shape, vertical adjcent
lshape((OX,OY), Fs, T, 1):-
	((X1 is OX + 1,
	\+ v_fence_at(OX,OY,Fs));
	(X1 is OX - 1,
	\+ v_fence_at(X1,OY,Fs))),
	Y1 = OY,
	T = (X1, Y1).
lshape((OX,OY), Fs, T, 2):-
	((Y1 is OY + 1,
	\+ h_fence_at(OX,OY,Fs));
	(Y1 is OY - 1,
	\+ h_fence_at(OX,Y1,Fs))),
	X1 = OX,
	T = (X1, Y1).
	
next1((X,Y), (X1,Y1), Fen, Acc):-
	(((X1 is X + 1;
	  X1 is X - 1),
	  Y1 = Y);
	((Y1 is Y + 1;
	  Y1 is Y - 1),
	  X1 = X)),
	\+ member((X1, Y1), Acc),
	\+ blocked((X,Y), (X1,Y1), Fen),
	X1 > 0,
	Y1 > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 4 (15%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 4 (a) game_over(+State)
%   Given a board state State, succeeds if one of the players has won the game.
game_over([p1(_, Y1, _), p2(_, Y2, _), _]):-
	Y1 = 9;
	Y2 = 1.

% 4 (b) next_state(+S0,+Player,+Move,?S1).
%   Returns S1, given S0, Player and Move.
%   S0 and S1 are states. 
%   Move is either (X,Y) or (X,Y,Dir)
%   Player is either p1 or p2.
next_state([P1, P2, Fences], _, (X, Y, D), [P1, P2, Fences1]):-
	Fences1 = [(X, Y, D)|Fences],!.
next_state([p1(_, _, F), P2, Fences], p1, (X, Y),
	   [p1(X, Y, F), P2, Fences]).

next_state([P1, p2(_, _, F), Fences], p2, (X, Y),
	   [P1, p2(X, Y, F), Fences]).


    
% 4 (c) play(+Player,+Name,+OppName,+State,-Winner)
%  Given Player (either p1 or p2) identifying the next player to take a turn,
%  their name Name, their opponent's name OppName, and the current board State,
%  returns the winner's name as Winner.

play(P, Name, OppName, 
	[p1(_, _, _), p2(_, 1, _), _], Na):-
	(P = p1
	-> Na = OppName
	;  Na = Name).
play(P, Name, OppName, 
	[p1(_, 9, _), p2(_, _, _), _], Na):-
	(P = p2
	-> Na = OppName
	;  Na = Name).
play(Player, Name, OppName, State, Winner):-
	select_move(Player, Name, State, Move),
	next_state(State, Player, Move, S1),
	(Player = p1
	-> play(p2, OppName, Name, S1, Winner)
	;  play(p1, OppName, Name, S1, Winner)).
	


