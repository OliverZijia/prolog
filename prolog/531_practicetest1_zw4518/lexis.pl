%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         MSc Prolog 531                           %
%                                                  %
%         Lexis Test                               %
%                                                  %
%         Question 1 (prison)                      %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         Question 1 (prison)                      %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% compile the Prison Database etc.

:- ensure_loaded(utilities).



cell(N) :-
   cells(Cells),
   in_range(1,Cells,N).


forall(C1, C2) :- \+ (C1, \+ C2).

%% ------ Add your code to this file here.

%% Uncomment the next two lines to skip Question 1 (a)
%% :- use_module(library(between)). 
%% in_range(Min,Max,N) :- between(Min,Max,N).

% in_range(+Min,+Max,?N)
/*
tail recursion:
counting_sum(Count, Sum):-
  counting_sum(Count, 0, Sum).

counting_sum(0, Sum, Sum).
counting_sum(Num, PrevSum, Sum):- Num > 0, PrevNum is Num - 1,
    NextSum is PrevSum + Num,
    counting_sum(PrevNum, NextSum, Sum).

in_range(Min,Max,N):-
	generate(Min,Max,Min,N).


generate(Min,Max,Count,Count):-
	Max >= Count,
	N = Count;
	Max >= Min,
	Count1 is Count + 1,
	generate(Min, Max,Count, N).
*/



in_range(Min,Max,Min):-
	Max >= Min.
in_range(Min,Max,N):-
	Max >= Min,
	Count is Min + 1,
	in_range(Count,Max,N).



% --- empty cell
% empty_cell(?Cell)

empty_cell(Cell) :-
	cells(N),
	in_range(1, N, Cell),
	\+ prisoner(_, _, Cell, _, _, _).

% all_female_cell(?Cell)

all_female_cell(Cell):-
	cells(N),
	in_range(1, N, Cell),
	\+ empty_cell(Cell),
%	in_range(1, 132, Cell),
	forall(prisoner(_, Fir, Cell, _, _, _), female_name(Fir)).


% female_prisoners(?N)

female_prisoners(N) :- 
	findall((Sur,Fir),(prisoner(Sur, Fir, _, _, _, _),female_name(Fir)),L),
	length(L,N).

% not using findall
female_prisoners1(N):-
	female_pri(0, [], N).
/*
female_pri(N, Visited, N):-
	forall((prisoner(Sur, Fir, A, B, C, D),female_name(Fir)),
	        member(prisoner(Sur, Fir, A, B, C, D),Visited)),!.
*/
female_pri(Count, Visited, N):-
	prisoner(Sur, Fir, A, B, C, D),
	female_name(Fir),
	\+ member(prisoner(Sur, Fir, A, B, C, D),Visited),!,
	V1 = [prisoner(Sur, Fir, A, B, C, D)|Visited],
%	write(V1),
%	nl,
	Count1 is Count + 1,
	female_pri(Count1, V1, N).
female_pri(N, _, N).


cell_occupancy(Cell, N) :-
	cells(CellNum),
	in_range(1, CellNum, Cell),
	findall(Fir,prisoner(_, Fir, Cell, _, _, _),Names),
	length(Names,N).


% fullest_cell(?Cell)
fullest_cell(Cell) :- 
	cells(CellNum),
	in_range(1, CellNum, Cell),
	findall(N1,cell_occupancy(_, N1),Ns),
	forall(cell_occupancy(Cell,N),compare(N,Ns)).

compare(_,[]).
compare(N,[N1|Rest]):-
	N >= N1,
	compare(N,Rest).

% worst_psychopath(?S,?F,?Crime,?T)

worst_psychopath(S, F, Crime, T) :- 
	psychopath(S, F),
	prisoner(S, F, _, Crime, T, _),
	findall(Time,
	(psychopath(Sur,Fir),prisoner(Sur, Fir, _, _, Time, _)),
	TList),
	compare(T,TList).

% criminals(?Crime,?N)
/*
criminals(Crime,N) :-
  setof((S,F), 
        (Cell,Term,Left)^prisoner(S,F,Cell,Crime,Term,Left),
        Criminals
       ),
  length(Criminals, N).

*/


criminals(Crime, N) :-
%	setof((S,F), (Cell,S,T)^prisoner(S, F, Cell, Crime, S, T), Names),
% use S twice!!!!! stupid!

	setof((S,F), 
	      (Cell,Sen,T)^prisoner(S,F,Cell,Crime,Sen,T), 
	      Names),
	length(Names,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         Question 2 (ciphers)                     %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% character codes
%     a  97
%     z 122
%     A  65     (97 - 32)
%     Z  90     (122 -32)

%% ------ Add your code to this file here.


% upper_case_string(+String)
upper_case_string([]).
upper_case_string([H|Rest]) :- 
	H =< 90,
	H >= 65, 
	upper_case_string(Rest).

% subst_string(+Input, +Subst, -Output)

subst_string(Input, Subst, Output) :-
	subst(Input, Subst, [], Output).

subst([],_,Acc,Acc).
subst([Input|Rest1],In-Out,Acc,Output):-
	(member(Input, In)
	-> append(Acc,[Sub],Acc1),
	   sub(Input,In-Out,Sub)
	;  append(Acc,[Input],Acc1)),
	subst(Rest1,In-Out,Acc1,Output).

sub(In,[In|_]-[Sub|_],Sub).
sub(In,[_|IRest]-[_|ORest],Sub):-
	sub(In,IRest-ORest,Sub).
	
% encrypt_string(+Plain, +Key, -Cipher) 
/*
encrypt_string(Plain, Key, Cipher) :- 
	encrypt(Plain, Key, [], Cipher).

encrypt([], _, Acc, Acc).
encrypt([H|Rest], Key, Acc, Cipher):-
	((H =< 90,
	H >= 65,
	H1 is H + 32)
	-> encry_sub(H1, Key, Sub)
	;  encry_sub(H, Key, Sub)),
	append(Acc,[Sub],Acc1),
	encrypt(Rest, Key, Acc1, Cipher). %TODO Acc

encry_sub(Plain, Key, Sub):-
	((Plain =< 122,
	Plain >= 97)
	-> Count is Plain - 97,
	   subbykey(Count, Key, Sub)
	;  Sub = Plain).

subbykey(0, [Sub|_], Sub).
subbykey(Count, [_|Rest], Sub):-
	Count1 is Count - 1,
	subbykey(Count1, Rest, Sub).
*/
encrypt_string(Plain, Key, Cipher) :- 
	encrypt(Plain, Key, [], Cipher).
%Base case
encrypt([], _, C, C).

%Recursive case
encrypt([P|Rest], K, Acc, C):-
	upper_case_string([P]),
	P1 is P + 32,
	subst_string([P1], "abcdefghijklmnopqrstuvwxyz"-K, PNew),
	append(Acc, PNew, Acc1),
	encrypt(Rest, K, Acc1, C),!.

encrypt([P|Rest], K, Acc, C):-
	subst_string([P], "abcdefghijklmnopqrstuvwxyz"-K, PNew),
	append(Acc, PNew, Acc1),
	encrypt(Rest, K, Acc1, C).


% decrypt_string(+Cipher, +Key, -Plain)

decrypt_string(Cipher, Key, Plain) :- 
	decrypt(Cipher, Key, [], Plain).
/*
decrypt([], _, Acc, Acc).
decrypt([H|Rest], Key, Acc, Plain):-
	decry_sub(H, Key, Sub),
	append(Acc,[Sub],Acc1),
	decrypt(Rest, Key, Acc1, Plain). 

decry_sub(Cipher, Key, Sub):-
	((Cipher =< 90,
	Cipher >= 65)
	-> de_sub(Cipher, Key, 97, Sub)
	;  Sub = Cipher).

de_sub(Cipher,[Cipher|_],Count, Count).
de_sub(Cipher,[_|Rest],Count, Sub):-
	Count1 is Count + 1,
	de_sub(Cipher, Rest, Count1, Sub).
*/
%Base case
decrypt([], _, C, C).

%Recursive case
decrypt([P|Rest], K, Acc, C):-
	subst_string([P], K-"abcdefghijklmnopqrstuvwxyz", PNew),
	append(Acc, PNew, Acc1),
	decrypt(Rest, K, Acc1, C).

% keyphrase_cipher(+Keyphrase, -Key)

keyphrase_cipher(KeyPhrase, Key) :- 
%	remove_duplicate(KeyPhrase,ReKey),
%	write(ReKey),
%	nl,
	remove_duplicate(KeyPhrase, [], ReKey),
	length(X,1),
	append(_, X, ReKey),
%	write(X),
%	nl,
	Start is X + 1,
	remain_char(X, Start, ReKey, Rest),
%	write(Rest),
%	nl,
	append(ReKey, Rest, Key).

/*wrong order BORINGLECTURE-> BOINGLCTURE(shoule be BORINGLECTU)
% TODO if we need result to have same order with input
%      then we can't use Acc, and base case is kind of different
remove_duplicate([Key], [Key]):- 
	upper_case_string([Key]),!.
remove_duplicate([Key|Rest], [Key|T]):-
	upper_case_string([Key]),	
	\+ member(Key, Rest),
	remove_duplicate(Rest,T).
remove_duplicate([_|Rest], Rekey):-
	remove_duplicate(Rest,Rekey).
*/
%TODO write this as case of tail recursion, and causion to order
remove_duplicate([], Acc, Acc).
remove_duplicate([Key|Rest], Acc, Rekey):-
	upper_case_string([Key])	
	-> (member(Key, Acc)
	    -> remove_duplicate(Rest, Acc, Rekey)
	    ;  append(Acc, [Key], Acc1),
	       remove_duplicate(Rest, Acc1, Rekey))
	; remove_duplicate(Rest, Acc, Rekey).

remain_char([X], S, Key, Rest):-
	X is S + 1,
	(member(S, Key)
	-> Rest = []
	;  Rest = [S]),!.
remain_char(X, S, Key, Rest):-
	S > 90,
	S1 is 65,
	remain_char(X, S1, Key, Rest).
remain_char(X, S, Key, Rest):-
	S =< 90,
	S1 is S + 1,
	(member(S,Key)
	-> remain_char(X, S1, Key, Rest)
	;  Rest = [S|Rest1],
	   remain_char(X, S1, Key, Rest1)).


	
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         Question 3 (graphs)                      %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% ------ Add your code to this file here.


/* Uncomment this to skip Question 3 (a)

merge_ordered(Left,Right,Merged) :-
   append(Left,Right,Both),
   sort(Both,Merged).

*/

% merge_ordered(+Left,+Right,-Merged)

merge_ordered(L, R, Mer):-
	merge(L, R, [], Mer).

merge([], R, Acc, Mer):-
	append(Acc, R, Mer).
merge(L, [], Acc, Mer):-
	append(Acc, L, Mer).
merge([L|LRest], [R|RRest], Acc, Mer):-
	(L @=< R
	-> append(Acc, [L], Acc1),
	   (L = R
	   -> merge(LRest, RRest, Acc1, Mer)
	   ;  merge(LRest, [R|RRest], Acc1, Mer))
	;  append(Acc, [R], Acc1),
  	   merge([L|LRest], RRest, Acc1, Mer)).



% hf_to_graph_term(+Hform, -Graph)

hf_to_graph_term(Hf, G) :- 
	trans_hf(Hf, [], [], G).

% TODO trans_hf(Hf, AccNode, AccArc, G), G= graph(AccNode, AccArc)
trans_hf([], Node, Arc, graph(Node, Arc)).
trans_hf([X>Y|HRest], AccN, AccA, G):-
	merge_ordered([e(X,Y)], AccA, AccA1),
	merge_ordered([X], AccN, AccN1),
	merge_ordered([Y], AccN1, AccN2),
	trans_hf(HRest, AccN2, AccA1, G),!.

trans_hf([X|HRest], AccN, AccA, G):-
	merge_ordered([X], AccN, AccN1),
	trans_hf(HRest, AccN1, AccA, G).

% graph_term_to_adj_list(+Graph, -AdjList)

graph_term_to_adj_list(G, Adj) :- 
	trans_graph(G, [], Adj).

trans_graph(graph([],[]), Acc, Acc).
trans_graph(graph([X|RN],[e(X,Y)|RArc]), Acc, Adj):-
	gen_adj(X, [Y], RArc, RestArc, AdjNodes),
	append(Acc, [n(X, AdjNodes)], Acc1),
	trans_graph(graph(RN,RestArc), Acc1, Adj),!.
trans_graph(graph([X|RN],RArc), Acc, Adj):-
	append(Acc, [n(X,[])], Acc1),
	trans_graph(graph(RN,RArc), Acc1, Adj),!.	

gen_adj(_, Acc, [], [], Acc).
gen_adj(X, Acc, [e(Y,Z)|RArc], [e(Y,Z)|RArc], Acc):-
	X \= Y.
gen_adj(X, Acc, [e(X,Y)|RArc], Rest, AdjNodes):-
	append(Acc, [Y], Acc1),
	gen_adj(X, Acc1, RArc, Rest, AdjNodes),!.

a(1,2).
a(1,3).
test(A,L):-
	setof(X, a(A,X), L).





