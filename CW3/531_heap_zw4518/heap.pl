% 531 Prolog
% Assessed Exercise 3
% heap.pl
:- consult(support).

% Write your answers to the exercise here


% Task 1. is_heap(+H). Succeeds if H is a binary heap.
is_heap(empty).
is_heap(heap(K,_,LH,RH)):-
	integer(K),	
	check(K,LH),	
	check(K,RH).

check(_,empty).
check(K,H):-
	H = heap(KL,_,_,_),
	K >= KL,
	is_heap(H).

% Task 2. add_to_heap(+K, +I, +H, -NewH)
add_to_heap(K,I,empty,heap(K,I,empty,empty)).
add_to_heap(K, I, heap(K1,I1,LH,empty), NewH) :-        % the bottom can only have two condition, both R,L are empty or only R is empty
	(K < K1	
	-> NewH = heap(K1,I1,heap(K,I,empty,empty),LH)
	;  NewH = heap(K,I,heap(K1,I1,empty,empty),LH)
	).
add_to_heap(K, I, heap(K1, I1, LH, RH), NewH) :-
	(K >= K1
	-> add_to_heap(K1, I1, RH, NewR),
	   NewH = heap(K, I, NewR, LH)
	;  add_to_heap(K,I,RH,NewR),
	   NewH = heap(K1, I1, NewR, LH)
	).

% Task 3. remove_max(+H, -K, -I, -NewH)
remove_max(heap(K,I,empty,empty),K,I,empty).
remove_max(H_O, Re_K, Re_I, NewH) :-    
	remove_left(H_O,H_N,K,I),
	H_N = heap(Re_K,Re_I,L_N,R_N),
	N_H = heap(K,I,L_N,R_N),
	(is_heap(N_H)
	-> NewH = N_H
	;  reorder(N_H,NewH)
	).
reorder(empty,empty).
reorder(heap(K,I,empty,empty),heap(K,I,empty,empty)).
reorder(heap(K,I,LH,RH),NewH):-
	LH = heap(KL,IL,LL,RL),	
	(RH = empty
	-> (KL >= K
	   -> NewH = heap(KL,IL,heap(K,I,LL,RL),RH)
	   ;  NewH = heap(K,I,LH,RH)
	   )
	;  RH = heap(KR,IR,LR,RR),
	   KR >= KL
	   ->reorder(LH,LN),
	     (KR >= K
	      -> reorder(heap(K,I,LR,RR),RN),
	         NewH = heap(KR,IR,LN,RN)
	      ;  reorder(RH,RN),
		 NewH = heap(K,I, LN,RN)
	     )
	   ; reorder(RH,RN),
	       (KL >= K
	       -> reorder(heap(K,I,LL,RL),LN),
	          NewH = heap(KL,IL,LN,RN)
	       ;  reorder(LH,LN),
		  NewH = heap(K,I,LN,RN)
	     )
	).


remove_left(heap(K_O,I_O,L_O,R_O),heap(K_O,I_O,LN,RN),K,I):-
	(
	R_O = empty
	-> (LN = empty,			
	   RN = empty,
	   L_O = heap(K,I,_,_))
	;  L_O = heap(K,I,empty,empty)
	-> LN = R_O,
	   RN = empty
	;  (remove_left(L_O,RN,K,I),		
	   LN = R_O)
	).

% Task 4. heap_sort_asc(+L, -S)
heap_sort_asc(L, S) :-        
	create_heap(L, empty, H),
	sort_heap(H,[],S).

sort_heap(empty,S,S).
sort_heap(H,S1,S):-
	remove_max(H, K, I, HN),
	S2 = [(K,I)|S1],
	sort_heap(HN,S2,S).

create_heap([], H, H).
create_heap([(K,I)|Rest],H,HN):-
	add_to_heap(K,I,H,H1),
	create_heap(Rest,H1,HN).

% Task 5. delete_from_heap(+I, +H, -NewH)
delete_from_heap(I, heap(K, I, LH, RH), NewH) :-   	%root or left subtree node
	remove_max(heap(K, I, LH, RH), _, _, NewH),!.
delete_from_heap(I, H, NewH):-
	H = heap(K1, I1, LH, RH),
	(delete_from_heap(I, LH, LN)			%left subtree
	-> N_H = heap(K1, I1, RH, LN)	
	;  remove_left(H,heap(NK,NI,_,NL),DeK,DeI),
	   delete_from_right(I, DeK, DeI, RH, RN),	%right subtree
	   N_H = heap(NK, NI, RN, NL)
	),
	(is_heap(N_H)
	-> NewH = N_H
	;  reorder(N_H,NewH)
	).


delete_from_right(I, K1, I1, heap(_,I,LH,RH), RN):-
	RN = heap(K1, I1, LH,RH).
delete_from_right(I,K1,I1,heap(KO,IO,LH,RH),N_H):-
	(delete_from_right(I,K1,I1,LH,LN)
	-> N_H = heap(KO,IO,LN,RH)
	;  delete_from_right(I,K1,I1,RH,RN),
	   N_H = heap(KO,IO,LH,RN)
	).


