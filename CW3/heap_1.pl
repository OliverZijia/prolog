% 531 Prolog
% Assessed Exercise 3
% heap.pl
:- consult(support).

% Write your answers to the exercise here


% Task 1. is_heap(+H). Succeeds if H is a binary heap.
is_heap(empty).
is_heap(heap(K,_,LH,RH)):-
	integer(K),	
	checkL(K,LH),	
	checkR(K,RH).

checkL(K,LH):-
	LH = empty;
	LH = heap(KL,_,_,_),
	integer(KL),
	K >= KL,
	is_heap(LH).
checkR(K,RH):-
	RH = empty; 
	RH = heap(KR,_,_,_),
	integer(KR),
	K >= KR,
	is_heap(RH).

  
% Task 2. add_to_heap(+K, +I, +H, -NewH)
% add_to_heap(6,i,heap(1,one,empty,empty),_1273)
% 1.add 2.check if it's balance invariant
/*can appropriately add items to right bottom(first draft),
add_to_heap(K, I, heap(K1,I1,LH,empty), NewH) :-        % the bottom can only have two condition, both R,L are empty or only L is empty
	NewH = heap(K1,I1,LH,heap(K,I,empty,empty)).
add_to_heap(K, I, heap(K1, I1, LH, RH), NewH) :-
	add_to_heap(K, I, RH, NewR),
	NewH = heap(K1, I1, LH, NewR),
	portray_heap(NewH).

% add_to_heap(7,i,heap(6,i,heap(5,i,heap(3,i,empty,empty),heap(4,i,empty,empty)),heap(4,i,heap(3,i,empty,empty),empty)),N).  TODO test more complicated case.
add_to_heap(K,I,empty,heap(K,I,empty,empty)).
add_to_heap(K, I, heap(OldK,OldI,OldL,empty), NewH) :-        % the bottom can only have two condition, both R,L are empty or only L is empty
	(OldK >= K	
	-> NewH = heap(OldK,OldI,OldL,heap(K,I,empty,empty))
	;  NewH = heap(K,I,heap(OldK,OldI,empty,empty),OldL)
	).
add_to_heap(K, I, heap(OldK, OldI, OldL, OldR), NewH) :-
	add_to_heap(K, I, OldR, NewRH),
	NewRH = heap(NewK,NewI,NewL,NewR),
	(OldK >= NewK
	-> NewH = heap(OldK, OldI, OldL, NewRH)
	;  NewH = heap(NewK, NewI, heap(OldK,OldI,NewL,NewR),OldL)
	),
	portray_heap(NewH).
*/
add_to_heap(K,I,empty,heap(K,I,empty,empty)).
add_to_heap(K, I, heap(K1,I1,LH,empty), NewH) :-        % the bottom can only have two condition, both R,L are empty or only L is empty
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

test_addto(K,I,H,HN):-
	add_to_heap(K, I, H, HN),
	portray_heap(H),
	portray_heap(HN).


test_add(L,H):-
	create_heap(L,empty,H),
	portray_heap(H).

% Task 3. remove_max(+H, -K, -I, -NewH)
remove_max(heap(K,I,empty,empty),K,I,empty).
remove_max(H_O, Re_K, Re_I, NewH) :-      % TODO replace this clause.
	remove_left(H_O,H_N,K,I),   %return the deleted node(K,I)
	H_N = heap(Re_K,Re_I,L_N,R_N),
	N_H = heap(K,I,L_N,R_N),
	(is_heap(N_H)
	-> NewH = N_H
	;  reorder(N_H,NewH)
	).
%	portray_heap(H_O),
%	portray_heap(NewH).
	
test_reorder(H1,H2):-
	reorder(H1,H2),
	portray_heap(H1),
	portray_heap(H2).

/*
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
	   ->(KR >= K
	      -> reorder(heap(K,I,LR,RR),RN),
	         NewH = heap(KR,IR,LH,RN)
	      ;  NewH = heap(K,I, LH,RH)
	     )
	   ; (KL >= K
	       -> reorder(heap(K,I,LL,RL),LN),
	          NewH = heap(KL,IL,LN,RH)
	       ;  NewH = heap(K,I,LH,RH)
	     )
	).
*/
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




/*
reorder(heap(K,I,LH,RH),NewH):-
	(RH = empty
	-> LH = heap(KR,IR,LR,RR),
	   (KR >= K
	   -> NewH = heap(KR,IR,heap(K,I,LR,RR),RH)
	   ;  NewH = heap(K,I,LH,RH)
	   )
	;  RH = heap(KR,IR,LR,RR),
	   (KR >= K
	   -> reorder(heap(K,I,LR,RR),RN),
	      NewH = heap(KR,IR,LH,RN)
	   ;  NewH = heap(K,I,LH,heap(KR,IR,LR,RR))
	   )
	).
	   
reorder(heap(K,I,empty,empty),heap(K,I,empty,empty)).
*/
/*
test_remove_left(HO,HN,K,I):-
	remove_left(HO,HN,K,I),
	portray_heap(HO),
	portray_heap(HN).
*/
remove_left(heap(K_O,I_O,L_O,R_O),heap(K_O,I_O,LN,RN),K,I):-
	(
	R_O = empty
	-> (LN = empty,			%if R_O is empty, then directly delete the left node
	   RN = empty,
	   L_O = heap(K,I,_,_))		%returns (K,I) of deleted node
	;  L_O = heap(K,I,empty,empty)  %if R_O has no child, means this is the bottom and left and right are neither empty, delete the left node, returns (K,I) of left node
	-> LN = R_O,
	   RN = empty
	;  (remove_left(L_O,RN,K,I),		%if this is not the bottom, recurse and save the R_O
	   LN = R_O)
	).
	



% Task 4. heap_sort_asc(+L, -S)
heap_sort_asc(L, S) :-        
	create_heap(L, empty, H),
%	portray_heap(H),
	sort(H,[],S).

sort(empty,S,S).
sort(H,S1,S):-
	remove_max(H, K, I, HN),
%	portray_heap(HN),
	S2 = [(K,I)|S1],
	sort(HN,S2,S).

create_heap([], H, H).
create_heap([(K,I)|Rest],H,HN):-
	add_to_heap(K,I,H,H1),
%	portray_heap(H1),
	create_heap(Rest,H1,HN).

% Task 5. delete_from_heap(+I, +H, -NewH)
delete_from_heap(I, heap(K, I, LH, RH), NewH) :-   	%root or left subtree node
	remove_max(heap(K, I, LH, RH), _, _, NewH),!.
delete_from_heap(I, H, NewH):-
	H = heap(K1, I1, LH, RH),
	(delete_from_heap(I, LH, LN)		%left subtree
	-> N_H = heap(K1, I1, RH, LN)	
	;  remove_left(H,heap(NK,NI,_,NL),DeK,DeI),
	   delete_from_right(I, DeK, DeI, RH, RN),	%right subtree
	   N_H = heap(NK, NI, RN, NL)
	),
%	portray_heap(N_H),
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



test_delete(I,H,NewH):-
	portray_heap(H),
	delete_from_heap(I, H, NewH),
	portray_heap(NewH).



























