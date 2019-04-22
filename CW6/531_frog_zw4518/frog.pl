:- use_module(library(clpfd)).


%%%%%%%%%%%%%%%%%%%%%   Part 1 (CLPFD program) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


frog([(Slot1,Slot1R),(Slot2,Slot2R),(Slot3,Slot3R),
      (Slot4,Slot4R),(Slot5,Slot5R),(Slot6,Slot6R),
      (Slot7,Slot7R),(Slot8,Slot8R),(Slot9,Slot9R)]):-

    domain([Slot1,Slot2,Slot3,Slot4,Slot5,Slot6,Slot7,Slot8,Slot9],1,9),
    all_different([Slot1,Slot2,Slot3,Slot4,Slot5,Slot6,Slot7,Slot8,Slot9]),

% you do not need a domain declaration for the SlotR variables
% because their values are generated by calls to rotated_card/6

        rotated_card(Slot1,Slot1R,_,Frog1E,Frog1S,_),
        match(Frog1E,Frog2W),
        rotated_card(Slot2,Slot2R,_,Frog2E,Frog2S,Frog2W),

        match(Frog2E,Frog3W),
        rotated_card(Slot3,Slot3R,_,_,Frog3S,Frog3W),

	rotated_card(Slot4,Slot4R,Frog4N,Frog4E,Frog4S,_),
	match(Frog1S,Frog4N),
	
	rotated_card(Slot5,Slot5R,Frog5N,Frog5E,Frog5S,Frog5W),
	match(Frog4E,Frog5W),
	match(Frog2S,Frog5N),
	
	rotated_card(Slot6,Slot6R,Frog6N,_,Frog6S,Frog6W),
	match(Frog5E,Frog6W),
	match(Frog3S,Frog6N),
	
	rotated_card(Slot7,Slot7R,Frog7N,Frog7E,_,_),
	match(Frog4S,Frog7N),
	
	rotated_card(Slot8,Slot8R,Frog8N,Frog8E,_,Frog8W),
	match(Frog7E,Frog8W),
	match(Frog5S,Frog8N),

	rotated_card(Slot9,Slot9R,Frog9N,_,_,Frog9W),
	match(Frog8E,Frog9W),
	match(Frog6S,Frog9N).

 

match((Col,head),(Col,body)).
match((Col,body),(Col,head)).
        
   
card(1, (yellow, body), (green, head),  (red, head),    (blue, body)   ). 
card(2, (yellow, body), (green, body),  (red, head),    (yellow, head) ). 
card(3, (green, head), (red, head), (blue, body), (red, body)).
card(4, (green, body), (blue, head), (red, head), (blue, body)).
card(5, (yellow, body), (green, body), (yellow, head), (red,head)).
card(6, (green,body), (blue, body), (yellow, head), (red, head)).
card(7, (green, head), (yellow, head), (blue, body), (red, body)).
card(8, (blue, head), (yellow, head), (green, body), (yellow, body)).
card(9, (yellow, head), (green, body), (red, body), (blue, head)).


rotated_card(Id, 0, FrogN,FrogE, FrogS, FrogW) :- 
    card(Id, FrogN,FrogE, FrogS, FrogW ). 
rotated_card(Id, 1,  FrogW, FrogN,FrogE, FrogS) :- 
    card(Id, FrogN,FrogE, FrogS, FrogW).
rotated_card(Id, 2, FrogS, FrogW, FrogN, FrogE) :-
    card(Id, FrogN, FrogE, FrogS, FrogW). 
rotated_card(Id, 3, FrogE, FrogS, FrogW, FrogN) :-
    card(Id, FrogN, FrogE, FrogS, FrogW).


% query with ?-frog(L).

/* You should get 8 answers, perhaps in diff order.

L = [(2,0),(1,2),(4,2),(7,1),(5,2),(3,0),(8,1),(6,2),(9,1)] ? ;
L = [(4,0),(3,1),(2,2),(9,2),(7,1),(5,2),(1,0),(8,1),(6,2)] ? ;
L = [(6,0),(8,3),(1,2),(5,0),(7,3),(9,0),(2,0),(3,3),(4,2)] ? ;
L = [(1,1),(9,3),(4,1),(8,2),(7,2),(3,2),(6,3),(5,3),(2,3)] ? ;
L = [(2,1),(5,1),(6,1),(3,0),(7,0),(8,0),(4,3),(9,1),(1,3)] ? ;
L = [(4,1),(3,3),(9,0),(1,1),(5,1),(6,1),(2,3),(7,0),(8,0)] ? ;
L = [(8,2),(7,2),(2,1),(6,3),(5,3),(1,3),(9,2),(3,1),(4,3)] ? ;
L = [(9,3),(6,0),(8,3),(3,2),(5,0),(7,3),(4,0),(1,0),(2,2)] ? ;
no

*/


%%%%%%%%%%%%%%%%%%   Part 2 (Prolog program) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





frogProlog([(Slot1,Slot1R),(Slot2,Slot2R),(Slot3,Slot3R),
            (Slot4,Slot4R),(Slot5,Slot5R),(Slot6,Slot6R),
            (Slot7,Slot7R),(Slot8,Slot8R),(Slot9,Slot9R)]):-

        rotated_card(Slot1,Slot1R,_,Frog1E,Frog1S,_),
        match(Frog1E,Frog2W),
        rotated_card(Slot2,Slot2R,_,Frog2E,Frog2S,Frog2W),
        Slot1\==Slot2,

        match(Frog2E,Frog3W),
        rotated_card(Slot3,Slot3R,_,_,Frog3S,Frog3W),
        Slot3\==Slot2,
        Slot1\==Slot3,

	match(Frog1S,Frog4N),
	rotated_card(Slot4,Slot4R,Frog4N,Frog4E,Frog4S,_),
	Slot4\==Slot3,
	Slot4\==Slot2,
	Slot4\==Slot1,

	match(Frog4E,Frog5W),
	match(Frog2S,Frog5N),	
	rotated_card(Slot5,Slot5R,Frog5N,Frog5E,Frog5S,Frog5W),
	Slot5\==Slot4,	
	Slot5\==Slot3,
	Slot5\==Slot2,
	Slot5\==Slot1,

	match(Frog5E,Frog6W),
	match(Frog3S,Frog6N),
	rotated_card(Slot6,Slot6R,Frog6N,_,Frog6S,Frog6W),
	Slot6\==Slot5,	
	Slot6\==Slot4,	
	Slot6\==Slot3,
	Slot6\==Slot2,
	Slot6\==Slot1,	

	match(Frog4S,Frog7N),	
	rotated_card(Slot7,Slot7R,Frog7N,Frog7E,_,_),
	Slot7\==Slot6,	
	Slot7\==Slot5,	
	Slot7\==Slot4,	
	Slot7\==Slot3,
	Slot7\==Slot2,
	Slot7\==Slot1,	

	match(Frog7E,Frog8W),
	match(Frog5S,Frog8N),	
	rotated_card(Slot8,Slot8R,Frog8N,Frog8E,_,Frog8W),
	Slot8\==Slot7,
	Slot8\==Slot6,	
	Slot8\==Slot5,	
	Slot8\==Slot4,	
	Slot8\==Slot3,
	Slot8\==Slot2,
	Slot8\==Slot1,	

	match(Frog8E,Frog9W),
	match(Frog6S,Frog9N),
	rotated_card(Slot9,Slot9R,Frog9N,_,_,Frog9W),
	Slot9\==Slot8,	
	Slot9\==Slot7,
	Slot9\==Slot6,	
	Slot9\==Slot5,	
	Slot9\==Slot4,	
	Slot9\==Slot3,
	Slot9\==Slot2,
	Slot9\==Slot1.




