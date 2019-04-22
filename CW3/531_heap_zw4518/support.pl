/*
 * Support file for Prolog 531 Heap lab exercise.
 * Author: Tim Kimber (tk106)
 * Date:   January 2015
 */

:- use_module(library(queues)).

% This predicate will print a representation of a heap to standard output.

portray_heap(H):-
  measure(H, Height, KeyWidth),
  list_queue([H, break], Q),
  nl,
  portray_heap(Q, Height, KeyWidth).
  
portray_heap(Q, _, _):-
  queue_length(Q, 1),
  !,
  nl.
portray_heap(Q, Height, KW):-
  queue_cons(break, Tail, Q),
  !,
  nl,
  queue_last(Tail, break, NewQ),
  Smaller is Height - 1,
  portray_heap(NewQ, Smaller, KW).
portray_heap(Q, Height, KW):-
  queue_cons(empty, Tail, Q),
  !,
  portray_node(' ', Height, KW),
  (Height > 0
   ->   queue_append(Tail, [empty, empty], NewQ)
   ;    NewQ = Tail
  ),
  portray_heap(NewQ, Height, KW).
portray_heap(Q, Height, KW):-
  queue_cons(heap(K, _, LH, RH), Tail, Q),
  portray_node(K, Height, KW),
  queue_append(Tail, [LH, RH], NewQ),
  portray_heap(NewQ, Height, KW).
  

portray_node(N, Height, KeyWidth):-
  Col is integer(2 ** (Height - 1)) * (KeyWidth + 1),
  format('~|~t~p~t~*+', [N, Col]).
  

measure(H, Hi, KW):-
  measure(H, 0, 0, Hi, 1, KW).
measure(empty, HThis, Longest, Hi, KW, KW):-
  (HThis > Longest
  ->  Hi = HThis
  ;   Hi = Longest).
measure(heap(K, _, LH, RH), M, Longest, Hi, Wacc, KW):-
  N is M + 1,
  number_codes(K, Codes),
  length(Codes, WK),
  (
    WK > Wacc
    -> measure(LH, N, Longest, HL, WK, WL)
    ;  measure(LH, N, Longest, HL, Wacc, WL)
  ),  
  measure(RH, N, HL, Hi, WL, KW).

