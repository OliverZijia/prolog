% ---------------------------------------------------------------------------- %
% Provided predicates for differentiation question
% ---------------------------------------------------------------------------- %

% simplify(+E1, -E2)
%   Attempts to simplify the expression E1 
%   by removing occurrences of +0, ×1 etc., returning the expression E2. 
%   (If E1 cannot be simplified then it is returned unchanged.)

simplify(-0.0, Zero) :- 
  !, Zero = 0.0.
simplify(E1+E2, E) :-
  !,
  simplify(E1, SE1),
  simplify(E2, SE2),
  (
    number(SE1),
    SE1 =:= 0
  ->  E = SE2
  ; number(SE2),
    SE2 =:= 0
  ->  E = SE1
  ;   E = SE1+SE2
  ).
simplify(E1*E2, E) :-
  !,
  simplify(E1, SE1),
  simplify(E2, SE2),
  (
    number(SE1),
    SE1 =:= 0
  ->  E = 0.0
  ; number(SE2),
    SE2 =:= 0
  ->  E = 0.0
  ; number(SE1),
    SE1 =:= 1
  ->  E = SE2
  ; number(SE2),
    SE2 =:= 1
  ->  E = SE1
  ;   E = SE1*SE2
  ).
simplify(-E1, -E2) :-
  !,
  simplify(E1, E2).
simplify(sin(E1), sin(E2)) :-
  !,
  simplify(E1, E2).
simplify(cos(E1), cos(E2)) :-
  !,
  simplify(E1, E2).
simplify(E, E).



% ---------------------------------------------------------------------------- %
% Provided predicates for tiles puzzle.
% ---------------------------------------------------------------------------- %

% Search Node Utility Programs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A position is a list [T1, ..., T7] where Ti is one of [b, w, e]
% A node N is a term n(F, G, Pos, ID, PID) where
% Pos is the game state represented by the node,
% G is the number of moves taken to reach N,
% F is the value of the evaluation function F = G + h(Pos),
% where h(Pos) is the value of the heuristic function for Pos,
% ID is the (integer) id of N, and
% PID is the (integer) id of the parent of N

node_f(n(F, _, _, _, _), F).
node_g(n(_, G, _, _, _), G).
node_pos(n(_, _, Pos, _, _), Pos).
node_id(n(_, _, _, I, _), I).
node_pid(n(_, _, _, _, I), I).
node_h(n(F, G, _, _, _), H) :-
  H is F - G.

% make_node(+G, +H, +Pos, +PID, -N)
%   Given a position Pos, an integer G, an integer H = h(Pos), 
%   and a parent id PID creates a new node N

make_node(G, H, Pos, PID, N) :-
  F is G + H,
  N = n(F, G, Pos, _, PID).


% Puzzle Program
%%%%%%%%%%%%%%%%

% tiles
%   Solves puzzle and prints solution.

tiles :-
  tiles(Solution),
  show_solution(Solution).


% show_solution(+Seq)
%   Prints positions in list Seq, one per line.

show_solution(Seq) :-
  \+ (
    member(S, Seq),
    \+ (write(S), nl)
  ).


% tiles(-Solution)
%   Initialises the search, runs the search and extracts the solution.

tiles(Solution) :-
  initial_node(Node),
  search_agenda([Node], Visited, Final),
  trace_moves(Final, Visited, Solution).


% initial_node(-Node)
%   Creates the root node of the search tree.

initial_node(Node) :-
  start(Start),
  h_function(Start, H),
  make_node(0, H, Start, none, Node).


% start(-StartPos)
%   The starting position.

start([b,b,b,e,w,w,w]).


% h_function(+Pos, -H)
%   Wrapper for the chosen heuristic.

h_function(Pos, H) :-
  b_to_left(Pos, H).



