% Script to run unit tests for all questions.

:- consult(lexis).
:- consult(support).
:- consult(testcases).


% Tests for the algebraic expressions question.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% UNCOMMENT LINES TO RUN TESTS.
% ?- run_tests(eval).
% ?- run_tests(commutes).
% ?- run_tests(diff).
% ?- run_tests(maclaurin).


% Tests for the tiles puzzle.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% UNCOMMENT LINES TO RUN TESTS.
 ?- run_tests(b_to_left).
 ?- run_tests(move).
 ?- run_tests(search_agenda).
 ?- run_tests(trace_moves).
