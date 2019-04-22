% Supporting file for 531 Prolog Exercise 1 (Prop formulas)



% ------------ logical_atom/1 -------------

% logical atom : string beginning with lower case letter
% call to atom(F) won't work as things like atom(!)
% and atom(;) succeed in Prolog.
% So convert to list of character codes and examine first character.

logical_atom( A ) :-
    atom( A ),     % Prolog 'atom'
    atom_codes( A, [AH|_] ),
    AH >= 97,
    AH =< 122.


% -------------- list library
%
% If you want to use the subseq0/2 utility from the Sicstus lists library
% when testing your programs, uncomment the following line.
%
:- use_module(library(lists)).
    

% ----------------------     Some sample formulas    --------------------------
formula( 1, or(p, q)).              
formula( 2, and(p, q)).             
formula( 3, imp(p, q)).              
formula( 4, or(or(p, q), r)).               
formula( 5, and(p, or(q, r))).          
formula( 6, imp(imp(p, q), r)).       
formula( 7, imp(r, imp(p, q))).           
formula( 8, or(p, neg(p))).             
formula( 9, and(imp(p, q), imp(q, p))).   
formula(10, imp(imp(p, q), p)).           
formula(11, and(p, q)).
formula(12, or(p, q)).
formula(13, or(q, neg(p))).
formula(14, or(p, or(q, r))).
formula(15, or(and(p, q), and(p, r))).
formula(16, and(imp(q, r), or(p, r))).
formula(17, imp(and(p, r), q)).
formula(18, and(q, neg(q))).
formula(19, or(and(p, q), and(neg(p), neg(q)))).
formula(20, imp(imp(q, p), q)).
% ----------------------------------------------------------------------------


    
