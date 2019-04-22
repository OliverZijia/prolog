
% ---------- Question 2: Utilities for testing -------------------

% This uses the subst_string program. Given String and Subst, prints the result.
test_subst(String,Subst) :-
   subst_string(String,Subst,NewString),
   format("input string  : ~s~n",[String]),
   format("new string    : ~s~n",[NewString]),
   nl.

% Uses the encrypt_string program to encrypt the plain text from one of the
% examples listed below (supply the example id). If incorrect, outputs the 
% generated cipher text followed by the correct solution.
test_encrypt(N) :-
   encrypt_example(N,Plain,Key,CipherExpected),
   encrypt_string(Plain, Key, CipherActual),
   format("Example ~a: ",[N]),
   (CipherExpected == CipherActual ->
        format("TEST PASSED~n",[])
      ; format("TEST FAILED~n",[]),
        format("plain text           : ~s~n",[Plain]),
        format("actual cipher text   : ~s~n",[CipherActual]),
        format("expected cipher text : ~s~n",[CipherExpected])
   ),
   nl.

% Uses the decrypt_string program to decrypt the cipher text from one of the
% examples listed below (supply the example id). If inccorect, outputs the 
% generated plain text followed by the correct solution.
test_decrypt(N) :-
   decrypt_example(N,Cipher,Key,PlainExpected),
   decrypt_string(Cipher, Key, PlainActual),
   format("Example ~a: ",[N]),
   (PlainExpected == PlainActual ->
        format("TEST PASSED~n",[])
      ; format("TEST FAILED~n",[]),
        format("cipher text         : ~s~n",[Cipher]),
        format("actual plain text   : ~s~n",[PlainActual]),
        format("expected plain text : ~s~n",[PlainExpected])
   ),
   nl.
   
% Uses the keyphrase_cipher program to generate a cipher key from one of the
% examples listed below (supply the example id). If incorrect, outputs the 
% generated key followed by the correct solution.
test_keyphrase(N) :-
   keyphrase_example(N,Phrase,KeyExpected),
   keyphrase_cipher(Phrase, KeyActual),
   format("Example ~a: ",[N]),
   (KeyExpected == KeyActual ->
        format("TEST PASSED~n",[])
      ; format("TEST FAILED~n",[]),
        format("phrase       : ~s~n",[Phrase]),
        format("actual key   : ~s~n",[KeyActual]),
        format("expected key : ~s~n",[KeyExpected])
   ),
   nl.   


% ---------- Question 2: Examples for testing -------------------

encrypt_example(e1,
   "more paper please",
   "JLPAWIQBCTRZYDSKEGFXHUONVM",
   "YSGW KJKWG KZWJFW").
encrypt_example(e2,
   "More paper. Please!!",
   "JLPAWIQBCTRZYDSKEGFXHUONVM",
   "YSGW KJKWG. KZWJFW!!").
encrypt_example(e3,
   "Lexis test? What a nice way to start the New Year!",
   "JLPAWIQBCTRZYDSKEGFXHUONVM",
   "ZWNCF XWFX? OBJX J DCPW OJV XS FXJGX XBW DWO VWJG!").

% the following three examples use the cipher alphabet from
% the keyphrase "BORING LECTURE".

encrypt_example(b1,
   "more paper please",
   "BORINGLECTUVWXYZADFHJKMPQS",
   "WYDN ZBZND ZVNBFN").
encrypt_example(b2,
   "More paper. Please!!",
   "BORINGLECTUVWXYZADFHJKMPQS",
   "WYDN ZBZND. ZVNBFN!!").
encrypt_example(b3,
   "Lexis test? What a nice way to start the New Year!",
   "BORINGLECTUVWXYZADFHJKMPQS",
   "VNPCF HNFH? MEBH B XCRN MBQ HY FHBDH HEN XNM QNBD!").

decrypt_example(e1,
   "YSGW KJKWG KZWJFW",
   "JLPAWIQBCTRZYDSKEGFXHUONVM",
   "more paper please").
decrypt_example(e2,
   "YSGW KJKWG. KZWJFW!!",
   "JLPAWIQBCTRZYDSKEGFXHUONVM",
   "more paper. please!!").
decrypt_example(e3,
   "ZWNCF XWFX? OBJX J DCPW OJV XS FXJGX XBW DWO VWJG!",
   "JLPAWIQBCTRZYDSKEGFXHUONVM",
   "lexis test? what a nice way to start the new year!").

% the following three examples use the cipher alphabet from
% the keyphrase "BORING LECTURE".

decrypt_example(b1,
   "WYDN ZBZND ZVNBFN",
   "BORINGLECTUVWXYZADFHJKMPQS",
   "more paper please").
decrypt_example(b2,
   "WYDN ZBZND. ZVNBFN!!",
   "BORINGLECTUVWXYZADFHJKMPQS",
   "more paper. please!!").
decrypt_example(b3,
   "VNPCF HNFH? MEBH B XCRN MBQ HY FHBDH HEN XNM QNBD!",
   "BORINGLECTUVWXYZADFHJKMPQS",
   "lexis test? what a nice way to start the new year!").


keyphrase_example(b1,
   "BORING LECTURE",
   "BORINGLECTUVWXYZADFHJKMPQS").


% ---------- Question 3: Examples for testing -------------------

testgraph(g1,
          [b > f, f > c, c > b, g > h, g > g, d, b, f > k, k > f, f > c],
          graph([b,c,d,f,g,h,k],
                [e(b,f),e(c,b),e(f,c),e(f,k),e(g,g),e(g,h),e(k,f)]),
          [n(b,[f]),n(c,[b]),n(d,[]),n(f,[c,k]),n(g,[g,h]),n(h,[]),n(k,[f])]).
testgraph(g2,
          [s, s > r, t, u > r, s > u, u > s, v > u],
          graph([r,s,t,u,v],[e(s,r),e(s,u),e(u,r),e(u,s),e(v,u)]),
          [n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r,s]),n(v,[u])]).
testgraph(g3,
          [p>q, m>q, q>m, m>p, p>p],
          graph([m,p,q],[e(m,p),e(m,q),e(p,p),e(p,q),e(q,m)]),
          [n(m,[p,q]),n(p,[p,q]),n(q,[m])]).


% ---------- Question 1: Prison Data -------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                  %
%         DO NOT EDIT                              %
%                                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cells(132).


psychopath('Colton', 'Alessio').
psychopath('Magee', 'Ian').
psychopath('Knottenbelt', 'Susan').
psychopath('Kramer', 'Duncan').
psychopath('Yang', 'Sophia').


female_name('Alessandra').
female_name('Margaret').
female_name('Fariba').
female_name('Krysia').
female_name('Susan').
female_name('Philippa').
female_name('Sophia').
female_name('Julie').
female_name('Francesca').
female_name('Maja').
female_name('Nobuko').


% there are 270 prisoners

prisoner('Hankin', 'Emil', 1, robbery, 9, 1).
prisoner('Mencer', 'Murray', 1, rape, 19, 2).
prisoner('van Bakel', 'Stephen', 1, murder, 37, 18).
prisoner('McBrien', 'Alex', 3, murder, 21, 8).
prisoner('Gardner', 'Daniel', 3, robbery, 13, 3).
prisoner('McBrien', 'Jeff', 4, plagiarism, 1, 1).
prisoner('Kuhn', 'Simon', 4, murder, 20, 15).
prisoner('Phillips', 'Abbas', 6, rape, 19, 4).
prisoner('Pietzuch', 'Paul', 6, robbery, 12, 8).
prisoner('Sloman', 'Emil', 7, burglary, 7, 3).
prisoner('Sloman', 'Margaret', 7, rape, 19, 2).
prisoner('Harrison', 'Alessandra', 8, plagiarism, 5, 1).
prisoner('Gillies', 'Fariba', 8, burglary, 3, 2).
prisoner('Pantic', 'Ian', 8, burglary, 2, 2).
prisoner('Yoshida', 'Alessio', 9, attempted_murder, 32, 22).
prisoner('Lupu', 'Andrew', 9, plagiarism, 2, 1).
prisoner('Hodkinson', 'Chris', 9, plagiarism, 5, 4).
prisoner('Yoshida', 'Tony', 9, burglary, 2, 1).
prisoner('Guo', 'Emil', 10, rape, 10, 2).
prisoner('Lupu', 'Oskar', 10, murder, 47, 44).
prisoner('Mencer', 'Fariba', 11, assault, 9, 9).
prisoner('Guo', 'Guang-Zhong', 11, robbery, 15, 7).
prisoner('Huth', 'Krysia', 11, attempted_murder, 28, 16).
prisoner('Calcagno', 'Naranker', 11, plagiarism, 5, 5).
prisoner('Rueckert', 'Alex', 12, attempted_murder, 31, 29).
prisoner('Bradley', 'Chris', 12, burglary, 4, 1).
prisoner('Rueckert', 'Keith', 12, attempted_murder, 40, 31).
prisoner('Davidson', 'Naranker', 12, murder, 37, 7).
prisoner('Kuhn', 'Susan', 12, rape, 18, 3).
prisoner('Gardner', 'Cristiano', 13, plagiarism, 1, 1).
prisoner('Kelly', 'Fariba', 14, assault, 10, 3).
prisoner('Muggleton', 'Guang-Zhong', 14, assault, 13, 7).
prisoner('Cunningham', 'Susan', 14, rape, 23, 3).
prisoner('Harrison', 'Susan', 14, murder, 28, 1).
prisoner('Colton', 'Wayne', 14, attempted_murder, 33, 7).
prisoner('Broda', 'Murray', 15, rape, 10, 6).
prisoner('van Bakel', 'Berc', 16, rape, 12, 3).
prisoner('Pantic', 'Fariba', 16, robbery, 11, 6).
prisoner('Magee', 'Ian', 16, murder, 38, 25).
prisoner('McCann', 'Jeremy', 16, rape, 7, 4).
prisoner('Pietzuch', 'Philippa', 17, attempted_murder, 34, 20).
prisoner('Bradley', 'Alessio', 18, attempted_murder, 19, 3).
prisoner('Hankin', 'Paul', 18, burglary, 4, 2).
prisoner('Pattinson', 'Susan', 18, assault, 15, 3).
prisoner('Clark', 'Chris', 19, attempted_murder, 40, 31).
prisoner('Dulay', 'Alessio', 20, plagiarism, 2, 2).
prisoner('Toni', 'Alex', 20, murder, 47, 37).
prisoner('Davidson', 'Tony', 20, rape, 9, 2).
prisoner('Gillies', 'Daniel', 21, murder, 41, 18).
prisoner('Field', 'Duncan', 21, attempted_murder, 40, 18).
prisoner('Broda', 'Wayne', 21, robbery, 8, 8).
prisoner('van Bakel', 'Chris', 22, assault, 13, 9).
prisoner('Eisenbach', 'John', 22, rape, 20, 16).
prisoner('Pattinson', 'Sophia', 22, murder, 33, 21).
prisoner('Wolf', 'Wayne', 22, robbery, 9, 9).
prisoner('Pietzuch', 'Julie', 24, murder, 43, 42).
prisoner('Rueckert', 'Paul', 24, plagiarism, 1, 1).
prisoner('Kelly', 'Will', 24, assault, 12, 2).
prisoner('Gardner', 'Jeremy', 25, rape, 12, 7).
prisoner('Kuhn', 'Duncan', 26, murder, 48, 22).
prisoner('Pattinson', 'Iain', 26, plagiarism, 2, 1).
prisoner('Hankin', 'Ian', 26, murder, 39, 20).
prisoner('Rustem', 'Sebastian', 26, attempted_murder, 22, 3).
prisoner('Knottenbelt', 'John', 28, assault, 7, 5).
prisoner('Eisenbach', 'Maja', 30, attempted_murder, 17, 5).
prisoner('Gardner', 'Dirk', 31, rape, 9, 4).
prisoner('Sergot', 'Dirk', 31, assault, 7, 6).
prisoner('Luk', 'Duncan', 31, plagiarism, 4, 3).
prisoner('Hankin', 'Marek', 31, robbery, 11, 9).
prisoner('Kelly', 'Sebastian', 31, burglary, 5, 2).
prisoner('Clark', 'Tony', 31, rape, 21, 6).
prisoner('Harrison', 'Margaret', 32, assault, 5, 4).
prisoner('Bradley', 'Fariba', 33, robbery, 7, 5).
prisoner('Sadri', 'Jeremy', 33, plagiarism, 5, 4).
prisoner('Uchitel', 'Daniel', 34, plagiarism, 4, 3).
prisoner('Davidson', 'Marek', 34, robbery, 5, 5).
prisoner('Sloman', 'Michael', 34, plagiarism, 3, 3).
prisoner('Knottenbelt', 'Peter', 34, burglary, 7, 5).
prisoner('Field', 'Wayne', 34, attempted_murder, 38, 13).
prisoner('Sergot', 'Jeremy', 35, burglary, 2, 1).
prisoner('Gillies', 'Steffen', 35, attempted_murder, 21, 8).
prisoner('Kramer', 'Duncan', 36, assault, 23, 7).
prisoner('Pantic', 'Naranker', 36, attempted_murder, 33, 24).
prisoner('Clark', 'Maja', 37, plagiarism, 2, 2).
prisoner('Toni', 'Steffen', 37, assault, 7, 3).
prisoner('Guo', 'Will', 37, burglary, 5, 5).
prisoner('Gillies', 'Francesca', 38, rape, 17, 11).
prisoner('Muggleton', 'Ian', 38, robbery, 11, 2).
prisoner('Hodkinson', 'Peter', 38, assault, 5, 5).
prisoner('Luk', 'Stephen', 38, burglary, 5, 3).
prisoner('Rueckert', 'Cristiano', 39, plagiarism, 3, 1).
prisoner('Uchitel', 'Maja', 39, burglary, 4, 3).
prisoner('Uchitel', 'Abbas', 40, assault, 9, 8).
prisoner('Hankin', 'Nobuko', 40, plagiarism, 2, 2).
prisoner('Rustem', 'Oskar', 40, assault, 7, 7).
prisoner('Colton', 'Tony', 40, murder, 48, 16).
prisoner('Harrison', 'Dirk', 41, assault, 10, 10).
prisoner('Clark', 'Guang-Zhong', 41, attempted_murder, 31, 8).
prisoner('van Bakel', 'Daniel', 42, robbery, 6, 6).
prisoner('Magee', 'Nobuko', 43, robbery, 10, 1).
prisoner('Field', 'Naranker', 44, rape, 19, 9).
prisoner('Colton', 'Chris', 46, plagiarism, 1, 1).
prisoner('Mencer', 'Ian', 47, robbery, 13, 7).
prisoner('Phillips', 'Stephen', 47, attempted_murder, 26, 3).
prisoner('McBrien', 'Iain', 48, plagiarism, 5, 5).
prisoner('Sadri', 'Dirk', 50, attempted_murder, 20, 19).
prisoner('Wiklicky', 'Naranker', 50, assault, 6, 5).
prisoner('Lomuscio', 'Cristiano', 51, murder, 47, 46).
prisoner('Hankin', 'Margaret', 51, burglary, 6, 6).
prisoner('Kramer', 'Michael', 51, rape, 25, 20).
prisoner('Sadri', 'Herbert', 52, attempted_murder, 26, 9).
prisoner('Toni', 'Simon', 52, plagiarism, 2, 1).
prisoner('Davidson', 'Chris', 53, murder, 26, 19).
prisoner('Guo', 'Fariba', 53, murder, 46, 16).
prisoner('Sadri', 'Maja', 53, attempted_murder, 16, 13).
prisoner('Kramer', 'Fariba', 54, assault, 8, 6).
prisoner('Darlington', 'Morris', 54, burglary, 5, 3).
prisoner('Rueckert', 'Susan', 54, murder, 41, 27).
prisoner('Pietzuch', 'Daniel', 55, robbery, 13, 10).
prisoner('Sloman', 'Murray', 55, attempted_murder, 21, 3).
prisoner('Kuhn', 'Steffen', 55, attempted_murder, 20, 15).
prisoner('Yang', 'Berc', 56, robbery, 8, 2).
prisoner('Sergot', 'Maja', 56, murder, 29, 19).
prisoner('Edalat', 'Margaret', 56, assault, 7, 7).
prisoner('Magee', 'Paul', 56, murder, 43, 29).
prisoner('Dulay', 'Yike', 56, plagiarism, 5, 1).
prisoner('McCann', 'Yike', 56, burglary, 3, 1).
prisoner('Hodkinson', 'Alex', 57, murder, 37, 8).
prisoner('Rustem', 'Jeff', 57, assault, 5, 4).
prisoner('Huth', 'Paul', 57, robbery, 7, 6).
prisoner('Yoshida', 'Yike', 57, plagiarism, 1, 1).
prisoner('Kramer', 'Will', 58, burglary, 3, 2).
prisoner('Muggleton', 'Alessandra', 59, plagiarism, 2, 2).
prisoner('Colton', 'Alessio', 59, rape, 25, 3).
prisoner('Harrison', 'Alessio', 60, rape, 14, 2).
prisoner('Uchitel', 'Iain', 60, assault, 15, 14).
prisoner('Russo', 'Morris', 60, plagiarism, 5, 3).
prisoner('Phillips', 'Philippa', 60, murder, 49, 6).
prisoner('Sergot', 'Stephen', 61, plagiarism, 3, 2).
prisoner('Darlington', 'Alessandra', 62, plagiarism, 5, 2).
prisoner('Sadri', 'Alessandra', 62, robbery, 12, 10).
prisoner('Field', 'Sophia', 62, plagiarism, 2, 1).
prisoner('Edalat', 'Tony', 62, murder, 49, 30).
prisoner('Broda', 'Alessio', 63, assault, 13, 13).
prisoner('van Bakel', 'Emil', 63, burglary, 4, 1).
prisoner('Knottenbelt', 'Jeff', 63, rape, 5, 2).
prisoner('Luk', 'Paul', 63, robbery, 15, 4).
prisoner('Wiklicky', 'Dirk', 64, attempted_murder, 23, 10).
prisoner('Toni', 'Margaret', 64, burglary, 6, 3).
prisoner('Yang', 'Will', 64, rape, 10, 6).
prisoner('Edalat', 'Iain', 65, attempted_murder, 18, 12).
prisoner('Pattinson', 'Morris', 65, murder, 49, 43).
prisoner('Edalat', 'Alex', 67, rape, 11, 6).
prisoner('Huth', 'Margaret', 67, attempted_murder, 39, 4).
prisoner('Lupu', 'Chris', 68, attempted_murder, 21, 17).
prisoner('Darlington', 'Keith', 68, burglary, 6, 1).
prisoner('Drossopoulou', 'Duncan', 69, rape, 15, 4).
prisoner('Yoshida', 'Peter', 69, plagiarism, 1, 1).
prisoner('Phillips', 'Andrew', 70, robbery, 11, 2).
prisoner('Drossopoulou', 'Chris', 70, burglary, 6, 2).
prisoner('Knottenbelt', 'Andrew', 71, attempted_murder, 21, 12).
prisoner('Sloman', 'Andrew', 71, assault, 8, 2).
prisoner('McCann', 'Daniel', 72, robbery, 15, 13).
prisoner('Kuhn', 'Herbert', 72, murder, 25, 10).
prisoner('Yang', 'Wayne', 72, assault, 8, 4).
prisoner('Russo', 'Andrew', 73, plagiarism, 3, 3).
prisoner('Calcagno', 'Keith', 73, burglary, 2, 1).
prisoner('Luk', 'Krysia', 73, assault, 14, 4).
prisoner('McBrien', 'Maja', 73, murder, 28, 27).
prisoner('Magee', 'Philippa', 73, burglary, 2, 2).
prisoner('Kramer', 'Yike', 74, rape, 19, 19).
prisoner('Rustem', 'Chris', 75, robbery, 8, 2).
prisoner('Russo', 'Fariba', 75, attempted_murder, 24, 9).
prisoner('Guo', 'Abbas', 76, robbery, 8, 3).
prisoner('Shanahan', 'Alex', 76, assault, 6, 2).
prisoner('Calcagno', 'Alessio', 77, robbery, 15, 15).
prisoner('Pietzuch', 'Morris', 78, robbery, 11, 7).
prisoner('Lomuscio', 'Stephen', 79, plagiarism, 3, 2).
prisoner('Eisenbach', 'Daniel', 80, plagiarism, 2, 2).
prisoner('Phillips', 'Francesca', 80, murder, 33, 14).
prisoner('Calcagno', 'Alessandra', 82, assault, 15, 2).
prisoner('Hodkinson', 'Alessandra', 82, murder, 37, 16).
prisoner('Wolf', 'Alessio', 83, robbery, 13, 1).
prisoner('Broda', 'Peter', 83, murder, 48, 14).
prisoner('Huth', 'Berc', 84, attempted_murder, 26, 13).
prisoner('Gillies', 'Keith', 84, attempted_murder, 25, 10).
prisoner('Mencer', 'Daniel', 86, murder, 20, 16).
prisoner('Darlington', 'Jeff', 86, burglary, 3, 2).
prisoner('Wiklicky', 'Murray', 86, attempted_murder, 19, 19).
prisoner('Wolf', 'Will', 86, rape, 12, 4).
prisoner('Shanahan', 'Berc', 87, rape, 19, 14).
prisoner('Muggleton', 'Fariba', 87, assault, 14, 12).
prisoner('van Bakel', 'Fariba', 89, assault, 5, 1).
prisoner('Sergot', 'Michael', 89, rape, 10, 7).
prisoner('Cunningham', 'Krysia', 90, attempted_murder, 28, 24).
prisoner('Russo', 'Stephen', 90, attempted_murder, 28, 17).
prisoner('Huth', 'Ian', 92, robbery, 9, 8).
prisoner('Lupu', 'Simon', 92, robbery, 7, 4).
prisoner('Wolf', 'Chris', 93, rape, 6, 2).
prisoner('Calcagno', 'Jeremy', 93, burglary, 5, 4).
prisoner('Wiklicky', 'Jeremy', 93, rape, 22, 7).
prisoner('Bradley', 'Paul', 93, rape, 5, 1).
prisoner('Bradley', 'Wayne', 93, assault, 12, 3).
prisoner('Lomuscio', 'Simon', 96, rape, 17, 3).
prisoner('Russo', 'Oskar', 97, rape, 24, 1).
prisoner('Edalat', 'Sebastian', 97, attempted_murder, 39, 2).
prisoner('Davidson', 'Alessandra', 98, rape, 7, 2).
prisoner('Lomuscio', 'Maja', 98, murder, 40, 21).
prisoner('Luk', 'Michael', 98, rape, 24, 23).
prisoner('Cunningham', 'Jeremy', 99, attempted_murder, 17, 10).
prisoner('Lomuscio', 'Berc', 100, assault, 6, 2).
prisoner('Darlington', 'Alessio', 101, plagiarism, 1, 1).
prisoner('Drossopoulou', 'Susan', 101, rape, 9, 2).
prisoner('Cunningham', 'Dirk', 102, attempted_murder, 25, 4).
prisoner('Eisenbach', 'Abbas', 103, attempted_murder, 29, 15).
prisoner('Cunningham', 'Alessandra', 103, plagiarism, 4, 4).
prisoner('Broda', 'Francesca', 103, murder, 43, 43).
prisoner('Dulay', 'Iain', 103, murder, 50, 32).
prisoner('Drossopoulou', 'Julie', 103, burglary, 2, 2).
prisoner('Dulay', 'Jeremy', 104, robbery, 5, 4).
prisoner('Lupu', 'Stephen', 104, attempted_murder, 21, 8).
prisoner('Yoshida', 'Wayne', 104, assault, 9, 2).
prisoner('Sergot', 'Oskar', 105, attempted_murder, 21, 16).
prisoner('Shanahan', 'Sebastian', 105, assault, 13, 8).
prisoner('Magee', 'Marek', 106, rape, 12, 2).
prisoner('Pantic', 'Alessandra', 107, robbery, 10, 6).
prisoner('Kelly', 'Peter', 107, murder, 28, 2).
prisoner('Shanahan', 'Simon', 107, assault, 15, 4).
prisoner('Eisenbach', 'Wayne', 107, rape, 25, 21).
prisoner('Toni', 'Andrew', 109, burglary, 3, 2).
prisoner('Lupu', 'Fariba', 109, murder, 32, 9).
prisoner('Rustem', 'Herbert', 112, burglary, 7, 4).
prisoner('Broda', 'Jeremy', 112, assault, 11, 9).
prisoner('Wolf', 'Simon', 112, rape, 5, 3).
prisoner('Rueckert', 'Sophia', 112, attempted_murder, 27, 20).
prisoner('Colton', 'Oskar', 113, murder, 31, 2).
prisoner('Pietzuch', 'Berc', 114, attempted_murder, 26, 9).
prisoner('Kuhn', 'Guang-Zhong', 115, attempted_murder, 40, 16).
prisoner('Pattinson', 'Emil', 116, plagiarism, 5, 1).
prisoner('Darlington', 'Peter', 116, robbery, 15, 14).
prisoner('Hodkinson', 'Julie', 117, rape, 7, 1).
prisoner('Uchitel', 'Tony', 117, robbery, 13, 10).
prisoner('Hodkinson', 'Yike', 117, murder, 34, 4).
prisoner('Gardner', 'Duncan', 118, rape, 20, 3).
prisoner('McCann', 'Francesca', 118, burglary, 7, 5).
prisoner('Uchitel', 'Wayne', 118, rape, 12, 4).
prisoner('Yang', 'Oskar', 119, robbery, 14, 3).
prisoner('Drossopoulou', 'Alessandra', 120, attempted_murder, 17, 10).
prisoner('Muggleton', 'Michael', 120, attempted_murder, 16, 12).
prisoner('Wiklicky', 'Tony', 120, rape, 24, 9).
prisoner('Pantic', 'Abbas', 121, burglary, 4, 1).
prisoner('Pantic', 'Will', 121, assault, 8, 8).
prisoner('Kramer', 'Iain', 122, assault, 10, 9).
prisoner('McCann', 'Krysia', 122, assault, 13, 3).
prisoner('Mencer', 'Philippa', 122, burglary, 6, 4).
prisoner('Yang', 'Sophia', 122, murder, 38, 6).
prisoner('Muggleton', 'Cristiano', 123, robbery, 10, 3).
prisoner('Lomuscio', 'Duncan', 123, robbery, 13, 1).
prisoner('Shanahan', 'Dirk', 124, rape, 22, 5).
prisoner('Russo', 'Iain', 124, rape, 23, 10).
prisoner('Knottenbelt', 'Murray', 125, rape, 22, 9).
prisoner('Dulay', 'Nobuko', 125, rape, 9, 4).
prisoner('Huth', 'Murray', 127, rape, 25, 23).
prisoner('Kelly', 'Iain', 129, murder, 23, 22).
prisoner('McBrien', 'Alessandra', 130, plagiarism, 3, 3).
prisoner('Clark', 'Simon', 130, murder, 31, 11).
prisoner('Kelly', 'Sophia', 131, assault, 6, 2).
prisoner('Shanahan', 'Ian', 132, murder, 24, 7).
prisoner('Field', 'Simon', 132, robbery, 12, 9).
prisoner('Knottenbelt', 'Susan', 132, attempted_murder, 20, 2).


