%% File: syllogisms.pl
%% Name: Wang Zijia
%% Date: 2018.11.15
%%
%% This program is a solution to Prolog 531 Assessed Exercise 5 'Syllogisms'
%% The exercise is to develop a parser and meta-interpreter for syllogistic
%% sentences, and use these to build a tool to determine the validity of a
%% syllogistic argument.

%% ---------------------------- Step 1 ---------------------------------------%%

%% opposite(+L, -Opp)

% sentences started with some
opposite([some|Rest],Opp) :-
	(member(not,Rest)
	-> length(XRest,2),
	   append(XRest,[not|YRest],Rest),
	   append(XRest,YRest,Opp1),
	   Opp = [a|Opp1]
	;  Opp = [no|Rest]),!.

% sentences not started with some
opposite([Article,B,is|Rest],Opp):-
	(Article = no
	-> Opp = [some,B,is|Rest]	
	;  Opp = [some,B,is,not|Rest]
	).

%% ---------------------------- Step 2 ---------------------------------------%%
/*
%% Stage 2.1 - This is the suggested way to develop the solution.
%% Once Stage 2.2 is complete you can delete or comment out this code.
%% syllogism/0

syllogism -->
	article_phrase(_),
	[is],
	optional_article_phrase(_).
syllogism -->
	[no, _, is],
	optional_article_phrase(_).		
syllogism -->
	[some, _, is, not],
	optional_article_phrase(_).
syllogism -->
	[some, _, is],
	optional_article_phrase(_).
*/



%% Stage 2.2 
%% syllogism(-Clauses)

%<article> B is <optional_article> C 
syllogism([(Cl1 :- Cl2)]) -->
	article_phrase(B),
	[is],
	optional_article_phrase(C),
	{Cl1 =.. [C,X],
	Cl2 =.. [B,X]}.			
%no B is <optional_article> C
syllogism([(false :- Cl1,Cl2)]) --> 
	[no, B, is],
	optional_article_phrase(C),
	{Cl1 =.. [B,X],
	Cl2 =.. [C,X]}.
%some B is <optional_article> C
syllogism([(Cl1 :- true),
	   (Cl2 :- true)]) --> 
	[some, B, is],
	optional_article_phrase(C),
	{A =.. [some, B, C],
	Cl1 =.. [B, A],
	Cl2 =.. [C, A]}.
%some B is not <optional_article> C
syllogism([(Cl1 :- true),
	   (false :- Cl2)]) --> 
	[some, B, is,not],
	optional_article_phrase(C),
	{NotC =.. [not, C],
	A =.. [some, B, NotC],
	Cl1 =.. [B, A],
	Cl2 =.. [C, A]}.

% Define the possible article phrases
article_phrase(B) --> 
	[a, B];
	[every, B].

% Define the possible optional article phrases
optional_article_phrase(C) -->
	[a, C];
	[C].
%% ---------------------------- Step 3 ---------------------------------------%%

%% translate(+N)

translate(N) :- 
	forall((p(N,S),phrase(syllogism(Cl1),S)),assertall(N,Cl1)),
	forall((c(N,Sc),get_opposite(Sc,Cl2)),assertall(N,Cl2)).

% get the opposite clause of conclusion Sc
get_opposite(Sc,Clause):-
	opposite(Sc,O),
	phrase(syllogism(Clause),O).

%% ---------------------------- Step 4 ---------------------------------------%%

%% eval(+N, +Calls)
eval(_,true).
eval(N,(H,T)) :-
	cl(N,H,B),
	eval(N,B),
	eval(N,T).
eval(N,H) :-
	cl(N,H,B),
	eval(N,B).

%% valid(?N)
valid(N) :- 
	eval(N,false).

%% invalid(?N)

invalid(N) :- 
	\+forall(c(N,_),eval(N,false)).

%% ---------------------------- Step 5 ---------------------------------------%%

%% test(+N)
% if valid
test(N) :- 
	valid(N),
	write('syllogism '),
	write(N),
	write(':\n'),
	forall(p(N,S),(write('   '),writeL(S),write('\n'))),
	write('=>\n'),
	c(N,Sc),
	write('   '),
	writeL(Sc),
	write('\n'),
	write('Premises and opposite conclusion converted to clauses:\n'),
	show_clauses(N),
	write('\n\nfalse can be derived, syllogism '),
	write(N),
	write(' is valid.').
% if invalid
test(N) :- 
	invalid(N),
	write('syllogism '),
	write(N),
	write(':\n'),
	forall(p(N,S),(write('   '),writeL(S),write('\n'))),
	write('=>\n'),
	c(N,Sc),
	write('   '),
	writeL(Sc),
	write('\n'),
	write('Premises and opposite conclusion converted to clauses:\n'),
	show_clauses(N),
	write('\n\nfalse cannot be derived, syllogism '),
	write(N),
	write(' is invalid.').

