% Interpreter prostego języka imperatywnego w Prologu.
%
% interpreter(+INSTRUKCJE)
% INSTRUKCJE = []
% INSTRUKCJE = [INTRUKCJA | INSTRUKCJE]
% INSTRUKCJA = read(ID)
% INSTRUKCJA = write(WYRAŻENIE)
% INSTRUKCJA = assign(ID, WYRAŻENIE)
% INSTRUKCJA = if(WARUNEK, INSTRUKCJE)
% INSTRUKCJA = if(WARUNEK, INSTRUKCJE, INSTRUKCJE)
% INSTRUKCJA = while(WARUNE, INSTRUKCJE)
% WYRAŻENIE = int(NUM)
% WYRAŻENIE = id(ID)
% WYRAŻENIE = WYRAŻENIE + WYRAŻENIE
% WYRAŻENIE = WYRAŻENIE - WYRAŻENIE
% WYRAŻENIE = WYRAŻENIE * WYRAŻENIE
% WYRAŻENIE = WYRAŻENIE / WYRAŻENIE
% WYRAŻENIE = WYRAŻENIE mod WYRAŻENIE
% WARUNEK = WYRAŻENIE =:= WYRAŻENIE
% WARUNEK = WYRAŻENIE =\= WYRAŻENIE
% WARUNEK = WYRAŻENIE < WYRAŻENIE
% WARUNEK = WYRAŻENIE > WYRAŻENIE
% WARUNEK = WYRAŻENIE >= WYRAŻENIE
% WARUNEK = WYRAŻENIE =< WYRAŻENIE
% WARUNEK = (WARUNEK, WARUNEK)
% WARUNEK = (WARUNEK; WARUNEK)
%
% ?- program1(X), interpreter(X).
% |: 10.
% 55
%  X = [read('N'), assign('SUM', int(0)), while(id('N')>int(0),
% [assign('SUM', id('SUM')+id('N')), assign('N', id('N')-int(1))]),
% write(id('SUM'))].
%
% ?- program2(X), interpreter(X).
% |: 123.
% |: 32.
% 3
% 27
%  X = [read('M'), read('N'), assign('D', int(0)), assign('R', id('M')),
% while(id('R')>id('N'), [assign('D', id(...)+int(...)), assign('R', ...
% - ...)]), write(id('D')), write(id('R'))].
%
% ?- X is 3*32+27.
% X = 123.
%
% ?- program3(X), interpreter(X).
% |: 2.
% |: 2.
% 4
% X = [read('A'), read('B'), assign('X', id('A')), assign('Y',
%  id('B')), while(id('Y')>int(0), [assign('X', id(...)+int(...)),
%  assign('Y', ... - ...)]), write(id('X'))].
%
interpreter(INSTRUKCJE) :-
	interpreter(INSTRUKCJE, []).

% interpreter(+INSTRUKCJE, +ASOCJACJE)
% ASOCJACJE = []
% ASOCJACJE = [ASOCJACJA | ASOCJACJE]
% ASOCJACJA = ID = NUM

interpreter([], _).
interpreter([read(ID) | INSTRUKCJE], ASOC) :- !,
	read(N),
	integer(N),
	podstaw(ASOC, ID, N, ASOC1),
	interpreter(INSTRUKCJE, ASOC1).
interpreter([write(W) | INSTRUKCJE], ASOC) :- !,
	wartość(W, ASOC, WART),
	write(WART), nl,
	interpreter(INSTRUKCJE, ASOC).
interpreter([assign(ID, W) | INSTRUKCJE], ASOC) :- !,
	wartość(W, ASOC, WAR),
	podstaw(ASOC, ID, WAR, ASOC1),
	interpreter(INSTRUKCJE, ASOC1).
interpreter([if(C, P) | INSTRUKCJE], ASOC) :- !,
	interpreter([if(C, P, []) | INSTRUKCJE], ASOC).
interpreter([if(C, P1, P2) | INSTRUKCJE], ASOC) :- !,
	(   prawda(C, ASOC)
	->  append(P1, INSTRUKCJE, DALEJ)
	;   append(P2, INSTRUKCJE, DALEJ)),
	interpreter(DALEJ, ASOC).
interpreter([while(C, P) | INSTRUKCJE], ASOC) :- !,
	append(P, [while(C, P)], DALEJ),
	interpreter([if(C, DALEJ) | INSTRUKCJE], ASOC).

podstaw([], ID, N, [ID = N]).
podstaw([ID = _ | ASOC], ID, N, [ID = N | ASOC]) :- !.
podstaw([ID1 = W1 | ASOC1], ID, N, [ID1 = W1 | ASOC2]) :-
	podstaw(ASOC1, ID, N, ASOC2).

pobierz([ID = N | _], ID, N) :- !.
pobierz([_ | ASOC], ID, N) :-
	pobierz(ASOC, ID, N).

wartość(int(N), _, N).
wartość(id(ID), ASOC, N) :-
	pobierz(ASOC, ID, N).
wartość(W1+W2, ASOC, N) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N is N1+N2.
wartość(W1-W2, ASOC, N) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N is N1-N2.
wartość(W1*W2, ASOC, N) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N is N1*N2.
wartość(W1 / W2, ASOC, N) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N2 =\= 0,
	N is N1 div N2.
wartość(W1 mod W2, ASOC, N) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N2 =\= 0,
	N is N1 mod N2.

prawda(W1 =:= W2, ASOC) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N1 =:= N2.
prawda(W1 =\= W2, ASOC) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N1 =\= N2.
prawda(W1 < W2, ASOC) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N1 < N2.
prawda(W1 > W2, ASOC) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N1 > N2.
prawda(W1 >= W2, ASOC) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N1 >= N2.
prawda(W1 =< W2, ASOC) :-
	wartość(W1, ASOC, N1),
	wartość(W2, ASOC, N2),
	N1 =< N2.
prawda((W1, W2), ASSOC) :-
	prawda(W1, ASSOC),
	prawda(W2, ASSOC).
prawda((W1; W2), ASSOC) :-
	(   prawda(W1, ASSOC),
	    !
	;   prawda(W2, ASSOC)).

% Przykłady prostych programów:
%
% obliczenie sumy liczb od 1 do N:
% read N;
% SUM := 0;
% while N > 0 do
%   SUM := SUM + N;
%   N := N - 1;
% od;
% write SUM;
%
program1([
    read('N'),
    assign('SUM', int(0)),
    while(id('N') > int(0),
	  [assign('SUM', id('SUM') + id('N')),
	   assign('N', id('N') - int(1))]),
    write(id('SUM'))]).

% obliczenie ilorazu D i reszty R z dzielenia M przez N:
% read M;
% read N;
% D := 0;
% R := M;
% while R > N do
%   D := D + 1;
%   R := R - N;
% od;
% write D;
% write R;
%
program2([
    read('M'),
    read('N'),
    assign('D', int(0)),
    assign('R', id('M')),
    while(id('R') > id('N'),
	  [assign('D', id('D') + int(1)),
	   assign('R', id('R') - id('N'))]),
    write(id('D')),
    write(id('R'))]).

% obliczenie sumy dwóch wczytanych liczb:
% read A;
% read B;
% X := A;
% Y := B;
% while Y > 0 do
%   X := X + 1;
%   Y := Y - 1;
% od;
% write X;
%
program3([
    read('A'),
    read('B'),
    assign('X', id('A')),
    assign('Y', id('B')),
    while(id('Y') > int(0),
	  [assign('X', id('X') + int(1)),
	   assign('Y', id('Y') - int(1))]),
    write(id('X'))]).

% program Turinga obliczajacy N!
% read N;
% U := 1;
% R := 1;
% while R < N do
%   V := U;
%   S := 1;
%   while S <= R do
%     U := U + V;
%     S := S + 1;
%   od;
%   R := R + 1;
% od;
% write U;
%
program4([
    read('N'),
    assign('U', int(1)),
    assign('R', int(1)),
    while(id('R') < id('N'),
	  [assign('V', id('U')),
	   assign('S', int(1)),
	   while(id('S') =< id('R'),
		 [assign('U', id('U') + id('V')),
		  assign('S', id('S') + int(1))]),
	   assign('R', id('R') + int(1))]),
    write(id('U'))]).

% Interpreter podzbioru Prologu w Prologu:
%
% ?- udowodnij(app(X, Y, [1, 2, 3])).
% X = [],
% Y = [1, 2, 3] ;
% X = [1],
% Y = [2, 3] ;
% X = [1, 2],
% Y = [3] ;
% X = [1, 2, 3],
% Y = [] ;
% false.
%
% ?- udowodnij(app([[1,2], [3], [4, 5, 6]], X)).
% X = [1, 2, 3, 4, 5, 6].
%
udowodnij(true) :- !.
udowodnij((G1, G2)) :- !,
	udowodnij(G1),
	udowodnij(G2).
udowodnij(A) :-
	clause(A, B),
	udowodnij(B).

app([], []).
app([L1 | L2], L3) :-
	app(L1, L4, L3),
	app(L2, L4).

app([], X, X).
app([X | L1], L2, [X | L3]) :-
	app(L1, L2, L3).





