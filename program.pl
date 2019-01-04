%Lekser

keys(['read', 'write', 'if', 'then', 'else', 'fi', 'while', 'do', 'od', 'and', 'or', 'mod']).
seps([';', '+', '-', '*', '/', '(', ')', '<', '>', '=<', '>=', ':=', '=', '/=']).

scanner(X,Y) :-
	get_char(X,Znak),
	scanner(X,Y,[],Znak).

scanner(_,Y,Y,Znak) :-
	char_type(Znak, end_of_file).

scanner(X,Y,TempY,Znak) :-
	sprLow(X,Y,TempY,Znak);
	sprUp(X,Y,TempY,Znak);
	sprDig(X,Y,TempY,Znak);
	sprSpec(X,Y,TempY,Znak);
	sprWhite(X,Y,TempY,Znak).


sprLow(X,Y,TempY,Znak) :-
	char_type(Znak,lower),
	sprLow(X,Y,TempY,Znak,'').

sprLow(X,Y,TempY,Znak,Str) :-
	char_type(Znak,lower),
	atom_concat(Str,Znak,NStr),
	get_char(X,NZnak),
	sprLow(X,Y,TempY,NZnak,NStr).

sprLow(X,Y,TempY,Znak,Str) :-
	\+ char_type(Znak, lower),
	keys(KeyList),
	member(Str,KeyList),
	NTerm =.. [key, Str],
	append(TempY,[NTerm],NTempY),
	scanner(X,Y,NTempY,Znak).


sprUp(X,Y,TempY,Znak) :-
	char_type(Znak,upper),
	sprUp(X,Y,TempY,Znak,'').

sprUp(X,Y,TempY,Znak,Str) :-
	char_type(Znak,upper),
	atom_concat(Str,Znak,NStr),
	get_char(X,NZnak),
	sprUp(X,Y,TempY,NZnak,NStr).

sprUp(X,Y,TempY,Znak,Str) :-
	\+ char_type(Znak, upper),
	NTerm =.. [id, Str],
	append(TempY,[NTerm],NTempY),
	scanner(X,Y,NTempY,Znak).


sprDig(X,Y,TempY,Znak) :-
	char_type(Znak,digit),
	sprDig(X,Y,TempY,Znak,'').

sprDig(X,Y,TempY,Znak,Str) :-
	char_type(Znak,digit),
	atom_concat(Str,Znak,NStr),
	get_char(X,NZnak),
	sprDig(X,Y,TempY,NZnak,NStr).

sprDig(X,Y,TempY,Znak,Str) :-
	\+ char_type(Znak, digit),
	atom_number(Str, IntStr),
	NTerm =.. [int, IntStr],
	append(TempY,[NTerm],NTempY),
	scanner(X,Y,NTempY,Znak).


sprSpec(X,Y,TempY,Znak) :-
	char_type(Znak,punct),
	sprSpec(X,Y,TempY,Znak,'').

sprSpec(X,Y,TempY,Znak,Str) :-
	char_type(Znak,punct),
	atom_concat(Str,Znak,NStr),
	get_char(X,NZnak),
	sprSpec(X,Y,TempY,NZnak,NStr).

sprSpec(X,Y,TempY,Znak,Str) :-
	\+ char_type(Znak, punct),
	seps(SepList),
	member(Str,SepList),
	NTerm =.. [sep, Str],
	append(TempY,[NTerm],NTempY),
	scanner(X,Y,NTempY,Znak).


sprWhite(X,Y,TempY,Znak) :-
	(
		char_type(Znak, white);
		char_type(Znak, end_of_line)
	),
	get_char(X,NZnak),
	scanner(X,Y,TempY,NZnak).

run :-
	open('d:/Semestr4/Prolog/ex1.prog', read, Stream),
	scanner(Stream, Tokeny),
	write(Tokeny),
	nl,
	close(Stream),
	phrase(program(Program), Tokeny),
	write(Program).

program([])-->[].
program([Instrukcja|Program])-->!, instrukcja(Instrukcja), program(Program).

instrukcja(read(ID))--> [key(read)], [id(ID)], [sep(;)].
instrukcja(assign(ID, Wyrazenie))-->  [id(ID)], [sep(:=)], wyrazenie(Wyrazenie), [sep(;)].
instrukcja(write(Wyrazenie))--> [key(write)], wyrazenie(Wyrazenie), [sep(;)].
instrukcja(if(Warunek, Program))--> [key(if)], warunek(Warunek), [key(then)], program(Program), [key(fi)], [sep(;)].
instrukcja(if(Warunek, Program1, Program2))--> [key(if)], warunek(Warunek), [key(then)], program(Program1), [key(else)], program(Program2), [key(fi)], [sep(;)].
instrukcja(while(Warunek, Program))--> [key(while)], warunek(Warunek), [key(do)], program(Program), [key(od)], [sep(;)].


wyrazenie(Skladnik + Wyrazenie)-->skladnik(Skladnik),[sep(+)],wyrazenie(Wyrazenie).
wyrazenie(Skladnik - Wyrazenie)-->skladnik(Skladnik),[sep(-)],wyrazenie(Wyrazenie).
wyrazenie(Skladnik)-->skladnik(Skladnik).

skladnik(Czynnik * Skladnik)-->czynnik(Czynnik), [sep(*)], skladnik(Skladnik).
skladnik(Czynnik / Skladnik)-->czynnik(Czynnik), [sep(/)], skladnik(Skladnik).
skladnik(Czynnik mod Skladnik)-->czynnik(Czynnik), [sep(mod)], skladnik(Skladnik).
skladnik(Czynnik)-->czynnik(Czynnik).

czynnik(Identyfikator)-->[id(Identyfikator)].
czynnik(Liczba)-->[int(Liczba)].
czynnik( (Wyrazenie) )-->[sep('(')], wyrazenie(Wyrazenie), [sep(')')].

warunek(Koniunkcja; Warunek)-->koniunkcja(Koniunkcja), [key(or)], warunek(Warunek).
warunek(Koniunkcja)-->koniunkcja(Koniunkcja).

koniunkcja(Prosty, Koniunkcja)-->prosty(Prosty), [key(and)], koniunkcja(Koniunkcja).
koniunkcja(Prosty)-->prosty(Prosty).

prosty(Wyrazenie = Wyrazenie)-->wyrazenie(Wyrazenie), [sep(=)], wyrazenie(Wyrazenie).
prosty(Wyrazenie = Wyrazenie)-->wyrazenie(Wyrazenie), [sep(/=)], wyrazenie(Wyrazenie).
prosty(Wyrazenie < Wyrazenie)-->wyrazenie(Wyrazenie), [sep(<)], wyrazenie(Wyrazenie).
prosty(Wyrazenie > Wyrazenie)-->wyrazenie(Wyrazenie), [sep(>)], wyrazenie(Wyrazenie).
prosty(Wyrazenie >= Wyrazenie)-->wyrazenie(Wyrazenie), [sep(>=)], wyrazenie(Wyrazenie).
prosty(Wyrazenie =< Wyrazenie)-->wyrazenie(Wyrazenie), [sep(=<)], wyrazenie(Wyrazenie).
prosty( (Warunek) )-->[sep('(')], warunek(Warunek), [sep(')')].



