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