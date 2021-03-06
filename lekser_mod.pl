key('read').
key("write").
key("if").
key("then").
key("else").
key("fi").
key("while").
key("do").
key("od").
key("and").
key("or").
key("mod").

sep(";").
sep("+").
sep("-").
sep("*").
sep("/").
sep("(").
sep(")").
sep("<").
sep("<=").
sep(">=").
sep("=").
sep("/=").
sep(":=").
sep(">").
	
reserved(X):-
	key(X);
	sep(X).
	   
	

	
words([],[]).
words([H|L], [H1|L1]):-
	H1 = H,
	words(L, L1).
	
digit([]).
digit([H|L]):-
	char_type(H, digit),
	digit(L).
	
upper([]).
upper([H|L]):-
	char_type(H, upper),
	upper(L).
	
	
	
check([H|L], Y):-
	char_type(H, digit),
	digit(L),
	atom_chars(X, [H|L]),
	atom_concat('int(', X, Y1),
	atom_concat(Y1, ')', Y).
	
check([H|L], Y):-
	char_type(H, upper),
	upper(L),
	atom_chars(X, [H|L]),
	atom_concat('id(', X, Y1),
	atom_concat(Y1, ')', Y).
	
check(L, Y):-
	key(X),
	atom_chars(X, L1),
	words(L, L1),
	atom_concat('key(', X, Y1),
	atom_concat(Y1, ')', Y).
	
check(L, Y):-
	sep(X),
	atom_chars(X, L1),
	words(L, L1),
	atom_concat('sep(', X, Y1),
	atom_concat(Y1, ')', Y).
	
	


get_word(Stream, L1, X):-
	get_char(Stream, C),
	\+ white(C),
	string_chars(C, LC),
	append(L1, LC, L2),
	get_word(Stream, L2, X), !.
	
get_word(_, L, X):-
	X = L.
	
one_word(Stream, L, L2):-
	get_word(Stream, [], X),
	N is 1,
	length(S, N),
	
	(append(LbS, S, X) ->
		(S = [;] ->
			(
				check(LbS, Y),
				L = Y,
				L2 = 'sep(;)'
			);
			(
				check(X, Y),
				L = Y,
				L2 = ''
			)	
		);
	
		(
			L = '',
			L2 = ''
		)
	).	

many_words(Stream, L, X) :-
	at_end_of_stream(Stream),
	X = L.
many_words(Stream, L, X):-
	one_word(Stream, LW, LS),
	(LW \= '' ->
		(
			append(L, [LW], LWs1)
		);
		(
			many_words(Stream, L, X)
		)
	),
	
	(LS \= '' ->
		(
			append(LWs1, [LS], LWs2),
			many_words(Stream, LWs2, X)
		);
		(
			many_words(Stream, LWs1, X)
		)
	).
	
	
run :-
	open('d:/Semestr4/Prolog/ex1.prog', read, Stream),
	scanner(Stream, Y), 
	close(Stream), 
	write(Y).
	
scanner(Stream, Y):-
	many_words(Stream, [], Y), !.
	
	
