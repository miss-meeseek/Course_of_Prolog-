key('read').
key('while').
key("write").
key("if").
key("then").
key("else").
key("fi").
key("do").
key("od").
key("and").
key("or").
key("mod").

sep("<=").
sep(">=").
sep("/=").
sep(":=").
sep(";").
sep("+").
sep("-").
sep("*").
sep("/").
sep("(").
sep(")").
sep(">").
sep("<").
sep("=").


	
digit(Stream, C, [], C1, L1):-
	atom_chars(C, LC),
	append([], LC, L),
	get_char(Stream, C2),
	digit(Stream, C2, L, C1, L1).
	
digit(Stream, C, L, C1, L1):-
	(char_type(C, digit) ->
		(
			atom_chars(C, LC),
			append(L, LC, L2),
			get_char(Stream, C2),
			digit(Stream, C2, L2, C1, L1)
		);
		(
			L1 = L,
			C1 = C
		)
	).
	
upper(Stream, C, [], C1, L1):-
	atom_chars(C, LC),
	append([], LC, L),
	get_char(Stream, C2),
	upper(Stream, C2, L, C1, L1).
	
upper(Stream, C, L, C1, L1):-
	(char_type(C, upper) ->
		(
			atom_chars(C, LC),
			append(L, LC, L2),
			get_char(Stream, C2),
			upper(Stream, C2, L2, C1, L1)
		);
		(
			L1 = L,
			C1 = C
		)
	).

lower(Stream, C, [], C1, L1):-
	key(X),
	atom_chars(X, [K|LKey]),
	C = K,
	get_char(Stream, C2),
	lower(Stream, [K|LKey], C2, LKey, C1, L1).
	
lower(_, [K|LKey], C, [], C1, L1):-
	C1 = C,
	L1 = [K|LKey].

lower(Stream, [K|LKey], C, [R|Rest], C1, L1):-
	(char_type(C, lower) ->
		(
			C = R,
			get_char(Stream, C2),
			lower(Stream, [K|LKey], C2, Rest, C1, L1)
		)
	).
	
punct(Stream, C, [], C1, L1):-
	sep(X),
	atom_chars(X, [S|LSep]),
	C = S,
	get_char(Stream, C2),
	punct(Stream, [S|LSep], C2, LSep, C1, L1).
	
punct(_, [S|LSep], C, [], C1, L1):-
	C1 = C,
	L1 = [S|LSep].
punct(Stream, [S|LSep], C, [R|Rest], C1, L1):-
	(char_type(C, punct) ->
		(
			C = R,
			get_char(Stream, C2),
			punct(Stream, [S|LSep], C2, Rest, C1, L1)
		)
	).
	

get_word(Stream, X, C, Q):-
	(char_type(C, digit)->
		(
			digit(Stream, C, [], C1, L1),
			atom_chars(Y, L1),
			write(" get_digit "),
			atom_concat('int(', Y, Y1),
			atom_concat(Y1, ')', Y2),
			append(X, [Y2], X2),
			write(X2),
			nl,
			get_word(Stream, X2, C1, Q)
		)
	).
get_word(Stream, X, C, Q):-	
	(char_type(C, upper)->
		(
			upper(Stream, C, [], C1, L1),
			write(" get_upper "),
			atom_chars(Y, L1),
			atom_concat('id(', Y, Y1),
			atom_concat(Y1, ')', Y2),
			append(X, [Y2], X2),
			write(X2),
			nl,
			get_word(Stream, X2, C1, Q)
		)
	).
get_word(Stream, X, C, Q):-
	char_type(C, lower),
	(lower(Stream, C, [], C1, L1) ->
		(
			write(" get_lower "),
			atom_chars(Y, L1),
			atom_concat('key(', Y, Y1),
			atom_concat(Y1, ')', Y2),
			append(X, [Y2], X2),
			write(X2),
			nl,
			get_word(Stream, X2, C1, Q)
		);
		(
			end(Q)
		)
	).	

get_word(Stream, X, C, Q):-
	char_type(C, punct),
	(punct(Stream, C, [], C1, L1) ->
		(
			write(" get_punct "),
			atom_chars(Y, L1),
			atom_concat('sep(', Y, Y1),
			atom_concat(Y1, ')', Y2),
			append(X, [Y2], X2),
			write(X2),
			nl,
			get_word(Stream, X2, C1, Q)
		);
		(
			end(Q)
		)
	).	

	
get_word(Stream, X, C, Q):-
	(char_type(C, space)->
		(
			write(" get_white "),
			get_char(Stream, C2),
			write(X),
			get_word(Stream, X, C2, Q)
		)
	).
get_word(Stream, X, _, Q):-
	at_end_of_stream(Stream),
	write(" got_end_of_file "),
	Q = X,
	nl,
	write(Q).


end(Q):-
		Q = [].
	

run :-
	open('d:/Semestr4/Prolog/ex1.prog', read, Stream),
	scanner(Stream, Q), 
	close(Stream), 
	nl,
	write(Q), !.
	
scanner(Stream, Q):-
	all_words(Stream, [], Q),!.
	
	
all_words(Stream, X, Q):-
	get_char(Stream, C),
	nl,
	write(X), 
	get_word(Stream, X, C, Q),
	nl,
	write(Q).
	
