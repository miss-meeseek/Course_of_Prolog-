key('read').
key('write').
key('if').
key('then').
key('else').
key('fi').
key('while').
key('do').
key('od').
key('and').
key('or').
key('mod').

sep(';').
sep('+').
sep('-').
sep('*').
sep('/').
sep('(').
sep(')').
sep('<').
sep('<=').
sep('>=').
sep('=').
sep('/=').
sep(':=').
sep('>').


run :-
	open('d:/Semestr4/Prolog/ex1.prog', read, Stream),
	scanner(Stream, L),
	write(L),
	close(Stream).
	
scanner(Stream, L):-
	get_char(Stream, C),
	get_tokens_list(Stream, C, [], LTokensEnd),
	L = LTokensEnd, !.
	
get_tokens_list(Stream, _, LTokens, LTokensEnd):-
	at_end_of_stream(Stream),
	LTokensEnd = LTokens.

get_tokens_list(Stream, C, LTokens, LTokensEnd):-
	char_type(C, space),
	get_char(Stream, C2),
	get_tokens_list(Stream, C2, LTokens, LTokensEnd).	

get_tokens_list(Stream, C, LTokens, LTokensEnd):-
	char_type(C, punct),
	get_sep_word(Stream, C, [], Token, C2),
	sep(Token),
	atom_concat('sep(', Token, Y1),
	atom_concat(Y1, ')', Y2),
	append(LTokens, [Y2], LTokens2),
	get_tokens_list(Stream, C2, LTokens2, LTokensEnd).
	
get_tokens_list(Stream, C, LTokens, LTokensEnd):-
	char_type(C, digit),
	get_digit_word(Stream, C, [], Token, C2),
	atom_concat('int(', Token, Y1),
	atom_concat(Y1, ')', Y2),
	append(LTokens, [Y2], LTokens2),
	get_tokens_list(Stream, C2, LTokens2, LTokensEnd).	
	
get_tokens_list(Stream, C, LTokens, LTokensEnd):-
	char_type(C, upper),
	get_upper_word(Stream, C, [], Token, C2),
	atom_concat('id(', Token, Y1),
	atom_concat(Y1, ')', Y2),
	append(LTokens, [Y2], LTokens2),
	get_tokens_list(Stream, C2, LTokens2, LTokensEnd).
	
get_tokens_list(Stream, C, LTokens, LTokensEnd):-
	char_type(C, lower),
	get_lower_word(Stream, C, [], Token, C2),
	key(Token),
	atom_concat('key(', Token, Y1),
	atom_concat(Y1, ')', Y2),
	append(LTokens, [Y2], LTokens2),
	get_tokens_list(Stream, C2, LTokens2, LTokensEnd).
	
	
	
get_lower_word(Stream, C, LTMP, Token, C2):-
	atom_chars(C, LC),
	append(LTMP, LC, LTMP2),
	get_char(Stream, C3),
	(char_type(C3, lower) ->
		(
			get_lower_word(Stream, C3, LTMP2, Token, C2)
		);
		(
			C2 = C3,
			atom_chars(Token, LTMP2)
		)
	).
			
get_upper_word(Stream, C, LTMP, Token, C2):-
	atom_chars(C, LC),
	append(LTMP, LC, LTMP2),
	get_char(Stream, C3),
	(char_type(C3, upper) ->
		(
			get_upper_word(Stream, C3, LTMP2, Token, C2)
		);
		(
			C2 = C3,
			atom_chars(Token, LTMP2)
		)
	).
			
get_digit_word(Stream, C, LTMP, Token, C2):-
	atom_chars(C, LC),
	append(LTMP, LC, LTMP2),
	get_char(Stream, C3),
	(char_type(C3, digit) ->
		(
			get_digit_word(Stream, C3, LTMP2, Token, C2)
		);
		(
			C2 = C3,
			atom_chars(Token, LTMP2)
		)
	).			
			
get_sep_word(Stream, C, LTMP, Token, C2):-
	atom_chars(C, LC),
	append(LTMP, LC, LTMP2),
	get_char(Stream, C3),
	(char_type(C3, punct) ->
		(
			get_sep_word(Stream, C3, LTMP2, Token, C2)
		);
		(
			C2 = C3,
			atom_chars(Token, LTMP2)
		)
	).			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			