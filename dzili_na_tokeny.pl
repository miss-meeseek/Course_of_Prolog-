start :-
       open('d:/Semestr4/Prolog/ex1.prog', read, Strumien),
       scanner(Strumien, Tokeny),
       close(Strumien),
       write(Tokeny), !.

key("read").
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


sep(';').
sep(";").
sep(" ").
sep("+").
sep("-").
sep("*").
sep("/").
sep("(").
sep(")").
sep("<").
sep(">").
sep("=").
sep(":=").
sep("=<").
sep(">=").
sep("/=").


int(X) :- atom_number(X, X2), integer(X2).


id(X) :- Y = X, string_upper(X, Z), Z == Y.


scanner(Strumien, Tokeny):-skaner(Strumien, Tokeny2),
                           append(Tokeny2, Tokeny).

skaner(Strumien, []) :-
                       at_end_of_stream(Strumien).

skaner(Strumien, [H | T]) :-
                        \+ at_end_of_stream(Strumien),
                        read_string(Strumien, "\n", "\r\t", End, X),
                        split_string(X, " ", "", X2),
                        append(_, [Y], X2),
                        sub_atom(Y, _, 1, 0, OstatniZnak),

                        ( OstatniZnak == ';' ->
                            ( usun(Y, OstatniZnak, Y2),
                              atom_string(Y2, Y3) ,
                              bezOstatniego(X2, X3),
                              append(X3, [Y3], X4),
                              append(X4, [OstatniZnak], X5)
                            )
                            ;

                            append(X2, [], X5)
                        ),
                        skladanie(X5, H, []),
                        skaner(Strumien, T).

skladanie([], H, H).
skladanie([X | L], H, Temp) :- (  key(X),
                                  append(Temp, [key(X)], Temp2),
				  skladanie(L, H, Temp2)
                                )
                               ;
                               (   int(X),
                                   append(Temp, [int(X)], Temp2),
                                   skladanie(L, H, Temp2)
                               )
                               ;
                               (   sep(X),
                                   append(Temp, [sep(X)], Temp2),
				   skladanie(L, H, Temp2)
                               )
                               ;
                               (   id(X),
                                   append(Temp, [id(X)], Temp2),
                                   skladanie(L, H, Temp2)
                               ).





usun(S,C,X) :- atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X).

bezOstatniego([_], []).
bezOstatniego([H|T], [H|BO]) :-
    bezOstatniego(T, BO).
