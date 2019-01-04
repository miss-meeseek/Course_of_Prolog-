% Zadanie o korutynach

ex1(N) :-
	drukuj(Y),
	podwajanie(X, Y),
	generowanie(1, N, X).

generowanie(I, J, S) :-
	(   I =< J
	->  S = [I | T],
	    I1 is I+1,
	    generowanie(I1, J, T)
	;   S = []).

podwajanie(S1, S2) :-
        freeze(S1,
               (   S1 = [H1 | T1]
               ->  H2 is 2*H1,
                   S2 = [H2 | T2],
                   podwajanie(T1, T2)
               ;   S2 = [])).
drukuj(S) :-
        freeze(S,
               (   S = [H | T]
               ->  writeln(H),
                   drukuj(T)
	       ;   true)).



my_merge(IN1, IN2, OUT):-
    freeze(IN1,
           freeze(IN2,
                  (IN1=[H1|T1]
                  ->(IN2=[H2|T2]
                    ->(H1<H2
                      ->OUT=[H1|T], my_merge(T1,IN2,T)
                      ; OUT=[H2|T], my_merge(IN1,T2,T))
                    ;OUT=IN1)
                  ;OUT=IN2))).



my_split(IN,OUT1,OUT2):-
    freeze(IN,
           (   IN=[H|T]
           ->(OUT1=[H|T1],
             my_split(T,OUT2,T1))
           ;   OUT1 =[], OUT2 = [])).


my_merge_sort(IN,OUT):-
	freeze(IN,
	(IN=[_|_]
	   ->(my_split(IN,OUT1,OUT2),
	     (OUT1=[_]
	        ->(OUT2=[]
		     ->OUT=OUT1
	             ; my_merge(OUT1,OUT2, OUT))

	        ;(my_merge_sort(OUT1,X), my_merge_sort(OUT2,Y), my_merge(X,Y,OUT))
	)))).
