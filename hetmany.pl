hetmany(N, P) :- 
				numlist(1, N, L),
				permutation(L, P),
				dobra(P).

dobra(P) :- \+ zla(P).

zla(P) :- 
		append(_, [Wi | L1], P),
		append(L2, [Wj | _], L1),
		length(L2, K),
		abs(Wi - Wj) =:= K + 1.


board(L) :- 
			length(L,N),
            rysuj(N,N,L), !.

rysuj(0,N,_) :- krawedz(N).

rysuj(Rzad,N,L):- 
			krawedz(N),
            pole(N,Rzad,N,L),
            pole(N,Rzad,N,L),
            Rzad2 is Rzad-1,
            rysuj(Rzad2, N, L).
			
krawedz(0) :- write("+\n").

krawedz(N) :- write("+-----"),
              N2 is N-1,
              krawedz(N2).
			  
pole(0,_,_,_) :- write("|\n").

pole(Kol,Rzad,P,L) :-  S is Rzad+Kol,
             R is P-Kol,
             N is (P mod 2),
             M is (S mod 2),
            (   0==N->
                      (M==0 -> ( nth0(R, L, Rzad) ->(jasnyHetman);(jasne))
                               ;
                               ( nth0(R, L, Rzad) ->(ciemnyHetman);(ciemne))
                      )
                      ;
                      (M==0 -> ( nth0(R, L, Rzad) ->(ciemnyHetman);(ciemne))
                               ;
                               ( nth0(R, L, Rzad) ->(jasnyHetman);(jasne))
                      )
            ),
             Kol2 is Kol-1,
             pole(Kol2,Rzad,P,L)
             .

jasne :- write("|     ").

ciemne :- write("|:::::").

jasnyHetman :-  write("| ### ").

ciemnyHetman :-  write("|:###:").



run(N) :-  hetmany(N, P), board(P), write(P).
