%Task about the House and the neighbors. Trivial in Prolog.

sasiad(X, Y, Lista) :- lewy(X, Y, Lista).
sasiad(X, Y, Lista) :- lewy(Y, X, Lista).

nr(1, [Elem | _], Elem).
nr(N, [_ | Rest], Elem) :- 
	N > 1, 
	K is N-1, 
	nr(K, Rest, Elem).
	
lewy(L, R, [L, R | _]).
lewy(L, R, [_ | Rest]) :- 
	lewy(L, R, Rest).

fakt(X, [X | _]).
fakt(X, [_ | Rest]) :- 
	fakt(X, Rest).

rybki(X) :-
   /* 0. Mamy 5 domow */
    Domy = [_,_,_,_,_],
   /* 1. Norweg zamieszkuje pierwszy dom */
    nr(1, Domy, [norweg,_,_,_,_]),
   /* 2. Anglik mieszka w czerwonym domu. */
    fakt([anglik,_,_,_,czerwony], Domy),
   /* 3. Zielony dom znajduje się bezpośrednio po lewej stronie domu białego. */
    lewy([_,_,_,_,zielony], [_,_,_,_,bialy], Domy),
   /* 4. Duńczyk pija herbatkę. */
    fakt([dunczyk,_,_,herbata,_], Domy),
   /* 5. Palacz papierosów light mieszka obok hodowcy kotów. */
    sasiad([_,_,marlboro,_,_], [_,kot,_,_,_], Domy),
   /* 6. Mieszkaniec żółtego domu pali cygara. */
    fakt([_,_,dunhill,_,zolty], Domy),
   /* 7. Niemiec pali fajkę. */
    fakt([niemiec,_,rothmans,_,_], Domy),
   /* 8. Mieszkaniec środkowego domu pija mleko */
    nr(3, Domy, [_,_,_,mleko,_]),
   /* 9. Palacz papierosów light ma sąsiada, który pija wodę. */
    sasiad([_,_,marlboro,_,_], [_,_,_,woda,_], Domy),
   /* 10.Palacz papierosów bez filtra hoduje ptaki.*/
    fakt([_,ptak,pallmall,_,_], Domy),
   /* 11. Szwed hoduje psy. */
    fakt([szwed,pies,_,_,_], Domy),
   /* 12. Norweg mieszka obok niebieskiego domu. */
    sasiad([norweg,_,_,_,_], [_,_,_,_,niebieski], Domy),
   /* 13.Hodowca koni mieszka obok żółtego domu. */
   sasiad([_,kon,_,_,_], [_,_,_,_,zolty], Domy),
   /* 14. Palacz mentolowych pija piwo. */
    fakt([_,_,winfield,piwo,_], Domy),
   /* 15. W zielonym domu pija się kawę. */
    fakt([_,_,_,kawa,zielony], Domy),

   /* Kto trzyma rybki? */
    fakt([X,rybki,_,_,_], Domy).
