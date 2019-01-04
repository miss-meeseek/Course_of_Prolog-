/** LISTA2:
	-ŚRODKOWY
	-JEDNOKROTNIE
	-DWUKROTNIE
	-DAJESZ/MASZ
	-PORZĄDEK.
*/
getLenList([],0).
getLenList([_|L],N):-
    getLenList(L,N1),
    N is N1+1.

notequal(0,s(_)).
notequal(s(_),0).
notequal(s(A),s(B)):-
   notequal(A,B).
   
mniejszy(0,s(_)).
mniejszy(s(A),s(B)):-
	mniejszy(A,B).
	
indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

/** ZADANIE1:
	ZNALEŻĆ ELEMENT ŚRODKOWY LISTY NIEPARZYSTEJ
*/
srodkowy2([Y],Y).
srodkowy2([_|L],X):-
    length(L,N),
    N\=0,
    reverse(L,[_|L1]),
    srodkowy2(L1,X).

srodkowy([Y],Y).
srodkowy(L,X):-
	append([_|L1],[_],L),
    srodkowy(L1,X).

	/********************************************/

jednokrotnie(X,L):-
    member(X,L),
    select(X,L,L1),
    \+member(X,L1).

	/********************************************/
	
dwu(X,L):-
    member(X,L),
    select(X,L,L1),
    member(X,L1),
    select(X,L1,L2),
    \+member(X,L2).


dwukrotnie2(X,[X|L]):-
    jednokrotnie(X,L).
dwukrotnie2(X,[Y|L]):-
    dif(Y,X),
    dwukrotnie2(X,L).


count(_,2,[]).
count(N,K,[N|T]) :-
    K1 is K+1,
    count(N,K1,T).
count(N,K,[P|T]) :-
    dif(N,P),
    count(N,K,T).
dwukrotnie(N,L):-
    count(N,0,L).


/* ?- delete_all(5,[3,2,5,3,3,5,6,5],L).
  L = [3, 2, 3, 3, 6] ;
  false. */
delete_all(_, [],[]).
delete_all(X, [X|T], W):-
    delete_all(X, T, W).
delete_all(X, [Y|T], [Y|W]):-
    (X\=Y),
    delete_all(X, T, W).

	/********************************************/


arc(a,b).
arc(b,a).
arc(b,c).
arc(c,d).

find_way_deep(From, To, VisitedNodes):-
    arc(From, To),
    \+(member(To, VisitedNodes)).
find_way_deep(From, To, VisitedNodes):-
    arc(From,X),
    \+(member(X, VisitedNodes)),
    find_way_deep(X, To, [From|VisitedNodes]).

osiagalny(X,X).
osiagalny(X,Y):-
    find_way_deep(X,Y,[]).


/**ZADANIE4:
*  DAJESZ/MASZ
*/
ma(a,1).
ma(b,4).
ma(a,2).
daje(s(i),a,1,c).
daje(s(s(i)),b,4,a).

maT(i,On,Co):-
	ma(On,Co).
maT(s(T),On,Co):-
	daje(T,_,Co,On).
maT(s(T),On,Co):-
	maT(T,On,Co),
	\+(daje(T,On,Co,_)).

/*ma(i,On, Co):-
	nl,write(check1),nl,
	
	ma(On,Co).

ma(s(i),On,Co):-
	nl,write(check2),nl,
	ma(On,Co),
	\+(daje(i,On,Co,_)).

ma(s(s(T)),On,Co):-
	nl,write(check3),nl,
	
	daje(T,_,Co,On),
	\+(daje(s(T),On,Co,_)).
ma(s(T),On,Co):-
	nl,write(check4),nl,

	daje(T,_,Co,On).
	
ma(s(T),On,Co):-
	nl,write(check5),nl,
	daje(s(T),On,Co,_).*/
	

/** ZADANIE5:
	2*N
	KAZDA LICZBA 2 RAZY
	MIEDZY TAKIMI SAMYMI PARZYŚCIE
*/
lista1(P,L):-
    LL is P*2,
    length(L,LL).

lista2(0,_).
lista2(N,L):-
    dwukrotnie(N,L),
    N1 is N-1,
    lista2(N1,L).

/*spr_parz(X,L):-
    indexOf(L,X,I1),
    reverse(L,L2),
    indexOf(L2,X,I2),
    I is I1+I2,
    M is I mod 2,
    M = 0.*/

/*lista3(0,_).
lista3(P,L):-
    spr_parz(P,L),
    P1 is P-1,
    lista3(P1,L).*/

parz(0,_).
parz(P,L):-
	append(_,[P|L1],L),
	append(H2,[P|_],L1),
	length(H2,N),
	M is N mod 2,
    M = 0,
	P1 is P-1,
	parz(P1,L).

lista(P,L):-
    lista1(P,L),
    lista2(P,L),
    parz(P,L).



