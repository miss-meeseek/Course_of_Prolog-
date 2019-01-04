
/**	LISTA3:ZAD1
	Napisz predykat wariancja(L, D), który dla danej listy liczb L wylicza wartość
	wariancji D
*/
sum([],0).
sum([X|L],S):-
	sum(L,S1),
	S is S1 + X.

licznik([],_,0).
licznik([X|L],Sr,C):-
	licznik(L,Sr,C1),
	Pom is X - Sr,
	C is (C1 + Pom*Pom).

var(L,D):-
	length(L,N),
	sum(L,S),
	Sr is S/N,
	licznik(L,Sr,C),
	D is C/N.

/**	LISTA3:ZAD2
	Napisz predykat max_sum(L, S), który dla danej listy L znajduje największą
	wartość S spośród wszystkich sum po wszystkich możliwych sekcjach.
*/	

max_sum([],_,S,S).
max_sum([X|L],C,T,S):-
	C1 is C + X,
	(T > C1 -> T1 is T; T1 is C1),
	(C1 > 0 -> C2 is C1; C2 is 0),
	max_sum(L,C2,T1, S).
	
max_sum([X|L],S):-
	max_sum(L,X,X,S).
	

/**	LISTA3:ZAD3
	Napisz program dla even_permutation(Xs, Ys) i odd_permutation(Xs, Ys), 
	który znajduje listę Ys będącą, odpowiednio, parzystą i nieparzystą permutacją listy Xs.
*/	

	
/* position(X, Ys, Zs) is true if Zs is the result of removing one occurrence  */
/*   of the element X from the list Ys.                                    */
position(X, [X|Xs], Xs).
position(X, [Y|Ys], [Y|Zs]):-
  position(X, Ys, Zs).
	
/* transposition(Xs, Ys) is true if Ys is a permutation of the list Xs.          */
transposition([], []).
transposition([X|Xs], Ys1):-
  transposition(Xs, Ys), 
  position(X, Ys1, Ys).

sign_of_product_of_differences([], D, D).
sign_of_product_of_differences([Y|Xs], D0, D):-
	sign_of_product_of_differences_1(Xs, Y, D0, D1),
	sign_of_product_of_differences(Xs, D1, D).

sign_of_product_of_differences_1([], _, D, D).
sign_of_product_of_differences_1([X|Xs], Y, D0, D):-
  Y =\= X,
  D1 is D0 * (Y - X) // abs(Y - X),  
  sign_of_product_of_differences_1(Xs, Y, D1, D).

/* even_permutation(Xs, Ys) is true if Ys is an even permutation of Xs.    */
even_permutation(Xs, Ys):-
  transposition(Xs, Ys), 
  sign_of_product_of_differences(Xs, 1, D),
  sign_of_product_of_differences(Ys, 1, E),
  D = E.

/* odd_permutation(Xs, Ys) is true if Ys is an odd permutation of Xs.      */
odd_permutation(Xs, Ys):-
  transposition(Xs, Ys),
  sign_of_product_of_differences(Xs, 1, D),
  sign_of_product_of_differences(Ys, 1, E),
  D =\= E.


  
  
 /* 
  domains
list=integer*
 
predicates
p0(list,list)
even_head(integer,list,integer,list,integer)
even(list,list,integer)
inv(integer,integer,integer,integer,integer)
 
clauses
p0(L,L1):-
  even(L,L1,K),
  K=1.
 
even([],[],1).
even([H|T],[H1|T1],M):-
  even_head(H,T,H1,T1,K),
  even(T,T1,M1),
  M=M1*K.
  
even_head(X,[],A,[],1).
even_head(X,[H|T],X1,[H1|T1],P):-
  inv(X,H,X1,H1,K),
  even_head(X,T,X1,T1,P1),
  P=K*P1.
  
inv(X,H,X1,H1,1):-
  X<H,X1<H1,!.  
inv(X,H,X1,H1,-1):-
  X<H,X1>H1,!.
inv(X,H,X1,H1,1):-
  X>H,X1>H1,!.
inv(X,H,X1,H1,-1):-
  X>H,X1<H1,!.          
  
goal
 clearwindow,
 p0([2,3,4],[4,3,2]),!,write("yes");
 write("no").*/
 
/*
perm([],[]).
perm(L1,[X|L3]):−
	select(X, L1, L2),
	perm(L2,L3).
*/