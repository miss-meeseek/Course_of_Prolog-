/**Opisać funkcje higher dla obiektów z obrazka,gdzie one znajdują się w wierszu kolumn*/


left_of(olowek, zegar).
left_of(zegar,motyl).
left_of(motyl,ryba).
left_of(rower,aparat).
above(rower,olowek).
above(aparat,motyl).

right_of_rek(X,Y):-
    left_of_rek(Y,X).

below_rek(X,Y):-
    above_rek(Y,X).

above_rek(X,Y):-
    above(X,Y).
above_rek(X,Y):-
    above(X,P),above_rek(P,Y).

left_of_rek(X,Y):-
    left_of(X,Y).
left_of_rek(X,Y):-
    left_of(X,P),left_of_rek(P,Y).

higher(X,Y):-
    above_rek(X,Y);
    (   above_rek(X,Z),
    (   left_of_rek(Z,Y); right_of_rek(Z,Y))).



