/**Podane relacje ludzi w rodzinie. Zaimolementowano funkcje sprawdzające czy obiekty są w relacji*/

mezczyzna(eric).
mezczyzna(stefun).
mezczyzna(jack).
mezczyzna(din).
mezczyzna(sasha).
mezczyzna(denis).
mezczyzna(jozef).
kobieta(pola).
kobieta(tania).
kobieta(matilda).
kobieta(ola).
kobieta(kate).
rodzic(jozef,kate).
rodzic(jozef,eric).
rodzic(eric, ola).
rodzic(eric, stefun).
rodzic(matilda, ola).
rodzic(matilda, stefun).
rodzic(kate, din).
rodzic(kate, sasha).
rodzic(din,pola).
rodzic(din,denis).
rodzic(tania,pola).
rodzic(tania,denis).
ojciec(X, Y):- mezczyzna(X),rodzic(X,Y).
matka(X, Y):-kobieta(X), rodzic(X,Y).

diff(X, Y):- X\=Y.

jest_matka(X):-
    kobieta(X),
    rodzic(X,_).

jest_ojcem(X):-
    mezczyzna(X),
    rodzic(X,_).

jest_synem(X):-
    mezczyzna(X),
    rodzic(_,X).

siostra(X,Y):-
    kobieta(X),
    rodzic(Z,X),
    rodzic(Z,Y),
    diff(X,Y).

dziadek(X,Y):-
    mezczyzna(X),
    rodzic(X,Z),
    rodzic(Z,Y).

rodzenstwo(X,Y):-
    rodzic(Z,X),
    rodzic(Z,Y),
    diff(X,Y).

ciocia(X,Y):-
    kobieta(X),
    rodzenstwo(X,Z),
    rodzic(Z,Y).
ciotka(X,Y):-
    siostra(X,Z),
    rodzic(Z,Y).
