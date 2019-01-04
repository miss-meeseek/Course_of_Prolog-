%Częściowy porządek: version 1.1

le(1,2).
le(2,4).
le(4,6).
le(6,7).

le(2,2).
le(4,4).
le(6,6).
le(7,7).
le(1,4).
le(1,6).
le(1,7).
le(2,6).
le(2,7).
le(4,7).


not_max(X):-
    le(X,Y),
    X\=Y.

najw(X):-
    le(_,X),
    \+ not_max(X),
    \+ le(X,X).

max(X):-
    le(_,X),
    \+ not_max(X).

not_min(X):-
    le(Y,X),
    Y\=X.

min(X):-
    le(X,_),
    \+ not_min(X).

najm(X):-
    min(X),
    \+ le(X,X).
