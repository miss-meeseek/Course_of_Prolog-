%Częściowy porządek: version 1.0

le(1,2).
le(2,4).
le(4,6).
le(6,7).
le(1,1).
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

zwr(X):-
    le(X,X).

not_zwr(X):-
    \+zwr(X).

slab_ant(X,Y):-
    le(X,Y),
    le(Y,X),
    X==Y.

not_slab_ant(X,Y):-
    \+slab_ant(X,Y).

przech(X,Y):-
    le(X,Y);
    (   ( le(Z,X), Z\=X);
        ( le(Y,Z), Z\=Y)   ).

not_cz_porz():-
    le(X,Y),
    not_slab_ant(X,Y).
