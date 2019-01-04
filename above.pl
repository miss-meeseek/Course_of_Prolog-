on(a,b).
on(b,c).
on(c,d).
on(d,e).
on(e,f).
on(f,h).

above(A,D):-
    on(A,D).
above(A,D):-
    on(A,P),above(P,D).
