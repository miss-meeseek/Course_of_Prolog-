s2-->``.
s2-->`(`,s2,`)`,s2.

anbn-->``.
anbn-->`a`,anbn,`b`.



s-->`a`, BD.
s-->`abc`.
b-->`a`, BD.
b-->`ab`.
bd

%----------------------------------------------------
%-- Gramatyka do liczb binarnych

bin(X)-->bin(0,X).
bin(X,X)-->``.
bin(X,Z)-->`0`, {Y is 2*X}, bin(Y,Z).
bin(X, Z)-->`1`, {Y is 2*X + 1}, bin(Y,Z).


pr-->`id(SUM)`.

