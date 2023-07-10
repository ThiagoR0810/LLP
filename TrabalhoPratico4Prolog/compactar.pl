compactar(X, L) :- compactar2(0, X, L).

compactar2(_, [], []).
compactar2(Q, [X], [[Q1, X]]) :- Q1 is Q + 1.
compactar2(Q, [X, Y|Z], L) :- X = Y, Q1 is Q + 1, compactar2(Q1, [Y|Z], L).
compactar2(Q, [X, Y|Z], [[Q1, X]|L]) :-  X \= Y,  Q1 is Q + 1, compactar2(0, [Y|Z], L).