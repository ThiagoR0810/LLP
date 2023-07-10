linearizar([X|[]], X).
linearizar([X|[Y|L1]], L2) :- concatenar(X, Y, Z), linearizar([Z|L1],L2).

concatenar([], L, L).
concatenar([X|L1], L2, [X|L3]) :- concatenar(L1, L2, L3).