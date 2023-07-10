rotacionarn(_,[],[]).
rotacionarn(0,L,L). 
rotacionarn(N, [X|L1], L2) :- N1 is N - 1, concatenar(L1, [X], L3), rotacionarn(N1, L3, L2).

concatenar([], L, L).
concatenar([X|L1], L2, [X|L3]) :- concatenar(L1, L2, L3).