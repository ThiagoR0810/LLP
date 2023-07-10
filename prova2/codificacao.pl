is_digit(X) :- atom_length(X,1), X @>= '0', X @=< '9'.

concatenar([], L, L).
concatenar([X|L1], L2, [X|L3]) :- concatenar(L1, L2, L3).
  
linearizar([], []).
linearizar([X|L], LR) :- linearizar(L, LT), concatenar(X, LT, LR).
 
compactar([], []).
compactar([X], [[1,X]]) :- !.
compactar([X,X|L1], [[N,X]|L2]) :- compactar([X|L1], [[Z,X]|L2]), N is Z+1.
compactar([X,Y|L1], [[1,X]|L2]) :- not(X == Y), compactar([Y|L1], L2).

codificarAux([], []).
codificarAux([X,Y|L], R) :-  codificarAux(L, T1), number_chars(X, T2), concatenar(T2, [Y], T3), concatenar(T3, T1, R).

codificar(S, C) :- string_chars(S, T1),
                   compactar(T1, T2),
                   linearizar(T2, T3),
                   codificarAux(T3, T4),
                   string_chars(C, T4).
repetir(0, _, []).
repetir(N, C, R) :- N1 is N - 1, repetir(N1, C, T1), concatenar(T1, [C], R).

decodificarAux(_, [], []).
decodificarAux(DS, [X|L], R) :- is_digit(X), concatenar(DS, [X], T1), decodificarAux(T1, L, R).
decodificarAux(DS, [X|L], R) :- not(is_digit(X)), number_chars(N, DS), repetir(N, X, T1), decodificarAux([], L, T2), concatenar(T1, T2, R).

decodificar(S, D) :- string_chars(S, T1), decodificarAux([], T1, T2), string_chars(D, T2).