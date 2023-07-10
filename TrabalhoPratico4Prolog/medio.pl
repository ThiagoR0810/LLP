medio([], 0).
medio([X], X).
medio(L, M) :- somaElementos(L, S), nelementos(L, N), M is S / N.

nelementos([], 0).
nelementos([_|X], Y) :- nelementos(X, (Z)), Y is Z + 1.

somaElementos([], 0).
somaElementos([X|L], S) :- somaElementos(L, Z), S is Z + X.