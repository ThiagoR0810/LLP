maior([X], X).
maior([X|L], M) :- maior(L, Y), (X > Y, M is X; Y >= X, M is Y).