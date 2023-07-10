gerar(X, X, [X]).
gerar(X, Y, [X|L]) :- X1 is X+1, gerar(X1, Y, L).