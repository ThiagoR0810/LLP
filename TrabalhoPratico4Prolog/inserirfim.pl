inserirfim(X, [], [X]).
inserirfim(X, [Y|L1], [Z|L2]) :- inserirfim(X, L1, L2), Z is Y.