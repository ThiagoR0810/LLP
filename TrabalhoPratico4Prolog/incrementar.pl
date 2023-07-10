incrementar([], []).
incrementar([X|L1], [Y|L2]) :- incrementar(L1, L2), Y is X + 1.