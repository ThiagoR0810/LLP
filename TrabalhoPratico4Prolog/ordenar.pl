ordenar(L, LF) :- trocarElemento(L, L1), ordenar(L1, LF).
ordenar(LF, LF).

trocarElemento([E, NextE | L], [NextE, E | L]) :- E > NextE.
trocarElemento([Z | L], [Z | L1]) :- trocarElemento(L, L1).