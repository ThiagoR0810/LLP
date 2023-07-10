nelementos([], 0).
nelementos([_|X], Y) :- nelementos(X, (Z)), Y is Z + 1.