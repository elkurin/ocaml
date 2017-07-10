add(z, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

nobigger(X, Y) :- add(X, _, Y).

mult(z, _, z).
mult(_, z, z).
mult(s(X), Y, W) :- nobigger(X, W), nobigger(Y, W), mult(X, Y, Z), add(Y, Z, W).
