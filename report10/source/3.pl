append([], Y, Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

len([], []).
len([_|X], [_|Y]) :- len(X, Y).

reverse([], []).
reverse(X, Y) :- [A|B] = X, len(X, Y), append(C, [A], Y), reverse(B, C).

concat([], []).
concat([XS|X], Z) :- append(XS, Y, Z), concat(X, Y).
