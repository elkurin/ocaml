append([], Y, Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

hamilton_sub([], _, _).
hamilton_sub(V, E, P) :- append(_, [[P|Q]|_], E),
						 append(V1, [Q|V2], V), append(V1, V2, VN),
						 hamilton_sub(VN, E, Q).
hamilton([], _).
hamilton(V, E) :- append(V1, [U|V2], V), append(V1, V2, VN), hamilton_sub(VN, E, U).

hamilton_close_sub([], E, P, Q) :- append(_, [[P|Q]|_], E).
hamilton_close_sub(V, E, P, U) 
					  :- append(E1, [[P|Q]|E2], E), append(E1, E2, EN),
						 append(V1, [Q|V2], V), append(V1, V2, VN),
						 hamilton_close_sub(VN, EN, Q, U).
hamilton_close([], _).
hamilton_close([_|[]], _).
hamilton_close([U|V], E) :- hamilton_close_sub(V, E, U, U).
