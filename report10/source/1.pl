male(kobo).
male(koji).
male(iwao).
male(tonarinoojisan).
male(tonarinooya).
female(sanae).
female(mine).
female(miho).
female(tonarinoobasan).
female(kakusigo).

parent(kobo, koji).
parent(kobo, sanae).
parent(miho, koji).
parent(miho, sanae).
parent(sanae, iwao).
parent(sanae, mine).
parent(kakusigo, koji).
parent(tonarinoojisan, tonarinooya).
parent(tonarinoobasan, tonarinooya).

father(X, Y) :- parent(X, Y), male(Y).
mother(X, Y) :- parent(X, Y), female(Y).
sibling(X, Y) :- parent(X, Z), parent(Y, Z).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
ancestor(X, Z) :- parent(X, Z).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
ancestor(X, Z) :- X = Z.

bloodrelative(X, Y) :- ancestor(X, Z), ancestor(Y, Z).
