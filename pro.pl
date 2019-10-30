female(mary).
female(sandra).
female(juliet).
female(lisa).
male(peter).
male(paul).
male(dick).
male(bob).
male(harry).
parent(bob, lisa).
parent(bob, paul).
parent(bob, mary).
parent(juliet, lisa).
parent(juliet, paul).
parent(juliet, mary).
parent(peter, harry).
parent(lisa, harry).
parent(mary, dick).
parent(mary, sandra).
father(X,Y) :-
  parent(X,Y),
  male(X).

sister(X,Y):-
    parent(Z,X),
    parent(Z, Y),
    female(X).

grandmother(X,Y):-
  parent(X,Z),
  parent(Z,Y),
  female(X).
cousin(X,Y):-
  grandmother(Z,X),
  grandmother(Z,Y),
  X \= Y.

  factorial(1, 1).
  % base case
  factorial(N, Result) :- % recursion step
  N > 1,
  N1 is N - 1,
  factorial(N1, Result1),
  Result is Result1 * N.
