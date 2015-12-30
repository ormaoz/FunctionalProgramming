/*  Functional and Logical Programming 2014 - Excrecise 9 Solution

My name is Or Maoz
My Id is 029983111

Part 1 - Unifications

1. A = p1(a, b)

2. X = Y = 42

3. fail, cannot unify b and a

4. A = B = p2(3)

5. fail, cannot unify A to both 3 and 4

6. A = 3, B = 3, C = p3(B)

7. fail, cannot unify A to both p2(a, Y) and p2(Y, 3)

8. X = p3(7, 8), W = p2(2, p3(7, 8))

9. A = p2(a, 3) B = 2, X = a, Y = 3
 
Part 2 - Simpe Prolog
1. */

even(2) :- !.
even(A) :- A > 2, A1 is A - 2, even(A1).

/* 2. */
fiboNth(0, 0) :- !.
fiboNth(1, 1) :- !.
fiboNth(X, Y) :- X1 is X - 1, fiboNth(X1, Y1), X2 is X - 2, fiboNth(X2, Y2), Y is Y2 + Y1.
 
