/*  Functional and Logical Programming 2014 - Excrecise 10 Solution

My name is Or Maoz
My Id is 029983111

Part 1: Tree Representation
 */

/* tree_insert(X, Tbefore, Tresult)
   tree(Left, Value Right) */
tree_insert(X, [], tree([], X, [])) :- !.
tree_insert(X, tree(Left, Value, Right), tree(NewLeft, Value, Right)) :- X =< Value, tree_insert(X, Left, NewLeft), !.
tree_insert(X, tree(Left, Value, Right), tree(Left, Value, NewRight)) :- X > Value,	tree_insert(X, Right, NewRight).

/* A. */
/* In case of leaf */
tree_contains(tree(_, X, _), X) :- !. 
/* In case X is smaller than the currnet element */
tree_contains(tree(Left, Value, _), X) :- X =< Value, tree_contains(Left, X).
/* In case X is bigger than the current element */
tree_contains(tree(_, Value, Right), X) :- X > Value, tree_contains(Right, X).

/* B. */
/* Base case - singeltone */
list_to_tree([L], T) :- tree_insert(L, [], T), !.
/* In case of a list, work on the first element and recursive call the rest of the list */
list_to_tree([L | L2], T) :- list_to_tree(L2, T2), tree_insert(L, T2, T).

/* C. */
/* Base case */
tree_flip([], []) :- !.
/* Conduct the flip, recursevly */
tree_flip(tree(Left, Value, Right), tree(Fleft, Value, Fright)) :- tree_flip(Left, Fright), tree_flip(Right, Fleft).

/* D. */
/* Base case */
tree_inorder([], []) :- !.
/* In case it's a leaf */
tree_inorder(tree([], Value, []), [Value]) :- !.
/* Recursive call from left sub-tree and right sub-tree and append them together with the Current element (Value) */
tree_inorder(tree(Left, Value, Right), Result) :- tree_inorder(Left, L), tree_inorder(Right, R), append(L, [Value | R], Result).

/* E. */
tree_sort([], []) :- !.
tree_sort(L, R) :- list_to_tree(L, T), tree_inorder(T, R).

/* Part 2 – More Prolog */

/* A. */

/* Base case */
myflatten([], []) :- !.
/* In case the first element is a list */
myflatten([[First1 | First2] | Rest], Result) :- myflatten([First1 | First2], R0), myflatten(Rest, R1), append(R0, R1, Result), !.
/* In case the first element isn't a list */
myflatten([First | Rest], Result) :- myflatten(Rest, R0), append([First], R0, Result).

/* B. */
helper(0, Before, Last, List, List):- !.  /* This is the end of the recursino, counter is 0, return result */
helper(0, Before, Last, List, Notlist):- !, fail. /* In case fibonacci was called with a list that didn't represent a fibonacci series. */

/* The recursive call: change the counter to counter-- and call helper with the updated result and the new before-last and last elements */ 
helper(X, Before, Last, List, Result) :-    X1 is X - 1,	  
											Newlast is Last + Before, 
											append(List, [Newlast], R1), 
											helper(X1, Last, Newlast, R1, Result). 

/* Base case */											
fibonacci(1, [1]) :- !.

/* calling a helper function with 3 more args: the last two elements of the list and a list to hold the result */
fibonacci(X, Result) :- X > 0, X1 is X - 1, helper(X1, 0, 1, [1], Result).

find_min([X], X).
find_min([X | Y], X) :- find_min(Y, MinY), X < MinY, !.
find_min([_ | Y], X) :- find_min(Y, X), !.

take_min([X], X, []).
take_min([X | Y], X, Y) :- take_min(Y, MinY, _), X < MinY, !.
take_min([A | Y], X, [A | Y1]) :- take_min(Y, X, Y1), !.

find_k(L,0,R) :- find_min(L,R), !.
find_k(L,X,R) :- X1 is X - 1, take_min(L,M,R1), find_k(R1,X1,R).

