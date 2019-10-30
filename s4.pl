%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% PSS - Homework assignment 4
% Tutor group A
% 
% Bahareh Afshari , Bachelor Programme in AI , 1st year 
% b.afshari@uva.nl 
  /*
   * I hereby declare I have actively participated 
   * in solving every exercise. All solutions are 
   * entirely my own work and no part has been 
   * copied from other sources. 
   */
% no. of hours in lab: 5
% no. of hours spent on homework assignment: 10 
%++++++++++++++++++++++++++++++++++++++++++++++++++++
 /*
  * The sorting algorithms used in this homework are
  * included at the end of this file (excluding the
  * predicate check/3, which is being redefined in
  * Question 2.
  */
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Question 1
%++++++++++++++++++++++++++++++++++++++++++++++++++++
/* Implementing a counter.
 * We are going to use a dynamic predicate called
 * counter/1 to store the current value of the counter.
 */

/* Set the counter to the value specified (by first
 * retracting any previous assertions regarding
 * counter/1, and then asserting the appropriate fact):
 */

set_counter(Value) :-
  retractall(counter(_)),
  assert(counter(Value)).

/* To retrieve the value of the counter, we simply
 * check the current value stored using counter/1:
 */

get_counter(Value) :-
  counter(Value).

% Initialising the counter means setting it to 0:

init_counter :-
  set_counter(0).

/* Incrementing the counter means retrieving its
 * current value, adding 1, and setting the counter
 * to this new value:
 */

step_counter :-
  get_counter(Value),
  NewValue is Value + 1,
  set_counter(NewValue).

/* At compilation time, initialise the counter.
 * This avoids error messages in case get_counter/1
 * is used before init_counter/0 has been executed
 * by the user or by another module of the program:
 */

:- init_counter.

/* Observe that the only dynamic predicate is counter/1.
 * The idea is that the user should never call this
 * predicate. This helps to limit unintended behaviour
 * of the program, which is a common problem when dynamic
 * predicates are used.
 */

/* Also observe that our implementation is modular and 
 * we are trying to avoid solving the same problem more
 * than once. For example, both init_counter/0 and
 * step_counter/0 are using set_counter/1 rather than
 * accessing the dynamic predicate counter/1 directly 
 * themselves.
 */

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Question 2                                              
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
/* Run an experiment for a given sorting algorithm
 * and a given list of integers and return the number
 * of primitive comparison operations required. The
 * ordering relation is always going to be <.
 *
 * We first change the implementation of check/3 in
 * such a way that each time that predicate is called,
 * the counter gets incremented. This implementation
 * overrides the implementation of check given in sort.pl.
 */

check(Rel, A, B) :-
  step_counter,
  Goal =.. [Rel,A,B],
  call(Goal).

/* Note that it is important to call step_count/0 *before*
 * the call to the comparison goal. If we change the order  
 * of these two lines, then we only count the number of 
 * comparison operations that happened to have succeeded. 
 */

/* Implementation of experiment/3: In the first line of
 * the rule body the goal is being composed. The functor
 * of the goal will be the name of the chosen algorithm,
 * the first argument will be the ordering relation <,
 * and the third will be the list provided. As we are
 * not actually interested in the sorted list itself,
 * we use the anonymous variable for the final argument
 * of the goal. Then we initialise the counter, execute
 * the goal, and finally return the new value of the
 * counter as the answer.
 */

experiment(Algorithm, List, Count) :-
  Goal =.. [Algorithm,<,List,_],
  init_counter,
  call(Goal),
  get_counter(Count).

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Question 3                                               
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 

/* Generate a list of a given length containing random
 * integers between 1 and a given maximum. For length 0,
 * the answer is the empty list (base case). For the
 * recursive rule, we first use the built-in function
 * random/1 to generate a new random number. This will
 * be the head of the output list. Then we count down
 * the length parameter and call random_list/3 again
 * recursively to generate the tail of the output list.
 */

random_list(0, _, []).

random_list(Length, MaxElem, [Head|Tail]) :-
  Length > 0,
  NewLength is Length - 1, 
  Head is random(MaxElem) + 1,
  random_list(NewLength, MaxElem, Tail).

/* Note that the call to the function random(MaxElem)
 * will return numbers between 0 and MaxElem-1, so we
 * are adding 1 (to fully conform to the specification
 * given in the exercise).
 */

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Question 4                                               
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 

/* Part a: Running an experiment on a random list.
 * We use random_list/3 to generate a random list of
 * the specified length, and then experiment/3 to run
 * an experiment on that list. 
 */

random_experiment(Algorithm, Length, MaxElem, Count) :-
  random_list(Length, MaxElem, List),
  experiment(Algorithm, List, Count).

/* Part b: Running a number of experiments on random lists.
 * The predicate random_experiments_list/5 runs the
 * specified number of experiments using random_experiment/4
 * and collects the answers (counts) in a list. Then
 * the main predicate computes the mean of that list of
 * integers, rounds that number to the nearest integer,
 * and returns this as the answer. The predicate mean/2 is
 * used to calculate the the arithmetic mean (i.e., the
 * average) of a list of integers.
 */

random_experiments(Algorithm, Length, MaxElem, Number, AvgCount) :-
  random_experiments_list(Algorithm, Length, MaxElem, Number, Counts),
  mean(Counts, Average),
  AvgCount is round(Average).

/* The cut in the following predicate is not necessary
 * from a "logical" point of view (there would be no wrong
 * answers during backtracking, for instance). It is
 * included here solely for the purpose of making the
 * program more efficient. If a recursive rule involves
 * only a single recursive call, that recursive call is
 * the last subgoal in the rule body, and there is a cut
 * preceding that last subgoal, then Prolog can manage the
 * memory required to keep track of the current state of
 * the recursion stack in a more efficient manner (because
 * it knows for sure that backtracking will not be required
 * later on). This is known as "tail recursion" (see Bratko,
 * Section 8.5.3, for more information).
 */

random_experiments_list(_, _, _, 0, []).

random_experiments_list(Algorithm, Length, MaxElem, Number, [Count|Counts]) :-
  Number > 0,
  random_experiment(Algorithm, Length, MaxElem, Count),
  NewNumber is Number - 1, !, % cut for efficiency only
  random_experiments_list(Algorithm, Length, MaxElem, NewNumber, Counts).

/* The predicate mean/2 can be used to compute the
 * arithmetic mean of a given list of numbers. Note that
 * the base case is not the empty list, because without
 * any numbers you cannot calculate a mean (the mean would
 * be undefined in that case). In the recursion step we
 * take the mean of the tail, multiply it with the length
 * of the tail (this will give us the sum of all elements
 * but the head), add the head, and finally divide this
 * by the length of the entire list.
 */

mean([N], N).

mean([N | Tail], Result) :-
  mean(Tail, TailResult),
  length(Tail, TailLength),
  Result is (TailResult * TailLength + N) / (TailLength + 1).

/* An alternative solution would be to first compute the sum
 * of the numbers in the list and then to divide by the length
 * of the list. (Btw, this is Exercise 3.6 in the Lecture
 * Notes.)
 */

/* Runtimes (sorting 100 numbers from 1 to 500):
 * Here are a few sample queries that show both the average
 * number of comparisons required and the number of milliseconds
 * required (for 100 runs).
 *
 * ?- time(random_experiments(bubblesort, 100, 500, 100, Count)).
 * % 109,805,182 inferences, 11.843 CPU in 13.875 seconds (85% CPU, 9271769 Lips)
 * Count = 109527 
 *
 *
 * ?- time(random_experiments(bubblesort2, 100, 500, 100, Count)).
 * % 8,894,102 inferences, 0.983 CPU in 1.141 seconds (86% CPU, 9045956 Lips)
 * Count = 8827
 *
 *
 * ?- time(random_experiments(quicksort, 100, 500, 100, Count)).
 * % 749,453 inferences, 0.090 CPU in 0.103 seconds (87% CPU, 8338466 Lips)
 * Count = 646
 *
 * That is, it takes over 100,000 comparisons and just
 * under 140 milliseconds to sort a list of 100 elements
 * using naive bubblesort. For improved bubblesort, these
 * figures improve by a factor of about 12 (just under 9,000
 * comparisons and ~12ms). Using quicksort, we get a further
 * improvement by again a similar factor (~650 comparisons
 * and ~1ms). So, on my machine, quicksort is around 140
 * times faster than naive bubblesort on a list of 100
 * random elements. For a list of only 10 elements, on the
 * other hand, quicksort is only around 3 times as fast as
 * bubblesort (try it). This exemplifies that the difference
 * in complexity that we have observed at a theoretical
 * level only really starts to matter as the problem size
 * increases.
 */

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Question 5                                               
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 

/* Visualise the runtimes required to sort random lists of
 * different lengths.
 * The input parameters are the name of the algorithm to
 * be used, the maximum length of the list, the maximum
 * random integer, and the number of experiments to be run
 * for each list length.
 *
 *
 * We use an auxiliary predicate chart/5 to implement
 * chart/4. The additional parameter is a counter, counting
 * from 1 up to the maximum list length. For each value of
 * the counter, random_experiments/5 is called to compute
 * the average number of comparisons required. Then a line
 * of asterisks of that length is printed on the screen.
 * This is done using the predicate line/2. We also print
 * out the length of the list tested for each line. In this
 * context, the predicate rightflush_number/2, explained
 * below, is used to make sure that numbers of varying
 * length all take up the same width (i.e., a certain
 * number of blank characters will be printed first).
 */

chart(Algorithm, MaxLength, MaxElem, Number) :-
  chart(Algorithm, 1, MaxLength, MaxElem, Number).

chart(_, Length, MaxLength, _, _) :-
  Length > MaxLength,!.

chart(Algorithm, Length, MaxLength, MaxElem, Number) :-
  random_experiments(Algorithm, Length, MaxElem, Number, Count),
  rightflush_number(Length, MaxLength),
  write(' > '), line(Count, '*'), nl,
  NewLength is Length + 1,
  chart(Algorithm, NewLength, MaxLength, MaxElem, Number).

/* Given two numbers, the second of which should not be
 * smaller than the first, print out the first number preceded
 * by as many blank characters as are needed to take up the
 * same space as printing the second number would. This is
 * done by first computing the number of digits for both
 * numbers and then using line/1 to print out the
 * appropriate number of blank characters.
 */

rightflush_number(Number, BigNumber) :-
  digits(Number, NumberDigits),
  digits(BigNumber, BigNumberDigits),
  Shift is BigNumberDigits - NumberDigits,
  line(Shift, ' '),
  write(Number).

/* Compute the number of digits of a given positive integer.
 * Explanation: The logarithm with base 10 gives a first
 * approximation. For instance, we have log_10(1000)=3 and
 * log_10(999)=2.99... The floor/1 function returns the
 * integer we get by removing anything after the decimal point.
 */

digits(Number, Digits) :-
  Digits is floor(log10(Number)) + 1.

% Print a given number of copies of a given string:

line(0, _):-!.
	
line(Length, Char) :-
  write(Char),
  Length1 is Length - 1,
  line(Length1, Char). 

% Here are some sample runs:

/*++++++++++++++++++++++++++++++++++++++++++++++++++++ 

?- chart(bubblesort, 8, 50, 100).
1 >
2 > *
3 > ****
4 > *********
5 > ***************
6 > *************************
7 > **************************************
8 > *********************************************************
true

?- chart(bubblesort2, 10, 50, 100).
 1 >
 2 > **
 3 > ****
 4 > ********
 5 > **************
 6 > *********************
 7 > ******************************
 8 > ******************************************
 9 > ****************************************************
10 > *******************************************************************
true
  
?- chart(quicksort, 15, 50, 100).
 1 >
 2 > *
 3 > ***
 4 > *****
 5 > *******
 6 > **********
 7 > **************
 8 > ******************
 9 > *********************
10 > *************************
11 > *****************************
12 > ********************************
13 > *************************************
14 > ******************************************
15 > **********************************************
true

++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/* These graphs confirm the theoretical complexity results
 * discussed in class. For example, for bubblesort2, we clearly
 * see a typical quadratic function (a parabola), corresponding
 * to the O(n^2) complexity of the improved bubblesort
 * algorithm. For simple bubblesort, we see a similar curve
 * that increases significantly faster. For quicksort, we see
 * a curve that grows much more slowly and that is much closer
 * to a linear function, reflecting the O(n log n) complexity
 * of quicksort.
 */


%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Sorting Algorithms: Bubblesort and Quicksort
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 

/* This part of the program provides the implementations
 * of naive bubblesort, improved bubblesort, and quicksort
 * as introduced during the course. The code is identical
 * to the code given on the lecture slides. Refer to the
 * slides for explanations. Note that the predciate check/3
 * has been (re)implemented above.
 */

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% BUBBLESORT (naive version)
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
/* Implementation of naive bubblesort (where we return
 * to the front of the list after every successful swap):
 */

bubblesort(Rel, List, SortedList) :-
  swap(Rel, List, NewList), !,
  bubblesort(Rel, NewList, SortedList).

bubblesort(_, SortedList, SortedList).

swap(Rel, [A,B|List], [B,A|List]) :-
  check(Rel, B, A).

swap(Rel, [A|List], [A|NewList]) :-
  swap(Rel, List, NewList).

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% BUBBLESORT (improved version)
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
/* Implementation of improved bubblesort (where we
 * continue to swap wrongly ordered pairs until we
 * reach the end of the list, before returning to the
 * front again):
 */

bubblesort2(Rel, List, SortedList) :-
  swap2(Rel, List, NewList),
  List \= NewList, !,
  bubblesort2(Rel, NewList, SortedList).

bubblesort2(_, SortedList, SortedList).

swap2(Rel, [A,B|List], [B|NewList]) :-
  check(Rel, B, A),
  swap2(Rel, [A|List], NewList).

swap2(Rel, [A|List], [A|NewList]) :-
  swap2(Rel, List, NewList).

swap2(_, [], []).

%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% QUICKSORT
%++++++++++++++++++++++++++++++++++++++++++++++++++++ 
% Implementation of quicksort:

quicksort(_, [], []).

quicksort(Rel, [Head|Tail], SortedList) :-
  split(Rel, Head, Tail, Left, Right),
  quicksort(Rel, Left, SortedLeft),
  quicksort(Rel, Right, SortedRight),
  append(SortedLeft, [Head|SortedRight], SortedList).

split(_, _, [], [], []).

split(Rel, Middle, [Head|Tail], [Head|Left], Right) :-
  check(Rel, Head, Middle), !,
  split(Rel, Middle, Tail, Left, Right).

split(Rel, Middle, [Head|Tail], Left, [Head|Right]) :-
  split(Rel, Middle, Tail, Left, Right).

%++++++++++++++++++++++++++++++++++++++++++++++++++++
% Self−check passed!
%++++++++++++++++++++++++++++++++++++++++++++++++++++