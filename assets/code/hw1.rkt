#lang racket

#| Recursion and Higher-order Functional Abstraction |# 

;; Recursion is the root of computation since it trades description
;; for time. 

;; -- Alan Perlis 
 
#| Assignment Guidelines |#


;; In addition to the standard Assignment Guidelines
;; (https://jasonhemann.github.io/21FA-CS3800/hw/)

;; You should write your solutions without creating explict help
;; functions. You may, however re-use your solutions to prior
;; problems.

#| 

0. I've recently updated the course syllabus for this semester. Please
read through it carefully before beginning the rest of the assignment.

|# 


#| 

 1. Define and test a procedure countdown that takes a natural number
and returns a list of the natural numbers less than or equal to that
number, in descending order. Our natural numbers begin at zero.

|#



#| 

2. Define and test a procedure insertR that takes two symbols and a
list and returns a new list with the second symbol inserted after each
occurrence of the first symbol. For this and later questions, these
functions need only hold over eqv?-comparable structures.

|#



#| 

3. Define and test a procedure remv that takes an atom and a list of
atoms and returns a list similar except its missing the first
occurrence (if any) of the input atom.

|# 



#| 

4. Define and test a procedure list-index-ofv that takes an element
and a list containing that element and returns the (base 0) index of
that element in the list. List without that element are bad data.

|# 



#| 

5. Define and test a procedure filter that takes a predicate and a
list and returns a new list containing the elements that satisfy the
predicate. A predicate is a procedure that takes a single argument and
returns either #t or #f. The number? predicate, for example, returns
#t if its argument is a number and #f otherwise. The argument
satisfies the predicate, then, if the predicate returns #t for that
argument.

|# 



#| 

6. Define and test a procedure zip that takes two lists and forms a
new list, each element of which is a pair formed by combining the
corresponding elements of the two input lists. If the two lists are of
uneven length, zip will drop the tail of the longer one.

|# 



#| 

7. Define and test a procedure map that takes a procedure p of one
argument and a list ls and returns a new list containing the results
of applying p to the elements of ls. Do not use Racket's built-in map
in your definition.

|# 



#| 

8. Define and test a procedure append that takes a list l and any
racket datum (any old racket, be it a list or not) d, and returns a
new racket datum with the elements of l prepended. This should work
for any racket datum d, but testing against the data we have talked
about in class is sufficient.

|# 



#| 

9. Define and test a procedure reverse that takes a list l and returns
list with the elements of l in the opposite order

|# 



#| 

10. Define and test a procedure fact that takes a natural number and
computes the factorial of that number. The factorial of a number is
computed by multiplying it by the factorial of its predecessor. The
factorial of 0 is defined to be 1 (https://oeis.org/A000142).

|# 



#|

11. Define and test a procedure fib that takes a natural number n as
input and computes the nth number, starting from zero, in the
Fibonacci sequence (0, 1, 1, 2, 3, 5, 8, 13, 21, …). Each number in
the sequence is computed by adding the two previous numbers.

|# 


#| 

12. Define a function cons-every that takes an element x and a list l
and returns a list with x consed to the front of each element of l. 

|# 

#| 

13. Define and test a procedure binary->natural that takes a flat list
of 0s and 1s representing an unsigned binary number in reverse bit
order and returns that number. For example:

'()      ;; 0
'(1)     ;; 1
'(0 1)   ;; 2
'(1 1)   ;; 3
'(0 0 1) ;; 4
'(1 0 1) ;; 5
'(0 1 1) ;; 6

|# 



#| 

14. Define subtraction using natural recursion. Your subtraction
function, minus, need only take nonnegative inputs where the result
will be nonnegative.

|# 



#| 

15. Define division using natural recursion. Your division function,
div, need only work when the second number evenly divides the
first (that is, for the divisible abelian group that's a subgroup of
Nat). Divisions by zero is of course bad data.

|# 



#| 

16. Define a function append-map that, similar to map, takes both a
procedure p of one argument a list of inputs ls and applies p to each
of the elements of ls. Here, though, we mandate that the result of p
on each element of ls is a list, and we append together the
intermediate results. Do not use Racket's built-in append-map in your
definition.

|# 



#| 

17. Define a function set-difference that takes two flat sets (lists
with no duplicate elements) s1 and s2 and returns a list containing
all the elements in s1 that are not in s2.

|# 



#| Brainteasers |#

#| 

19. In mathematics, the power set of any set S, denoted P(S), is the
set of all subsets of S, including the empty set and S itself. The
procedure combinations takes a list and returns the power set of the
elements in the list. Implementations may different in the exact order
of their results' sublists. You should not use Racket's combinations
in your solution.

|# 


#| 

20. The cartesian-product is defined over a non-empty list of
sets (again, by our agreed upon convention, sets are lists that don't
have duplicates). The result is a list of tuples (i.e. a list of
lists). Each tuple has in the first position an element of the first
set, in the second position an element of the second set, etc. The
output list should contains all such tuples. The exact order of your
tuples may differ; this is acceptable. You should not use Racket's
cartesian-product in your solution.

|#


#| 

21. Rewrite some of the natural-recursive programs from above instead
using foldr. That is, the bodies of your definitions should not refer
to themselves. The names should be as follows.

I recommend this (http://www.cs.nott.ac.uk/~pszgmh/fold.pdf) treatise
on fold operators. It contains answers to several of the above
sub-problems. It will also teach you about programming with
foldr. There are stunningly beautiful definitions of the last two
sub-problems. They're just mind-blowing. And to tease you further,
know that some (pretty clever, albeit) folk solved this almost 50
years ago, back when lexical scope wasn't a thing, higher-order
functions weren't commonplace like they are today, and many of the
common programming idioms and that we take for granted just weren't
around. Since I hate to pass up an excuse to show off something cool,
I gotta tell you about
[this](https://www.brics.dk/RS/07/14/BRICS-RS-07-14.pdf) derivation
and explanation of answers to the last couple of problems, but you
have to promise (1) you'll try it first on your own, and (2) that if
you peek at the answers, you'll read the whole thing. It's short,
moves quickly, and very high enlightenment/text ratio. That's my sales
pitch.

- insertR-fr
- filter-fr
- map-fr
- append-fr
- reverse-fr
- cons-every-fr
- binary->natural-fr
- append-map-fr
- set-difference-fr
- combinations-fr
- cartesian-product-fr

|# 

#| 

22. Consider the below function f. It is an open question in
mathematics, known as the Collatz Conjecture, as to whether, for every
positive integer n, the function power limit of f on n is 1. Your task
is to, complete the below definition of collatz. collatz should be a
function which will, when given a positive integer as an input,
operate in a manner similar to the straightforward recursive function
f.

Your completed answer should be very short. It must be no more than
one line long (and prettily-indented, so don't think about squeezing
something big on a single line), and must not use lambda. Your
collatz should compute the collatz of positive integers; for
non-positive integers, it should signal an error “Invalid value”.

|# 

(define (f n)
  (cond
    ((even? n) (/ n 2))
    ((odd? n) (add1 (* n 3)))))

#| 

(define collatz
  (letrec
    ((odd-case
      (lambda (recur)
        (lambda (x)
          (cond 
            ((odd? x) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
      (lambda (recur)
        (lambda (x)
          (cond 
            ((even? x) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
      (lambda (recur)
        (lambda (x)
          (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
      (lambda (x)
        (error 'error "Invalid value ~s~n" x))))
    ... ;; this should be a single line, without lambda
    ))

|# 

#| Just Dessert |# 

#| 

23. A quine is a program whose output is the listings (i.e. source
code) of the original program. In Racket, 5 and #t are both quines.

> 5
5
> #t
#t

We will call a quine in Racket that is neither a number nor a boolean
an interesting Racket quine. Below is an interesting Racket quine.

> ((lambda (x) (list x (list 'quote x)))
  '(lambda (x) (list x (list 'quote x))))
'((lambda (x) (list x (list 'quote x)))
   '(lambda (x) (list x (list 'quote x))))

Write your own interesting Racket quine, and define it as quine. The
following should then be true. Not every Racket list is a quine;
Racket's standard printing convention will prepend a quote to a
list. Make sure to use the above tests.

|# 



