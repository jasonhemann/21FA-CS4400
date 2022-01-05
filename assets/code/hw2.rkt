#lang racket

#| Free, Bound, and Lexical Address |# 

;; There may, indeed, be other applications of the system than its use
;; as a logic.

;; -- Alonzo Church
 
#| Assignment Guidelines |#

;; You will likely find memv, remv, assv, and letrec useful.

;; A lambda-calculus expressions is one of:
;;  - variables (implemented as symbols)
;;  - lambda expressions that take exactly one argument and have exactly one body
;;  - applications (lists of exactly two lambda calculus expressions)

;; In this assignment, you will use the functions you define in the
;; first section as helpers in the later problems.

;; An association list is a list of pairs of associated values. For
;; example, the following is an association list:
;; '((a . 5) (b . (1 2)) (c . a))

;; You must use match in each of the problems from section 2 of this
;; assignment, unless the assignment states otherwise. 

;; Most of section 2's problems require both match and recursion on
;; lambda-calculus expressions.

;; You will want each of these problems to be structurally recursive. 

;; You may find match also simplifies other problems.

;; The match features we demonstrated in class are sufficient. Do not
;; use features of match we did not discuss in class.

;; Yes, the same expression expression can contain both a free
;; reference to `x` and a bound reference to `x`.

;; In a subset of Racket where lambdas have only one argument, the
;; lexical address of a variable is the number of lambdas between the
;; place where the variable is declared (i.e. the formal parameter)
;; and the place where it is referenced. The o at the very bottom of
;; the following expression is a bound variable reference. It has a
;; lexical address of 4, because there are four lambda expressions
;; between the formal parameter o at the top and the reference to o at
;; the bottom.

(lambda (o)
  (lambda (r)
    (lambda (s)
      (lambda (p)
        (lambda (g)
          o)))))

#| Part I Natural Recursion Refresher |# 

#| 

1. Consider the following incomplete definition of the list-ref
function. It should behave like Racket's list-ref.

|# 

(define (list-ref ls n)
  (letrec
      ([nth-cdr
        (λ (n)
          ;; complete the definition
          )])
    (car (nth-cdr n))))

;; The inner, nested function nth-cdr needs its body. Complete the
;; list-ref's definition by completing a naturally-recursive
;; implementation of nth-cdr, so that the following tests pass. You
;; must not modify the provided code beyond adding a body. You may of
;; course add newlines as needed. Do not call list-ref, either ours or
;; Racket's, in your definition. Follow our pattern: figure out the
;; base case, write the natural recursion, and figure out what to do
;; with the result of the recursion.

#|

2. Define and test a procedure union that takes two lists, where
neither list contains the same element twice (that is, the list
represents a set), and returns a list containing the union of the two
input lists. Again, the order of the elements in your answer does not
matter. You should use `memv` on this problem. 

|# 

#| 

3. Define and test a procedure extend that takes two arguments, say x
and a predicate pred. (Recall from the previous assignment the
definition of a predicate.) extend should return a predicate. The
returned predicate should hold for exactly those things that are eqv?
to x or satisfy pred.

> ((extend 1 even?) 1)
#t

|# 


#| 

4. Define and test a procedure walk-symbol that takes a Racket datum x
and s, an association list of symbols to Racket data. Your procedure
should search through s for the value associated with x. If the
associated value is a symbol, it too must be walked in s. If x has no
association, then walk-symbol should return x. You should use `assv`
on this problem. Cycles are absolutely bad data.

> (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
5

|# 




#| Part II Free, Bound, Lexical Address |# 

#| 

5. Define and test a procedure lambda->lumbda that takes a
lambda-calculus expression and returns the expression unchanged with
the exception that each lambda as a keyword has been replaced with the
word lumbda (notice you should not change declarations of a variable
`lambda`).

|# 


#| 

6. Define and test a procedure reference-occurs? that takes a variable
name and a lambda-calculus expression and returns a boolean answering
whether the expression contains a reference to that variable.

|# 


#| 

7. Define and test a procedure var-references that takes a lambda-calculus
expression and returns a list containing all the variable references
(the variable name itself) in the expression. This should be a
straightforward modification of lambda->lumbda, and the order of the
variables in your answer does not matter.

|#


#| 

8. Define and test a function called unique-var-references that
behaves like var-references but returns a set---i.e. it eliminates
duplicates as it evaluates. Use union in your definition.

|#


#| 

9. Define and test a procedure free-reference-occurs? that takes a
symbol and a lambda-calculus expression and returns #t if that that
expression contains a free reference to that variable, and #f
otherwise. You may have seen solutions in class to these problems that
use accumulators. You are not permitted to use accumulators on these
problems; such solutions do not receive credit.

|#


#| 

10. Define and test a procedure bound-reference-occurs? that takes a
symbol and a lambda-calculus expression and returns #t if that
expression contains a bound reference to the variable, and #f
otherwise.

|# 


#| 

11. Define and test a procedure unique-free-references that takes a
lambda-calculus expression and returns a set (represented as a list)
of all the free variable references in that expression. Order doesn't
matter, but the list must not contain duplicate variables. You may
find it helpful to use the definition of unique-var-references as a
starting point.

|#

;; Note that for instance

;; '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c))))))

;; is a single lambda-calculus expression (a procedure application),
;; not a list of lambda-calculus expressions.

#| 

12. Define and test a procedure unique-bound-references that takes a
lambda-calculus expression and returns a set (represented as a list)
of all the bound variable references in the input expression. Order
doesn't matter, but the list must not contain duplicate variables.

|# 



#| 

13. Define and test a procedure lex that takes a lambda-calculus
expression and an accumulator (which starts as the empty list), and
returns the same expression with all bound variable references
replaced by lists of two elements whose car is the symbol var and
whose cadr is the lexical address of the referenced variable. For this
problem expressions with free variable references are bad data.

This problem has several good solutions. I suggest you start by
building some lambda expressions on paper and then, by hand, find the
lexical addresses of some variable references that occur in them. Try
and do it almost mechanically, starting from the top of the expression
and working your way down. Then think about what it is you're doing,
and try and figure out how to do it without having to go back up the
tree. That is, ensure that when you get to a variable position in the
expression where you need to fill in the lexical address, that you
already have all the information you need to figure it out. Then code
that.

> (lex '(lambda (y) (lambda (x) (x y))) '())
'(lambda (lambda ((var 0) (var 1))))


If you find the list-index-ofv function from your hw1
will help you, copy it over to this file. 

|#


#| Brainteasers |#

;; Boxes are mutable memory references, meaning we can change the
;; value the box contains. You will find it useful to consult the
;; [Racket Documentation about
;; boxes](https://docs.racket-lang.org/reference/boxes.html) for
;; information about the box, unbox, and set-box! functions for this
;; problem.

;; For this problem we will now, instead, write our association list
;; such that the right-hand side of each association is always a box
;; that contains a value.

;; Without boxes (or some side-effect) we would have to (re-)copy the
;; entire data structure each time we wanted to change a portion of
;; the data structure.

;; Consider again the scenario of the walk-symbol problem. Imagine
;; that we frequently look up values in that association list.

#| 

14. You will implement walk-symbol-update, a version of walk-symbol
that implements path-compression. The function walk-symbol may become
prohibitively expensive, as certain perverse chains may be arbitrarily
long. Consider the work you would have to do to walk a0 twice in the
following association list.

'((z . 26) (y . z) (x . y) ... (b . c) (a0 . b))

With path-compression, when the function walks the association list to
find the final value for the symbol we started with, it also changes
the values in boxes we had to walk through along the way (this
sequence is called the path) so that the right-hand side of each of
those also contains the final value. Thus, if we have to walk that
same symbol again, the lookup will be faster. See the following
example:

> (define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
> a-list
((c . #&15) (e . #&f) (b . #&c) (a . #&b))
> (walk-symbol-update 'a a-list)
15
> a-list
((c . #&15) (e . #&f) (b . #&15) (a . #&15))
> (walk-symbol-update 'a a-list)
15
> a-list
((c . #&15) (e . #&f) (b . #&15) (a . #&15))

|# 

#| Just Dessert |# 

;; In order to return multiple values, you should see the Racket
;; documentation on values and let-values (and call-with-values though
;; you probably won't need it to define
;; free-and-bound-references-occur?).

#| 

15. Define a predicate free-and-bound-references-occur? that takes a
variable x and a lambda-calculus expression, and returns two values,
the first of which is a boolean answering whether the a free reference
to x occurs in the expression, and the second is a boolean answering
whether the a bound variable reference to x occurs bound in the
expression. Your solution should be a one-pass solution, meaning you
should not recur over the same data twice, and you should not use an
accumulator.

|# 

