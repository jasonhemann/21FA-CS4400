#lang racket
(require rackunit)

#| Environments and Interpreters |# 


;; Inverting the adage that a data type is just a simple programming
;; language, we take the position that a programming language is,
;; semantically, just a complex data type; evaluation of a program is
;; just another operation in the data type.

;; -- Mitch Wand
 
#| Assignment Guidelines |#

;; Recall that in recent lectures, we've learned how to write an
;; interpreter that takes a Racket expression and returns the
;; expression's value. We have also learned to make this interpreter
;; representation independent with respect to environments, and we
;; have written two different representations of the helpers
;; extend-env, apply-env, and empty-env.

;; In the first part of this assignment you will implement the three
;; interpreters I presented in lecture.

;; For the 2nd and 3rd interpreters you must also define two sets of
;; environment helpers: one that uses functional (higher-order)
;; representation of environments, and one that uses data-structural
;; representation of environments. 

;; Your data structure representations should be the tagged list
;; representation demonstrated in class.

;; You must name your interpreters and helpers for each of the first
;; three problems respectively by the following naming
;; conventions. These below names may differ sligtly from the names I
;; used in lecture.

#| 
(define value-of ...)
 
(define value-of-fn ...)
(define empty-env-fn ...)
(define extend-env-fn ...)
(define apply-env-fn ...)
 
(define value-of-ds ...)
(define empty-env-ds ...)
(define extend-env-ds ...)
(define apply-env-ds ...)
|# 

;; Your first three interpreters must all handle the following forms:
;; numbers, booleans, variables, lambda-abstraction, application,
;; zero?, sub1, *, if, and let.

;; In the second part you will implement a fourth interpreter, this
;; time an interpreter for a new language.

;; For this assignment your solutions must be compositional or you
;; will lose credit.  E.g. although we could rewrite the expression
;; (let ([x e]) body) as ((lambda (x) body) e), you must not use
;; lambda in this way for your interpreter's line for let
;; expressions. Instead, you must implement let in its own right.

#| Interpreters and Environments |#

#|

1. value-of 

|# 

(test-equal?
 "A nearly-sufficient test-case of your program's functionality"
 (value-of
  '(((lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
     (lambda (f)
       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
    5)
  (lambda (y) (error 'value-of "unbound variable ~a" y)))
 120)


#| 

2. value-of-fn. 

We walked through the steps of implementing this in lecture, at least
for the basic forms. From a software engineering point-of-view, you
can see this process as follows. In the above, we hard-coded our
implementation of "environment", tightly coupling the client
code (value-of) to the implementation of environments. Now, we will
correct that mistake by engineering an interface: `apply-env-fn`,
`extend-env-fn`, and `empty-env-fn` collectively make up the interface
for environments. By replacing those hard-coded implementations of
environments with calls to these "help functions" we will construct an
interface against which we can program, correctly separating the
client code that uses environment from the implementation that
provides environment across the interface.

|# 

#| 

3. value-of-ds

We walked through the steps of implementing this in lecture, at least
for the basic forms. We can also see this step from a software
engineering point-of-view as follows. In the above, we shimmed in an
interface to separate our client code that uses environment and our
implementation code that provides environment, across an interface
that is the three functions `apply-env-fn`, `extend-env-fn`, and
`empty-env-fn`. In this step, we will now demonstrate to ourselves
that this was a well-defined interface, that is, that our interface is
not "leaky." We will re-implement environment, using an entirely
different representation behind-the-scenes. Since our client (the
interpreter) is programming against the interface, though, the client
code won't have to change at all*.

With this switch to a representation of environments as data
structures, we notice another neat thing. We changed our environment
went from a higher-order, functional representation in the last part
of the assignment to now, a first-order data structure
representation. And we can match against our environment like you
would any old data definition. 


* Okay, so we're in point of fact changing both the interpreter client
code and the "helper" interface functions from -fn to -ds, but this is
just so that we can have the different versions of this interpreter in
the same file.

|# 

#| A New Syntax |#

#| 

4. Implement an interpreter fo-eulav. Let the below examples guide
you. I only require you to implement those forms I use in those
examples.

|# 

(test-equal?
 "Ppa"
 (fo-eulav '(5 (x (x) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
 5) 
(test-equal?
 "Stnemugra sa Snoitcnuf"
 (fo-eulav '(((x 1bus) (x) adbmal) ((5 f) (f) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
 4) 
(test-equal?
 "Tcaf"
 (fo-eulav
  '(5  
    (((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
       (n) adbmal)
      (f) adbmal)
     ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
       (n) adbmal)
      (f) adbmal))) 
  (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
 120)

#| Brainteasers |#

;; Consider the following interpreter for a deBruijnized version of
;; the lambda-calculus (i.e. lambda-calculus expressions using lexical
;; addresses addresses instead of variables). Notice this interpreter
;; is representation-independent with respect to environments. There
;; are a few other slight variations in the syntax of the
;; language. These are of no particular consequence.

(define (value-of-lex exp env)
  (match exp
    [`(const ,expr) expr]
    [`(mult ,x1 ,x2) (* (value-of-lex x1 env) (value-of-lex x2 env))]
    [`(zero ,x) (zero? (value-of-lex x env))]
    [`(sub1 ,body) (sub1 (value-of-lex body env))]
    [`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env))]
    [`(var ,num) (apply-env-lex env num)]
    [`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env)))]
    [`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))]))
 
(define (empty-env-lex)
  '())

(test-equal?
 "This test shows we're using a data-structure representation of environments."
 (value-of-lex '((lambda (var 0)) (const 5)) (empty-env-lex))
 5)

#| 

5. Without using lambda or the implicit lambda in an "MIT-define",
define apply-env-lex and extend-env-lex. A correct solution is very
short.  

|#

#|

6. Go back and extend your interpreter value-of to support set! and
begin2, where begin2 is a variant of Racket's begin that takes exactly
two arguments, and set! mutates variables.

|# 

#| Just Dessert |# 

;; The lambda calculus can be used to define a representation of
;; natural numbers, called Church numerals, and arithmetic over
;; them. For instance, c5 is the definition of the Church numeral for
;; 5. This is often described as "representing a number by its
;; fold". What they mean by this is: think of any given number not a
;; piece of data, but in terms of "the interface it implements." What
;; does a number *do* for you? It tells you how many times to iterate
;; some behavior. 



(define c0 (lambda (s) (lambda (z) z)))
(define c5 (lambda (s) (lambda (z) (s (s (s (s (s z))))))))

(test-equal?
 "Church 5 acts like 5"
 ((c5 add1) 0)
 5)

(test-equal?
 "Church 0 acts like 0"
 ((c0 add1) 0)
 0)

;; The following is a definition for Church plus, which performs
;; addition over Church numerals.

(define c+
  (lambda (m) 
    (lambda (n) 
      (lambda (s)
        (lambda (z)
          ((m s) ((n s) z)))))))


(define c10 ((c+ c5) c5))

(test-equal?
 "Church addition acts like addition on Church numerals"
 ((c10 add1) 0)
 10)

;; One way to understand the definition of c+ is that it, when
;; provided two Church numerals, returns a function that, when
;; provided a meaning for add1 and a meaning for zero, uses provides
;; to m the meaning for add1 and, instead of the meaning for zero,
;; provides it the meaning for its second argument. m is the sort of
;; thing that will count up m times, so the resulting function is the
;; meaning of m + n.

#| 

7. Your task, however, is to implement csub1, Church predecessor. You
should also provide tests. The Church predecessor of Church zero is
zero, as we haven't a notion of negative numbers. This was a difficult
problem, but it's fun, so don't Google it. If you think it might help
though, consider taking a [trip to the
dentist](http://link.springer.com/chapter/10.1007%2FBFb0062850).

|#


