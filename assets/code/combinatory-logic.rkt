#lang racket
(require rackunit-abbrevs)

#| Combinatory Logic and Bracket Abstraction |# 

;; [Currying] Named after Haskell Curry, one of the inventors of
;; combinatory logic. Curry always insisted that he got the idea of
;; using h from M. Schönfinkel’s [Sch24] (see [CF58, pp. 8, 10]), but
;; most workers seem to prefer to pronounce "currying" rather than
;; "schönfinkeling".

;; J. Roger Hindley, "Lambda-Calculus and Combinators: An
;; Introduction"
 
#| Assignment Guidelines |#

;; Simple, implement the basic algorithm and make it go. You should
;; produce the results of the plain ol' ordinary usual bracket
;; abstraction. More complicated bracket abstraction algorithms are
;; available, but do this first and the rest if you find you have
;; time.

;; MORE THAN USUAL, THESE TESTS ARE NOT COMPREHENSIVE. YOU SHOULD
;; DEFINITELY WRITE YOUR OWN AND INCLUDE THEM HERE.

#| 

Bracket abstraction, as usually presented. These rules are listed in
order or precedence. 

T[x]                            = x
T[E1 E2]                        = (T[E1] T[E2])
T[(lambda (x) x)]               = I 
T[(lambda (x) E)]               = (K T[E])                                  when x is not in FV(E)
T[(lambda (x) (lambda (y) E))]  = T[(lambda (x) T[(lambda (y) E]])]         when x is in FV(E)
T[(lambda (x) (E1 E2))]         = S(T[(lambda (x) E1)] T[(lambda (x) E2)])  when x is in either FV(E1) or FV(E2)

|#

;; We also provide to you `ski-interp` an interpreter for SKI
;; expressions into the CBV lambda calculus as implemented in Racket.

(define (ski-interp expr)
  (match expr
    [`S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z)))))]
    [`K (lambda (x) (lambda (y) x))]
    [`I (lambda (x) x)]
    [`,d #:when (and (symbol? d) (not (memv d '(S K I)))) d]
    [`(,rator ,rand) ((ski-interp rator) (ski-interp rand))]))

;; We also provided you (a modified version of) `var-occurs-free?`
;; from one of your earlier homework assignments. We stipulate here
;; that S, K, and I are special constant symbols, and are not to be
;; shadowed by lambda bindings. (We will follow the usual convention
;; that upper-case letters are primitive combinators, that lower-case
;; letters are variables, and futher we will fully parenthesize all
;; combinations.)

(define (var-occurs-free? z expr)
  (match expr
    [`S #f]
    [`K #f]
    [`I #f]
    [`,y #:when (symbol? y) (eqv? z y)]
    [`(lambda (,x) ,b) (and (not (eqv? x z)) (var-occurs-free? z b))]
    [`(,rator ,rand) (or (var-occurs-free? z rator) (var-occurs-free? z rand))]))

;; 1. Define and implement `abstract-out`, a Racket function that
;; implements the standard bracket abstraction algorithm transforming
;; lambda calculus expressions to SKI-calculus expressions. Unlike the
;; usual pen-and-paper presentation, you should fully curry out your
;; combinator expression, and in specific your function should output
;; Sxy as ((S x) y).










(check-true* equal?
  [(ski-interp 'x) 'x] 
  [(abstract-out '(lambda (a) a)) 'I]
  [(ski-interp `(,(abstract-out '(lambda (a) a)) x)) 'x]
  [((ski-interp (abstract-out '(lambda (a) a))) 'x) 'x]
  [(abstract-out '(lambda (a) ((lambda (z) z) a))) '((S (K I)) I)]
  [(ski-interp `(,(abstract-out '(lambda (a) ((lambda (z) z) a))) x)) 'x]
  [((ski-interp (abstract-out '(lambda (a) ((lambda (z) z) a)))) 'x) 'x]
  [(abstract-out '(lambda (a) ((lambda (b) (b a)) (lambda (z) z)))) '((S ((S (K (S I))) ((S (K K)) I))) (K I))]
  [(ski-interp `(,(abstract-out '(lambda (a) ((lambda (b) (b a)) (lambda (z) z)))) x)) 'x]
  [((ski-interp (abstract-out '(lambda (a) ((lambda (b) (b a)) (lambda (z) z))))) 'x) 'x])

#| Just Dessert |#

;; As we mentioned there are plenty of more sophisticated bracket
;; abstraction algorithms. See, for instance [Tromp, Sec
;; 3.2](https://tromp.github.io/cl/LC.pdf). And of course,
;; [Oleg](http://okmij.org/ftp/tagless-final/ski.pdf) has an algorithm
;; for converting lambda terms to combinators that's both linear in
;; time and space.

;; Your challenge---should you choose to accept it---is to implement
;; one of these more sophisticated algorithms, and show off how it
;; compares by providing tests to help us see the difference. 

