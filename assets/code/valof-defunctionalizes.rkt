#lang racket

#| What does our valof defunctionalize? |# 

;; So far we fully defunctionalized environments and closures. Rather
;; than explicit higher-order function calls, we instead dispatch
;; against the choices, where we have represented each choice with a
;; corresponding data structure.

(define (make-closure x b env)
  `(closure ,x ,b ,env))

(define (apply-closure c a)
  (match c
    [`(closure ,x ,b ,env) 
     (valof b (extend-env x a env))]))

(define (extend-env x a env)
  `(extension x a env))

(define (empty-env)
  `(empty))

(define (apply-env env y)
  (match env
    [`(empty) (error "unbound id" y)]
    [`(extension x a env) 
      (if (eqv? x y) a (apply-env env y))]))

;; However, we began with one match expression already: in valof. We
;; didn't start by defunctionalizing higher-order functions to get
;; here. But morally, we should have. What did we defunctionalize in
;; order to create these first-order data structures?

(define (valof exp env)
  (match exp
    [`,n #:when (number? n) n]
    [`(+ ,ne1 ,ne2) (+ (valof ne1 env) (valof ne2 env))]
    ;; the core 
    [`,y #:when (symbol? y) (apply-env env y)]
    [`(lambda (,x) ,b) (make-closure x b env)]
    [`(,rator ,rand) (apply-closure (valof rator env) (valof rand env))]))

;; In principle, we shouldn't be constructing programs willy-nilly. We
;; oughtn't be building raw syntax. Instead each of these forms ought
;; to be the result of some syntax constructors.

(define (make-plus ne1 ne2)
  `(+ ,ne1 ,ne2))

(define (make-lambda x b)
  `(lambda (,x) ,b))

(define (make-app rator rand)
  `(,rator ,rand))

;; With these latter ones, we don't need to use (can no longer)
;; fancier operations w/Racket's pattern matching. Truth be told, we
;; were cheating all along by using Racket's special operations.

(define (make-sym s)
  `(symbol ,s))

(define (make-num n)
  `(number ,n))

;; These are the constructors that we ontensibly used to *build* our
;; programs, rather than constructing their (internal) syntax
;; directly.

;; But further---from what higher-order function representation did we
;; defunctionalize them to get these data structure representations?
;; We can follow the pattern and the usual clues. What did we do
;; before to construct the match clauses of our `apply` functions? We
;; took the bodies of the higher-order functions and the parameter
;; lists, constructed data structures in those constructors, and then
;; matched against them in the `apply` version.

(define (make-plus ne1 ne2)
  (lambda (env)
    (+ (valof ne1 env) (valof ne2 env))))

(define (make-lambda x b)
  (lambda (env)
    (make-closure x b env)))

(define (make-app rator rand)
  (lambda (env)
    (apply-closure (valof rator env) (valof rand env))))

(define (make-sym s)
  (lambda (env)
    (apply-env env y)))

(define (make-num n)
  (lambda (env)
    n))

(define (valof exp env)
  (exp env))

;; We don't normally begin writing or expressing intepreters in this
;; style. But why not? If it's so critical to first expose students to
;; the denotations of environments and closures, then why isn't it
;; equally important to expose them first to the denotations of
;; expressions?

;; Further, if I wanted to express to a code-reading student the
;; importance of compositionality, I would ask that student to
;; implement a new form for my language in this setting. I think that
;; underlines it especially well.

;; This means, though, that we should have a
;; representation-/de/pendent, *functional* version of this same
;; structure. What should *that* be? What kind of thing is that?

;; /That/ is the denotation of a program:

(lambda (env)
  (apply-closure
   (valof 
    (lambda (env)
      (apply-closure
       (valof (lambda (env)
                (make-closure 'x
                              (lambda (env)
                                (make-closure 'y (make-symbol 'x) env))
                              env))
              env)
       (valof (lambda (env) '5) env)))
    env)
   (valof (lambda (env) '6) env)))
;; (This is what *I* call functional programming :-)

;; It was a little bothersome that eval ended up being so much bigger
;; than apply. We have many more forms of syntax than we have kinds of
;; closures to apply. But this also relieves that fundamental
;; disparity.

;; With this version, both eval and apply are simple, one-line
;; functions. They are in fact syntactically /identical/ functions,
;; and they look more like true mirror images or a Yin-Yang pair than
;; they had defunctionalized.

(lambda (c a)
  (c a))

(lambda (expr env)
  (expr env))

;; Here, an expression is truly a function from environments to values

;; The same way a closure is truly a function from values to values

;; A lambda expression is not a closure. A lambda is a function that
;; first takes an environment. 

;; When we say "closures are values", this denotationally means that
;; functions from values to values are *themselves* values.

;; Further I think this gives one *more* view on how small-but-big is
;; the mistake of implementing dynamic scope instead of lexical
;; scope.

;; There is still a really interesting story to tell here wrt
;; continuations
