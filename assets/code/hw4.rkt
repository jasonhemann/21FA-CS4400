#lang racket
(require rackunit)

#| RI Closures and Dynamic Scope |# 

;; On two occasions I have been asked [by members of
;; Parliament],--"Pray, Mr. Babbage, if you put into the machine wrong
;; figures, will the right answers come out?"

;; – Charles Babbage, 1864
 
#| Assignment Guidelines |#

;; To begin with, copy your correctly-implemented value-of-fn
;; interpreter and its three environment helpers (extend-env-fn,
;; apply-env-fn, empty-env-fn) over from last assignment. If you did
;; not get this correct last week, please see a TA and get help on
;; this before you go further.

;; In this file, rename that interpreter value-of, rename its
;; environment helpers extend-env, apply-env, and empty-env,
;; respectively.

;; Copy your correctly-implemented lex over from the second
;; assignment. If you do not yet have this correct please seek
;; guidance from a TA. This is a second chance at a whack at that
;; earlier problem.

;; The main goal of this assignment is to implement dynamic scope; we
;; are weighting that part of the assignment more heavily.

#| Regression Testing and Enhancing |# 


#| 

1. The following regression tests from your last assignment should
still work on your re-copied lex and your newly-renamed value-of and
its associated environment help functions.

|# 

  (check-equal?
   (apply-env (extend-env 'x 5 (empty-env)) 'x)
   5)
  (check-equal? 
   (apply-env (extend-env 'x 7 (extend-env 'x 5 (empty-env))) 'x)
   7)
  (check-equal?
   (apply-env (extend-env 'x 7 (extend-env 'y 5 (empty-env))) 'y)
   5)

  (check-equal?
   (value-of
    '((lambda (x) (if (zero? x)
                      #t
                      #f))
      0)
    (empty-env))
   #t)   
  (check-equal?
   (value-of 
    '((lambda (x) (if (zero? x) 
                      12 
                      47)) 
      0) 
    (empty-env))
   12)    
  (check-equal?
   (value-of
    '(let ([y (* 3 4)])
       ((lambda (x) (* x y)) (sub1 6)))
    (empty-env))
   60)
  (check-equal?
   (value-of
    '(let ([x (* 2 3)])
       (let ([y (sub1 x)])
         (* x y)))
    (empty-env))
   30)
  (check-equal?
   (value-of
    '(let ([x (* 2 3)])
       (let ([x (sub1 x)])
         (* x x)))
    (empty-env))
   25)
  (check-equal?
   (value-of
    '(((lambda (f)
         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
       (lambda (f)
         (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
      5)
    (empty-env))
   120)

  (check-equal?
   (lex '(lambda (x) x) '())
   '(lambda (var 0)))
  (check-equal?
   (lex '(lambda (y) (lambda (x) y)) '())
   '(lambda (lambda (var 1))))
  (check-equal?
   (lex '(lambda (y) (lambda (x) (x y))) '())
   '(lambda (lambda ((var 0) (var 1)))))
  (check-equal?
   (lex '(lambda (x) (lambda (x) (x x))) '())
   '(lambda (lambda ((var 0) (var 0)))))
  (check-equal?
   (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
   '(lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1)))))))
  (check-equal?
   (lex '(lambda (a)
           (lambda (b)
             (lambda (c)
               (lambda (a)
                 (lambda (b)
                   (lambda (d)
                     (lambda (a)
                       (lambda (e)
                         (((((a b) c) d) e) a))))))))) '())
   '(lambda
      (lambda
        (lambda
          (lambda
            (lambda
              (lambda
                (lambda
                  (lambda
                    ((((((var 1) (var 3)) (var 5)) (var 2)) (var 0)) (var 1)))))))))))
  (check-equal?
   (lex '(lambda (a)
           (lambda (b)
             (lambda (c)
               (lambda (w)
                 (lambda (x)
                   (lambda (y)
                     ((lambda (a)
                        (lambda (b)
                          (lambda (c)
                            (((((a b) c) w) x) y))))
                      (lambda (w)
                        (lambda (x)
                          (lambda (y)
                            (((((a b) c) w) x) y))))))))))) '())
   '(lambda 
      (lambda 
        (lambda 
          (lambda 
            (lambda 
              (lambda 
                ((lambda
                   (lambda
                     (lambda
                       ((((((var 2) (var 1)) (var 0)) (var 5)) (var 4)) (var 3)))))
                 (lambda
                   (lambda
                     (lambda
                       ((((((var 8) (var 7)) (var 6)) (var 2)) (var 1)) (var 0)))))))))))))


#| 

2. You previously implemented lex to handle variables, application,
and lambda-abstraction forms. Extend your previous definition of
lex so that it can handle not only those forms, but also numbers,
zero?, sub1, *, if, and let. This should be a
straightforward extension (let should be the only line that requires
any real effort), but it also serves as a chance to improve a
misbehaving lex from an earlier assignment. In order to disambiguate
numbers from lexical addresses, you should transform a number n into
(const n).

|# 


#| Representation Independence wrt Closures |# 

#| 

3. Create a version of your interpreter from the first part of this
assignment that is representation-independent with respect to closures
and uses a higher-order function representation named value-of-fn.
Name your two new closure helpers apply-closure-fn and
make-closure-fn.

|# 


#| 

4. Create a version of this interpreter named value-of-ds that is
representation-independent with respect to closures and uses a
tagged-list data structure representation. Name your two new closure
helpers apply-closure-ds and make-closure-ds.

Other than the cosmetic change of -fn to -ds, you shouldn't have to
change the implementations of valof. This is once again a good thing!

This is evidence we have a tight, well-defined interface for
closures. We've been able to, on the "behind the
interface"/implementation side of things, radically change how we're
implementing closures, and we haven't had to change the client
code. That's evidence we've done a good job designing our interface.


|#




#| Dynamic Scope |# 

#| 

The second part of this week's assignment is to create an
interpreter that uses dynamic scope.

So far, we have implemented our interpreters so that, if there are
free variables references in a procedure, they take their values from
the environment in which the lambda expression is defined. We
accomplish this by creating a closure for each procedure we see, and
we save the environment in the closure. We call this technique static
binding of variables, or static scope. Lexical scope is a kind of
static scope.

Alternatively, we could implement our interpreters such that any free
variable references in the body of a procedure get their values from
the environment from which the procedure is called, rather than from
the environment in which the procedure is defined.

For example, consider what would happen if we were to evaluate the
following expression in an interpreter that used lexical scope:

(let ([x 2])
  (let ([f (lambda (e) x)])
    (let ([x 5])
      (f 0))))

Our lexical interpreter would add x to the environment with a
value of 2. For f, it would create a closure that contained the
binding of x to 2, and it would add f to the environment with
that closure as its value. Finally, the inner let would add x to
the environment with a value of 5. Then the call (f 0) would be
evaluated, but since it would use the value of x that was saved
in the closure (which was 2) rather than the value of x that was
current at the time f was called (which was 5), the entire
expression would evaluate to 2.

Under dynamic scope, we wouldn't save the value of x in the
closure for f. Instead, the application (f 0) would use the
value of x that was current in the environment at the time it was
called, so the entire expression would evaluate to 5.

|# 

#| 

4. Define value-of-dynamic, an interpreter that implements dynamic
scope. You should be able to share use your environment helpers from
the first part of this assignment, but you should not implement an
abstraction for closures in this interpreter. This will largely
correspond to the dynamically-scoped interpreter we wrote in class
that passes an additional parameter, "env^". In your implementation,
you should shadow the name of the existing env. You'll find then, that
when you go to evaluate an application, there's only one environment
in which you can evaluate the body. This is a pretty simple change. To
liven things up a little (and also to allow us a more interesting test
case), this interpreter should also implement let, if, *, sub1, null?,
zero?, cons, car, cdr, and quote. When evaluating the expression (cons
1 (cons 2 '())) value-of-dynamic should return (1 2). Now quote is a
bit of a tricky beast. So here's the quote line for the interpreter.

[(quote ,v) v]

|# 



#| Brainteasers |# 

#| 5. What program defunctionalizes to our valof? So far we fully
defunctionalized environments and closures. Rather than explicit
higher-order function calls, we instead dispatch against the choices,
where we have represented each choice with a corresponding data
structure. Consider our apply-env-ds for example. This is to say, the
match expressions always come out as the result of defunctionalizing
some higher-order functional program. Well, almost always. aWe began
our story with one match expression already: in valof. We didn't start
by defunctionalizing higher-order functions to get here. But morally,
we should have. What did we defunctionalize in order to create these
first-order data structures of our expression language?

Really, we should never have been constructing programs willy-nilly.
Instead our actual programs should be the result of invoking
constructors. Since programs are data, we ought to have a
well-structured datatype and constructors for creating elements of
this datatype.

(define (make-plus ne1 ne2)
  `(+ ,ne1 ,ne2))

(define (make-lambda x b)
  `(lambda (,x) ,b))

(define (make-app rator rand)
  `(,rator ,rand))

With these latter ones, we don't need to use (can no longer)
fancier operations w/Racket's pattern matching. Truth be told, we
were cheating all along by using Racket's special operations.

(define (make-sym s)
  `(symbol ,s))

(define (make-num n)
  `(number ,n))

These are the constructors that we ontensibly used to *build* our
programs, rather than constructing their (internal) syntax
directly.

But further---from what higher-order function representation did we
defunctionalize _these_ _constructors_ to get these data structure
representations? We can follow the pattern and the usual clues. What
did we do before to construct the match clauses of our `apply`
functions? We took the bodies of the higher-order functions and the
parameter lists, constructed data structures in those constructors,
and then matched against them in the `apply` version. Here are the
answers.

|#

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

#|

This means, though, that we should have a representation-/de/pendent,
*functional* version of this same structure. What should *that* be?
What kind of thing is that? Here's an example. We didn't
defunctionalize or representation-*de*pendent closures. Because these
are orthogonal choices.

|#

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

#|


It's a program. As expression is a certain kind of function from
environment to value.

The same way a closure is truly a function from values to values,
here, an expression is truly a function from environments to values

A lambda expression is not a closure. A lambda is a function that
first takes an environment. 

When we say "closures are values", this denotationally means that
functions from values to values are *themselves* values.

It was always a little bothersome that eval ended up being so much
bigger than apply. We have many more forms of syntax than we have
kinds of closures to apply, so what could we do?

All that said, your task is to define valof. It should be short and
sweet, like our functional apply-closure.

|#






#| Just Dessert |#

#| 

6.

We have implemented all our interpreters as accumulator-passing
functions. Whether we use data structures or functions the environment
is accumulating the meanings of the declared variables.

What is the direct "natural recursion" style of implementation of such
a function? One answer is to instead use direct substitution and treat
evaluation as a set of program rewrite rules

For example:


((lambda (x) (z x)) (lambda (y) y))

rewrites to

(z (lambda (y) y))

More formally, these grammars define our expressions and values

e ::= x (variables)
    | n (natural numbers 0, 1, ...)
    | b (booleans #t, #f)
    | (if e e e)
    | (zero? e)
    | (sub1 e)
    | (* e e)
    | (lambda (x) e)
    | (e e)

values
v ::= x
    | n
    | b
    | (lambda (x) v)

We define our main rewriting rule, beta-n, over this syntax.

((lambda (x) e1) e2) beta-n e1[e2/x]

This relation tells us that when we have an expression of the form:

((lambda (x) e1) e2)

We can rewrite it to the expression e1 where e2 has been substituted
for x. This substitution is not trivial, however, since it must not
change the scope of variables involved in the substitution. So, we
define e1[e2/x] as:

               x1[e/x1] = e
               x2[e/x1] = x2 if x1 is not x2
(lambda (x1) e1)[e2/x1] = (lambda (x1) e1)
(lambda (x1) e1)[e2/x2] = (lambda (x3) e1[x3/x1][e2/x2])
                          if x1 is not x2,
                             x2 is not x3,
                             x3 is not a free variable reference in (lambda (x1) e1)
                             and x3 is not a free variable reference in e2
          (e1 e2)[e3/x] = (e1[e3/x] e2[e3/x])
                 n[e/x] = n if n is a constant
    (if e1 e2 e3)[e4/x] = (if e1[e4/x] e2[e4/x] e3[e4/x])
       (zero? e1)[e2/x] = (zero? e1[e2/x])
                  and so on  ...

There are other rewriting rules for constants as well as for deciding
where in a nested expression to apply the next rewriting rule; we
include these in the provided starter code. 

If we rewrite a term until no more rewriting rules apply, we have
found a term's //normal form//, or we've encountered an error. This
process is called //normalization//. 

;; Example 1
   ((lambda (x) (+ x 3)) 5)
-> (+ 5 3)
-> 8 ;; normal form


;; Example 2
   ((lambda (x) (3 x)) 5)
-> (3 5) ;; error

You can use (gensym 'x) to get a new unique symbol. 

Complete the following definitions to implement normalization for
lambda. When you complete this problem you can reduce expressions in
this language to normal form. We make no use of an environment when
doing so. Also write tests for your code.

|#

(define (subst e^ x e)
  (match e
    [`,n #:when (number? n) n]
    [`,b #:when (boolean? b) b]
    [`(if ,e1 ,e2 ,e3)
     (let ([e1^ (subst e^ x e1)]
           [e2^ (subst e^ x e2)]
           [e3^ (subst e^ x e3)])
       `(if ,e1^ ,e2^ ,e3^))]
    [`(zero? ,e1)
     (let ([e1^ (subst e^ x e1)])
       `(zero? ,e1^))]
    [`(sub1 ,e1)
     (let ([e1^ (subst e^ x e1)])
       `(sub1 ,e1^))]
    [`(* ,e1 ,e2)
     (let ([e1^ (subst e^ x e1)]
           [e2^ (subst e^ x e2)])
       `(* ,e1^ ,e2^))]
    ;; Finish me!
    [`(,e1 ,e2)
     (let ([e1^ (subst e^ x e1)]
           [e2^ (subst e^ x e2)])
       `(,e1^ ,e2^))]))

(define (value? exp)
  (match exp
    [`,x #:when (symbol? x) #t]
    [`,n #:when (number? n) #t]
    [`,b #:when (boolean? b) #t]
    ;; Finish me!
    [else #f]))

(define (norm exp)
  (match exp
    [`,v #:when (value? v) v]
    [`(if #t ,conseq ,altern)
     (norm conseq)]
    [`(if #f ,conseq ,altern)
     (norm altern)]
    [`(if ,test ,conseq, altern)
     (let ([test^ (norm test)])
       (norm `(if ,test^ ,conseq ,altern)))]
    [`(zero? 0) #t]
    [`(zero? ,e)
     (let ([e^ (norm e)])
       (norm `(zero? ,e^)))]
    [`(sub1 ,n) #:when (number? n)
     (sub1 n)]
    [`(sub1 ,e)
     (let ([e^ (norm e)])
       (norm `(sub1 ,e^)))]
    [`(* ,n1 ,n2)
     #:when (and (number? n1) (number? n2))
     (* n1 n2)]
    [`(* ,n1 ,e)
     #:when (number? n1)
     (let ([e^ (norm e)])
       (norm `(* ,n1 ,e^)))]
    [`(* ,e1 ,e2)
     (let ([e1^ (norm e1)])
       (norm `(* ,e1^ e2)))]
    [`(lambda (,x) ,e)
     (let ([e^ (norm e)])
       (norm `(lambda (,x) ,e^)))]
    [`(,x ,v)
     #:when (and (symbol? x) (value? v))
     `(,x ,v)]
    [`(,x ,e)
     (guard (symbol? x))
     (let ([e^ (norm e)])
       (norm `(,x ,e^)))]
    [`((lambda (,x) ,e1) ,e2)
     ;; Finish me!
     ]
    [`(,e1 ,e2)
     (let ([e1^ (norm e1)])
       (norm `(,e1^ ,e2)))]
    [else (error 'norm "invalid expression ~s" x)]))
