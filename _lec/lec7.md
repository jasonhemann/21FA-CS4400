---
title: (Lexical vs) Dynamic Scope
date: 2021-09-29
---


# Questions

-   Homework - coming due Wednesday.
-   I deputize you to explain and to help others.

# Y Combinator

```
(lambda (x) 
  (lambda (y)
    (lambda (z) 
	  ((((x y) (z w)) ((a b) (p q)))))))
```	  

;; K (lambda (x) (lambda (y) x))
;; S (lambda (x) (lambda (y) (lambda (z) ((x z) (y z)))))

<your-fav-lambda-expression> = (((K K) S) (S (S K)))

(define Y ...) 


;; To get around the lack of define in our PL 
(define !
  (lambda (n)
    (if (zero? n)
	  1
	  (* n (! (sub1 n))))))

;; Turn your 1-arg function created w/define into another lambda binding the name.
;; Then pass that whole deal as an argument to Y.

(Y
  (lambda (!)
	(lambda (n)
	  (if (zero? n)
		1
		(* n (! (sub1 n))))))
	)

((Y
  (lambda (!)
	(lambda (n)
	  (if (zero? n)
		1
		(* n (! (sub1 n))))))
	)
 5)
120

;; To get to Y



;; ([Listof X] -> Nat) -> ([Listof X] -> Nat)
(lambda (len)
  (lambda (ls)
    (if (empty? ls)
       0
       (add1 (len (cdr ls))))))

;; (Nat -> Nat) -> (Nat -> Nat)
((lambda (!)
   (lambda (n)
     (if (zero? n)
	1
	(* n (! (sub1 n))))))
 <some real-honest-to-goodness-factorial-function>)

(lambda (!)
   (lambda (n)
     (if (zero? n)
	1
	(* n ((lambda (n)
                (if (zero? n)
                    1
                    (* n (! (sub1 n)))))
              (sub1 n))))))

   (lambda (n)
     (if (zero? n)
	1
	(* n (! (sub1 n)))))

   (lambda (n)
     (if (zero? n)
	1
	(* n (if (zero? (sub1 n))
                 1
                 (* (sub1 n) (! (sub1 (sub1 n))))))))

;; Omega 
((lambda (x) (x x)) (lambda (x) (x x)))


(lambda (f) ;; f, the "business logic" of the function we want to compute
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))

;; add1 => (λ (n) (add1 n))


(lambda (f) ;; f, the "business logic" of the function we want to compute
  ((lambda (x) (f (λ (y) ((x x) y)))) ;; η (eta) expansion
   (lambda (x) (f (λ (y) ((x x) y))))))

(f (f (f (f 1))))

;; The "call by value Y combinator"

(lambda (f) 
  ((lambda (x) (f (λ (y) ((x x) y))))
   (lambda (x) (f (λ (y) ((x x) y))))))


# History: The bad days

# Dynamic Scope

## How we might have (mis-)implemented our interpreter

```racket
#lang racket

(define (apply-env env y)
  (env y))

(define (extend-env x a env)
  (λ (y) (if (eqv? x y) a (apply-env env y))))

(define (valof exp env)
  (match exp
    [`(* ,n1 ,n2) (* (valof n1 env) (valof n2 env))]
    [`(sub1 ,n1) (sub1 (valof n1 env))]
    [`(zero? ,n1) (zero? (valof n1 env))]
    [`,y #:when (symbol? y) (env y)]
    [`,n #:when (number? n) n]
    [`(if ,n1 ,n2 ,n3) (if (valof n1 env) (valof n2 env) (valof n3 env))]
    [`(let ([,x ,e]) ,body) 
	  (let ([a (valof e env)])
        (valof body (extend-env x a env)))]
    [`(λ (,x) ,body) (lambda (a env^) (valof body (extend-env x a env^)))]
    [`(,rator ,rand) ((valof rator env) (valof rand env) env)]))

(define (empty-env)
  (λ (y) 
    (error 'empty-env "unbound identifier ~s\n" y)))
```

## What happens as a consequence?


### `my` vs. `local` in Perl


## Syntax vs. Semantics

# Examples

# Is this a real thing? (Lurking example)


