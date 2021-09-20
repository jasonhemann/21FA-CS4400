---
title: (Lexical vs) Dynamic Scope
date: 2021-09-29
---


# Questions

-   Homework - coming due Wednesday.
-   I deputize you to explain and to help others.

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
    [`(let ([,x ,e]) ,body) (let ([a (valof e env)])
			      (valof body (extend-env x a env)))]
    [`(λ (,x) ,body) `(lambda (,x) ,body)]
    [`(,rator ,rand) (match-let ([`(lambda (,x) ,body) (valof rator env)]
				 [a (valof rand env)])
		       (valof body (extend-env x a env)))]))

(define (empty-env)
  (λ (y) 
    (error 'empty-env "unbound identifier ~s\n" y)))
```

## What happens as a consequence?

## Syntax vs. Semantics

# Examples

# Is this a real thing? (sitting example)

