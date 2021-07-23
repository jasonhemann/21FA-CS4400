---
author: Jason Hemann
title: Continuation-passing style
date: 2021-03-03
---

# Questions

## Exams

## Grades

-   Grades calculations
-   Most grades in

## Homework

-   Homework out this evening
-   Excited to get back into it
-   First part of your final project coming soon!

# Continuation-passing Style {#continuation-passing-style-1}

## Simple vs Serious

-   We decide
-   Serious:

## How to CPS expressions:

### Simple expressions

### Serious expressions

### Tail position

### Non-tail position

### Caution about tail position

## Example

``` {racket}
(define !
  (lambda (n)
    (if (zero? n)
    1
    (* n (! (sub1 n))))))
```

## Special/tricky cases

### let

### cond

## You try

``` {racket}
(define extend-env
  (lambda (x a env)
    (lambda (y)
  (if (eqv? x y)
      a
      (apply-env env y)))))

(define apply-env
  (lambda (env y)
    (env y)))
```
