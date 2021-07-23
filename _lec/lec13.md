---
author: Jason Hemann
title: CPSing an interpreter
date: 2021-03-08
---

# Questions, so far?

## Homework

-   First part of your final project coming tonight.

# Recap continuation-passing Style

## You try

``` {racket}
(define a-particular-extend-env
  (lambda (y)
    (if (eqv? 'p y)
        12
    (apply-env (lambda (y) (error "badness" y)) y))))

(define apply-env
  (lambda (env y)
    (env y)))
```

## Special/tricky cases

### let

A special shortcut to make `let` easier.

### cond

There\'s a trick to `cond`.

# CPSing an interpreter {#cpsing-an-interpreter-1}

``` {racket}
(define value-of
  (lambda (expr env)
    (pmatch expr
      [,c (guard (or (boolean? c) (number? c))) c]
      [(* ,ne1 ,ne2) (* (value-of ne1 env) (value-of ne2 env))]
      [(sub1 ,ne) (sub1 (value-of ne env))]
      [(if ,test ,conseq ,alt) (if (value-of test env)
                                   (value-of conseq env)
                                   (value-of alt env))]
      [(let ((,x ,e)) ,body) (let ((a (value-of e env)))
                               (value-of body (lambda (y) (if (eqv? x y) a (env y)))))]


      [,y (guard (symbol? y)) (env y)]
      [(lambda (,x) ,body) (lambda (a) (value-of body (lambda (y) (if (eqv? x y) a (env y)))))]
      [(,rator ,rand) ((value-of rator env) (value-of rand env))])))
```

# Let\'s see how far we get here.

If we make it through this, then we get to do some **really** cool
stuff(!)

# call/cc

# let/cc & throw

# implementing let/cc & throw in terms of let/cc

# implementing them in the CPSed interpreter.

# What they do, how to do things with them
