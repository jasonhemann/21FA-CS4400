---
author: Jason Hemann
title: CPSing an interpreter
date: 2021-10-25
---

# Today topics

 - Recap CPSing
 - Complex examples
 --- serious `cond` questions
 --- applying anonymous lambdas 
 - the interpreter
 -- Tricky forms
 --- `if`
 --- `let` 
 --- explicit sequencing

## PITCH

```
Properly
Implemented
Tail
Call 
Handling
```


# Questions, so far?

## Homework

-   First part of your final project coming out soon.

# Recap continuation-passing Style, and implement CPSing an interpreter. 

For a nice resource walking you through some complex examples, please
see [the following](
https://www.cs.bgu.ac.il/~ppl202/wiki.files/class/notebook/4.2CPS.html)
(hat tip to a 4400 student). 


## Two kinds of forms: CPSing as a function definition, vs CPSing as an expression. 

Why? Because `define`, basically. We aren't threading the definition of functions into one control flow.


# Special/tricky cases

# Basic examples

 ``` {racket}
(define a-particular-extended-env
  (lambda (y)
    (if (eqv? 'p y)
        12
    (apply-env (lambda (y) (error "badness" y)) y))))

(define apply-env
  (lambda (env y)
    (env y)))
```

### cond

There's a trick to `cond`.

```racket
(define g-the-fs 
  (lambda (f g ls)
    (cond
	  ((empty? ls) '())
	  ((f (car ls)) (cons (g (car ls)) (g-the-fs f g (cdr ls))))
	  (else (cons (car ls) (g-the-fs f g (cdr ls)))))))
``` 

### let

A special shortcut to make `let` easier.


# CPSing an interpreter {#cpsing-an-interpreter-1}

``` {racket}
(define value-of
  (lambda (expr env)
    (pmatch expr
      [,c #:when (or (boolean? c) (number? c)) c]
      [(* ,ne1 ,ne2) (* (value-of ne1 env) (value-of ne2 env))]
      [(sub1 ,ne) (sub1 (value-of ne env))]
      [(if ,test ,conseq ,alt) (if (value-of test env)
                                   (value-of conseq env)
                                   (value-of alt env))]
      [(let ((,x ,e)) ,body) (let ((a (value-of e env)))
                               (value-of body (lambda (y) (if (eqv? x y) a (env y)))))]


      [,y #:when (symbol? y) (env y)]
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
