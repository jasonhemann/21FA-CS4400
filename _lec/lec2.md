---
title: letrec, quasiquote, and match
date: 2021-09-13
---

# Objectives 

  1. Recapitulate our notions of natural recursion
  1. (Re)learn some basic Racket tools we'll need for this class

# HW Notes

## Truthiness
## Help functions, etc, accumulator parameters. 

# All the Racket you need to know II

## Again, it's important to write this *style* of programs

### `count`

A function we wrote using the style of natural recursion.

```racket
(define (count x ls)
  (cond
    ((empty? ls) 0)
    ((eqv? x (car ls)) (add1 (count x (cdr ls))))
    (else (count x (cdr ls)))))
```

### `count*`

```racket
(define (count* x ls)
  (cond
    ((empty? ls) 0)
    ;; this is our test for listitude 
    ;; we have a list, and it's car is a list
    ((cons? (car ls)) (+ (count* x (car ls)) (count* x (cdr ls))))
    ((eqv? (car ls) 8) (add1 (count* (cdr ls))))
    (else (count* x (cdr ls)))))
```

```racket
> (count* 8 '(4 (8 (5 (((8)) 7))) (3 8)))
3
```

#  let vs letrec 

## let 

It can `let` us avoid recomputing expressions

```racket
  (+ (f 50) (f 50) (f 50))

  ;; a better way is:

  (let ((x (f 50)))
    (+ x x x))
```

### Why do you think these two are the same?

```racket
  (let ((x e))
    b)

  ;; is equivalent to

  ((lambda (x) b) e)
```

```racket
> (let ((! (lambda (n)
      (if (zero? n) 
          1
          (* n (! (sub1 n)))))))
    (! 5))

ERROR: unbound identifier '!'
> 
```
   
## `letrec` 

For our purposes, for constructing anonymous recursive or anonymous mutually recursive functions.

### "anonymous recursion"

```racket
> (letrec 
    ((! (lambda (n)
          (if (zero? n) 
              1
             (* n (! (sub1 n)))))))
    (! 5))
120
>
```

### anonymous *mutual* recursion 

```racket
> (letrec 
    ((e? (lambda (n)
           (if (zero? n) 
               #t
        	   (o? (sub1 n)))))
     (o? (lambda (n)
           (if (zero? n) 
               #f
               (e? (sub1 n))))))
   (e? 5))
#f
```


# Pattern-matching and `match`

## Why pattern matching
   - pay attention! you won't get this anywhere else.
   - often unfriendly to use `car` and `cdr`; ugly to write, ugly to read 
   - Wonderful to explicitly match against cases in your data definitions. 
   - You will see this in [your favorite language](https://www.python.org/dev/peps/pep-0636/) soon!
   - `cond`s same if-then-else structure, but uses the "shape" of an expression instead.


### `sum-tree` 

When you /define/ the datatype, you can /match/ against it's pieces.
shape-wise comparison

# quasiquote and unquote

  `(list 'a 'thing 'that 'is 'built 'with 'all 'but (- 5 4) 'value)`



