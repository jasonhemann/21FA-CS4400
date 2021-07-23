---
title: Naturally Recursive Functions
date: 2021-01-25
---
# All the Racket you need to know x2

## Again, it's important to write this *style* of programs

### `length`


### `count8`

```racket
(define (count8 ls)
  (cond
    ((null? ls) 0)
    ((eqv? (car ls) 8) (add1 (count8 (cdr ls))))
    (else (count8 (cdr ls)))))
```

### `count8*`

```racket
(define (count8* ls)
  (cond
    ((null? ls) 0)
    ;; this is our test for listitude 
    ;; we have a list, and it's car is a list
    ((pair? (car ls)) (+ (count8* (car ls)) (count8* (cdr ls))))
    ((eqv? (car ls) 8) (add1 (count8* (cdr ls))))
    (else (count8* (cdr ls)))))
```

	```racket
(count8* '(4 (8 (5 (((8)) 7))) (3 8)))
3
```

# Arithmetic examples.


## `plus`

## `times`

## `expt` 

## Generalizing 

### This is not news. 

  - Wilhelm Ackermann
  - Gabriel Sudan

```racket
(define (phi p m n)
  (cond
    [(zero? p) (+ m n)]
    [(and (zero? n) (zero? (sub1 p))) 0]
    [(and (zero? n) (zero? (sub1 (sub1 p)))) 1]
    [(zero? n) m]
    [else (phi (sub1 p) m (phi p m (sub1 n)))]))
```

> After Ackermann's publication of his function (which had three
> nonnegative integer arguments), many authors modified it to suit
> various purposes, so that today "the Ackermann function" may refer to
> any of numerous variants of the original function. One common version,
> the two-argument Ackermann–Péter function, is defined as follows for
> nonnegative integers m and n:
>
> -- wiki


```racket
(define (ack-peter m n)
  (cond
    [(zero? m) (add1 n)]
    [(zero? n) (ack-peter (sub1 m) 1)]
    [else (ack-peter (sub1 m) (ack-peter m (sub1 n)))]))
```

