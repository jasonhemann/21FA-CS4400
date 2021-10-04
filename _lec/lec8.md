---
title: Equational reasoning and higher order functions
date: 2021-10-04
---

# A couple of notes.

## Website: Changed

CCS Github is a jerkface.

## Gradescope Grading Scale: Updated phrasing

Now phrases should better reflect what you're seeing.

## Canvas Grading Scale

Now 0-9.

## Racket's `match`

[Cf.](https://www.python.org/dev/peps/pep-0636/ "Python match")

## 



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

