---
author: Jason Hemann
title: RI wrt K
date: 2021-03-10
---

# Opening act

## Getting some great questions.

-   What else?

## HW Grades -- still working at.

# Recap

## Let\'s go wild!

CPS me :-)

``` {racket}
(lambda (f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (y) ((x x) y))))))
```

## `call/cc` example program

Complete the following definition of `last-non-zero`, a function which
takes a list of numbers and return the last `cdr` whose `car` is `0`. In
other words, when starting from the right of the list, it should be all
numbers before the first `0` is reached. Your solution should be
naturally recursive, and should not contain any calls to `member`-like
operations, nor should you be reversing the list.

Don\'t CPS anything, don\'t use `empty-k`, etc.

``` {racket}
(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
    ((last-non-zero
       (lambda (ls)
         (cond
           ;; fill in lines here
           ))))
    (last-non-zero ls)))))
```

``` {racket}
> (last-non-zero '(0))
()
> (last-non-zero '(1 2 3 0 4 5))
(4 5)
> (last-non-zero '(1 0 2 3 0 4 5))
(4 5)
> (last-non-zero '(1 2 3 4 5))
(1 2 3 4 5)
>
```

# CPS something.
