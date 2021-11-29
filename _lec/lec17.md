---
author: Jason Hemann
title: Macros
date: 2021-11-10
---

An exploration of macros, and some debugging tips from them.

# Macro expansion time is different than run-time. 

```
;; (define (my-thunkify x)
;;   (lambda () x))

(define-syntax thunkify
  (syntax-rules ()
    [(_ e) (lambda () e)]))
```

# See the special "reserved keywords" list

```
(define-syntax if-t-e
  (syntax-rules (then else)
    [(_ t then conseq else alt)
     (if t conseq alt)]))
#|
(define-syntax if
  (syntax-rules (then else)
    [(_ t then conseq else alt)
     (if t conseq alt)]))
|#
```

# We can loop @ macro expansion time!

```
(trace-define-syntax loop
  (syntax-rules ()
    [(loop e) (loop (list e e))]))

(define-syntax let
  (syntax-rules ()
    [(_ ((x e1) ...) e2)
     ((lambda (x ...) e2) e1 ...)]))
```

## `or` is a macro! 

```
(define-syntax or2
  (syntax-rules ()
    [(_ e1 e2)
     (let ((v e1))
       (if v v e2))]
    [(_ e1 e2 e3 ...) (raise-syntax-error 'or2 "badness")]))

(define-syntax or*
  (syntax-rules ()
    [(_) #f]
    [(_ e1) e1]
    [(_ e1 e2 ...)
     (let ((v e1))
       (if v v (or* e2 ...)))]))
```

### A macro that can help _you_!

```
(define-syntax print-vals
  (syntax-rules ()
    [(_ r ...)
     (begin
       (printf "the register ~s~n has value~n ~s~n" 'r r)
       ...)]))
```
