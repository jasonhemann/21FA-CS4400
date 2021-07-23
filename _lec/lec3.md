---
title: letrec, quasiquote, and match
date: 2021-01-27
---


# Questions? 
   + Homework, otherwise
## Homework
   + Remember, homework 0 is due Wednesday night @ 10:00pm
   + Homework 1 available that evening 

## Truthiness
## Help functions, etc, accumulator parameters. 

#  let & letrec 
## let 

It can `let` us avoid recomputing expressions

```racket
  (+ (f 50) (f 50) (f 50))

  ;; is equivalent to

  (let ((x (f 50)))
    (+ x x x))
```

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
   
## letrec 

For our purposes, for constructing anonymous recursive or anonymous mutually recursive functions.

### anonymous recursion

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

### anonymous mutual recursion 

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

We don't need define to write recursive functions. 


# Pattern-matching and `match`

## Why pattern matching
   - pay attention! you won't get this anywhere else.
   - often unfriendly to use `car` and `cdr`; ugly to write, ugly to read 
   - pattern matching gives us the same if-then-else structure of a
     cond expression, but uses the "shape" of an expression instead.

```racket
(define (fun x)
  (match x
    (`(,item1 ,item2 ,item3) `(,item1 ,item3))
    (`(,a . ,d) `(,d . ,a))
    (else x)))
```

   - it's like Wheel of Fortune
   - `pair?` vs `(,a . ,d)` 
   - comma for wildcards with superpowers
   - when patterns aren't enough

## A complicated example

```racket
(define (new-fun x)
  (match (fun x)
    (`(,a b ,c) (list c))
    (`(1 ,b 3) (* b b))
    (`,y #:when (symbol? y) (cons y y))
    (`,n #:when (number? n) (* 2 n))))
```

Try it out

```racket
> (new-fun '((b 2) 4))
???
> (new-fun '((2 3) . 1))
???
> (new-fun 'cat)
???
> (new-fun 12)
???
```

# quasiquote and unquote

  `(list 'a 'thing 'that 'is 'built 'with 'all 'but (- 5 4) 'value)`

