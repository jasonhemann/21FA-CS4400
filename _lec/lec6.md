---
title: Universality of lambda calculus, alpha, beta, and eta.
date: 2021-02-08
---

# Questions 
  + Brief Homework questions 

# Review 

## *α-equivalence*

Remember, we said that, in a particular sense

```racket
    (lambda (x)
      (lambda (y)
	x))
```

  and 

```racket
    (lambda (p)
      (lambda (q)
	p))
```
  
  "the same", but different from the program:
  
```racket
    (lambda (z)
      (lambda (w)
	w))
```
  
  We called that sense *α-equivalence*.

## β, η

# Universality of the `lambda` calculus

-   `cons`, `car`, `cdr`
-   Booleans, `if`

## Church numerals

The λ calculus can be used to define a representation of natural
numbers, called Church numerals, and arithmetic over them. For instance,
`c5` is the definition of the Church numeral for 5.

```racket
> (define c0 (lambda (f) (lambda (x) x)))
> (define c5 (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
> ((c5 add1) 0)
5
> ((c0 add1) 0)
0
```

The following is a definition for Church plus, which performs addition
over Church numerals.

```racket
> (define c+ (lambda (m) 
       (lambda (n) 
         (lambda (a) (lambda (b) ((m a) ((n a) b)))))))
> (let ((c10 ((c+ c5) c5)))
    ((c10 add1) 0))
10
```

One way to understand the definition of `c+` is that it, when provided
two Church numerals, returns a function that, when provided a meaning
for `add1` and a meaning for zero, uses provides to `m` the meaning for
`add1` and, instead of the meaning for zero, provides it the meaning for
its second argument. `m` is the sort of thing that will count up *m*
times, so the result is the meaning of *m* *+* *n*.

(For fun implement `csub1`, Church predecessor. The following tests
should pass.)

```racket
> (((csub1 c5) add1) 0)
4
> (((csub1 c0) add1) 0)
0
```

In the second case, the Church predecessor of Church zero is zero, as we
haven\'t a notion of negative numbers.

## `Ω`, `Y` and recursion


# Recursion in the `lambda` calculus

```racket
((lambda (x) (x x))
 (lambda (x) (x x)))
```

-   What? Huh

## The objective: recursion

Pick a simple function we\'d like to write, something like factorial.
It\'s *pretty* close. We have a free variable in there, `!`.

```racket
(lambda (n)
  (if (zero? n) 1
  (* n (! (sub1 n)))))
```

We have one, and only one, way bind a free variable - `lambda`.

```racket
(lambda (!)
  (lambda (n)
(if (zero? n) 1
    (* n (! (sub1 n))))))
```

Now we have a working definition! (provided we already have a definition
of `!`. Not [exactly]{.ul} useful.)

```racket
((lambda (!)
   (lambda (n)
 (if (zero? n) 1
     (* n (! (sub1 n))))))
 the-real-working-factorial)
```

And of course, if *that* blob is a real working factorial, then we could
pass it in to our \"almost factorial\", and that too, would work.

```racket
((lambda (!)
   (lambda (n)
 (if (zero? n) 1
     (* n (! (sub1 n))))))
 ((lambda (!)
(lambda (n)
  (if (zero? n) 1
      (* n (! (sub1 n))))))
  the-real-working-factorial))
```

It would be pretty slick to be able to pass this \"almost factorial\"
into itself.

```racket
((lambda (!) (! !))
 (lambda (!)
   (lambda (n)
 (if (zero? n) 1
     (* n ((! !) (sub1 n)))))))
```

We can do that. One additional change, the recursion now needs to take
`!` into itself.

``` {.example}
> (((lambda (!) (! !))
    (lambda (!)
  (lambda (n)
    (if (zero? n) 1
        (* n ((! !) (sub1 n)))))))
   5)
120
```

Voila!

## Abstracting it out.

There\'s two things going on here:

1.  The \"business logic\" of the actual function we\'re writing
2.  The specialty that\'s giving us recursion.

It\'s a hop, skip, and a jump to separating out the recursion-y part.
This is the call-by-value Y combinator.

```racket
(lambda (f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (y) ((x x) y))))))
```

To write a recursive function in our language use the following recipe:

1.  Get rid of the `define`, and turn it into an \"almost-\" function.
2.  Pass that function as an argument to the CBV Y-combinator.

```
> (((lambda (f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (y) ((x x) y))))))
    (lambda (!)
  (lambda (n)
    (if (zero? n)
        1
        (* n (! (sub1 n)))))))
   5)
120
```
