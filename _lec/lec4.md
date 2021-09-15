---
title: Free, bound, and lexical address
date: 2021-09-15
---

# Questions and Update
  + I renamed some questions on hw to correspond w/these notes. 
  + Brief Homework questions 

# `lambda` calculus expressions as datatype.

```
        x, y ∈ Vars
   b, e2, e2 ∈ E ::= y | (lambda (x) b) | (e₁ e₂)
```

There are three and exactly three forms in *the* λ-calculus. There are
many kinds of λ-calculi, but when we discuss "the" λ-calculus this is
what we'll mean.

# `match` expressions over these 
  
  You can borrow this and keep it in a file somewhere. A large
  majority of our programs will start with something very
  similar. Almost all of the second portion of this assignment.
  
```racket
(define (_ e)
  (match e
    [`,y #:when (symbol? y)  ]
    [`(lambda (,x) ,body)   ]
    [`(,rator ,rand)      ]))
```

## Non-structural matching.

We can't tell the difference, with just pattern-matching, between a
list and a number. Our pattern matching lets us slice up and match
against the shapes of any arbitrary lisp tree. However, that doesn't
help us with symbols and numbers because these are atoms---indivisible
into `car`s and `cdr`s.


## Example

```racket
  (define (expression-depth e)
   (match e
     [`,y #:when (symbol? y) 0]
     [`(lambda (,x) ,body) (add1 (expression-depth body))]
     [`(,rator ,rand) (add1 (max (expression-depth rator) (expression-depth rand)))]))
```

## Try it out: 

```racket
;; Expr -> Listof Symbol
;; Takes an expression and returns a bag (set with duplicates) of 
;; all the formal parameters in the expression.
 (define (bag-of-parameters e)
   (match e
	 [`,y #:when (symbol? y)                      ]

	 [`(lambda (,x) ,body)                        ]

	 [`(,rator ,rand)                           ])))
```

# Dissecting an expression in the λ-calculus

During the execution of a program, variables become associated with
values we computed. We call this association a _binding_. We will use
this as one of several technical terms to describe the meanings of
variables in our programs, and how our programs behave with respect to
variables as they execute. 

In the expressions `(f x y)` and `(λ (a) b)`, `a` has a much different
role than `f`, `x`, `y`, and `b`. The latter are all _variable
references_---this is to say *uses* of the variable.

In the expression above, however, `a` is a _variable declaration_:
this is where we introduce the variable as a name for some value.

Each variable declaration corresponds to some part of a program over
which uses of that variable name refer to the declaration. The part of
the program over which uses refer to that declaration is called the
variable's _scope_. You can see this with DrRacket's green arrows.

For us, a variable *reference* is a technical term. Notice that we can
reach each variable *reference* alone by itself via structural
recursion on the datatype. The _binding site_ `(x)` in `(λ (x) b)` is
not a recursive position in our datatype.


|-------------------------|--------|----|--------------------------|---------|
| Is there a reference to |        | in |                          | ? (Y/N) |
|-------------------------|--------|----|--------------------------|---------|
|                         | z      |    | (lambda (z) x)           |         |
|                         | x      |    | x                        |         |
|                         | y      |    | (lambda (x) y)           |         |
|                         | z      |    | (lambda (z) (x y)        |         |
|                         | lambda |    | (lambda (lambda) lambda) |         |
|-------------------------|--------|----|--------------------------|---------|

#  Free variable references, bound variable references 

  A _free variable reference_ is a reference to a variable outside the
  scope of any declaration of that variable. A free reference to a
  variable x _occurs_ in an expression e when there is a free variable
  reference to x in e. 

  A _bound variable reference_ is a reference to a variable inside the
  scope of a declaration of that variable. A free reference to a
  variable x _occurs_ in an expression e when there is a bound
  variable reference to x in e.

### Shadowing

When we declare a variable inside the scope of an existing variable
declaration and the inner and outer variable have the same name, we
say that the inner declaration _shadows_ the outer declaration. This
is a hole in the scope of the outer declaration

```racket
(λ (x) ;; outer
  ...
  (λ (x) ;; inner
   ...
    x))
```

We say that because inside the scope of that inner declaration, any
references to variable x are associated with the inner declaration.
Inside the scope of the inner "x", there is *no* way to refer to the
outer "x"; it's been eclipsed by the inner declaration.

<img src="{{ site.baseurl }}/assets/images/shadow-analogy.png">

We know every variable's scope /statically/. This means that without
running the program, we can determine the referent (declaration) of
any particular variable reference. To do so, we start from the
variable reference `x` and proceed outward (structurally) until
reaching the nearest encompassing declaration of `x`. The field now
considers this the correct approach.

λ is a /binder/. A binder is an operator that ties a variable's
declaration with its scope. 

|--------|-------|--------------|---|----------|--------------------|---------|
| Does a |       | reference to |   | occur in |                    | ? (Y/N) |
|--------|-------|--------------|---|----------|--------------------|---------|
|        |       |              |   |          |                    |         |
|        | free  |              | x |          | x                  |         |
|        | free  |              | y |          | x                  |         |
|        | bound |              | x |          | x                  |         |
|        | bound |              | z |          | (lambda (z) x)     |         |
|        | bound |              | y |          | (lambda (x) y)     |         |
|        | bound |              | z |          | (lambda (z) x)     |         |
|        | bound |              | y |          | (lambda (y) y)     |         |
|        | free  |              | x |          | ((lambda (x) x) x) |         |
|        | bound |              | x |          | ((lambda (x) x) x) |         |
|        | free  |              | z |          | ((lambda (x) x) x) |         |
|        | bound |              | z |          | ((lambda (x) x) x) |         |
|--------|-------|--------------|---|----------|--------------------|---------|


## Bear in mind we sit on the shoulders of giants. 

Programs where variables had *only* global scope. Where there were no
_blocks_, or regions to limit the scope of a variable. 

Some languages (FORTRAN) had the idea that the first letter of a
variable's name should determine it's type. This meant variables
starting with letters i .. n were Integers, and those starting with
anything else were Reals. FORTRAN 77 you couldn't introduce all the
attributes or information you wanted for a variable (type, dimensions,
etc) in one declaration.

# Lexical address

  It may surprise you to learn that we don't really need variable
  names. There is a sense in which

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
  
  are the same, but different from the program. 
  
```racket
    (lambda (z)
      (lambda (w)
	w))
```
  
  What is that sense? How do we describe it? 
  
  We call them *α-equivalent*, and when we write these expressions
  using numbers rather than variable names, that they have their
  corresponding variable references have the same *lexical*
  *addresses* (or *DeBruijn* *indices*)
  
  
  
