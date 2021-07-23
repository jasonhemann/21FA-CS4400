---
title: Free, bound, and lexical address
date: 2021-02-01
---

# Questions 
  + Brief Homework questions 

# `lambda` calculus expressions as datatype.

```
        x, y ∈ Vars
   b, e2, e2 ∈ E ::= y | (lambda (x) b) | (e₁ e₂)
```

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

## Example

```racket
  (define (expression-depth e)
   (match e
     [`,y #:when (symbol? y) 0]
     [`(lambda (,x) ,body) (add1 (expression-depth body))]
     [`(,rator ,rand) (add1 (max (expression-depth rator) (expression-depth rand)))]))
```

## Try it out

```racket
 (define (_ e)
(match e
  [`,y #:when (symbol? y)                       ]

  [`(lambda (,x) ,body)                        ]

  [`(,rator ,rand)                           ])))
```

# Variable occurrences

 For us, to say a variable *occurs* is a technical term. An
 *occurrence* of a variable is something we can reach via recursion.

 A variable *occurs* in an expression when we can reach that variable
 by structural recursion on that expression.

    |------+--------+----------+--------------------------+---------|
    | Does |        | occur in |                          | ? (Y/N) |
    |------+--------+----------+--------------------------+---------|
    |      | z      |          | (lambda (z) x)           |         |
    |      | x      |          | x                        |         |
    |      | y      |          | (lambda (x) y)           |         |
    |      | z      |          | (lambda (z) (x y)        |         |
    |      | lambda |          | (lambda (lambda) lambda) |         |
    |------+--------+----------+--------------------------+---------|

#  Free variable occurrences, bound variable occurrences 

  A variable *occurs* *free* in an expression when an occurrence of
  that variable in the expression is outside the *scope* of any
  declaration of that variable.

  A variable *occurs* *bound* in an expression when an occurrence of
  that variable in the expression is inside the *scope* of a
  declaration of that variable.

    |------+---+-------+-------+----+--------------------+---------|
    | Does |   | occur |       | in |                    | ? (Y/N) |
    |------+---+-------+-------+----+--------------------+---------|
    |      |   |       |       |    |                    |         |
    |      | x |       | free  |    | x                  |         |
    |      | y |       | free  |    | x                  |         |
    |      | x |       | bound |    | x                  |         |
    |      | z |       | bound |    | (lambda (z) x)     |         |
    |      | y |       | bound |    | (lambda (x) y)     |         |
    |      | z |       | bound |    | (lambda (z) x)     |         |
    |      | y |       | bound |    | (lambda (y) y)     |         |
    |      | x |       | free  |    | ((lambda (x) x) x) |         |
    |      | x |       | bound |    | ((lambda (x) x) x) |         |
    |      | z |       | free  |    | ((lambda (x) x) x) |         |
    |      | z |       | bound |    | ((lambda (x) x) x) |         |
    |------+---+-------+-------+----+--------------------+---------|

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
  corresponding variable occurrences have the same *lexical*
  *addresses* (or *DeBruijn* *indices*)
  
  
  
