---
title: Class intro; Racket 101
date: 2021-01-20
---

# Orientation
  
  - Instructor
  - Course 
  - Technical and Administrative 

# Introduction to Jason

  About Your Instructor: 
   - Jason Hemann
   - Doctorate in Computer Science, Indiana University
   - Concentration in Logic
   - Philosophy undergraduate
   - Dance and run and talk software and how it helps people
   - More at [recent news article!](https://twitter.com/KhouryCollege/status/1351680876254539776)

# What is this class about, and why? 

   - Why? from 10,000 ft. 
   - Where do we start, and where are we going? 
   - Show you some amazing stuff.
   - Give you opportunities to do some pretty awesome stuff
   - "Learn by doing"
   - Implementations and applications

# Technical and Administrative 

## Are you with us? 
   - Are you enrolled? 
   - Can you see yourself in Canvas (Combined section)?
   - Can you see yourself in Piazza? 

## I'm not going to go through administrativa. You should! 

  - Course Website
   - Full Course Schedule
   - Syllabus
   - Resources
   - (Optional) Textbooks and notes
   - Office hours 
  - Piazza 
  - Plan
   
## Delta from previous years/semesters
   - "Choose your own" v. "#langs" v. Haskell v. "#lang racket"
   - late work
   - breadth vs. depth 
   - continuations, control flow
   - laptops

# How to succeed in this class

 1. Come to class
 1. Take _vigorous_ notes. 
 1. No laptops
 1. Do not re-write when I re-write. 
    1. Instead, write down deltas-program xformations
 1. Revisit these notes to "replay" the events of lecture
    1. Use this replay to help you decipher and cement concepts in your head
	1. Working through in this level of detail will force you to grapple

# If you have not already ...

## Download and (re-)install DrRacket

Go to `https://download.racket-lang.org/`, and you can download and
install Racket and the DrRacket IDE for your platform. If you have
used DrRacket in a previous course, you should upgrade to the latest
version.

We will go ahead and install a handful of additional plugins. There
are for right now a couple of different tweaks and toggles we can hit.

Via the package manager (on OSX `File>Package manager`) , install
`faster-minikanren`.

We will use these at the appropriate time. 


# All the Racket you need to know

## `cons`, `car`, and `cdr`.

```racket
;; (cons (car (cons α β)) (cdr (cons α β)))
;; =
;; (cons α β)
```

## Important to write *these* style of programs

 -- As or more important than it is that you write them

## We'll do several examples.

### `length`


### `count8`

```racket
(define (count8 ls)
  (cond
    ((null? ls) 0)
    ((eqv? (car ls) 8) (add1 (count8 (cdr ls))))
    (else (count8 (cdr ls)))))
```

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

### `rember8` 

```racket
;; '(4  8 5 8 7 3 8) => '(4  5 7 3) 
;;    '(8 5 8 7 3 8) =>    '(5 7 3)

(define (rember8 ls)
  (cond
    ((null? ls) '())
    ((eqv? (car ls) 8) (rember8 (cdr ls)))
    (else (cons (car ls) (rember8 (cdr ls))))))
```

### `rember8*`

```racket
(define (rember8* ls)
  (cond
    ((null? ls) '())
    ((pair? (car ls)) (cons (rember8* (car ls)) (rember8* (cdr ls))))
    ((eqv? (car ls) 8) (rember8* (cdr ls)))
    (else (cons (car ls) (rember8* (cdr ls))))))

(rember8* '(4 (8 (5 (((8)) 7))) (3 8)))
```
	
### Arithmetic examples, if we get there. 
