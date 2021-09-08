---
title: Class intro; Racket 101
date: 2021-09-08
---

# Objectives

  - Orient students to the class
  - introduce our style of programming

# Orientation
  
  - Instructor
  - Course 
  - Technical and Administrative 

# Introduction to Jason

  About Your Instructor: 
   - Jason Hemann
   - Associate Teaching Professor, Northeastern
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

## The primary tool we'll be using--Racket and DrRacket
 - Download a new version
 - Different languages
 - Additional tools and packages
 

## 15m - stop and everyone go install, we can try and help.

DrRacket Go to `https://download.racket-lang.org/`

We will use these at the appropriate time. 

# All the Racket you need to know

## `cons`, `car`, and `cdr`. The story, lisp printing conventions.

```racket
;; (cons (car (cons α β)) (cdr (cons α β)))
;; =
;; (cons α β)
```

## Different equalities



## Natural recursion


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

Remembering the natural recursion.

```racket
;; '(4  8 5 8 7 3 8) => '(4  5 7 3) 
;;    '(8 5 8 7 3 8) =>    '(5 7 3)

(define (rember8 ls)
  (cond
    ((null? ls) '())
    ((eqv? (car ls) 8) (rember8 (cdr ls)))
    (else (cons (car ls) (rember8 (cdr ls))))))
```

## Some additional basic operations

 - `zero?`
 - `add1`, `sub1`
 - `+`, `-`
 - `lambda`
 - 


### You all try!

`memv` - have it mirror Racket's `memv`


