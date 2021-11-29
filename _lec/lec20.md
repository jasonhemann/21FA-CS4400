---
title: Types, and type systems.
date: 2021-11-29
---

# Preliminaries

## Report, Progress. 

Make _sure_ and submit. 
I also want to know that you're on track. 

 How's that going? Someone stuck?

## Additional Bonus - TRACE surveys 

Reminder: If TRACE eval scores completion % >= 85% I'll add 2 overall grade points to class grade average. 


## Last time 

Implementing our own `lambda` calc language
Implementing languages in terms of host languages.

## Qs


# Types

## Why and where? 

> Perhaps the most pervasive formal methods are those that are not
> even viewed as formal methods anymore! These include the pervasive
> use of static type systems in mainstream languages like Java and C#
> ....

> -- Ranjit Jhala et al. [Formal Methods Successes and
> Directions](https://ranjitjhala.github.io/static/nsf-workshop-report-summary.pdf)

## A division and a trade-off

A division and a trade-off between the heavyweight formalisms, for
general correctness properties, but often cumbersome for programmers
to use, and require lots of sophistication from the programmer.

## A kind of lightweight automation. 

On the other hand, we have the kinds of less powerful but easier to
apply, implement and use. Language implementers can build these into
automated language tooling

Programmers who don't know the formalisms can still use them! 

### By far the most common of these are type systems. 

> A type system is a tractable syntactic method for proving the
> absence of certain program behaviors by classifying phrases
> according to the kinds of values they compute.
> Benjamin Pierce, TAPL

### Types: not just for CS

 - Russell to avoid antimony (1902) 
 - STLC 1940
 - More expressive type systems (Per Martin-Lof + forward) 
   (open research questions still!!)
   

### Note also: classifies terms wrt run-time values

#### Type systems provide a _static_ _approximation_ of the program (fragments)' run-time behavior

#### Compositionality 

We calculate the types of "bigger" terms compositionally, from only
the types of the immediate subexpressions.

### Static > Conservative approxmation of what you want.

A type system can assure us that accepted programs *don't* have
certain errors, but doesn't mean that the rejected programs *do* have
those errors.

### "Certain errors" 

Rules out specific kinds of bad behaviors, not *all* bad behaviors.

 - Right # args? yes
 - Div-by-0? Prolly not!

### "Bad behaviors" ~= "run-time type errors."

### Typically checkers built-in and fully automated

Lots of room, space for design

### benefits (Ancillary)
  - Correct Documentation 
  - Enforce Abstraction 
  - *Detecting errors
  - Surprisingly useful
  
  
  
## Inference rule (schemas)

These "inference rules" are actually rule *schemas*; each schema
represents the infinite set of concete rules that we can obtained
consistently by replacing each metavariable by all phrases from the
appropriate syntactic category

"If we have established the statements in the premise(s) listed above
the line, then we may derive the conclusion below the line."

These can be inferences about values, or communicating processes, or
in our case, types.

————————————————-
  
## Last time:

### We established 
  
  1. t ∈ Terms 
  2. v ∈ Values
  3. An *evaluation* function.
  4. We "disallowed" bad programs. We didn't say what to do. 

## General structer "inference rules"

```
  aoeuaoeuo   345345345443
  ————————————————-
  choeurh
```

## Writing some rules, for example! 

``` 
  ne : Nat 
————————————————-
 (zero? ne) : Bool


  ne1 : Nat    ne2 : Nat 
———————————————-—————————
   (+ ne1 ne2) : Nat


————————————————————
 true : Bool

————————————————————
 false : Bool

—————————————————— (natural? ne)
 ne : Nat 


 ne : Nat
—————————————————— 
 (sub1 ne) : Nat 

  t : Bool c : τ  a : τ
—————————————————— 
  (if t c a) : τ 

```




### Now 


The most basic property of type systems is safety (also called soundness): well-typed terms do not "go wrong." 

We have already chosen how to formalize what it means for a term to go
wrong: it means reaching a "stuck state" (Definition 3.5.15) that is
not designated as a final value but where the evaluation rules do not
tell us what to do next.


What we want to know, then, is that well-typed terms do not get stuck. 

We show this in two steps, commonly [3] known as the progress and
preservation theorems.  


Progress: A well-typed term is not stuck (either it is a value or it
can take a step according to the evaluation rules).  

(base)

Preservation: If a well-typed term takes a step of evaluation, then
the resulting term is also well [4] typed.  

(induction)









### Conservativity vs. expressiveness

Job security: wanting to permit more programs to type check without
devolving into chaos or further overburdening the programmer, that's
the good stuff.


