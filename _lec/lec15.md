---
author: Jason Hemann
title: Trampolining
date: 2021-03-17
---

# Questions? Let\'s do them!

-   This is an important thing for you to get, and to get right.
-   I see a lot of people without submissions so far. Let\'s get on it!
-   The second part also will take some time, especially if you are
    doing it by hand!

# Walking through, where we last left our heroes.

Either pretend we had a program that was already CPSed, with
data-structure continuations. Or this could also serve as a refresher.

# The new stuff.

We can discuss this in the context of registerization, but we don\'t
have to. This is somewhat cleaner.

We have unbounded amounts of computation. Infinite loops cannot be
universally ruled out as bad data. Moreover, a finite of any size and
depth that produces a value is also allowed. So what do we do?

## Interlude: Charlie!

## The trampoline.

The pure trampoline.

``` {racket}
(define (trampoline th)
  (trampoline (th)))
```

The real question: For the marbles .... how do we get out of an infinite
loop?

## What about several computations?

-   in sequence?
-   in parallel?
-   at random?
-   What about more stringent requirements still?
