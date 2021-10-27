---
author: Jason Hemann
title: RI wrt K
date: 2021-10-27
---

# Opening act

## Getting some great questions.

-   What else?

# Recap-CPS

## Let\'s go wild!

CPS me :-)

``` {racket}
(lambda (f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (y) ((x x) y))))))
```

# Recap CPSing the interpreter, + two new features. 

## `let/cc` 

## `throw` 

# Defunctionalize, RI continuations:

See in-class development and the notes posted online 
