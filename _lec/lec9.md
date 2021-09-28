---
author: Jason Hemann
title: Parameter Passing
date: 2021-10-06
---

# Questions

-   Homework questions?
-   Anyone else gotten started on the one from yesterday?
-   Look for test code

# Parameter Passing Conventions

Today we implement 4, **4** interpreters, which exhibit different
parameter-passing conventions.

# Preludes: Boxes

``` {racket}
> (box 'cat)
#&cat
> (define b (box 'cat))
> b
#&cat
> (unbox b)
cat
> b
#&cat
> (set-box! b 'dog)
> b
#&dog
> 
```

> All problems in computer science can be solved by another level of
> indirection. -- David Wheeler

# Call-by-value interpreter, revisited

We will at first revisit this interpreter, and a short-circuit line.
This is for a derivation, not initially motivated in and of itself. But
you could *imagine* wanting this, if you know this situation happens
often.

# Call-by-reference interpreter

Notice!

``` {racket}
> (define b (box 'cat))
> (define c (box (unbox b)))
> (eqv? b c)
#f
```

# Call-by-name interpreter

# Call-by-need interpreter

  ----------------- -------------------- -----------------------
                    Strict evalutation   Non-strict evaluation
  Passes new box    CBV (the standard)   
  Passes same box   CBReference          CBName
  ----------------- -------------------- -----------------------

And our call-by-need interpreter is a variation on the CBName one, that
*uses* the side-effect for efficiency.

See both your own lecture notes, and also our linked PDF document
deriving these four interpreters to refresh and recapitulate.
