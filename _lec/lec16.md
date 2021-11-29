---
author: Jason Hemann
title: ParentheC
date: 2021-11-08
---

# The rest of the program ... 


## We assume the 7th assignment. 

We assume you have this in the right format from the 7th
assignment. If not, it's time to get it there. To recap, let's go get
it there.

## Additional changes:

  1. Load `parenthec`
  2. Transform every `match` expression into a `union-case`
     `union-case <var> <union>`. You must precede each constructor
     invocation w/the name of the union, w/a literal `_`. I know. It's
     a
     [load-bearing](https://frinkiac.com/video/S08E08/f6IcBG4MPz6-WQBf5vIGQrrxuGA=.gif)
     underscore.
  3. `let*`, and construct a `main` that produces the output, and takes no parameters. 
  4. `(define-registers n k v)`, `set!`s, thunks.
  5. `(define-label`
  6. `(define-program-counter pc)` (set! pc)
  7. Don't assign `k` in the `main`, instead `mount-trampoline` w/name of constructor, `k` var, and `pc`
  8. `(empty-k )` has to take a j, so that we can pass it along, and `(dismount-trampoline j)` in end case.
  9. Make sure you `print` out the answer you get there. 
	 

```
  ;; one additional thing to bear in mind. 
  ;; every place you /construct/ a continuation
  ;; <union-name>_ . A Literal underscore ಠ_ಠ
```


## Then 

Load `"pc2c.ss"`. Then, in the repl.

```
> (pc2c "foo.pc" "foo.c" "foo.h")
```

## Booking meetings:

Book a meeting [here](v2.waitwhile.com/book/4400).
 
 
