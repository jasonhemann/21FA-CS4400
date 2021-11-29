---
author: Jason Hemann
title: Staging
date: 2021-04-14
---

# The three projections of Dr. Futamura 

[http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html](http://blog.sigfpe.com/2009/05/three-projections-of-doctor-futamura.html). Consider
this a great read.

Partial evaluation is great when there isn't a *single* fixed order in
which to universally expect your parameters to arrive. Consider a
classic example, the `power(x,n)` function. Depending what you're
expecting, you might get either the base first or the exponent
first. Either could be useful. So there isn't a /single/ uniform
/best/ way to curry the function out. 

# Staging 

Sometimes we can expect a best way to describe the inputs. An order in
which it always makes sense. Then we can manually /stage/ that
program, rather taking in all of these parameters, we can separate it
out into multiple functions. This is nothing more than taking a
function `h`, and separating it out into a composition of two
different functions `g ∘ f` so that `g` never refers to or calls,
directly or indirectly, `f`. 

After naively staging, we can do some optimization, execute some
additional work in earlier stages to make the final stage more
efficient. 




