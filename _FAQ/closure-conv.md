---
title: Closure conversion? Lambda lifting
---

Consider at least two different approaches to taking a "block
structured program" (for our purposes, a program with nested lambdas)
and transforming it to the sort of program we could run easily on our
standard machines. Both of the techniques we will study solve the
problem of sub-expressions like `(lambda (a) (* a y))`. To put a finer
point on it, if we don't do *something* we will lose the
context-specific meaning of `y` when we use this function. When we
call `(f 7)` then we can figure out that `a` ought to be 7. But with
`y` we are out of luck.

So, the first possibility, is to transform each such function into
something like `(lambda (a y) (* a y))`. That is, every function
should take in *all* the values of its free variables. All except for
global the names of global functions; if we are in some `(define foo
...)` then we will not have to pass the name `foo` in also; such
global functions names will not be a problem. By changing the arity of
such inner, local functions, we also force a change to the calling
procedure. The call sites of this function must now also pass along
the values for those parameters. This transformation can be
expensive---`n^3` in a naive implementation, and `n^2` when very
clever. But as a consequence of this transformation, your inner
functions are now scope-free, so you can lift them up (thus the name)
to global definitions. Your whole program becomes a set of mutually
recursive definitions. We might call this first step "combinator
conversion", and then the second, semi-trivial step lambda
lifting. Notice this transformation does not, in and of itself,
address the use of higher-order functions; it simply permits
block-structured programming.

We have also omitted small details like coming up with adequate global
names for formerly-anonymous inner functions. In this model, `letrec`
causes no difficulty. The names of all functions are potentially
mutually recursive, certainly mutually accessible, globals.

The other alternative is "closure conversion." In closure conversion,
we convert each such inner function to a closure. A closure is a way
to "close over" an open lambda expression ("open" meaning the
sub-expression like `(lambda (a y) (* a y))` has free variables in
it.) We transform the each such function into a pair consisting of a a
pointer to a code block and an environment, a mapping consisting of
the meanings of the free variables. Inner, nested lambda expression
have free variables. The two major approaches are: pass those in too
as additional parameters wherever you call the function, or keep track
of the meanings of the free variables from the context of the
function. Here, unlike lambda lifting, we don't get mutually recursive
functions for free. 

So the "closure conversion approach" splits in two possibilities
here. The closure conversion approach itself keeps all of the mutually
scoped function-names in each others' environments, as another part of
their environment. With the "supercombinator approach", instead there
is a mutual-recursion combinator that solves the problem of mutual
recursion without having the heretofore mutually-referring expressions
refer to each other. 


