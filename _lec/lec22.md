---
author: Jason Hemann
title: Combinatory Logic, Abstraction, iota and jot
date: 2021-12-06
---

## The problem with lambdas: compiling them to run on a computer.

The problem with lambda expressions is compiling them to run on a
computer. 

## Nested functions, lambdas

When we try and implement programs with higher-order functions in an
ordinary language on an ordinary computer, there are "problems." We
have seen that many of these problems center around the environment.

### Open lambda expressions. 

Somewhere, in some program, we have a lambda expression.

```
 ...
    
	... (lambda (x) ... ) ...
 ...
```

It is likely an *open* lambda expression: the body of that lambda
expression `(lambda (x) ... y ... x ... z ... x ... y)` has, in
addition to `xs`, some other variables that are free in the body of
that expression.

This has caused us much difficulty. So far our response has been
*lambda lifting*. We will move these inner lambdas to top-level
functions, and make sure to "close over" the body of that lambda. 

And we had to perform this operation for any number of, and all manner
of, lambda expressions. Please recall this became especially difficult
because in our languages, there are any number of lambdas and all
manner of expressions and what have you. At any point in time, you
might have:

```
(lambda (a)
  (lambda (b)
    (lambda (c)
	  (lambda (d)
	    ...
		(lambda (z) ... ((z a) (b c) ...) ...)
		...))))
```		 

and we might have to close over *who knows how many* different
variables, different expressions. 

#### Closure conversion

Another and different approach is something called /closure
conversion/. These are not the same, and have different results, with
the latter being the more popular approach. You will learn more about
this in your compilers class.

### Another way out of the problem...

One way to look at the cause of this problem is: at any depth in our
program, we can find a lambda expression with an arbitrary numbers of
free variables in its body.

... What if we *didn't*? 

1. How can we avoid needing arbitrary quantities of free variables?

2. What would happen to the expressivity of our programming language
   as a result?

## Combinators 

A *combinator* is a closed lambda expression. That is to say, a lambda
expression with no free variables in it. The Y combinator. The Z
combinator. The Omega combinator. What's that intimidating word
"combinator" there mean? (The startup accelerator "ycombinator",
btw). What they share in common is that they are all lambda
expressions without any free variables.

We know that we can transform any program into a combinatory form, by
lambda lifting or closure conversion. These processes can result,
though, in an arbitrary number of different combinators that we must
then compile to separate functions.

What if instead we needed only a certain finite number of these
combinators, re-used multiple times? The same way you would you a
stamp template over and over again to produce multiple copies of the
same form. If we only needed calls to just a few forms, then that
would be sufficient.

# Lambda definability

So, follow up questions.

1. Is there a set of `{C1, C2, ... Cn}` of such closed expressions
repetitions of which would be sufficient?  

A. (Trivially, yes: all of them.) 

Okay smartypants, trying again. 

1. Is there a *finite* set of `{C1, C2, ... Cn}` of such closed
expressions repetitions of which would be sufficient?
2. How would we prove this property of such a set? 
3. ... How many such sets are there? 

This could also inspire us to ask additional questions, such as:

1. What is the minimum number of combinators you need in such a set?
2. Is there a smallest such set? 
3. Which set has the "simplest" combinators? (and what metrics could
   we use for "simplest"?)

## Combinators

This became a popular research project in the 1980s and 1990s. See
David Turner's work for details. Though related approaches, using
ideas like
[supercombinators](https://dl.acm.org/doi/abs/10.1145/800068.802129),
did gain some traction. But the story goes back way earlier, and even
predates computers. It begins with Moses Schonfinkel, if you remember
the name. See Hindley's ["History of Lambda Calculus and Combinatory
Logic"](https://hope.simons-rock.edu/~pshields/cs/cmpt312/cardone-hindley.pdf)
if you want to get a general timeline for work in this area. Like work
in the lambda calculus, this was a project in mathematical logic until
computer scientists decided to make it run.

Recall also that substitution was a difficult problem. It took a long
time to correctly implement substitution, and this is still generally
tedious to work with; we must be especially careful to avoid variable
capture. If we don't have lambdas or lambda abstraction, we don't
introduce local variables. Therefore, we don't need to worry about
correctly implementing (or reasoning about) capture-avoiding
substitution.

### `{S, K, I}` combinator basis

We call a finite set of combinators a *basis*. Think
(universal/linear) algebra. A *complete basis* is a basis sufficient
to express all of computation. The set of combinators `{S, K, I}` is a
complete basis of 3 combinators.

We can identify the combinators `S`, `K`, and `I` with the following
closed lambda expressions.

```
I = (lambda (x) x)
K = (lambda (x) (lambda (y) x))
S = (lambda (x) (lambda (y) (lambda (z) ((x z) y z))))
```

We can demonstrate this set is a complete basis by exhibiting an
algorithm to translate any lambda-calculus expression into an
SKI-calculus expression. If we have such an algorithm, then, we can
piggyback on results about lambda-definability. 

#### A bracket-abstraction algorithm. 

Algorithms for transforming expressions to combinatory form are
traditionally called "bracket algorithms", because of common notation
for these expressions. 

Here are the clauses for an algorithm to translate lambda calculus
expressions to an SKI-basis.

```
1. T[x]                            = x
2. T[E1 E2]                        = (T[E1] T[E2])
3. T[(lambda (x) E)]               = (K E)  when x is not in FV(E)

4. T[(lambda (x) x)]               = I 
5. T[(lambda (x) (lambda (y) E))]  = T[(lambda (x) T[(lambda (y) E]])]   when x is in FV(E)
6. T[(lambda (x) (E1 E2))]         = S(T[(lambda (x) E1)] T[(lambda (x) E2)])  when x is in either FV(E1) or FV(E2)
```

Remember, the aim of this algorithm is to eliminate the lambda
abstractions. So we focus carefully on the lambda abstraction cases,
and think about what "shape" the lambda forms might have when x occurs
in the body of a lambda expression binding x. If x occurs free in the
body of such a lambda, how many different shapes might the body have?
3. And we know precisely what shapes those are. 

If you work through several examples, you will see that this takes any
closed lambda term to an expression in the language of SKI. That is,
simply uses of `S`, `K`, `I`, and parentheses. If you want to get rid
of the parentheses, you could [join the US Military Academy at West
Point](https://dl.acm.org/doi/10.1017/S0956796802004483).

### Semantics of combinators

You can look at these combinator expressions as ways to "shunt"
("route") their arguments through to some specific directions. That's
their whole job in life. When the combinator does not have its full
expected complement of arguments, it is not yet *saturated* and so we
don't do anything with it.

Or, if you want to see it execute real-time, you can try your hand at
[SKI Combinator Tetris](https://dirk.rave.org/combinatris/)

Terminological note: since we represent each combinator by a single
variable there's no context for confusion if we express combination by
concatenation. 

### Other bases 

- Shonfinkel's [S,K,B,C system](https://en.wikipedia.org/wiki/Combinatory_logic#Combinators_B,_C)
- Curry's [B,C,K,W system](https://en.wikipedia.org/wiki/B,_C,_K,_W_system)

### Bracket abstraction algorithms

There are many different such bracket abstraction algorithms to take
you to many different such combinatory bases. Often there is a
trade-off between the size of the basis and the size of the
expressions produced by the abstraction algorithm. With more
complicated algorithms, taking advantage for instance of further
specializing the cases, we can get more clever about the combinators
we produce. [Tromp, Sec 3.2](https://tromp.github.io/cl/LC.pdf)
describes Turner's solution, and compares this to a particularly
elegant lambda encoding to the SKI calculus.

## A two-point combinatory basis

Let us for example see what would happen if we try and apply the
expression `(SKK)` to some variable `x` 

```
(SKK)x
= removing parens
SKKx
= routing through a saturated S
((K x) (K x))
= removing parens
Kx(Kx)
= routing through a saturated K
x
```

Wait a minute! Notice that `(SKK)` behaves like `I`: when applied to
any expression, it ultimately reduces back to that expression. 

So it seems like we only need *two* combinators, `{S, K}`!

... can we go smaller?

### `{X}` 

There are plenty of derivations of an X combinator. (It's called "X"
in the business.) See
[Fokker](https://link.springer.com/article/10.1007%2FBF03180572) ["The
systematic construction of a one-combinator
basis"](http://www.cs.uu.nl/research/techreps/repo/CS-1989/1989-14.pdf)
for more details.

![Comparison]({{ site.baseurl }}assets/images/one-point-combinator-comparison.jpeg)


### Iota and Jot 

Barker's [iota and
jot](https://web.archive.org/web/20160823182917id_/http://semarch.linguistics.fas.nyu.edu/barker/Iota/),
with many details lifted from his archived iota and jot page.

#### jot

```
Syntax:	    	Semantics:
F --> e		    \x.x
F --> F  0		[F]SK
F --> F  1		\xy.[F](xy)
```


Where `e` is the empty string. Every possible string of 1s and 0s is grammatical! {0,1}*

```
CL      Jot

K   ==> 11100
S   ==> 11111000
AB  ==> 1[A][B]
```

Notice further that the encoding of every CL expression begins with a
`1`. 

### Godel numbering

> Constructing a Goedel-numbering now becomes trivial: simply express
> each natural number in base 2 as a string of 1's and 0's. Since every
> such string is a legitimate Jot program, each base-2 numeral will give
> rise to a legitimate Jot program. Furthermore, since the base 2
> representation of a number always begins with a 1, and since the
> translation of each CL expression begins with a 1, for every CL
> program there will be a numeral with the same interpretation. Since CL
> is Turing-complete, we have a suitable Goedel-numbering in which the
> base-2 representation of the number itself is treated as a program
> directly.

This means, in particular, that you can ask ["In how many steps does
this number
terminate?"](https://dl.acm.org/doi/pdf/10.1145/2505341.2505348) We
asked that and got some interesting answers.

### Just Dessert 

If you are interested in combinatory logic and want a very fun
introductory book, I can highly recommend Ray Smullyan's "To Mock a
Mockingbird". You might enjoy the related [graphical
depictions](https://dkeenan.com/Lambda/index.htm) and find that they
help you. You can [try and catch'em
all!](https://www.angelfire.com/tx4/cus/combinator/birds.html)

Please also be sure to [take good care of them.]({{ site.baseurl
}}/assets/docs/hindley-seldin-combinatory-a4.pdf)

Backus, the author of Fortran, went gaga over combinators. His [Turing
award speech](https://dl.acm.org/doi/pdf/10.1145/359576.359579) was
entitled "Can programming be liberated from the von Neumann style?"
[Cf. Dijkstra's
review](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD06xx/EWD692.html)

If you want to compare Iota and Jot to others, see [Lazy-K](https://web.archive.org/web/20141129115513/http://homepages.cwi.nl:80/~tromp/cl/lazy-k.html)

Brief notes on [implementations and combinator-based graph reduction
machines](https://web.archive.org/web/20040921105306/http://www.ccs.neu.edu/home/matthias/369-s04/Transcripts/abstract-machines-for-graph-reduction-transcript.html)

