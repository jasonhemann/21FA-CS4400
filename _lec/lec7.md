---
title: Representation independence wrt env, closures
date: 2021-02-10
---


# Questions

-   Homework - how did that go?
-   Those of you who\'ve gotten started on the interpreter. Eh?

# Review, the interpreter

We wrote an interpreter for a small language. In a sense we can see our
interpreter as *defining* the meaning of expressions in that language.

# Interlude: A view from 10,000 feet.

Scheme -\> C

# Representation Independence

These are homework problems. You\'ll want these notes.

We want to abstract out an interface. Program to the interface. So we
can change the implementation, and user code won\'t matter. In some
sense, these say what it *means* to be an environment (resp., closure).

1.  RI wrt environments
2.  RI wrt closures

When we have made our interpreter representation-independent wrt
environments, closures, we can change the implementation, and because we
are programming to an interface, the usage code doesn\'t have to change.
