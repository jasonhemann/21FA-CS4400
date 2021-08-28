---
title: Why are homeworks graded holistically, and tests worth 1 point?
---

The [grading flowchart]({{ site.baseurl }}/syllabus/) says that will
assign your assignment a score between 0 and 10 points, inclusive. Why
are the autograder tests worth only 1 point, and the subjective
portion worth the other 9?

So. First off, please recall [that we consider a 9 and a 10
 /literally/ the same value.]({{ site.baseurl }}/FAQ/what-is-a-10/)

Merely passing the autograder tests is of course insufficient to be a
correct solution. We /need/ to manually inspect. Consider the
following:

    (define (countdown n)
	  (cond
	    ((0) '(0))
		((5) '(5 4 3 2 1 0))
		(else 'who-knows!)))
		
Such a definition would pass our unit test suite, but is obviously
insufficient. There are also aspects we look for in your solutions
that are not expressed in input and output. In fact there are [many
limitations to using naive automated testing for homework
assessments.](https://dl.acm.org/doi/10.1145/3230977.3230999) So
instead, we opt for a holistic assessment and use the automated test
suites as a guide when evaluating your assignments and also as a
minimal-threshold check.
	
