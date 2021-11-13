#lang racket
(require rackunit-abbrevs)


#| Bonus Assignment: Macros |#

;; > Once again, the R5RS macro system has exceeded our expectations.
;;
;; - Oleg Kiselyov, http://okmij.org/ftp/Scheme/macros.html

#| Assignment Guidelines |#

;; Let's implement some macros. For the following questions, make sure
;; to use only syntax-rules macros, as we wrote in class. For the and*
;; and cons* problems, it is //not// acceptable to use the and and
;; list* built into Racket in your implementations. Also, don't use
;; match in your solutions.

;; Note: As you work on the following problems, you can use quote-list
;; tricks, the macro stepper in DrRacket, (syntax->datum (expand
;; <syntax expr>)) and expand-only, or some of the utilities found
;; [here](http://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html)
;; to see how a macro expands.


;; > (syntax->datum (expand '(cond (#t #f) (else 7))))
;; (if '#t (let-values () '#f) (let-values () '7))

;; 1. and* This should work similarly to Racket's and. 

(check-true* equal? 
  [(and* 1 2 3) '3]
  [(and* #f) '#f]
  [(and*) '#t]
  [(and* 'a) 'a]
  [(and* #t #t #t #t #t #t #t #t #f) '#f])

;; 2. cons* cons-es together its arguments. If the final argument is
;; not a list, cons* should return an improper list. If a single
;; argument is passed, it should simply return that argument. When
;; called with no arguments, your macro should report an error by
;; calling: (raise-syntax-error "Incorrect argument-count to
;; cons*"). Your answer should operate similarly to Racket's list*.

(check-true* equal? 
 [(cons* 'a 'b 'c 'd) '(a b c . d)]
 [(cons* 'a) 'a])

;; 3. macro-list

;; The Racket function list can be implemented simply as a function in
;; Racket using _variadic_ (n-ary) lambdas.

(define mylist (lambda a a))
(mylist 1 2 3 4)

;; Note the absence of parentheses around the formal parameter to the
;; function. But in the early days of Lisp, there were no variadic
;; functions. Instead, list was implemented as a recursive
;; macro. Implement macro-list, which takes any number of arguments
;; and builds a list of them.

(check-true* equal?
 [(macro-list) '()]
 [(macro-list 1 'b 2 'd) '(1 b 2 d)])

;; 4. macro-map.

#| 
> (map (lambda (x) (list x x)) '(a b c))
((a a) (b b) (c c))
> (define-syntax copy-code
    (syntax-rules ()
      [(_ x) `(,x x)]))
> (copy-code (lambda (x) x))
(#<procedure> (lambda (x) x))
> (copy-code 'a)
(a 'a)
> (map copy-code '(a b c))
stdin::167: copy-code: bad syntax
  in: copy-code
  context...:
|# 

;; Macros cannot be passed as arguments to a function in that
;; manner. Instead, they have to be expanded from matches of a pattern
;; into some new template. To get around this problem, let's define a
;; macro macro-map, which //will// allow us to map a macro.

(define-syntax copy-code
  (syntax-rules ()
    [(_ x) `(,x x)]))

(define-syntax quote-quote
  (syntax-rules ()
    [(_ e) (quote (quote e))]))

(check-true* equal?
 ((macro-map quote 
             '((trinidad and tobago) 
               (saint vincent and the grenadines) 
               (antigua and barbuda))) 
  '((trinidad and tobago)
    (saint vincent and the grenadines)
    (antigua and barbuda)))
 ((macro-map copy-code
             '(((lambda (x) x) 5)
               ((lambda (x) (+ 2 x)) 5)
               ((lambda (x) 7) 5)))
  '((5 ((lambda (x) x) 5))
    (7 ((lambda (x) (+ 2 x)) 5))
    (7 ((lambda (x) 7) 5))))
 ((macro-map quote-quote 
             '((trinidad and tobago)
               (saint vincent and the grenadines)
               (antigua and barbuda))) 
  '('(trinidad and tobago)
     '(saint vincent and the grenadines)
     '(antigua and barbuda))))

#| Just Dessert |#  

;; 5. With or without peeking, implement an interpreter as a
;; syntax-rules macro. That is, by the time macro expansion is
;; finished, you'll have completed the evaluation of the program.

;; We have referred already you to a document that shows how to do
;; this. Furthermore, you can even see another (less featureful)
;; implementation [here]({{ site.baseurl
;; }}/assets/images/dvh-cbv-syntax-rules.jpeg) but it's more fun if
;; you can do it with a minimum of peeking. You'll need
;; continuation-passing macros, perhaps accumulator-style macros, and
;; maybe some of the Petrofsky/Kiselyov trickery to do some of the
;; tests you'll need with pattern matching.


