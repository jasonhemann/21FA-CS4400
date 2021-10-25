#lang racket
(require rackunit)
(require racket/trace)
#| Assignment 6: Continuation-Passing Style |#

;; Say you're in the kitchen in front of the refrigerator, thinking
;; about a sandwich. You take a continuation right there and stick it
;; in your pocket. Then you get some turkey and bread out of the
;; refrigerator and make yourself a sandwich, which is now sitting on
;; the counter. You invoke the continuation in your pocket, and you
;; find yourself standing in front of the refrigerator again, thinking
;; about a sandwich. But fortunately, there's a sandwich on the
;; counter, and all the materials used to make it are gone. So you eat
;; it.

;; -- Luke Palmer 


#| Assignment Guidelines |# 

;; When CPSing, you may treat built-in procedures such as empty?,
;; add1, assv, car, <, and the like as "simple".
   
;; No, you may not "simplify" or rewrite any of these programs to
;; another form and then CPS that simpler formula instead. Use the
;; method we described in class.

;; Test your CPSed procedures using the initial continuation returned
;; from the following (uncommented) empty-k. You may have seen empty-k
;; defined as 

;; (define (empty-k)
;;   (λ (v) v))

;; However, the one below is much better, in that it will help you
;; better detect if you have made a mistake in cps-ing.

(define (empty-k)
  (let ((once-only #f))
    (λ (v)
      (if once-only 
          (error 'empty-k "You can only invoke the empty continuation once")
          (begin (set! once-only #t) v)))))

#| let/cc |#

;; 1. Complete the following definition of last-non-zero, a function
;; which takes a list of numbers and returns the last cdr whose car is
;; 0. In other words, when starting from the right of the list, it
;; should be all numbers before the first 0 is reached. See the test
;; cases below for examples. Your solution should be naturally
;; recursive, and should not contain any calls to member-like
;; operations or last-non-zero. You must not modify the provided code
;; beyond adding a body. You may of course add newlines as needed. My
;; advice is to 1. Get it working for the base case. 2 Get it working
;; for a list without any 0s in it. 3. Get it working for a list with
;; one 0 in it. 4. Get it working for a list with more than one 0 in
;; it.

(define (last-non-zero ls)
  (let/cc k
    (letrec
      ((lnz
         (λ (ls)
           ;; complete the definition	     
           )))
      (lnz ls))))

  (test-equal?
   "test for empty list
   (last-non-zero '())"
   (last-non-zero '())
   '())

  (test-equal?
   "test for a non-empty list with no zeroes
   (last-non-zero '(1 2 3 4 5))"
   (last-non-zero '(1 2 3 4 5))
   '(1 2 3 4 5))

  (test-equal?
   "test for a list with one zero in the middle
   (last-non-zero '(1 2 3 0 4 5))"
   (last-non-zero '(1 2 3 0 4 5))
   '(4 5))

  (test-equal?
   "test for a list with two zeroes in the middle
   (last-non-zero '(1 0 2 3 0 4 5))"
   (last-non-zero '(1 0 2 3 0 4 5))
   '(4 5))


#| Direct vs. accumulator-passing vs. call/cc. |# 

;; 2. Consider the following definitions of mult and mult/acc, a
;; function which takes a list of numbers and returns the product,
;; respectively written in direct and accumulator-passing
;; styles. Then, complete the definition below to build a third
;; version, mult/cc, which instead uses a system continuation k to
;; return with 0 if the list contains a 0. Your implementation should
;; be naturally-recursive, and should not contain any calls to
;; member-like operations or last-non-zero. You must not modify the
;; provided code beyond adding a body. You may of course add newlines
;; as needed. For this problem, you should use my-* as your
;; multiplication operation. 

(define (my-* m n)
  (* m n))

#| 
> (define (mult n*)
    (letrec
      ((m
         (λ (n*)
           (cond
             ((empty? n*) 1)
             ((zero? (car n*)) 0)
             (else (my-* (car n*) (mult (cdr n*))))))))
      (m n*)))
> (define (mult/acc n*)
    (letrec 
      ((m/acc
         (λ (n* acc)
           (cond
             ((empty? n*) acc)
             ((zero? (car n*)) 0)
             (else (m/acc (cdr n*) (my-* (car n*) acc)))))))
      (m/acc n* 1)))
|# 


(define (mult/cc n*)
  (let/cc k
    (letrec
      ((m/cc
         (λ (n*)
           ;; complete the definition.
           ;; For this problem, use my-* as your mult operator 
           )))
      (m/cc n*))))

;; If you require the racket trace library, and define my-* to be
;; traced, we see an interesting property. When you have a 0 in the
;; list, both the direct and accumulator passing versions perform
;; multiplications until they reach a 0, yet mult/cc performs //no//
;; multiplications in the presence of a 0. Which is neat.

#| 
> (require racket/trace)
> (trace-define (my-* m n)
    (* m n))
> (mult '(1 2 3 4 0 6 7 8 9))
>(my-* 4 0)
<0
>(my-* 3 0)
<0
>(my-* 2 0)
<0
>(my-* 1 0)
<0
0
> (mult/acc '(1 2 3 4 0 6 7 8 9))
>(my-* 1 1)
<1
>(my-* 2 1)
<2
>(my-* 3 2)
<6
>(my-* 4 6)
<24
0
> (mult/cc '(1 2 3 4 0 6 7 8 9))
0
|# 

#| CPS |#

#| 

In this portion of the assignment, you will also have to construct and
submit your own tests. We use the explicit `λ` syntax here
because we think it may be helpful to you; of course you are not
required to do so.

|#

;; 3. Define and test a procedure walk-cps that is a CPSed version of
;; the following walk procedure:

(define walk
  (λ (v ls)
    (cond
      [(symbol? v)
       (let ((p (assv v ls)))
         (cond
           [p (walk (cdr p) ls)]
           [else v]))]
      [else v])))

;; Here are some sample calls to walk:

#| 

> (walk 'a '((a . 5) (b . 6) (c . 7)))
5
> (walk 'a '((a . b) (b . c) (c . 7)))
7
> (walk 'a '((a . q) (r . s) (q . r)))
s
> (walk 'a '((a . q) (r . s) (q . r) (s . 10)))
10

|# 

;; 4. Define and test a procedure times-cps that is a CPSed version of
;; the following times procedure.

(define times
  (λ (ls)
    (cond
      [(empty? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

;; 5. Define a modified version of your times-cps above, called
;; times-cps-shortcut that doesn't apply k in the zero case. Instead,
;; maintain the behavior of the zero? case in times - simply returning
;; the 0 and not performing further computation. While this certainly
;; violates the standard rules of CPSing the program, it provides an
;; interesting look at optimizations CPSing allows us: The
;; whole-program CPS transformation permits you the kinds of control
;; operations in languages that don't natively support them. 



;; 6. Define and test a procedure cexpt-cps that is a CPSed version of
;; the following cexpt procedure. 

(define cexpt
  (λ (m)
    (λ (n)
      (expt m n))))

;; Here are some examples of calls to cexpt:

#| 

> ((cexpt 2) 3)
8
> ((cexpt ((cexpt 2) 3)) 2)
64

|# 

;; 7. Define and test a procedure count-syms*-cps that is a CPSed
;; version of the following count-syms* procedure:

(define count-syms*
  (λ (ls)
    (cond
      [(empty? ls) 0]
      [(cons? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      [else (count-syms* (cdr ls))])))

;; Here are some example calls to count-syms*

#| 

> (count-syms* '(a 1 b 2 c 3))
3
> (count-syms* '((a 1) (b 2) (c 3)))
3
> (count-syms* '(1 (b (3 (d (5 e) 7) (g)) 9) ((h))))
5

|# 

;; 8. Define and test a procedure cons-cell-count-cps that is a CPSed
;; version of the following cons-cell-count procedure:

(define cons-cell-count
  (λ (ls)
    (cond
      [(cons? ls) 
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

;; 9. Define and test a procedure ack-cps that is a CPSed version of
;; the following ack
;; procedure. (http://en.wikipedia.org/wiki/Ackermann_function).
;; Warning: if you run this program with m >= 4 and n >= 2, you'll be
;; in for a long wait.

(define ack
  (λ (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

;; 10. Define and test a procedure fib-cps that is a CPSed version of
;; the following fib procedure:


(define fib
  (λ (n)
    ((λ (fib)
       (fib fib n))
     (λ (fib n)
       (cond
	 [(< n 2) n]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

;; 11. Define and test a procedure unfold-cps that is a CPSed version
;; of the following unfold procedure:

(define unfold
  (λ (p f g seed)
    ((λ (h)
       ((h h) seed '()))
     (λ (h)
       (λ (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

;; An example of its use is demonstrated below:

#| 

> (unfold empty? car cdr '(a b c d e))
(e d c b a)

|#

;; When testing your unfold-cps, you should consider its arguments to
;; be serious, so include the following helper definitions when
;; testing your code.


(define empty?-cps
  (λ (ls k)
    (k (empty? ls))))
(define car-cps
  (λ (pr k)
    (k (car pr))))
(define cdr-cps
  (λ (pr k)
    (k (cdr pr))))

#| 

> (unfold-cps empty?-cps car-cps cdr-cps '(a b c d e) (empty-k))
(e d c b a)

|# 


;; 12. Define and test a procedure pascal-cps that is a CPSed version
;; of the following pascal procedure:

(define pascal
  (λ (n)
    (let ((pascal
           (λ (pascal)
             (λ (m a)
               (cond
                 [(> m n) '()]
                 [else (let ((a (+ a m)))
                         (cons a ((pascal pascal) (add1 m) a)))])))))
      ((pascal pascal) 1 0))))



;; 13. Here is the definition of unify that uses the version of walk
;; given in question 8. Define and test a procedure unify-cps that
;; uses your walk-cps from question 8.


(define unify
  (λ (v w s)
    (cond
      [(eqv? v w) s]
      [(symbol? v) (cons `(,v . ,w) s)]
      [(symbol? w) (cons `(,w . ,v) s)]
      [(and (cons? v) (cons? w))
       (let ((s (unify (walk (car v) s) (walk (car w) s) s)))
         (cond
           [s (unify (walk (cdr v) s) (walk (cdr w) s) s)]
           [else #f]))]
      [(equal? v w) s]
      [else #f])))

;; Here are some example calls to unify:

#| 

> (unify 'x 5 '())
((x . 5))
> (unify '(x y) '(5 6) '())
((y . 6) (x . 5))
> (unify '(x x) '(5 6) '())
#f
> (unify '(x y z) '(5 x y) '())
((z . 5) (y . 5) (x . 5))

|# 

;; 14. Define and test a procedure M-cps that is a CPSed version of M,
;; which is a curried version of map. Assume for the CPSed version
;; that any f passed in will also be CPSed.

(define M
  (λ (f)
    (λ (ls)
      (cond
        ((empty? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

;; 15. Consider the corresponding call to M, called use-of-M. Using
;; your CPSed M-cps, re-write use-of-M to call M-cps, and make all the
;; appropriate changes (including CPSing the argument). Name it
;; use-of-M-cps

(define use-of-M
  ((M (λ (n) (add1 n))) '(1 2 3 4 5)))

#| Brainteasers 5400 Only |# 

;; 16. CPS the following program, and call it strange-cps:

(define strange
  (λ (x)
    ((λ (g) (λ (x) (g g)))
     (λ (g) (λ (x) (g g))))))


;; 17. Consider the following use of strange, called
;; use-of-strange. Using your CPSed strange, re-write use-of-strange
;; to call strange-cps, and make all the appropriate changes. Name it
;; use-of-strange-cps.

(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

;; 18. CPS the following program, and call it why-cps:

(define why
  (λ (f)
    ((λ (g)
       (f (λ (x) ((g g) x))))
     (λ (g)
       (f (λ (x) ((g g) x)))))))

;; To get you started, you may find it useful to see the
;; following-call to why.

#| 

> (define almost-length
    (λ (f)
      (λ (ls)
        (if (empty? ls)
            0
            (add1 (f (cdr ls)))))))
> ((why almost-length) '(a b c d e))
5

|# 

#| Just Dessert |# 

;; 19. CPS why-cps, and call it why-cps-cps.



