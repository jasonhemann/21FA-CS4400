#lang racket
(define k 0)
(define v 0)
(define x 0)
(define ls 0)

#| 
CPS
RI HOF
Do the renaming to get the ^s and the v's
RI w/DS
let*/sequencing
set!s

|#

(define (cons-equal-car-k ls^ k^) `(cons-equal-car-k ,ls^ ,k^))
(define (cons-not-equal-car-k ls^ k^) `(cons-not-equal-car-k ,ls^ ,k^))
(define (cons-car-k x^ ls^ k^) `(cons-car-k ,x^ ,ls^ ,k^))
(define (non-cons-non-match-k ls^ k^) `(non-cons-non-match-k ,ls^ ,k^))
(define (empty-k) `(empty-k))

(define (apply-k) ; k v
  (match k
    [`(empty-k)
     (let* ()
       v)]
    [`(cons-equal-car-k ,ls^ ,k^)
     (begin
      (set! k k^)
      (set! v (cons (car ls^) v))
      (apply-k))]
    [`(cons-not-equal-car-k ,ls^ ,k^)
     (begin
       (set! k k^)
       (set! v (cons v (cdr ls^)))
      (apply-k))]
    [`(cons-car-k ,x^ ,ls^ ,k^)
     (cond
       [(equal? (car ls^) v)
        (begin
          (set! x x^)
          (set! ls (cdr ls^))
          (set! k (cons-equal-car-k ls^ k^))
          (remv-1st-cps))]
       [else
        (begin
          (set! x x^)
          (set! ls (car ls^))
          (set! k (cons-not-equal-car-k ls^ k^))
          (remv-1st-cps))])]
    [`(non-cons-non-match-k ,ls^ ,k^)
     (begin
       (set! k k^)
       (set! v (cons (car ls^) v))
       (apply-k))])) ;  k v

(define remv-1st-cps
  (lambda ()  ;x ls k
    (cond
      [(empty? ls)
       (begin
         (set! v '())
         (apply-k))] ;  k v
      [(cons? (car ls))
       (begin
         (set! k (cons-car-k x ls k))
         (set! ls (car ls))
         (remv-1st-cps))]  ;; x ls k
      [(eqv? (car ls) x)
       (begin
         (set! v (cdr ls))
         (apply-k))]  ; k v
      [else
       (begin
         (set! k (non-cons-non-match-k ls k))
         (set! ls (cdr ls))
         (remv-1st-cps))])))  ; x ls k

(define (main)
  (begin
    (set! k (empty-k))
    (set! ls '(a b ((x y z) ((z a b) y d) e (x f))))
    (set! x 'x)
    (remv-1st-cps))) ;  x ls k

