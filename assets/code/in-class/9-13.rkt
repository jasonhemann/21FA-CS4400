#lang racket
(require rackunit)

;; An x-list is one of
;; '()
;; (cons x-list x-list)
;; (cons x x-list)
;; (cons non-x x-list)



;; Count :: Atom (Listof Atom) -> Number
(define (count x ls)
  (cond
    ((empty? ls) 0)
    ((cons? (car ls)) (+ (count x (car ls))
                         (count x (cdr ls)))) ;; for our purposes, effectively list? 
    ((eqv? x (car ls)) (add1 (count x (cdr ls))))
    (else (count x (cdr ls)))))


;(let ((var1 expr1)
;      ..)
;  body)

;(let ((dub (* 2 2))
;      (sin-of-point-five (sin .5)))
;    (+ sin-of-point-five sin-of-point-five sin-of-point-five dub))
;
;((lambda (var1) body) expr1)
;
;((lambda (var1 var2) body) expr1 expr2)

(let ((square (lambda (x) (* x x))))
  (square 5))

(letrec ((len (lambda (ls)
                (cond
                  ((empty? ls) 0)
                  (else (add1 (len (cdr ls))))))))
  (len '(a b c)))



(letrec
  ((oddp (lambda (n)
           (cond
             ((zero? n) false)
             (else (evenp (sub1 n))))))
   (evenp (lambda (n)
            (cond
              ((zero? n) true)
              (else (oddp (sub1 n)))))))
  (evenp 120))




;; An x-list is one of
;; '()
;; (cons x-list x-list)
;; (cons x x-list)
;; (cons non-x x-list)

;; a bintree is one of (node data left right) or (leaf data)



(define (show-bintree bt)
  (match bt
    [`(leaf ,data) data]
    [`(node ,data ,left ,right) (list 'this-is-the-left left)]))



(define (sum-bintree bt)
  (match bt
    [`(leaf ,data) data]
    [`(node ,data ,left ,right) (list 'this-is-the-left left)]))


;; a pair is (cons left right)
;; a pair is `(,left . ,right)

(define (stick-at-end e)
  `(apple boar turtle fish ,e))



(define fishy 'bassbass)

;; listof Number is one of '() (cons Number Listof Number)
;; square-all (listof Number) -> (listof Number)

;; YOU: write destructuring w/match, constructing w/ ` ,

(define (square-all lon)
  (match lon
    [`() `()]
    [(cons num rest)
     `(,(* num num) . ,(square-all rest))]))

(test-equal?
 "squares a list of 3 numbers"
 (square-all '(1 2 3))
 '(1 4 9))