#lang racket
(require rackunit)
#| 

RacketData : Symbol | Number | '() | Boolean | (cons RacketData RacketData)

BinTree : (leaf data) | (internal-node data BinTree BinTree)

Listof Number : '() | (cons Number (Listof Number)) 

|#

'x
'(lambda (x) b)
'(x y)
'((lambda (x) x) y)


(define (max-expr-depth expr)  
  (match expr
    [`,y #:when (symbol? y)                    0]
    [`(lambda (,x) ,b) (add1 (max-expr-depth b))]
    [`(,rator ,rand) (add1 (max (max-expr-depth rator) (max-expr-depth rand)))]))

(check-equal?
 (max-expr-depth '(lambda (x) (lambda (y) (lambda (z) q))))
 3)

(check-equal?
 (max-expr-depth '((lambda (x) x) (lambda (x) (lambda (y) x))))
 3)

;; BoD : Expr -> Bagof Var
(define (bag-of-declarations e)
  (match e
	[`,y #:when (symbol? y)  '()]
	[`(lambda (,x) ,body) (cons x (bag-of-declarations body))]
	[`(,rator ,rand)
         (append (bag-of-declarations rator) (bag-of-declarations rand))]))

(check-equal? 
  (bag-of-declarations '((lambda (x) y) (lambda (q) (lambda (p) (lambda (p) (q z))))))
  '(x q p p) ;; any order you wish
  )


(lambda (x)
  ((lambda (y)
    (lambda (x)
      (y x x)))
   (lambda (z)
     x)))


((lambda (lambda) (lambda 5 'z)) list)


(define (bound-variable-reference? z expr acc)
  (match expr
    [`,y #:when (symbol? y) (and (eqv? y z) (memv z acc) #t)]
    [`(lambda (,x) ,b) (bound-variable-reference? z b (cons x acc))] ;; :/
    [`(,rator ,rand) (or
                      (bound-variable-reference? z rator acc)
                      (bound-variable-reference? z rand acc))]))

(lambda
  (lambda
    (lambda
      (lambda 
        (lambda 
          (lambda 
            (list 0 1 3 5 1)))))))


(lambda (x) (lambda (y) x)) (lambda (lambda 1))
(lambda (p) (lambda (q) p)) (lambda (lambda 1))
(lambda (j) (lambda (k) k)) (lambda (lambda 0))