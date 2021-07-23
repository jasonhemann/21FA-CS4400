#lang racket
(require racket/trace)
;; Macros.

;; (define (my-thunkify x)
;;   (lambda () x))

(define-syntax thunkify
  (syntax-rules ()
    [(_ e) (lambda () e)]))

(define-syntax if-t-e
  (syntax-rules (then else)
    [(_ t then conseq else alt)
     (if t conseq alt)]))
#|
(define-syntax if
  (syntax-rules (then else)
    [(_ t then conseq else alt)
     (if t conseq alt)]))
|#

(trace-define-syntax loop
  (syntax-rules ()
    [(loop e) (loop (list e e))]))

(define-syntax let
  (syntax-rules ()
    [(_ ((x e1) ...) e2)
     ((lambda (x ...) e2) e1 ...)]))

(define-syntax lambda
  (syntax-rules ()
    [(_ ((a ...) b ...) body)
     (λ (a ...)
       (lambda (b ...) body))]
    [(_ (b ...) body)
     (λ (b ...) body)]))

(define-syntax or2
  (syntax-rules ()
    [(_ e1 e2)
     (let ((v e1))
       (if v v e2))]
    [(_ e1 e2 e3 ...) (raise-syntax-error 'or2 "badness")]))


(define-syntax or*
  (syntax-rules ()
    [(_) #f]
    [(_ e1) e1]
    [(_ e1 e2 ...)
     (let ((v e1))
       (if v v (or* e2 ...)))]))
    
(define-syntax print-vals
  (syntax-rules ()
    [(_ r ...)
     (begin
       (printf "the register ~s~n has value~n ~s~n" 'r r)
       ...)]))

;; syntax-parse

(define b 5)

(define-syntax effect-and-ans
  (syntax-rules ()
    [(_ e a) (begin (set! a (+ a 1))
                  e)]))