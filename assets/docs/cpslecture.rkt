#lang racket
(require racket/trace)
'(let ((v <expr>))
  <body-that-may-have-v-in-it>)

(trace-define (my-* m n)
  (* m n))


(define (mult-all/acc ls acc)
  (cond
    ((empty? ls) acc)
    ((zero? (car ls)) 0)
    (else (mult-all/acc (cdr ls) (my-* (car ls) acc)))))



(trace-define mult-all
  (lambda (ls)
    (cond
      ((empty? ls) 1)
      ((zero? (car ls)) 0)
      (else (my-* (car ls) (mult-all (cdr ls)))))))

;; 0. change the name
;; 1. every lambda gets an additional argument, k
;; 2. for a simple expression in tail position, apply k
;; 3. for a serious call in non-tail position, bring it into tail position, and then
;;;   build a continuation to do the rest of the work.
(define mult-all-cps
  (lambda (ls k)
    (cond
      ((empty? ls) (k 1))
      ((zero? (car ls)) 0)
      (else (mult-all-cps
             (cdr ls)
             (lambda (v)
               (k (my-* (car ls) v))))))))

(define ctimes
  (lambda (m)
    (lambda (n)
      (* m n))))

;;((ctimes 5) 10)

(define ctimes-cps
  (lambda (m k)
    (k (lambda (n k)
         (k (* m n))))))

(define (empty-k) (lambda (v) v))

(ctimes-cps 5 (lambda (f-cps) (f-cps 10 (empty-k))))


(define f
    (lambda (n)
      (* 7 n)))

(define f-cps
  (lambda (n k)
    (k (* 7 n))))

(define almost
  (lambda (m k)
    (k (lambda (n k)
      (k (* m n))))))

(ctimes-cps 7 (lambda (f-cps) (f-cps 10 (empty-k))))
