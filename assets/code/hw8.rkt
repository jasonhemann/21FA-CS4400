#lang racket
(require rackunit-abbrevs)

#| Assignment 8: Registerization |# 
 
;; A trampoline is strong enough to catch you before you hit the
;; ground, but not so cushy that you can live on it forever
;; 
;; Thomas Friedman 

#| ===== Assignment Guidlines ===== |# 

;; As you do the assignment, consider the following four procedures
;; and keep the following points in mind:

;; For each of the four initial programs, you should write a
;; registerized version of the original.

;; All definitions must be //representation-independent with respect
;; to continuations//. You must use a //data-structural//
;; representation of continuations. 

;; You should probably consider building a separate apply-k for each
;; of the below functions, each with its own continuations. We have
;; (prematurely) made the empty-ks representation independent for you.

;; You should also construct "driver" functions, to be invoked as the
;; following examples demonstrate. We might have named these "main" in
;; class. Each of these driver programs will take the arguments taken
;; by the original, pre-CPSed program. The job of the driver will be
;; to populate the registers with initial values and call the
;; registerized program.

;; Make sure to name your drivers exactly as we describe; this is our
;; interface for testing your procedures.

;; You **should not** trampoline these procedures. Once again, you
;; should not trampoline your registerized programs.

(define (ack-empty-k)
  (λ (v)
    v))

(define ack-cps
  (λ (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n)
                       (λ (v)
                         (ack-cps (sub1 m) v k)))])))

(define (depth-empty-k)
  (λ (v)
    v))

(define depth-cps
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth-cps (car ls)
                  (λ (l)
                    (depth-cps (cdr ls)
                               (λ (r)
                                 (let ((l (add1 l)))
                                   (if (< l r) (k r) (k l)))))))]
      [else (depth-cps (cdr ls) k)])))

(define (fact-empty-k)
  (λ (v)
    v))

(define fact-cps
  (λ (n k)
    ((λ (fact-cps k)
       (fact-cps fact-cps n k))
     (λ (fact-cps n k)
       (cond
         [(zero? n) (k 1)]
         [else (fact-cps fact-cps (sub1 n)
                         (λ (v)
                           (k (* n v))))]))
     k)))

(define (pascal-empty-k)
  (λ (v)
    v))

(define pascal-cps
  (λ (n k)
    (let ((pascal-cps
          (λ (pascal-cps k)
            (k (λ (m a k)
                 (cond
                   [(> m n) (k '())]
                   [else (let ((a (+ a m)))
                           (pascal-cps pascal-cps
                                       (λ (f-cps)
                                         (f-cps (add1 m) a (λ (v)
                                                             (k (cons a v)))))))]))))))
      (pascal-cps pascal-cps (λ (f-cps) 
                               (f-cps 1 0 k))))))



;; Here are examples of how to call these procedures:

;; (check-true* equal?
;;   [(ack-cps 2 2 (ack-empty-k)) 7]
;;   [(depth-cps '(1 (2 (3 (4)))) (depth-empty-k)) 4]
;;   [(fact-cps 5 (fact-empty-k)) 120]
;;   [(pascal-cps 10 (pascal-empty-k)) '(1 3 6 10 15 21 28 36 45 55)])

(check-true* equal? 
  [(ack-reg-driver 0 1) 2]
  [(ack-reg-driver 0 9) 10]
  [(ack-reg-driver 1 1) 3]
  [(ack-reg-driver 1 3) 5]
  [(ack-reg-driver 2 1) 5]
  [(ack-reg-driver 2 2) 7]
  [(depth-reg-driver '()) 1]
  [(depth-reg-driver '(1 (2 (3 (4))))) 4]
  [(depth-reg-driver '(1 2 3 4)) 1]
  [(fact-reg-driver 0) 1]
  [(fact-reg-driver 1) 1]
  [(fact-reg-driver 5) 120]
  [(pascal-reg-driver 10) '(1 3 6 10 15 21 28 36 45 55)]
  [(pascal-reg-driver 20) '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)]
  [(pascal-reg-driver 1) '(1)])


#| ===== Brainteaser (5400 Only) ===== |# 

;; Trampolines can be used to execute multiple trampolinized programs
;; simultaneously. Write a trampoline-like procedure rampoline that
;; takes three thunks containing trampolinized procedure calls, and
;; executes them in random order, returning the value of the first to
;; complete. Note that the procedures may go into an infinite loop. To
;; show that rampoline works correctly, execute it against a
;; trampolined ! function.

;; (define (!-empty-k j) `(!-empty-k ,j))
;; (define (!-k n k) `(!-k ,n ,k))

;; (define (!-apply-k k v)
;;   (match k    
;;     [(!-empty-k ,j) (j v)]
;;     [`(!-k ,n ,k) (λ () (!-apply-k k (* n v)))]))

;; (define (!-cps n k)  
;;   (if (zero? n)
;;       (λ () (!-apply-k k 1))
;;       (λ () (!-cps (sub1 n) (!-k n k)))))

;; (define (trampoline th)
;;   (trampoline (th)))

;; (define (main n)
;;   (let/cc jumpout
;;     (trampoline (λ () (!-cps n (empty-k))))))

;; ;; Use the following as a driver to your trampolinized fib: 

;; (define (!-ramp-driver n1 n2 n3)
;;   (let/cc jumpout
;;     (rampoline
;;       (λ () (!-cps n1 (!-empty-k jumpout)))
;;       (λ () (!-cps n2 (!-empty-k jumpout)))
;;       (λ () (!-cps n3 (!-empty-k jumpout))))))

#| ==== Just Dessert ==== |#

;; A trampoline can be used to interleave executions of two
;; trampolinized programs, we've seen (or can imagine) how executing
;; two such programs in a trampoline can return the first answer that
;; completes. But what if we want /both/ answers?

;; Devise a way to return both answers, wherein:

;;   * We still interleave executions of both programs with a trampoline.
;;   * We get back a list with both answers.
;;   * The answers are listed in the order they complete.
;;   * We don't redo any work after finding the first answer. 
 
;; You will need to define a bi-trampoline, a bi-tramp-driver,
;; and a trampolinized version of trib (that generates elements of
;; the
;; [[https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers#Tribonacci_numbers|tribonacci
;; sequence]]). Automated tests are set to run bi-tramp-driver
;; with two numbers. Note: here, you do not need to registerize in
;; order to trampolinize.

;; (check-true* equal?             
;;   [(bi-tramp-driver 3 4) '(2 4)]
;;   [(bi-tramp-driver 4 3) '(2 4)]
;;   [(bi-tramp-driver 6 6) '(13 13)])
