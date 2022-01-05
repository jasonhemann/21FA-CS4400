#lang racket
(require rackunit-abbrevs)
(require racket/trace)

#| Staging and Partial Evaluation |# 

;; Never put off until run time what you can do at compile time.
;; 
;; -- David Gries, in "Compiler Construction for Digital Computers",
;; circa 1969.
 
#| Assignment Guidelines |#

;; Throughout this assignment, I suggest you avoid eta reductions even
;; when possible. This is the sort of assignment where you would wish
;; you have a type system to help you keep your parameters straight.

;; I suggest you perform these as nested, whole-program
;; transformations, and save waypoints along the way, even when we
;; don't ask you to.

;; We have given you a traditional, 4400-style interpreter. It does
;; not match its test cases, though. It's test cases expect to take in
;; its arguments /curried/ (or schönfinkeled, if you prefer). 

;; 1. Rewrite the following interpreter so that it takes in its
;; arguments in a curried fashion. You should curry out the parameters
;; to your interpreter and curry the parameters in every one of your
;; interpreter's recursive calls. Also curry out the parameters to
;; your (CPSed) environment. Please don't do something clever like
;; writing a "help interpreter" that uncurries and passes them along,
;; or use a letrec or any such mishegas. You must "sink in" each
;; parameter as far as possible (specifically, the env-cps and k; expr
;; must stay where it is) Do not curry out your closures. You will
;; sometimes find you have to duplicate a lambda expression over both
;; sides of an if expression. The test suite is inadequate, you must
;; add your own.

#| 

To further clarify this transformation, we have transformed here for
you one of your interpreter's clauses

    [`(* ,nexp₁ ,nexp₂)
     (λ (env-cps)
       (λ (k)
         (((valof-cps nexp₁) env-cps)
          (λ (v₁)
            (((valof-cps nexp₂) env-cps)
             (λ (v₂)
               (k (* v₁ v₂))))))))]
|# 
 
(define (valof-cps expr env-cps k)
  (match expr
    [`,y #:when (symbol? y) (env-cps y k)]
    [`,n #:when (number? n) (k n)]
    [`(* ,nexp₁ ,nexp₂)
     (valof-cps nexp₁ env-cps
                (λ (v₁)
                  (valof-cps nexp₂ env-cps
                             (λ (v₂)
                               (k (* v₁ v₂))))))]
    [`(sub1 ,nexp)
     (valof-cps nexp env-cps
                (λ (v)
                  (k (sub1 v))))]
    [`(zero? ,nexp)
     (valof-cps nexp env-cps
                (λ (v)
                  (k (zero? v))))]
    [`(if ,te ,ce ,ae) 
     (valof-cps te env-cps 
                (λ (b)
                  (if b
                      (valof-cps ce env-cps k)
                      (valof-cps ae env-cps k))))] 
    [`(lambda (,x) ,b)
     (k (λ (a k)
          (valof-cps b (λ (y k^)
                         (if (eqv? x y)
                             (k^ a)
                             (env-cps y k^)))
                     k)))]
    [`(let ((,x ,e)) ,b)
     (valof-cps e env-cps
                (λ (a)
                  (valof-cps b (λ (y k^)
                                 (if (eqv? x y)
                                     (k^ a)
                                     (env-cps y k^)))
                             k)))]
    [`(,rator ,rand)
     (valof-cps rator env-cps
                (λ (c-cps)
                  (valof-cps rand env-cps
                             (λ (a)
                               (c-cps a k)))))]))

(define (eval/cps expr)
  (((valof-cps expr)
    (λ (y)
      (λ (k)
        (error 'valof-cps "badness"))))
   (λ (v) v)))

(check-true* equal?
  [(eval/cps '(((lambda (f) 
          ((lambda (x) (x x))
           (lambda (x) (f (lambda (y) ((x x) y))))))
        (lambda (!)
          (lambda (n)
            (if (zero? n)
                1 
                (* n (! (sub1 n)))))))
       5))
  120])


;; 2. Copy your interpreter from 1. above, and name this copy
;; valof-pe. You will here "lift up" evaluable sub-expressions as far
;; as possible, so that you do as much work as possible before taking
;; in the parameter. We have provided you tests below for this version
;; of the program. You know your transformation is correct when all
;; calls to valof-pe finish before taking in an environment. This will
;; not be the case for your environment and continuations,
;; however. The test suite is inadequate, you must add your own.

#| 

To further clarify this transformation, we have transformed here for
you one of your interpreter's clauses

    [`(* ,nexp₁ ,nexp₂)
     (let ([ve1 (valof-pe nexp₁)]
           [ve2 (valof-pe nexp₂)])
       (λ (env-cps)
         (λ (k)
           ((ve1 env-cps)
            (λ (v1)
              ((ve2 env-cps)
               (λ (v2)
                 (k (* v1 v2)))))))))]
|# 

;; 3a. 

#|

Think: Why is it that we can ensure all calls to valof-pe are finished
completed before ever evaaluating an environment. Why /can't/ we make
that guarantee about environments before continuations?

Answer: 

|#

;; 3b. 

#|

Think: why is it okay to evaluate these sub-expressions out of order?
Why are we (why am I) not concerned about causing unfortunate infinite
loops?

Answer: 

|#


(define (eval/pe expr)
  (((lvalof-pe expr)
    (λ (y)
      (λ (k)
        (error 'valof-cps "badness"))))
   (λ (v) v)))

(check-true* equal?
  [(eval/pe '(((lambda (f) 
                 ((lambda (x) (x x))
                  (lambda (x) (f (lambda (y) ((x x) y))))))
               (lambda (!)
                 (lambda (n)
                   (if (zero? n)
                       1 
                       (* n (! (sub1 n)))))))
              5))
   120])
