#lang racket

(define (apply-env env y)
  (env y))


(define (apply-env env y)
  (match y
    [`(empty-env) (error 'sad "badness")]
    [`(extended-env ,x ,a ,env)
     (if (eqv? x y)
        a
	(apply-env env y))]))

;; E, E1, E2 ∈ LamCalcExpr := (num n) | (var x) | (λ (x) E) | (app E1 E2) | (sub1 E) | (* E E1)
;;                         ... (let ((x E1)) E2)


;; Don't do this!!
[`(let ((,x ,e)) ,b)  (valof `((λ (,x) ,b) ,e) env)]


[`(let ((,x ,e)) ,b)  (valof b (lambda (y)
                                 (if (eqv? x y)
                                     (valof e env)
                                     (env y))))]

(let ((z (! (! (! (! 999))))))
  (+ z z z))

;; Here's where to start
[`(let ((,x ,e)) ,b) (let ((a (valof e env)))
                       (valof b (λ (y) (if (eqv? x y)
                                           a
                                           (env y)))))]


[`(let ((,x ,e)) ,b)  ((λ (a) (valof b (lambda (y)
                                         (if (eqv? x y)
                                             a
                                             (env y)))))
                       (valof e env))]

(sub1 (cons 5 6))

;; Concrete vs. Abstract Syntax.

IF

THEN

ELSE IF

THEN

FI 



 ;;;;;
(define (valof expr env)
  (match expr
    ...
    [`(sub1 ,nexp) (sub1 (valof nexp env))]
    [`(* ,ne1 ,ne2) (* (valof ne1 env) (valof ne2 env))]
    
    ...))

x | (λ (x) E) | (E E)

;; Currying

(λ (x1 ... xn)
  )

(λ (x1)
  (λ (x2)
    ...
    (λ (xn)
      ...)))

(map (λ (n) (* 3 n)) '(1 2 3 4))

(map (* 3) '(1 2 3 4))

(match expr
  [`,y #:when (symbol? y) (env y)]
  [`(λ (,x) ,b) (λ (a) (valof b (λ (y) (if (eqv? x y)
                                           a
                                           (env y)))))]
  [`(,rator ,rand) ((valof rator env)
                    (valof rand env))])

(car (cons a b)) = a
(cdr (cons a b)) = b

kons = (λ (a) (λ (b) (λ (op) ((op a) b))))
kar  = (λ (fst) (λ (snd) fst))
kdr  = (λ (fst) (λ (snd) snd))

iph  = (λ (true-branch)
         (λ (false-branch)
           (λ (b)
             ((b true-branch) false-branch))))

tru  = (λ (tb) (λ (fb) tb))
fals = (λ (tb) (λ (fb) fb))

(λ (s) (λ (z) (s (s (s (s z))))))

(λ (s) (λ (z) (s (s z))))

(4 * 2 = 8)

(λ (f-to-apply)
  (λ (base-value)
    (f-to-apply
     (f-to-apply
      (f-to-apply
       (f-to-apply base-value))))))


#| 
hw6-worked.rkt> (define kons (λ (a) (λ (b) (λ (op) ((op a) b)))))
hw6-worked.rkt> (kons 5)
#<procedure:hw6-worked.rkt:152:20>
hw6-worked.rkt> ((kons 5) 6)
#<procedure:hw6-worked.rkt:152:27>
hw6-worked.rkt> (define kar (λ (fst) (λ (snd) fst)))
hw6-worked.rkt> (((kons 5) 6) kar)
5
hw6-worked.rkt> (define the-cons-cell-5-cons-6)
; hw6-worked.rkt:157:0: define: bad syntax (missing expression after identifier)
;   in: (define the-cons-cell-5-cons-6)
; Context (plain; to see better errortrace context, re-run with C-u prefix):
;   /Applications/Racket v8.2/collects/racket/private/norm-define.rkt:165:4 normalize-definition
hw6-worked.rkt> (define the-cons-cell-5-cons-6 ((kons 5) 6))
hw6-worked.rkt> (the-cons-cell-5-cons-6 kar)
5
hw6-worked.rkt> (define four (λ (s) (λ (z) (s (s (s (s z)))))))
hw6-worked.rkt> ((four add1) 0)
4
hw6-worked.rkt> (define op (λ (n1) (λ (n2) (n1 n2))))
hw6-worked.rkt> (define two (λ (s) (λ (z) (s (s z)))))
hw6-worked.rkt> ((op four) two)
#<procedure:hw6-worked.rkt:160:20>
hw6-worked.rkt> ((((op four) two) add1) 0)
16
hw6-worked.rkt> ((((op two) two) add1) 0)
4
hw6-worked.rkt> ((((op two) four) add1) 0)
16
hw6-worked.rkt> (define two (λ (s) (λ (z) (s (s (s z))))))
hw6-worked.rkt> (define three (λ (s) (λ (z) (s (s (s z))))))
hw6-worked.rkt> (define two (λ (s) (λ (z) (s (s z)))))
hw6-worked.rkt> ((((op two) three) add1) 0)
9
hw6-worked.rkt> D 

|# 
