#lang racket
(require minikanren-ee)
(require rackunit)

#| Assignment 11: Type Inference |# 
 
;; The best way to understand type theory is to implement it!
;;
;; -- Anders Mörtberg


;; Implementing type inference (and hence program inference) is such a
;; perfect application of logic programming that it hurts to have to
;; do it any other way.

;; -- Lindsey Kuper, twitter.com/lindsey/status/1186929216689885190 


#| ===== Assignment Guidlines ===== |# 

;; Your assignment this week is to complete your own type inferencer
;; and inhabiter in miniKanren. You should start with the inferencer
;; in this file, and to it +, add1, cons, car, and cdr. To be clear,
;; your solution should not involve match at all; it should be written
;; entirely in miniKanren. I've used this exercise before in other
;; classes; if you've seen it before then maybe you can take a look at
;; the bonus problems.

;; Your extended inferencer should pass all of the provided test
;; cases.

;; You may find the old notes on types and type inference on the
;; website to be of some use to you. Of course, the best and most
;; recent information is that which we presented you in lecture. 

#| 

To avoid divergence for some of the tests, you must carefully order
goals within a conjunction to "fail fast". Keep these rules in
mind:

* Simple unifications should come before recursive calls. For
  example, (== 5 x) should come before (foo y z).

* Recursive calls with instantiated arguments should come before
  recursive calls with uninstantiated arguments. For example, (foo
  `(,x ,y ,z) 5) should come before (foo u v), assuming x, y, z, u,
  and v are fresh.

Other important hints for this assignment:

* The type of a lambda expression is an //arrow type//. For
  example, the type of (lambda (x) (sub1 x)) is (Nat -> Nat), while
  the type of (lambda (x) (zero? x)) is (Nat -> Bool).

* The //type environment//, often called Gamma, is similar to the
  environment used in our interpreter from last time. The only
  difference is that a type environment binds lexical variables to types
  instead of values. 

* In the lambda line, you should ensure that the binder (formal
  parameter to the function) is a symbol.

* Your not operator should expect only expressions which type at
  Bool, unlike not in Racket.

|# 

(define-relation (lookupo G e t)
  (fresh (a G^)
    (== `(,a . ,G^) G)
    (fresh (aa da)
      (== `(,aa . ,da) a)
      (conde
        ((== aa e) (== da t))
        ((=/= aa e) (lookupo G^ e t))))))

(define-relation (!- G e t)
  (conde
    ((numbero e) (== 'Nat t))
    ((== t 'Bool)
     (conde
       ((== 't e))
       ((== 'nil e))))
    ((fresh (teste conseqe alte)
       (== `(if ,teste ,conseqe ,alte) e)
       (!- G teste 'Bool)
       (!- G conseqe t)
       (!- G alte t)))
    ((symbolo e)
     (lookupo G e t))
    ((fresh (x b)
       (== `(lambda (,x) ,b) e)
       (symbolo x)
       (fresh (tx tb)          
         (== `(,tx -> ,tb) t)
         (!- `((,x . ,tx) . ,G) b tb))))
    ((fresh (e1 arg)
       (== `(,e1 ,arg) e)
       (fresh (targ)
         (!- G e1 `(,targ -> ,t))
         (!- G arg targ))))
    ((fresh (e1 e2)
       (== `(* ,e1 ,e2) e)
       (!- G e1 'Nat)
       (!- G e2 'Nat)
       (== t 'Nat)))
    ((fresh (n)
       (== `(zero? ,n) e)
       (!- G n 'Nat)
       (== t 'Bool)))
    ((fresh (n)
       (== `(sub1 ,n) e)
       (!- G n 'Nat)
       (== t 'Nat)))
    [(fresh (f fun x)
       (== `(fix (lambda (,f) ,fun)) e)
       (absento 'fix G)
       (!- `((,f . ,t) . ,G) fun t))]
    ((fresh (b)
       (== `(not ,b) e)
       (!- G b 'Bool)
       (== t 'Bool)))))

(test-equal? "t is a boolean"
 (run* (q) (!- '() 't q))
 '(Bool))

(test-equal? "17 is a number"
 (run* (q) (!- '() 17 q))
 '(Nat))

(test-equal? "zero? n is a number"
 (run* (q) (!- '() '(zero? 24) q))
 '(Bool))

(test-equal? "zero? add1 n is a number (so zero? takes expressions)"
 (run* (q) (!- '() '(zero? (add1 24)) q))
 '(Bool))

(test-equal?  "add1 add1 add1 so for sure non-trivial exprs"
 (run* (q)
   (!- '() '(add1 (add1 (add1 6))) q))
 '(Nat))

(test-equal? "zero? add1 add1 n (so add1 takes expressions)"
 (run* (q)
   (!- '() '(zero? (add1 (add1 18))) q))
 '(Bool))

(test-equal? "not zero? add1 add1 n (so add1 takes expressions)"
 (run* (q)
   (!- '() '(not (zero? (add1 (add1 18)))) q))
 '(Bool))

(test-equal? "test for +"
 (run* (q)
   (!- '() '(add1 (+ (add1 (add1 18)) (add1 (add1 18)))) q))
 '(Nat))

(test-equal? "If expressions work, and w/boolean expressions"
 (run* (q)
   (!- '() '(if (zero? 24) 3 4) q))
 '(Nat))

(test-equal? "If statement w/non-trivial conseq., alt. expressions "
 (run* (q)
   (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
 '(Bool))

(test-equal? "function types with boolean bodies"
 (run* (q)
   (!- '() '((lambda (n) (zero? n)) 5) q))
 '(Bool))

(test-equal? "function types w/nat valued bodies"
 (run* (q)
   (!- '() '(lambda (x) (add1 x)) q))
 '((Nat -> Nat)))

(test-equal? "function types work w/if expressions"
 (run* (q)
   (!- '()  '(lambda (n) (if (zero? n) n n)) q))
 '((Nat -> Nat)))

(test-equal? "addition in nested applications"
 (run* (q)
   (!- '() '(lambda (a) (lambda (x) (* a x))) q))
 '((Nat -> (Nat -> Nat))))

(test-equal? "nested application of function parameters"
 (run* (q)
   (!- '() '(lambda (f)
              (lambda (x)
                ((f x) x)))
       q))
 '(((_.0 -> (_.0 -> _.1)) -> (_.0 -> _.1))))

(test-equal? "self-application fails, as it ought in an STLC derivative"
 (run 1 (q)
   (fresh (t)
     (!- '() '(lambda (f) (f f)) t)))
 '())

(test-equal? "Lots of expressions type in an arbitrary environment"
 (length (run 100 (q)
           (fresh (g e t)
             (!- g e t)
             (== `(,g ,e ,t) q))))
 '100)

(test-equal? "Lots of expressions type in an empty environment"
 (length (run 100 (q)
           (fresh (e t)
             (!- '() e t)
             (== `(,e ,t) q))))
 '100)

(test-equal? "Lots of expressions type at nat"
 (length (run 30 (q) (!- '() q 'Nat)))
 '30)

(test-equal? "lots of applications type at nat"
 (length (run 20 (q)
           (fresh (lam a b)
             (!- '() `((,lam (,a) ,b) 5) 'Nat)
             (== `(,lam (,a) ,b) q))))
 '20)

(test-equal? "Lots of expressions type at bool -> nat"
 (length (run 30 (q) (!- '() q '(Bool -> Nat))))
 '30)

(test-equal? "Lots of expressions type at nat -> nat"
 (length (run 30 (q) (!- '() q '(Nat -> Nat))))
 '30)

(test-equal? "Lots and lots of expressions type at nat -> nat"
 (length (run 500 (q) (!- '() q '(Nat -> Nat))))
 '500)    

(test-equal? "Lots and lots of expressions type at nat -> nat -> nat"
 (length (run 30 (q) (!- '() q '(Nat -> (Nat -> Nat)))))
 '30)

(test-equal? "Looking up in an arbitrary environment"
 (length
  (run 100 (q)
    (fresh (g v)
      (!- g `(var ,v) 'Nat)
      (== `(,g ,v) q))))
 '100)

(define fix
  (lambda (f)
    (letrec ([g (lambda (x)
		  ((f g) x))])
      g)))

(test-equal? "With `fix`, fact has a type"
  (run 1 (q)
    (fresh (Γ)
      (!- Γ
         '((fix (lambda (!)
                  (lambda (n)
                    (if (zero? n)
                        1
                        (* n (! (sub1 n)))))))
           5)
         q)))
  '(Nat))

;; The following test demonstrates an interesting property:

(test-equal? "Just because a program typechecks doesn't mean it will terminate."
  (run 1 (q)
    (fresh (Γ)
      (!- Γ
         '((fix (lambda (!)
                  (lambda (n)
                    (* n (! (sub1 n))))))
           5)
         q)))
  '(Nat))

(test-equal? "What cons should look like"
 (run* (q) (!- '() '(cons (zero? 1) (zero? 0)) q))
 '((pairof Bool Bool)))

(test-equal? "Cons of other structures"
 (run* (q) (!- '() '(cons (zero? 1) (cons (zero? 1) (zero? 0))) q))
 '((pairof Bool (pairof Bool Bool))))

(test-equal? "cons of arbitrary elements"
 (run* (t) (!- '() '(lambda (x) (cons x x)) t))
 '((_.0 -> (pairof _.0 _.0))))

(test-equal? "cons works with both Bool and Nat"
 (run* (t) (!- '() '(lambda (x) (lambda (y) (cons (zero? x) (* x y)))) t))
 '((Nat -> (Nat -> (pairof Bool Nat)))))

(test-equal? "car lets us take elements of a pair"
 (run* (t) (!- '() '(lambda (x) (zero? (car x))) t))
 '(((pairof Nat _.0) -> Bool)))

(test-equal? "We can get atomic values out of cons structures"
 (run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons 0 1)) t))
 '(Bool))

(test-equal? "We can get atomic values out of cons structures even with mixed types"
 (run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons 0 nil)) t))
 '(Bool))

(test-equal? "We can get atomic values out of cons structures even with nested structures and mixed types"
 (run* (t) (!- '() '((lambda (x) (car x)) (cons (cons 0 0) nil)) t))
 '((pairof Nat Nat)))

(test-equal? "We can get atomic values out of cons structures even with nested structures and mixed types"
 (run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons nil 0)) t))
 '())

(test-equal? "cdr lets us take elements of a pair" 
 (run* (t) (!- '() '(lambda (x) (zero? (cdr x))) t))
 '(((pairof _.0 Nat) -> Bool)))

(test-equal? "With cdr, we can get atomic values out of cons structures"
 (run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 1)) t))
 '(Bool))

(test-equal? "With cdr, we can get atomic values out of cons structures even with mixed types"
 (run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 nil)) t))
 '())

(test-equal? "With cdr, we can get atomic values out of cons structures even with mixed types"
 (run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons nil 0)) t))
 '(Bool))

#| Just Dessert |#

;; We haven't yet implemented let, and we haven't implemented
;; polymorphism. You should go ahead and do that. This requires some
;; additional pondering and thinking, and AFAIK touches the limits of
;; this approach to type inference and typechecking.

;; (run* (q) (⊢ '()
;;              '(let ((f (lambda (x) x)))
;;                 (cons (f 1) (f t)))
;;              q))
;; '((ℕ × 𝔹))


;; (run* (q) (⊢ '()
;;              '((λ (f) (cons (f 1) (f t))) (lambda (x) x))
;;              q))
;; '()

;; (run* (q) (⊢ '()
;;              '(let ((g (lambda (x) x)))
;;                 (let ((f (lambda (x) x)))
;;                   (g (cons ((g f) 1) (f (g t))))))
;;              q))
;; '((ℕ × 𝔹))


