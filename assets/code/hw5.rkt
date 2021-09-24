#lang racket

#|  Parameter-passing Conventions |# 

;; These are your father's parentheses. Elegeant weapons for a more
;; ... civilized age.

;; - Randall Munroe,
;; [xkcd](http://imgs.xkcd.com/comics/lisp_cycles.png)
 
#| Assignment Guidelines |#

;; All four interpreters must handle the following: booleans, numbers,
;; variables, lambda, let, application, zero?, sub1, *, if, and
;; random.

;; Your val-of-cbr and val-of-cbv interpreters (not the other two)
;; must also handle begin2 and set!.

;; You will need to implement the empty-env, extend-env, apply-env
;; helpers. You can re-use this same set of helpers for every
;; interpreter.

;; You should use a **representation-dependent**, functional
;; representation of closures. (If it helps you to know, there's
;; nothing prohibiting you from making representation-independent
;; closures in whatever representation you wish. It is just slightly
;; unfortunate to re-implement them in every different interpreter.)

;; You should use //boxes// to help implement parameter-passing
;; conventions.

;; For more about boxes and the operations you can perform with them,
;; see the [Racket
;; Documentation](http://docs.racket-lang.org/reference/boxes.html).


#| Parameter-passing Conventions |# 

(define (value-of exp env)
  (match exp
    [`,b #:when (boolean? b) b]
    [`,n #:when (number? n)  n]
    [`(zero? ,n) (zero? (value-of n env))]
    [`(sub1 ,n) (sub1 (value-of n env))]
    [`(* ,n1 ,n2) (* (value-of n1 env) (value-of n2 env))]
    [`(if ,test ,conseq ,alt) (if (value-of test env)
                                (value-of conseq env)
                                (value-of alt env))]
    [`(let ([,x ,e]) ,b) (let ((a (value-of e env)))
                           (value-of b (extend-env x a env)))]
    [`(begin2 ,e1 ,e2) (begin (value-of e1 env) (value-of e2 env))]
    [`(random ,n) (random (value-of n env))]
    [`,y #:when (symbol? y) (apply-env env y)]
    [`(lambda (,x) ,body) (λ (a) (value-of body (extend-env x a env)))]
    [`(,rator ,rand) ((value-of rator env)
                      (value-of rand env))]))


#| 

1. val-of-cbv, your call-by-value interpreter. Implement this by
/boxing/ values before putting them in the environment, and add
our "`(rator ,y) #:when (symbol? y)" line that directly retrieves it's
value from the environment. This line should have the "(box (unbox
...))"  on the right-hand side. We're doing this because it'll help to
set up for the next interpreter.

|#



#| 

2. Here, implement val-of-cbr, your call-by-reference
interpreter. This should involve only a slight change from your
val-of-cbv interpreter.

|# 


#| 

3. Implement val-of-cbname, your call-by-name interpreter. This should
involve only a slight change from your val-of-cbr interpreter.

|# 



#| 

4. Implement val-of-cbneed, your call-by-need interpreter. This should
involve only a small change from your val-of-cbname interpreter.

|# 



#| Brainteasers - 5400 Only |#

#| 

5. ["CONS should not Evaluate its
Arguments"](http://www.cs.indiana.edu/pub/techreports/TR44.pdf), a
highly influential 1970s PL paper, points out the need for lazy data
structures. Cons that does not evaluate its arguments is easy to
implement as a macro in Racket, but you will be adding it to your
interpreter. To your "val-of-cbv" interpreter, add "cons^" that does
NOT evaluate its arguments strictly (in other words, it evaluates them
lazily). You should also create the corresponding versions of car and
cdr ("car^" and "cdr^") that operate on "cons^". You should add the
strict versions (regular "cons", "car", "cdr") to your val-of-cbv
interpreter along with add1, empty list, let, and null?. Ensure that
this cons test evaluates to '(1 2 3 4 5).

|# 

(define cons-test
  '(let ((fix (lambda (f)
               ((lambda (x) (f (lambda (v) ((x x) v))))
                (lambda (x) (f (lambda (v) ((x x) v))))))))
      (let ((map (fix (lambda (map)
                        (lambda (f)
                          (lambda (l)
                             (if (null? l)
                                 '()
                                 (cons^ (f (car^ l))
                                        ((map f) (cdr^ l))))))))))
        (let ((take (fix (lambda (take)
                           (lambda (l)
                             (lambda (n)
                               (if (zero? n)
                                   '()
                                    (cons (car^ l) 
                                          ((take (cdr^ l)) (sub1 n))))))))))
          ((take ((fix (lambda (m)
                         (lambda (i)
                           (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))

#| Just Dessert |#

#| 

6. To which value-of interpreter is norm, the dessert from the last
assignment, most similar? Why? For your choice of interpreter, give an
example expression where that interpreter still behaves differently
from norm.

|# 

#| 

Answer:


|# 

#| 

7. Loeb (and Moeb) are cool functions. Read
[this](https://github.com/quchen/articles/blob/master/loeb-moeb.md)
and do something neat with it. You could get it working in your
call-by-need interpreter, for instance. Or make a spreadsheet thingy
with it, I guess. If you have trouble with the syntax, well, then
[learn you a Haskell for great good!](http://learnyouahaskell.com).

|# 

