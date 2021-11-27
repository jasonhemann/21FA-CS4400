#lang racket

;; Where we are in the class

;; Object State + methods

;; Closure 1 method + internal state

;; (send a-particular-closure 'abc)

;; OO - messages
;; building objects
;; static data vs obj level data

;; [`,any #:when (symbol? any)]
;; [`(,expr ,env ,k) ....]
;; [`(,a . ,d)]

(define invalid-method-name-indicator "unknown")

(define base-object
  (λ msg
    (match msg
      [`(type) "base object"]
      [else invalid-method-name-indicator])))

(define delegate
  (λ (obj msg)
    (apply obj msg)))

(define stack-maker
  (lambda (init-val)
    (let [(val init-val)]
      (λ (msg)
        (match msg
          [`(type) "stack"]
          [`(peek) (if (empty? val)
                       (error 'stack "peek at empty stack")
                       (first val))]
          [`(pop!)
           (if (empty? val)
               (error 'stack "peek at empty stack")
               (let ([ans (first val)])
                 (set! val (cdr val))
                 ans))]
          [`(push! ,x) (set! val (cons x val))]
          [`(reset!) (set! val init-val)]
          [else (delegate base-object msg)])))))

(define box-maker
  (lambda (init-val)
    (let [(val init-val)]
      (λ (msg)
        (match msg
          [`(type) "box"]
          [`(show) val]
          [`(update! ,x) ;; only for effect
           (set! val x)]
          [`(swap! ,x) ;; set the val, return the old
           (let ([ans val])
             (set! val x)
             ans)]
          [`(reset!)
           (set! val init-val)]
          [else (delegate base-object msg)])))))



(define b (box-maker 'abc))

(define send
  (λ args
    (let ([obj (first args)]
          [msg (rest args)])
      (let ([try (obj msg)])
        (if (eqv? invalid-method-name-indicator try)
            (error 'send
                   "bad method name ~s sent to ~s~n"
                   (first msg)
                   (obj 'type))
            try)))))




(λ (s z)
  (s (s (s z))))

(define kar (λ (a) (λ (d) a)))
(define kdr (λ (a) (λ (d) d)))
(define kons (λ (a) (λ (d) (λ (c) ((c a) d)))))
