#lang racket

#| Object-oriented Programming |# 

;; I invented the term 'Object-Oriented', and I can tell you I did not
;; have C++ in mind.

;; Alan Kay
 
#| Assignment Guidelines |#

;; This is a slight extension of our development from class
;; implementing a stack in an OO style in Racket.

;; We will start you here with an implementation of a stack as
;; designed in the textbook, and we will ask you to implement another
;; elementary data structure, and we will ask you also to *use* one of
;; these data structures as a component of some larger structure.

;; This will not be an overly challenging assignment, and reaching the
;; middle of Ch 12 in SAOP should be sufficient to get you going.

#| Starter Code |#

(define invalid-method-name-indicator "unknown")

(define (base-object . msg)
  (match msg
    [`(type) "base-object"]
    [else invalid-method-name-indicator]))

(define (delegate obj msg)
  (apply obj msg))

;; This is a modernized, Racketized implementation of the `send`
;; function implemented in Ch 12.

(define (send-message . args)
  (match args
    [`(,obj ,msgname . ,params)
     (let ([try (apply obj `(,msgname . ,params))])
       (cond
         [(eqv? try invalid-method-name-indicator)
          (error 'send-message "Bad method name: ~s sent to a ~s object~n" msgname (obj 'type))]
         [else try]))]))

;; This is an implementation of Program 12.2. I translate it to
;; slightly more modern Racket. 

(define (box-maker init)
  (let ([contents init])
    (λ msg
      (match msg
        [`(type) "box"]
        [`(show) contents]
        [`(update! ,x) (set! contents x)]
        [else (delegate base-object msg)]))))

;; This is an implementation of Program 12.12. I translate it to
;; slightly more modern Racket. We drop the `size` and `print` and
;; `top` operators, implement a pop operation that returns the removed
;; element as a value. To peek, our user will have to pop and then
;; push back.

(define (stack-maker . init)
  (let ([stk init])
    (λ msg
      (match msg
        [`(type) "stack"]
        [`(empty?) (empty? stk)]
        [`(push! ,x)
         (set! stk (cons x stk))]
        ;; [`(peek)
        ;;  (if (empty? stk)
        ;;      (error 'peek "attempt to peek from empty stack")
        ;;      (car stk))]
        [`(pop!)
         (if (empty? stk)
             (error 'pop! "attempt to pop from empty stack")
             (let ([ans (car stk)])
               (set! stk (cdr stk))
               ans))]
        [else (delegate base-object msg)]))))

(define s1 (stack-maker))
(define s2 (stack-maker 'abra 'kadabra))
(send-message s1 'type)
(send-message s1 'push! 'dabba)
(send-message s2 'empty?)
(send-message s2 'push! '110)
;; (send-message s2 'peek)
(send-message s2 'push! 'yabba)
(send-message s2 'pop!)
;; (send-message s1 'peek)
(send-message s2 'type)

;; #| Problems |# 

;; 1. SAOP implements a two-pointer queue data structure directly. We
;; can instead implement a queue using two-stacks. The two-stack queue
;; implementation is a common approach. If you're not familiar with
;; this approach, see the PDF Okasaki-queue.pdf on the repo. We are
;; interested in the OO design of a two-stack queue. Define and
;; implement `queue-maker`, not by copying the implementation in the
;; text, and certainly not by copying in a Racket implementation, but
;; by mirroring the implementation. Your implementation should
;; construct queues that support `empty?`, `enqueue!`, `dequeue!`, and
;; `peek`. Your dequeue! should return the dequeued value. We're
;; assessing this based on OO design; you can pass all of these tests
;; and not have an OO design. Make sure and use two of our stacks
;; inside each queue, and make sure to use message passing to send and
;; receive messages.



(define q (queue-maker 'abc 'def 'ghi))
(send-message q 'type)
(send-message q 'empty?)
(send-message q 'enqueue! 'cat)
(send-message q 'enqueue! 'dog)
(send-message q 'enqueue! 'horse)
(send-message q 'dequeue!)
(send-message q 'enqueue! 'turtle)
(send-message q 'enqueue! 'fish)
(send-message q 'dequeue!)

;; 2. Since we already have a stack, and we already have a piece of
;; mutable state, and we darn well already have functions, let's build
;; a DFA! Here's my plan. We can treat the tape as a stack that begins
;; with some data on it. We can treat the DFA's current state as the
;; thing inside some box. I thought about doing a PDA because of the
;; stack but that's bothersome. We'll leave that for a future term. 


(define δ
  (λ (pr)
    (match pr
      [`(q1 . a) 'q2]
      [`(q2 . a) 'q2]
      [`(q1 . b) 'q1]
      [`(q2 . b) 'q1])))

(define d (dfa-maker 'q1 δ (list 'q2)))
(send-message d 'type)
(send-message d 'show-state)
(send-message d 'set-tape! 'a 'b 'b 'b 'b 'a 'b)
(send-message d 'advance!)
(send-message d 'advance!)
(send-message d 'advance!)
(send-message d 'advance!)
(send-message d 'advance!)
(send-message d 'in-accept?)



;; #| Just Dessert |#

;; ;; It won't be statically checked, but given two implementations of a
;; ;; particular interface, we can imagine extending. Implement a visitor
;; ;; here, and give us (yourself) a picture of what a visitor looks
;; ;; like and what it's like to add functionality by visitor. 

