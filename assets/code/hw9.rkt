#lang racket
(require "parenthec.rkt")

#| Assignment 9: ParentheC Interpreter |# 

;; Code should run as fast as necessary, but no faster; something
;; important is always traded away to increase speed.
;;
;; Richard Pattis

;; This, but un-ironically:
;; https://old.reddit.com/r/ProgrammerHumor/comments/81re59/why_i_never_worry_about_function_or_class/

#| ===== Assignment Guidelines ===== |# 


;; This assignment relies on your successful completion of hw7. If you
;; haven't successfully completed hw7 and maintained the versions of
;; your code along the way, please completely correct that before
;; starting this assignment. You will be much happier. 

;; Your assignment is to complete the transformation of your
;; interpreter from hw7 to a version we can translate to C.

;; When your interpreter is complete, turn it into C programs using
;; pc2c, and run the test program provided.

;; Save a new copy of your interpreter after you finish every
;; step. **We will expect you to have all of these intermediate files
;; available during your demonstration**. Also, you will likely need
;; at some point to go back to an older version of your interpreter to
;; correct a mistake; having copies of all previous steps will save a
;; lot of time.

;; You should turn in the `interp.pc` file that contains the exact
;; code you used to generate your C programs.

;; Once you've done the assignment, you must meet for an ~15m time
;; slot with the instructor or one of the TAs to demonstrate your
;; knowledge of your code. We will schedule meeting times, likely
;; during office hours, the week of April 5th to April 9th.

;; **You cannot receive a grade for this course until you complete this
;; assignment and demonstrate your understanding with one of the
;; instructors.**

;; Since you must successfully complete this assignment and the
;; corresponding code review in order to receive a passing grade for
;; this course (which is to say, this being a must-pass assignment)
;; this is the one assignment we /will/ take late. For assignments
;; handed in after the due date, credit is reduced proportionally to
;; lateness (dropping ~2 points/week).

;; If you haven't done so, consider reading the ParentheC paper,
;; "Using ParentheC to Transform Scheme Programs to C or How to Write
;; Interesting Recursive Programs in a Spartan Host". It is slightly
;; out of date viz. registerization, but can still prove a useful
;; resource.

;; Download pc2c.rkt and parenthec.rkt 

;; You will also need to use the following define-union for
;; expressions and main program:

(define-union expression
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))




;; PASTE YOUR VALOF-CPS, APPLY-K, APPLY-ENV, APPLY-CLOSURE, AND THEIR
;; CONSTRUCTORS HERE: 






;; (let ((f (lambda (f)
;;   	      (lambda (n)
;; 	        (if (zero? n) 
;; 		    1
;; 	            (* n ((f f) (sub1 n))))))))
;;   (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

;; The above program, translated
;; Notice that this test program is not quoted data. 

(define main 
  (lambda ()
    (value-of-cps 
     (expression_let 
      (expression_lambda
       (expression_lambda 
        (expression_if
         (expression_zero (expression_var 0))
         (expression_const 1)
         (expression_mult (expression_var 0) (expression_app (expression_app (expression_var 1) (expression_var 1)) (expression_sub1 (expression_var 0)))))))
      (expression_mult
       (expression_letcc
        (expression_app
         (expression_app (expression_var 1) (expression_var 1))
         (expression_throw (expression_var 0) (expression_app (expression_app (expression_var 1) (expression_var 1)) (expression_const 4)))))
       (expression_const 5)))
     (empty-env)
     (empty-k))))




#| ===== Project Part II ===== |# 

#| 

1. Here are the steps you will need to accomplish:

  1. Copy the relevant parts (Part II) of your fully correct version
     of your final product from assignment 7 into this file where
     indicated. Do not copy over the tests, however. Copy `parenthec.rkt`
     into the same directory as this file. Change the match-expression in
     value-of-cps to instead be a union-case-expression. Consult the
     ParentheC paper or the example from class to see how to do this. Make
     sure to remove the backquotes and commas in the patterns of what was
     your match expression. Ensure `main` is below your interpreter, and
     make sure it returns 120 when you invoke it.

  2. Transform your closure constructor to a define-union, change the
     match in apply-closure to instead use union-case, and ensure that your
     constructor invocations are preceeded with clos_, or something other
     than clos if you use a different name for your union. Make sure to
     remove the backquotes and commas in the patterns in what was your
     match expression.

  3. Transform your environment constructors to a define-union, change
     the match in apply-env to instead use union-case, and ensure all
     constructor invocations are preceeded with envr_, or something other
     than envr if you use a different name for your union. Make sure to
     remove the backquotes and commas in the patterns in what was your
     match expression.

  4. Transform your continuation constructors to a define-union,
     change the match in apply-k to instead use union-case, and ensure all
     constructor invocations are preceeded with kt_, or something other
     than kt if you use a different name for your union. Make sure to
     remove the backquotes and commas in the patterns in what was your
     match expression.

  5. Transform all your serious function calls to our A-normal form
     style, by adding let* above your serious calls, and ensuring that the
     names of the actual parameters to the serious calls are *exactly* the
     names of the formal parameters in the definition.

  6. Registerize the interpreter. Turn each let* expression to a begin
     block: the former let* bindings will become set! expressions, and the
     body becomes the invocation of a function of no arguments. Change all
     serious functions to be functions of no arguments. Define your global
     registers using define-registers at the top of the program.

  7. Change all of your (define name (lambda () ...)) statements to
     instead use define-label. Define your program counter at the top of
     the program using define-program-counter.

  8. Convert all label invocations into assignments to the program
     counter, and then add calls to mount-trampoline and
     dismount-trampoline. Note this will require modifying empty-k in your
     kt union, and the empty-k clause in the union-case inside apply-k. On
     the last line of main, print the register containing the final value
     of the program, e.g. (printf "Fact 5: ~s\n" v) See the parentheC
     document for notes on these steps.

  9. Comment out the lines `#lang racket`,
     `(require "parentheC.rkt")`. If you added it to your file, also 
     comment your invocation of main (that is `(main)` but leave the 
     definition of main intact). And save a copy of this file named 
     interp.pc. 

   After you have completed all of these steps, download `pc2c.rkt` to
   the same directory as this file, if you have not already done
   so. Using the DrRacket "open" dialog, open and run pc2c.rkt. This
   should load without errors. In the associated Racket REPL associated
   with your copy of pc2c.rkt, with **no** other files loaded,
   type `(pc2c "interp.pc" "a9.c" "a9.h")` which will generate C code
   from your interpreter. Compile the C program with the C compiler of
   your choice. The `login` linux machines have gcc installed, and you
   can find [binaries](http://gcc.gnu.org/install/binaries.html) for many
   different systems. This should look like `gcc a9.c`. Note, if you see a
   jmp_buf failure, this likely means you accidentally also tried to 
   compile `a9.h`. Don't do that. Alternately, you could use an [online C
   compiler](http://tutorialspoint.com/compile_c_online.php). This should
   generate an ELF executable, probably named `a.out` unless you
   specified otherwise. Run the generated executable; if you see the
   correct output, you are finished with this assignment/project.

   You should turn in the `interp.pc` file that contains the exact
   code you used to generate your C programs.

|#

#| ===== Just Dessert ===== |# 

;; 2. Add a callcc form to your interpreter that behaves like Racket's
;; call/cc. Change the test program to one that uses callcc and send
;; this, along with any other required changes, in an email to the
;; staff member with whom you conducted your code review.

;; 3. Hosts more spartan. If you have another nasty, spartan, ornery
;; host language in mind, and you'd like to hand compile to that
;; language or write yourself a transpiler for that host, do so. Send
;; Jason the upshot of your work. Interesting hosts might lack a
;; garbage collector, higher-order procedures, and/or tail-call
;; optimization. Java would, for instance, be an interesting choice
;; for a transpiler. DCL, Pascal, wasm, BLISS, etc might also make
;; interesting choices. If you are sufficiently interested and get
;; pre-clearance on the target, you could just do this /instead/ of
;; transpiling to C, esp, and I'm cool with that. However, I'm putting
;; this down here because I don't want anyone to be overly tempted the
;; wrong direction.

