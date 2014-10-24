;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require racket/format)
(require "extras.rkt")

(provide
 make-diff-exp 
 diff-exp-rand1 
 diff-exp-rand2
 make-mult-exp
 mult-exp-rand1
 mult-exp-rand2
 expr-to-image)

;;DATA DEFINITIONS

(define-struct diff-exp (rand1 rand2))
;; A Diff-exp is a (make-diff-exp Integer Integer)
;; Interpretation:
;; rand1 is the first number represented by this expression representing a subtraction.
;; rand2 is the second number represented by this expression representing a subtraction.
;; diff-exp-fn : Expression -> ??
;; (define (diff-exp-fn de)
;; (...
;; (diff-exp-rand1 de)
;; (diff-exp-rand2 de)))

(define-struct mult-exp (rand1 rand2))
;; A Multi-exp is a (make-mult-exp Integer Integer)
;; Interpretation:
;; rand1 is the first number represented by this expression representing a multiplication.
;; rand2 is the second number represented by this expression representing a multiplication.
;; diff-mult-fn : Expression -> ??
;; (define (diff-mult-fn me)
;; (...
;; (diff-mult-rand1 me)
;; (diff-mult-rand2 me)))

;; An Expression (expression) is one of
;; -- (make-diff-exp Integer Integer)
;; -- (make-mult-exp Integer Integer)
;; Interpretation: a diff-exp represents a difference,
;; and a mult-exp represents a multiplication
;; expression-fn : Expression -> ??
;; (define (expression-fn exp)
;; [cond
;;   [(diff-exp? expression)
;;    ...]
;;   [(mult-exp? expression)
;;    ...]])

;; An Operator is one of
;; -- "-"
;; -- "*"
;; Interpretation:
;; "-" represents subtraction
;; "*" represents multiplication

;; A Number1 is the first Integer extracted from the diff-exp or mult-exp structure
;; A Number2 is the second Integer extracted from the diff-exp or mult-exp structure

;; expr-to-image : Expression Boolean -> Renders expression as an image
;; GIVEN: an expression and a Boolean
;; RETURNS: the image representation of that expression, 
;; if the Boolean is true it will represent it in the traditicial mathematical sense.
;; If the Boolean is false, it will represent it as an expression Racket interpret.
;; EXAMPLES:
;; (expr-to-image (make-diff-exp 33 22) true) -> (33 - 22)
;; (expr-to-image (make-diff-exp 33 22) false) -> (- 33 22)
;; (expr-to-image (make-mult-exp 33 22) true) -> (33 * 22)
;; (expr-to-image (make-mult-exp 33 22) false) -> (* 33 22)
;; STRATEGY: Structural Decompositon on expression : Expression
 
 (define (expr-to-image expression Boolean)
 [cond
   [(diff-exp? expression)
    (string-construct Boolean "-" 
                      (diff-exp-rand1 expression) 
                      (diff-exp-rand2 expression))]
   [(mult-exp? expression)
    (string-construct Boolean "*" 
                      (mult-exp-rand1 expression) 
                      (mult-exp-rand2 expression))]])
 
;; TESTS
;(define-test-suite expr-to-image-tests
; (check-true (image? (expr-to-image (make-diff-exp 33 22) true)) "Did not create image!")
; (check-true (image? (expr-to-image (make-diff-exp 33 22) false)) "Did not create image!")
; (check-true (image? (expr-to-image (make-mult-exp 33 22) true)) "Did not create image!")
; (check-true (image? (expr-to-image (make-mult-exp 33 22) false)) "Did not create image!"))
;; (run-tests expr-to-image-tests) 
 
;; string-construct : Boolean Operator Number1 Number2 -> Constructs the image representing the Expression passed from expr-to-image
;; GIVEN: a boolean, operator, number1 and number2
;; RETURNS: the image representation of that expression.
;; if the Boolean is true it will represent it in the traditicial mathematical sense.
;; If the Boolean is false, it will represent it as an expression Racket interpret.
;; EXAMPLES:
;; (string-construct true "-" 1 1) -> (1 - 1)
;; (string-construct false "-" 1 1) -> (- 1 1)
;; (string-construct true "*" 1 1) -> ( 1 * 1)
;; (string-construct false "*" 1 1) -> (* 1 1)
;; STRATEGY: Functional Composition
  
 (define (string-construct Boolean operator number1 number2)
 (if (equal? Boolean true) 
     (text (string-append "(" (number->string number1) " " operator " "
                          (number->string number2) ")") 11 "black")
     (text (string-append "(" operator " " (number->string number1) " "
                          (number->string number2) ")") 11 "black")))

;; TESTS
;(define-test-suite string-construct-tests
; (check-true (image? (string-construct true "-" 1 1)))
; (check-true (image? (string-construct false "-" 1 1)))
; (check-true (image? (string-construct true "*" 1 1)))
; (check-true (image? (string-construct false "*" 1 1))))
;; (run-tests string-construct-tests)
