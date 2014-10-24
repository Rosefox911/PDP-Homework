;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |9|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; divisibleby2: Number -> Number
;; GIVEN: a number
;; RETURNS: if that number is divisible by 2
;; Examples:
;; (divisibleby2 8 ) => true

;; Modulo is the method by which you get the remainder
;; Remainder is the product, while modulo is the operator
;; (modulo -21 4 ) = 3 while (remainder -21 4) = 1

(define (divisibleby2 x)
  (cond
    [(= (remainder x 2) 0) true]
    [else false])
)

;; Tests
(define-test-suite tester
(and (divisibleby2 8) true))
(run-tests tester)