;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; sq : NonNegativeNumber => Number
;; GIVEN: a number
;; RETURNS: the square root of that number
;; Examples:
;; (sq 5) => 2.23606797749979
;; (sq 16) => 4

(define (sq number) (sqrt number))

;; Tests
(define-test-suite tester
(check-= (sq 5) 2.23606797749979 0.1 "Square root of 5 is 2.23606797749979.")
(check-= (sq 16) 4 0.1 "Square root of 16 is 4."))
(run-tests tester)