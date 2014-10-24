;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; circ-area : NonNegativeNumber => Number
;; GIVEN: a number that represents the radius
;; RETURNS: the circumference based on radius
;; Examples:
;; (circ-area 6) => 113.094
;; (circ-area 8) => 201.056

(define (circ-area r) (* 3.1415 (expt r 2)))

;; Tests
(define-test-suite tester
(check-= (circ-area 6) 113.094 0.1 "Circumference of 6 should be 113.094.")
(check-= (circ-area 8) 201.056 0.1 "Circumference of 8 should be 201.056."))
(run-tests tester)