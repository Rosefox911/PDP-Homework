;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; quadratic-root : Numbers => Number
;; GIVEN: three numbers
;; RETURNS: the result of their quadratic equation
;; Examples:
;; (quadratic-root 1 1 -4) => 1.5615528128088303
;; (quadratic-root 2 5 3) => -1


(define (quadratic-root a b c)
  (/ (+ (* b -1) (sqrt (- (expt b 2) (* 4 a c)))) (* 2 a)))

;; Tests
(define-test-suite tester
(check-= (quadratic-root 1 1 -4) 1.5615528128088303 0.1 "Quadratic root of 1, 1, -4 should be 1.5615528128088303.")
(check-= (quadratic-root 2 5 3) -1 0.1 "Quadratic root of 2 5 3 should be -1."))
(run-tests tester)