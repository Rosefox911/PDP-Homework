;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit )
(require rackunit/text-ui)
;; tip : NonNegNumber Number[0.0,1.0] -> Number
;; GIVEN: the amount of the bill in dollars and the
;; percentage of tip
;; RETURNS: the amount of the tip in dollars.
;; Examples:
;; (tip 10 0.15)  => 1.5
;; (tip 20 0.17)  => 3.4
(define (tip bill tippercentage ) (* bill tippercentage))

;;Tests
;; Tests
(define-test-suite tester
(check-= (tip 10 0.15) 1.5 0.1 "15% of 10 should be 1.5")
(check-= (tip 20 0.17) 3.4 0.1 "17% of 20 should be 3.4"))
(run-tests tester)