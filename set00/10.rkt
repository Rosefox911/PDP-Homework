;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; sum-largest : Number -> Number
;; GIVEN: three values
;; RETURNS: The sum of the two largest ones
;; Examples:
;; (sum-largest 1 2 3) => 5
;; (sum-largest 6 7 8) => 15

(define (sum-largest x y z)
  (- (+ x y z) (determine-smallest x y z)))
  
;; sum-largest : 3 Number -> Number
;; GIVEN: three values
;; RETURNS: smallest value of the three
;; Examples:
;; (determine-smallest 1 2 3) => 1
;; (determine-smallest 10 9 11) => 9

(define (determine-smallest x y z)
  (if (and (< x y ) (< x z)) x (if(< y z) y z)))
  
;; Tests
(define-test-suite tester
(check-= (sum-largest 1 2 3) 5 0.1 "Sum of largest is not 5!")
(check-= (sum-largest 6 7 8) 15 0.1 "Sum of largest is not 15!")
  (check-= (determine-smallest 1 2 3) 1 0.1 "Sum of largest is not 5!")
(check-= (determine-smallest 10 9 11) 9 0.1 "Sum of largest is not 15!"))
(run-tests tester)