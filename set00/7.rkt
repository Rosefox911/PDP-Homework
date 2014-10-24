;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; circumference : NonNegativeNumber -> Number
;; GIVEN: the radius r of a circle 
;; RETURNS: its circumference, using the formula 2 * pi * r.
;; Examples:
;; (circumference 1)  =>  6.283185307179586 
;; (circumference 0)  =>  0

(define (circumference r)
  (* 2 pi r)
)

;; Tests
(define-test-suite tester
(check-= (circumference 1) 6.283185307179586  0.1 "Circumference of 1 should be 6.283185307179586.")
(check-= (circumference 0) 0 0.1 "Circumference of 0 should be 0."))
(run-tests tester)