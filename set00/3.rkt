;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

;; f->c : Number -> Number
;; GIVEN: a temperature in degrees Fahrenheit as an argument
;; RETURNS: the equivalent temperature in degrees Celcius.
;; Examples:
;; (f->c 32)  => 0
;; (f->c 100) => 37.77777777777778
(define (f->c fahrenheit)
  (* (- fahrenheit 32) (/ 5 9)))
  
;; Tests
(define-test-suite tester
(check-= (f->c 100) 37.77777777777778 0.1 "100 Fahrenheit should be 37.77777777777778.")
(check-= (f->c 32) 0 0.1 "32 Fahrenheit should be 0."))
(run-tests tester)