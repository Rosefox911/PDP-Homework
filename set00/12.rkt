;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; make-posn : 2 Number -> Creates position for x and y
;; GIVEN: two value
;; RETURNS: Their position on x y axis
;; Examples:
;; (make-posn 5 3) -> (make-posn 5 3)
;; (make-posn 8 9) -> (make-posn 8 9)

(make-posn 5 3)

;; posn? : Number -> Boolean
;; GIVEN: A value
;; RETURNS: Whether the input is a posn or not
;; Examples:
;; (posn? 5) -> false
;; (posn? true) -> false
;; (posn? (make-posn 2 1)) -> true
(posn? 5)
(posn? true)

;; posn-x : Number -> Number
;; GIVEN: A posn
;; RETURNS: The value that corresponds to x within the posn.
;; Examples:
;; (posn-x (make-posn 8 5)) -> 8
;; (posn-x (make-posn 42 15)) -> 42
(posn-x (make-posn 8 5))
(posn-x (make-posn 42 15))

;; posn-y : Number -> Number
;; GIVEN: A posn
;; RETURNS: The value that corresponds to y within the posn.
;; Examples:
;; (posn-y (make-posn 8 5)) -> 5
;; (posn-y (make-posn 42 15)) -> 15
(posn-x (make-posn 8 5))
(posn-x (make-posn 42 15))