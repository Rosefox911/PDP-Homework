;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; make-posn : 2 Values -> 2 Values
;; GIVEN: two value
;; RETURNS: Creates a posn with the true values and shows them
;; Examples:
;; (make-posn true false) => (make-posn true false)
(make-posn true false)

;; posn-x : Number -> Number
;; GIVEN: a value
;; RETURNS: the correspondiong x value defined in make-posn
;; Examples:
;; (posn-x (make-posn 8 5)) => 8
;; (posn-x (make-posn 5 8 )) => 5

(posn-x (make-posn true false))