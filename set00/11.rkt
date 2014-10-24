;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
;; posn-x : Posn -> Number
;; GIVEN: a value
;; RETURNS: the correspondiong x value defined in make-posn
;; Examples:
;; (posn-x (make-posn 8 5)) => 8
;; (posn-x (make-posn 5 8 )) => 5

;; posn-y : Posn -> Number
;; GIVEN: a value
;; RETURNS: the correspondiong y value defined in make-posn
;; Examples:
;; (posn-y (make-posn 16 32)) => 32
;; (posn-y (make-posn 10 20)) => 20


;; posn-x: Displays the x value created with make-posn (or a structure), which is x on the xy axis
;; posn-y: Displays the y value created with make-posn (or a structure), which is y on the xy axis