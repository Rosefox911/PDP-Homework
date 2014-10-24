;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps08-obstacles-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "obstacles.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define blob1 '((1 1) (1 2)))
(define board1 '((1 1) (1 3) (2 3)))

(define-test-suite obstacles-tests
  ;; this only tests to see if required functions were provided. This
  ;; does not completely test correctness. 
  (check-provided position-set-equal?)
  (check-provided obstacle?)
  (check-provided board-to-obstacles))

(run-tests obstacles-tests)