;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps06-pretty-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "pretty.rkt")

(define expr1 (make-sum-exp (list 3 2)))
(define expr2 (make-mult-exp (list 4 5)))

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define-test-suite pretty-tests
  ;; this only tests to see if required functions were provided. 
  ;; This does not test correctness at all.
  (check-provided (expr-to-strings expr1 100))

  (check-provided (sum-exp-exprs expr1))
  (check-provided (mult-exp-exprs expr2)))

(run-tests pretty-tests)