;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps01-render-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

(require "3.rkt")                      

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define diff-exp1 (make-diff-exp 20 10))
(define mult-exp1 (make-mult-exp 10 5))

(define-test-suite expr-to-image-tests
  (check-equal? (diff-exp-rand1 diff-exp1) 20)
  (check-equal? (diff-exp-rand2 diff-exp1) 10)
  (check-equal? (mult-exp-rand1 mult-exp1) 10)
  (check-equal? (mult-exp-rand2 mult-exp1) 5)
  (check-true (image? (expr-to-image mult-exp1 true)))) 

"tests to see if functions are provided:"
(run-tests expr-to-image-tests)