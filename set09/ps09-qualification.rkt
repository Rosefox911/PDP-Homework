#lang racket

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

(require "1.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

;; these check make-world, on-key, and get-shapes
(define w1 (make-world 5))
(define w2 (send w1 on-key "r"))
(define r1 (first (send w2 get-shapes)))

(define empty-canvas (empty-scene 10 10))

(define-test-suite ps09-tests
  ;; this only tests to see if required functions were provided. This
  ;; does not completely test correctness. 

  (check-provided run)

  (check-provided (send w1 on-tick))
  (check-provided (send w1 on-mouse 3 3 "button-down"))
  (check-provided (send w1 add-to-scene empty-canvas))
  (check-provided (send w1 get-x))
  (check-provided (send w1 get-y))
  (check-provided (send w1 get-selected?))

  (check-provided (send r1 on-tick))
  (check-provided (send r1 on-mouse 3 3 "button-down"))
  (check-provided (send r1 on-key "n"))
  (check-provided (send r1 add-to-scene empty-canvas))
  (check-provided (send r1 get-x))
  (check-provided (send r1 get-y))
  (check-provided (send r1 is-selected?))

)

(run-tests ps09-tests)