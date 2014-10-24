#lang racket

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)

(require "buddies.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

;; these check make-world, on-key, and get-shapes
(define w1 (make-world))
(send w1 on-key "r")
(define r1 (first (send w1 get-shapes)))

(define empty-canvas (empty-scene 10 10))

(define-test-suite the-tests
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
  (check-provided (send r1 get-color)))

(run-tests the-tests)
