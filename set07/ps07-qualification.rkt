;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ps07-qualification) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)

(require "pitchers.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define-test-suite pitcher-tests
  ;; this only tests to see if required functions were provided. 
  ;; This does not test correctness at all.
  (check-provided list-to-pitchers)
  (check-provided pitchers-to-list)
  (check-provided pitchers-after-moves)
  (check-provided make-move)
  (check-provided move-src)
  (check-provided move-tgt)
  (check-provided solve)
  )

(run-tests pitcher-tests)