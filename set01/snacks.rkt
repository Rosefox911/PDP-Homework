;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snacks) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require "extras.rkt")

(provide
  initial-machine
  machine-next-state
  machine-chocolates
  machine-carrots
  machine-bank)

;; DATA DEFINITIONS

(define-struct machine(chocolate-bars-in-machine carrots-in-machine machine-bank customer-bank))
;; A Machine is a (make-machine PosInt PosInt PosInt PosInt
;; Interpretation
;; chocolate-bars-in-machine is the positive integer representation of the amount of chocolate bars available in the machine
;; carrots-in-machine is the positive integer representation of the amount of carrots available in the machine
;; machine-bank is the positive integer representation of the amount of cents in the machine's bank
;; customer-bank is the positive integer representation of the amount of cents the customer has entered into the machine
;; machine-fn : Machine -> ??
;; (define (machine-fn ma )
;; (...
;; (machine-chocolate-bars-in-machine ma)
;; (machine-carrots-in-machine ma)
;; (machine-bank ma)
;; (customer-bank ma))
;; PosInt represents a positive integer


;; A CustomerInput is one of
;; -- a PosInt          interp: insert the specified number of cents
;; -- "chocolate"       interp: request a chocolate bar
;; -- "carrots"         interp: request a package of carrot sticks
;; -- "release"         interp: return all the coins that the customer has put in

;; customerinput-fn : CustomerInput -> ??
;;(define (customerinput-fn a-ipt)
;;  [cond
;;    [(number? customerinput)
;;     ...]
;;    [(string=? customerinput "chocolate")
;;     ...]
;;    [(string=? customerinput "carrots") 
;;     ...]
;;    [(string=? customerinput "release") 
;;     ...]])

;; A State is one of
;; -- (make-machine PosInt PosInt PosInt PosInt)
;; -- (initial-machine PosInt PosInt)
;; Interpretation: 
;; a machine represents a vending machine being created with the specific perimeters in its chocolate bar, carrot, bank and customer bank
;; a initial-machine reperesents a vending machine being created with the specified perimeters in its chocolate bar and carrots field; bank and customer bank will be 0 by default.

;; state-fn : State -> ??
;; (define (state-fn st)
;;   (...
;;     (machine-chocolate-bar-in-machine st)
;;     (machine-carrots-in-machine st)
;;     (machine-machine-bank st)
;;     (machine-customer-bank state)))

;; A PosInt is a positive integer. 

;; initial-machine : PosInt PosInt -> Initializes the machine
;; GIVEN: two positive integers
;; RETURNS: initializes the machine with the amount of chocolate bars and carrots entered
;; With 0 cents in machine-bank and customer-bank
;; EXAMPLES:
;; (initial-machine 10 10) -> (make-machine 10 10 0 0)
;; (initial-machine 6 9) -> (make-machine 6 9 0 0)
;; (initial-machine 11 4) -> (make-machine 11 4 0 0)
;; STRATEGY: Functional Composition

(define (initial-machine amount-chocolate-bars amount-carrots)
  (make-machine amount-chocolate-bars amount-carrots 0 0))

;; TESTS
;(define-test-suite initial-machine-tests
; (check-equal? (initial-machine 10 10) (make-machine 10 10 0 0) "Result was NOT equal to (make-machine 10 10 0 0")
; (check-equal? (initial-machine 6 9) (make-machine 6 9 0 0) "Result was NOT equal to (make-machine 6 9 0 0)")
; (check-equal? (initial-machine 11 4) (make-machine 11 4 0 0)) "Result was NOT equal to (make-machine 11 4 0 0)")
;; (run-tests initial-machine-tests)

;; machine-next-state : Machine CustomerInput -> Machine
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's input
;; EXAMPLES:
;; (machine-next-state (make-machine 10 10 0 0) 200) -> (make-machine 10 10 0 200)
;; (machine-next-state (make-machine 10 10 30 175) "chocolate") -> (make-machine 9 10 205 0)
;; (machine-next-state (make-machine 20 10 30 175) "carrots") -> (make-machine 20 9 100 105)
;; (machine-next-state (make-machine 40 30 30 500) "release") -> (make-machine 30 30 30 0)
;; STRATEGY: Structural Decomposition on customerinput: CustomerInput

(define (machine-next-state state customerinput)
  [cond
    [(number? customerinput) 
     (increase-customer-bank state customerinput)]
    [(string=? customerinput "chocolate")
     (buy-chocolate-bar state)]
    [(string=? customerinput "carrots") 
     (buy-carrots state)]
    [(string=? customerinput "release") 
     (do-release state)]])

;; TESTS
;(define-test-suite machine-next-state-tests
;  (check-equal? (machine-next-state (make-machine 10 10 0 0) 200) (make-machine 10 10 0 200) "Result is NOT equal to (make-machine 10 10 0 200)")
;  (check-equal? (machine-next-state (make-machine 10 10 30 175) "chocolate") (make-machine 9 10 205 0) "Result is NOT equal to (make-machine 9 10 205 0)")
;  (check-equal? (machine-next-state (make-machine 20 10 30 175) "carrots") (make-machine 20 9 100 105) "Result is NOT equal to (make-machine 20 9 100 105)")
;  (check-equal? (machine-next-state (make-machine 40 30 30 500) "release") (make-machine 30 30 30 0) "Result is NOT equal to (make-machine 30 30 30 0)"))
;; (run-tests machine-next-state-tests)


;; increase-customer-bank : State PosInt -> Machine
;; GIVEN: a machine state and a positive integer
;; RETURNS: a machine
;; EXAMPLES: 
;; (increase-customer-bank (initial-machine 10 10) 100) -> (make-machine 10 10 0 100)
;; (increase-customer-bank (make-machine 10 10 100 100) 500) -> (make-machine 10 10 100 600)
;; STRATEGY: Structural Decomposition on state : State

(define (increase-customer-bank state customerinput)
(make-machine (machine-chocolate-bars-in-machine state) 
              (machine-carrots-in-machine state) 
              (machine-bank state)
              (+ (machine-customer-bank state) customerinput)))

;; TESTS
;(define-test-suite increase-customer-bank-tests
;  (check-equal? (increase-customer-bank (initial-machine 10 10) 100) (make-machine 10 10 0 100) "Result was NOT equal to (make-machine 10 10 0 100)")
;  (check-equal? (increase-customer-bank (make-machine 10 10 100 100) 500) (make-machine 10 10 100 600) "Result was NOT equal to (make-machine 10 10 100 600)"))
;; (run-tests increase-customer-bank-tests)

;; buy-chocolate-bar : State -> Machine
;; GIVEN: a machine state
;; RETURNS: a machine, if and only if you have enough in customer-bank to purchase the chocolate bar, else false.
;; EXAMPLES: 
;; (buy-chocolate-bar (make-machine 10 10 0 200)) -> (make-machine 9 10 175 25)
;; (buy-chocolate-bar (make-machine 10 10 0 175)) -> (make-machine 9 10 175 0)
;; (buy-chocolate-bar (make-machine 10 10 0 10)) -> false
;; (buy-chocolate-bar (make-machine 0 10 0 175)) -> false
;; STRATEGY: Structural Decomposition on state : State

(define (buy-chocolate-bar state)
 (if (and (<= 175 (machine-customer-bank state)) 
          (< 0 (machine-chocolate-bars-in-machine state)))
     (make-machine (- (machine-chocolate-bars-in-machine state) 1) 
     (machine-carrots-in-machine state) 
     (+ (machine-machine-bank state) 175) 
     (- (machine-customer-bank state ) 175)) 
     false))

;; Tests
;(define-test-suite buy-chocolate-bar-tests
;  (check-equal? (buy-chocolate-bar (make-machine 10 10 0 200)) (make-machine 9 10 175 25) "Result was NOT equal to (make-machine 9 10 175 25)")
;  (check-equal? (buy-chocolate-bar (make-machine 10 10 0 175)) (make-machine 9 10 175 0) "Result was NOT equal to (make-machine 9 10 175 0)")
;  (check-equal? (buy-chocolate-bar (make-machine 10 10 0 10)) false "Result was NOT false")
;  (check-equal? (buy-chocolate-bar (make-machine 0 10 0 175)) false "Result was NOT false"))
;; (run-tests buy-chocolate-bar-tests)

;; buy-carrots : State -> Machine
;; GIVEN: a machine state
;; RETURNS: a machine, if and only if you have enough in customer-bank to purchase the carrots, else false.
;; EXAMPLES: 
;; (buy-carrots (make-machine 10 10 0 200)) -> (make-machine 10 9 70 130)
;; (buy-carrots (make-machine 10 10 0 70)) -> (make-machine 10 9 70 0)
;; (buy-carrots (make-machine 10 10 0 60)) -> false
;; (buy-carrots (make-machine 10 0 0 70)) -> false
;; STRATEGY: Sturctural Decomposition on state : State


(define (buy-carrots state)
 (if (and (<= 70 (machine-customer-bank state)) 
          (< 0 (machine-carrots-in-machine state)))
     (make-machine (machine-chocolate-bars-in-machine state) 
     (- (machine-carrots-in-machine state) 1)  
     (+ (machine-machine-bank state) 70) 
     (- (machine-customer-bank state ) 70)) 
     false))

;; Tests
;(define-test-suite buy-carrots-tests
;  (check-equal? (buy-carrots (make-machine 10 10 0 200)) (make-machine 10 9 70 130) "Result is NOT (make-machine 10 9 70 130)")
;  (check-equal? (buy-carrots (make-machine 10 10 0 70)) (make-machine 10 9 70 0) "Result is NOT (make-machine 10 9 70 0)")
;  (check-equal? (buy-carrots (make-machine 10 10 0 60)) false "Result is NOT false")
;  (check-equal? (buy-carrots (make-machine 10 0 0 70)) false "Result is NOT false"))
;; (run-tests buy-carrots-tests)

;; do-release : State -> Machine
;; GIVEN: a machine state
;; RETURNS: a machine, if and only if you have money in customer-bank, which means you have entered money.
;; EXAMPLES: 
;; (do-release (make-machine 10 10 0 200)) -> (make-machine 10 10 0 0)
;; (do-release (make-machine 10 10 0 0)) -> false
;; STRATEGY: Structural Decomposition on state : State

(define (do-release state)
 (if (< 0 (machine-customer-bank state)) 
     (make-machine (machine-carrots-in-machine state) 
     (machine-carrots-in-machine state) 
     (machine-machine-bank state) 
     (- (machine-customer-bank state) (machine-customer-bank state)))
     false))

;; Tests
;(define-test-suite do-release-tests
;  (check-equal? (do-release (make-machine 10 10 0 200)) (make-machine 10 10 0 0) "Result is NOT (make-machine 10 10 0 0)")
;  (check-equal? (do-release (make-machine 10 10 0 0)) false) "Result is NOT (make-machine 10 10 0 0)")
;; (run-tests do-release-tests)

;; machine-chocolates : State -> NonnegInt
;; GIVEN: a machine state
;; RETURNS: the number of chocolate bars left in the machine
;; EXAMPLES: 
;; (machine-chocolates (initial-machine 1 1)) -> 1
;; (machine-chocolates (initial-machine 0 1)) -> 0
;; STRATEGY: Functional Composition

(define (machine-chocolates state)
   (machine-chocolate-bars-in-machine state))

;; Tests
;(define-test-suite machine-chocolates-tests
;  (check-equal? (machine-chocolates (initial-machine 1 1)) 1 "Result is NOT 1")
;  (check-equal? (machine-chocolates (initial-machine 0 1)) 0 "Result is NOT 0"))
;; (run-tests machine-chocolates-tests)


;; machine-carrots : State -> NonnegInt
;; GIVEN: a machine state
;; RETURNS: the number of packages of carrot sticks left in the machine
;; EXAMPLE: (machine-carrots (initial-machine 1 1)) -> 1
;; EXAMPLE: (machine-carrots (initial-machine 1 0)) -> 0
;; STRATEGY: Functional Composition

(define (machine-carrots state)
  (machine-carrots-in-machine state))

;; Tests
;(define-test-suite machine-carrots-tests
;  (check-equal? (machine-carrots (initial-machine 1 1)) 1 "Result is NOT 1")
;  (check-equal? (machine-carrots (initial-machine 1 0)) 0 "Result is NOT 0"))
;; (run-tests machine-carrots-tests)

;; machine-bank : State -> NonnegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; EXAMPLE: (machine-bank (initial-machine 1 1)) -> 0
;; EXAMPLE: (machine-bank (make-machine 10 10 100 10)) -> 100
;; STRATEGY: Functional Compossition

(define (machine-bank state)
  (machine-machine-bank state))

;; Tests
;(define-test-suite machine-bank-tests
;  (check-equal? (machine-bank (initial-machine 1 1)) 0 "Result is NOT 0")
; (check-equal? (machine-bank (make-machine 10 10 100 10)) 100 "Result is NOT 100"))
;; (run-tests machine-bank-tests)