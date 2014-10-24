;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require racket/format)
(require "extras.rkt")

(provide
 initial-state
 next-state
 accepting-state?
 error-state?)

;; DATA DEFINITIONS

;; A KeyEvent is a character string
;; A State1KeyEvent is one of the following character strings:
;; -- "a" interp: causes the machine to remain in state 1
;; -- "b" interp: causes the machine to shift to state 2
;; -- all else interp: causes the machine to go into the error state.

;; state1-fn : State1KeyEvent -> ??
;(define (state1-fn key)
;  [cond
;    [(equal? keyevent "a")...]
;    [(equal? keyevent "b")...]
;    [else ...]])

;; A State2KeyEvent is one of the following character strings:
;; -- "c" interp: causes the machine to remain in state 2
;; -- "d" interp: causes the machine to remain in state 2
;; -- "e" interp: causes the machine to shift to state 3
;; -- all else interp: causes the machine to go into the error state.
;(define (state2-fn key)
;  [cond
;  [(equal? keyevent "c")...]
;  [(equal? keyevent "d")...]
;  [(equal? keyevent "e")...]
;  [else ...]])


;; A State is a number between one and four, inclusive.
;; -- 1 interp: represents the initial state
;; -- 2 interp: represents state 2
;; -- 3 interp: represents the accepting state
;; -- 4 interp: represents the error state
;; The initial state is before the machine has recieved a "b" KeyEvent.
;; State 2 is after the machine has recieved a "b" KeyEvent.
;; The accepting state is after the machine has recieved an "e" KeyEvent and enters the final state.
;; The error state is after the machine recieves a string that does not match the current state.

;; state-fn : State -> ??
;; (define (state-fn st)
;; [cond
;; [(= state st)...]
;; [(= state st)...]
;; [(= state st)...]
;; [(= state st)...]])

;;initial-state : Number -> State
;;GIVEN: a number
;;RETURNS: a representation of the initial state
;;of your machine.  The given number is ignored.
;; Example:
;; (initial-state 10) -> 1
;; (initial-state 50) -> 1
;; Strategy: Functional Composition

(define (initial-state number)
  1)

;; Tests
;(define-test-suite initial-state-tests
;  (check-equal? (initial-state 10) 1 "It did not give state as 1!")
;  (check-equal? (initial-state 50) 1 "It did not give state as 1!"))
; (run-tests initial-state-tests)

;; next-state : State KeyEvent -> State
;; GIVEN: a state of the machine and a keyevent.
;; RETURNS: the state that should follow the given key event.  A key
;; event that is to be discarded should leave the state unchanged.
;; Examples:
;; (next-state 1 a) -> 1
;; (next-state 1 b) -> 2
;; (next-state 2 c) -> 2
;; (next-state 2 d) -> 2
;; (next-state 2 e) -> 3
;; (next-state 2 y) -> 4
;; (next-state 2 z) -> 4
;; STRATEGY: Structural Decomposition on state : State

(define (next-state state keyevent)
  [cond
    [(= state 1) (check-keyevent-state-1 keyevent)]
    [(= state 2) (check-keyevent-state-2 keyevent)]
    [(= state 3) (accepting-state? state)]
    [(= state 4) (error-state? state)]])

;; Tests
;(define-test-suite next-state-tests
;; (check-equal? (next-state 1 a) 1 "It did not return 1!")
;; (check-equal? (next-state 1 b) 2 "It did not return 2!")
;; (check-equal? (next-state 2 c) 2 "It did not return 2!")
;; (check-equal? (next-state 2 d) 2 "It did not return 2!")
;; (check-equal? (next-state 2 e) 3 "It did not return 3!")
;; (check-equal? (next-state 2 y) 4 "It did not return 4!")
;; (check-equal? (next-state 2 z) 4 "It did not return 4!")
; (run-tests next-state-tests)

;; check-keyevent-state-1 : KeyEvent -> State
;; GIVEN: a keyevent.
;; RETURNS: the state that will follow, given the keyevent.
;; EXAMPLES:
;; (check-keyevent-state-1 "a") -> 1
;; (check-keyevent-state-1 "b") -> 2
;; (check-keyevent-state-1 "c") -> 4
;; STRATEGY: Structural Decomposition on keyevent : KeyEvent

(define (check-keyevent-state-1 keyevent)
  [cond
    [(equal? keyevent "a") 1]
    [(equal? keyevent "b") 2]
    [else 4]])


;; Tests
;(define-test-suite check-keyevent-state-1-tests
;(check-equal? (check-keyevent-state-1 "a") 1 "It did not return 1!")
;(check-equal? (check-keyevent-state-1 "b") 1 "It did not return 2!")
;(check-equal? (check-keyevent-state-1 "c") 1 "It did not return 4!"))
;(run-tests check-keyevent-state-1-tests)

;; check-keyevent-state-2 :  KeyEvent -> State
;; GIVEN: a keyevent.
;; RETURNS: the state that will follow, given the keyevent.
;; EXAMPLES:
;; (check-keyevent-state-2 "c") -> 1
;; (check-keyevent-state-2 "d") -> 2
;; (check-keyevent-state-2 "e") -> 3
;; (check-keyevent-state-2 "f") -> 4
;; STRATEGY: Structural Decomposition on keyevent : KeyEvent

(define (check-keyevent-state-2 keyevent)
  [cond
  [(equal? keyevent "c") 2]
  [(equal? keyevent "d") 2]
  [(equal? keyevent "e") 3]
  [else 4]])


;; Tests
;(define-test-suite check-keyevent-state-2-tests
;(check-equal? (check-keyevent-state-2 "c") 2 "It did not return 2!")
;(check-equal? (check-keyevent-state-2 "d") 2 "It did not return 2!")
;(check-equal? (check-keyevent-state-2 "e") 3 "It did not return 3!")
;(check-equal? (check-keyevent-state-2 "f") 4 "It did not return 4!"))
;(run-tests check-keyevent-state-2-tests)

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; Examples: 
;; (accepting-state? 1) -> false
;; (accepting-state? 2) -> false
;; (accepting-state? 3) -> true
;; (accepting-state? 4) -> false
;; Strategy: Structural Decomposition on state : State

 (define (accepting-state? state)
 [cond
 [(= state 1) false]
 [(= state 2) false]
 [(= state 3) true]
 [(= state 4) false]])

;; Tests
;(define-test-suite accepting-state?-tests
;  (check-equal? (accepting-state? 1) false "It did not return false!")
;  (check-equal? (accepting-state? 2) false "It did not return false!")
;  (check-equal? (accepting-state? 3) true "It did not return true!")
;  (check-equal? (accepting-state? 4) false "It did not return false!") 
; (run-tests accepting-state?-tests)

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the string seen so far does not match the specified
;; regular expression and cannot possibly be extended to do so.
;; Examples:
;; (error-state? 1) -> false
;; (error-state? 2) -> false
;; (error-state? 3) -> false
;; (error-state? 4) -> true
;; Strategy: Strucuctural Decomposition on state: State

 (define (error-state? state)
 [cond
 [(= state 1) false]
 [(= state 2) false]
 [(= state 3) false]
 [(= state 4) true]])

;; Tests
;(define-test-suite initial-state-tests
;  (check-equal? (error-state? 1) false "It did not return false!")
;  (check-equal? (error-state? 2) false "It did not return false!")
;  (check-equal? (error-state? 3) false "It did not return false!")
;  (check-equal? (error-state? 4) true "It did not return true!") 
; (run-tests error-state?-tests)