;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname obstacles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALGORITHM
;; We are using a Depth-First Search (DFS) in this
;; problem. Instead of using list-ref we are keeping track of the current 
;; position using "current" which is increases at every recursive call to make 
;; sure we are handling the current position within the position set. 
;; We are using set-cons so an element not in the list is added in 
;; reverse order, ensuring we keep the currently formatted list. 
;; The general recursion starts in board-to-obstacles and search-adjacents
;; is called to find any possible adjacent blocks.
;; If a block is adjacent to the current block, then we know it is an obstacle.
;; However, we need to check the next element because the second block
;; can also be adjacent to another block. Once we have concluded that the
;; next element is not adjacent to the previous, we close that list
;;  and create a new list within the positionsetset for the next obstacle.
;; We do this till we reach the end of the list, and we have therefore, 
;; constructed all the obstacles.
;; END OF ALGORITHM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require rackunit/text-ui)
(require "sets.rkt")
(require "extras.rkt")

(provide
 position-set-equal?
 obstacle?
 board-to-obstacles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:

;; A Position is a 
;; --(list PosInt PosInt) (interp: is the x and y coordinates of the position)
;; TEMPLATE:
;; position-fn: Position -> ??
;(define (position-fn pos)
;  (...(first pos)
;      (second pos)))

;; A PositionSet is a
;; -- empty                       (interp: a PositionSet which is empty)
;; --(cons Position PositionSet)  (interp: a PositionSet
;;                                with a Position and a PositionSet)
;; WHERE: it has no duplicates
;; TEMPLATE
;; positionset-fn: PositionSet -> ??
;(define (positionset-fn poss)
;  (cond
;    [(empty? poss) ...]
;    [else
;     (...
;      (first poss)
;      (positionset-fn (rest poss)))]))

;; A PositionSetSet is a
;; -- empty                            (interp: a PositionSetSet 
;;                                               which is empty)         
;; --(cons PositionSet PositionSetSet) (interp: has a PositionSet
;;                                      and a PositionSetSet)
;; WHERE: No two PositionSets denote the same set of positions.
;; TEMPLATE:
;; positionsetset-fn: PositionSetSet -> ??
;(define (positionsetset-fn posss)
;  (cond
;    [(empty? posss)...]
;    [else
;     (...
;      (positionset-fn (first posss))
;      (positionsetset-fn (rest posss)))]))

;;  Nat denotes the set of natural numbers 
;; (also called the non-negative integers) 0, 1, 2, etc.
;; PosInt denotes the set of positive integers i.e 1,2,3..

;; END OF DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define ONE 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS FOR TESTS

(define POSITION-1 (list 1 2))
(define POSITION-2 (list 3 4))
(define POSITION-3 (list 2 3))
(define POSITION-4 (list 3 3))

(define POSITIONSET-1 (list POSITION-1))
(define POSITIONSET-2 (list POSITION-2 POSITION-3 POSITION-4))

(define ZERO 0)

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; position-set-equal? : PositionSet PositionSet -> Boolean
;; GIVEN: two PositionSets
;; RETURNS: true iff they denote the same set of positions.
;; EXAMPLS:
;; Check Tests
;; DESIGN STRATEGY: Function Composition

(define (position-set-equal? ps1 ps2)
  (set-equal? ps1 ps2))

;; TESTS
(define-test-suite position-set-equal?-tests
  (check-equal? (position-set-equal? (list 
                                      (list 1 2)
                                      (list 1 3)) 
                                     (list (list 1 3) 
                                           (list 1 2)))
                true
                "test for position-set-equal? when it is true")
  
  (check-equal? (position-set-equal? (list 
                                      (list 1 2)
                                      (list 1 5) 
                                      (list 1 3)) 
                                     (list (list 1 3) 
                                           (list 1 2)))
                false
                "test for position-set-equal? when it is false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; obstacle? : PositionSet -> Boolean
;; GIVEN: a PositionSet
;; RETURNS: true iff the set of positions would be an obstacle if they
;;          were all occupied and all other 
;;          positions were vacant.
;; EXAMPLES: 
;; (obstacle? POSITIONSET-1) -> true
;; DESIGN STRATEGY: Structural Decomposition on ps:PositionSet

(define (obstacle? ps)
  (cond 
    [(empty? ps) false]
    [else (obstacle-helper ps)]))

;; TESTS
(define-test-suite obstacle?-tests
  (check-equal? (obstacle? empty) 
                false "test for obstacles? 
when there are no list members")
  (check-equal?
   (obstacle? POSITIONSET-1)
   true
   "This should produce true, this has an obstacle.")
  (check-equal?
   (obstacle? (list (list 1 2) (list 3 4)))
   false
   "should return false, do not share edge"))

;; obstacle-helper : PositionSet -> Boolean
;; GIVEN: a PositionSet
;; RETURNS: true iff the set of positions would be an obstacle if they
;;          were all occupied and all other 
;;          positions were vacant.
;; EXAMPLES:
;; (obstacle-helper POSITIONSET-1)=> true
;; DESIGN STRATEGY: Fucntion composition
(define (obstacle-helper ps)
  (if (= (length ps) 1)
      true
      (obstacle-present? ZERO ps)))

;; obstacle-present? : Nat PositionSet -> Boolean
;; GIVEN: a n and a list of position
;; WHERE: n is the location of the position in the
;;        position set which is we are working on it keeps 
;;        increasing
;; RETURNS: true iff the set of positions would be an obstacle if they
;;          were all occupied and all other 
;;          positions were vacant.
;; EXAMPLES:
;; (obstacle-present? 0 (list (list 1 2) (list 2 1)))
;;  => false
;; DESIGN STRATEGY: HOFC
(define (obstacle-present? n ps)
  (andmap
   ;; Position -> Boolean
   ;; GIVEN: a position
   ;; RETURNS: true iff the position has any it has any 
   ;; other position from the position set adjacent to it
   ;; DESIGN STRATEGY: Function Composition
   (lambda (p)
     (position-is-adjacent-helper p (+ n 1) ps))
   ps))

;; position-is-adjacent-helper: Position Nat PositionSet -> Boolean
;; GIVEN: a position n and a list of position
;; WHERE: n is the location of the position in the
;;        position set which is we are working on
;; RETURNS: true if the position is present 
;;          in the given list of position
;; EXAMPLES: Check Tests
;; STRATEGY: General Recursion
;; TERMINATION ARGUMENT: Halting measure is n which is the
;; location of the position to be removed from the position set
;; which decrements at every recursion until its equal to 1 i.e 
;; when it has found the element
(define (position-is-adjacent-helper p n ps)
  (if (= n 1)
      (position-is-adjacent? p (rest ps))
      (position-is-adjacent-helper p (- n 1) (rest ps))))

;; TESTS
(define-test-suite position-is-adjacent-helper-tests
  (check-equal?
   (position-is-adjacent-helper (list 1 2) 2 (list (list 1 2) (list 2 3)))
false
"Testing to make sure it returns false, do not touch edges."))
 
;; position-is-adjacent? : Position PositionSet -> Boolean
;; GIVEN: a position and a positionset
;; RETURNS: true iff the position is a member of the 
;; position set or if it is adjacent
;; EXAMPLES:
;; (position-is-adjacent? (list 1 1) (list (list 1 2) (list 2 1)))
;; => true
;; STRATEGY: STRUCTURAL DECOMPOSITION on p: Position
(define (position-is-adjacent? p ps)
  (or
   (my-member? (list (add1 (first p)) (second p)) ps)
   (my-member? (list (sub1 (first p)) (second p)) ps)
   (my-member? (list (first p) (add1 (second p))) ps)
   (my-member? (list (first p) (sub1 (second p))) ps)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; board-to-obstacles : PositionSet -> PositionSetSet
;; GIVEN: the set of occupied positions on a chessboard
;; RETURNS: the set of obstacles on that chessboard.
;; WHERE: the position set is being updated 
;; at every recursive call and does not have duplicates
;; EXAMPLES: 
;; Check Tests
;; STRATEGY: General Recursion
;; TERMINATION ARGUMENT: Halting Measure is the PositionSet at every 
;; recursive call the size of the position set decreases as the elements
;; and its adjacent elements are removed from positionset and 
;; are being added to positionSeSset and stops when the 
;; position set is empty

(define (board-to-obstacles ps)
  (cond
    [(empty? ps) empty]
    [else
     (local 
       ((define init (list (first ps)))
        (define adjacent-ps (search-adjacents init init ps)))
     (set-cons 
      (reverse adjacent-ps)
      (board-to-obstacles 
       (set-diff ps 
        adjacent-ps))))]))

;; TESTS
(define-test-suite board-to-obstacles-tests
  (check-equal? 
   (board-to-obstacles (list (list 1 2) (list 1 3) (list 2 3) 
                             (list 3 2) (list 4 1) (list 3 4) (list 4 4)))
   (list (list (list 1 2) (list 1 3) (list 2 3)) (list (list 3 2)) 
         (list (list 4 1)) (list (list 3 4) (list 4 4)))
   "Testing to make sure it creates a list of obstacles, 
seperately if they do not share an edge."))


;; search-adjacents: PositionSet PositionSet PositionSet -> PositionSet
;; GIVEN: the current updated position 
;;        set with its adjacent positions updated position 
;;        set and the original position set
;; WHERE: new-ps is the new positionset we are working on, it is
;; being incrementally updated with positions added to it
;; that are adjacent to each other. Otherwise, they are added in a seperate 
;; list within the same positionset.
;; current also keeps track of the current position within the position set.
;; RETURNS: a position set with the adjacent positions 
;;         updated current position set
;; EXAMPLES: 
;; (search-adjacents (list (list 1 2)) (list (list 1 2)) 
;; (list (list 1 2) (list 1 3) (list 2 3) 
;; (list 3 2) (list 4 1) (list 3 4) (list 4 4))) =>
;; (list (list 2 3) (list 1 3) (list 1 2))
;; DESIGN STRATEGY: General Recursion
;; TERMINATION ARGUMENT: At every recursion there
;; search-reachable is empty i.e. we are finished with finding all 
;; the adjacents to the given initial position in current and its adjacents,
;; adjacents too.
(define (search-adjacents current new-ps ps)
  (local 
    ((define search-reachable (set-diff
                               (search-adjacent-helper new-ps ps)
                               current)))
    (if
     (empty? search-reachable)
     current
     (search-adjacents
      (append search-reachable current)
      search-reachable ps))))

;; search-adjacent-helper : PositionSet PositionSet -> PositionSet
;; GIVEN: a new position set whose adjacent positions are yet 
;;        to be searched with referance to the original 
;;        position set
;; RETURNS: a positions set with the positions 
;;          adjacent to the positions present 
;;          in the new position set
;; EXAMPLES: 
;; (search-adjacent-helper (list (list 1 1)) (list (list 1 2) (list 2 1)))
;; => (list (list 1 2) (list 2 1))
;; DESIGN STRATEGY: HOFC
(define (search-adjacent-helper new-ps ps)
  (foldr
   ;; Position PositionSet -> PositionSet
   ;; Given : a position and a positionset
   ;; Returns: a position set with new positions 
   (lambda (pos base) 
     (set-union (filter
                 ;; Position -> Boolean
                 ;; Given : A Postion for which 
                 ;;         the PositionSet has a position
                 ;;         which is adjacent
                 ;; Returns : true iff the position is adjacent 
                 ;;          to the position in position set
                 (lambda (p) 
                   (adjacent? p pos))
                 ps) base))
   empty
   new-ps))

;; adjacent? : Postion Position -> Boolean
;; GIVEN: a position from the new position 
;;        set and a position from existing 
;;        position set
;; RETURNS: true if the given two positions 
;;          are adjacent
;; EXAMPLES:
;; (adjacent? (list 1 1) (list 2 1)) => true
;; DESIGN STRATEGY: Structural decomposition on p : Position
(define (adjacent? p pos)
  (or (and (= (abs (- (first p) (first pos))) ONE) 
           (= (second p) (second pos)))
      (and (= (first p) (first pos)) 
           (= (abs (- (second p) (second pos))) ONE))))
  

;; TESTS
(run-tests position-set-equal?-tests)
(run-tests obstacle?-tests)
(run-tests board-to-obstacles-tests)
(run-tests position-is-adjacent-helper-tests)