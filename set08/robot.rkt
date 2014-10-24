;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #f none #f ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALGORITHM

;; In order to find the path i.e the list of moves in an infinite checker borad
;; to reach the target position fron the source position. In order to solve the
;; problem we defined a structure named state which includes the position and 
;; the path that is list of positions that has to be travelled to reach this 
;; position. There is an other invariant visited which keeps track of all
;; the visitited
;; positions. With the help of these we are using a Depth First Search (DFS) 
;; algorithm to solve our problem.Initially we are making our board into a 
;; finite board by taking the maximum from the coordinates of positions source 
;; target and given list of obstacles. Then we take each position initially it 
;; is source which is added to the state structure and its posiible moves are 
;; taken and we eliminate the moves which cant occur by doing set-diff with
;; visited and obstacles.After eliminating the duplicates we stack the posiible
;; moves and its respective path in states. When ever the possible move has 
;; target 
;; in it the path is given, else the recurssion takes palce until there are no 
;; possible moves and the states becomes empty then it returns false. 

;; END OF ALGORITHM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require rackunit/text-ui)
(require "sets.rkt")
(require "extras.rkt")

(provide
 path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITION

;;  Nat denotes the set of natural numbers 
;; (also called the non-negative integers) 0, 1, 2, etc.
;; PosInt denotes the set of positive integers i.e 1,2,3..

;; A Position is a (list PosInt PosInt)
;; (x y) represents the position at position x, y.
;; Note: this is not to be confused with the built-in data type Posn.

;; TEMPLATE:
;; position-fn : Position -> ??
;(define (position-fn pos)
;  (...(first pos)
;      (second pos)))

;; A ListOf<Position> is a
;; -- empty       (interp: an empty list of position)
;; -- (cons Position ListOf<Position>) (interp: a list of position with a 
;;                                      Position and a ListOf<Position>)
;; WHERE: it has no duplicates

;; TEMPLATE
;; lop-fn : ListOf<Position> -> ??
;(define (lop-fn poss)
;  (cond
;    [(empty? (rest poss)) ...]
;    [else
;     (...
;      (first poss)
;      (lop-fn (rest poss)))]))

;; A NEListOf<Position> is a
;; -- (cons Position ListOf<Position>) (interp: a position followed by 
;;                                         ListOf<Position>)
;; TEMPLATE
;; nelop-fn : NEListOf<Position> -> ??
;(define (nelop-fn nelop)
;  (...
;     (position-fn
;      (first nelop))
;      (lop-fn (rest nelop))))

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction.

;; TEMPLATE
;; move-fn : Move -> ??
;(define (move-fn mov)
;  (...
;   (direction-fn (first mov))
;   (second mov)))


; A ListOf<Move> is
;; -- empty (interp: no moves within the list)
;; -- (cons Move ListOf<Move>) (interp: a list of move 
;;     with a move and a list of moves in it)

;; TEMPLATE
;; lom-fn : ListOf<Move> -> ??
;(define (lom-fn lom)
;  (cond
;    [(empty? lom) ...]
;    [else
;     (...
;     (move-fn (first lom))
;     (lom-fn (rest lom)))]))

;; A Plan is a ListOf<Move>
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.

;; A NEListOf<Move> is
;; -- (cons Move ListOf<Move>) (interp: a list with a move 
;;            followed by list of moves)
;; TEMPLATE:
;; nelom-fn: NEListOf<Move> -> ??
;(define (nelom-fn nelom)
;  (...
;   (move-fn (first nelom))
;   (lom-fn (rest nelom)))]))

;; A Maybe<Plan> is either
;; -- false (interp: it is false, no plan)
;; -- Plan (interp: there is a plan)

;; TEMPLATE:
;; maybeplan-fn : Maybe<Plan> -> ??
;(define (maybeplan-fn may)
;  (cond
;    [(false? may) ...]
;    [else
;     (...
;      (lom-fn may))]))

;; A MaybeListOf<Position> is either
;; -- false (interp: it is false, no list of position)
;; -- ListOf<Position> (interp: there is a list of position)

;; TEMPLATE:
;; maybelistofposition-fn : MaybeListOf<Position> -> ??
;(define (maybelistofposition-gn maylop)
;  (cond
;    [(false? maylop) ...]
;    [else
;     (...
;      (lop-fn maylop))]))

;; A Direction is one of
;; -- "north" (interp: moving north, y coordinate decreases)
;; -- "east" (interp: moving east, x coordinate increases)
;; -- "south" (interp: moving south, y coordinate increases)
;; -- "west" (interp: moving west, x coordinate decreases)
;; WHERE: Elements of direction are strings

;; TEMPLATE
;; direction-fn : Direction -> ??
;(define (direction-fn dir)
;  (cond
;    [(string=? "north" dir) ...]
;    [(string=? "east" dir) ...]
;    [(string=? "south" dir) ...]
;    [(string=? "west" dir) ...]))

(define-struct state (position plan))
;; A State is a (make-state Position PositionSet)
;; a position detonates the starting position of the robot
;; a plan detonates the series of movements the robot has gone through

;; TEMPLATE
;; state-fn : State -> ??
;(define (state-fn st)
;  (...
;   (state-position st)
;   (state-plan st)))

;; A States is a list of State
;; A ListOf<State> is one of
;; --empty                      (interp:the list is empty)
;; --(cons State ListOf<State>) (interp: the list contains
;;                                                 a State followed
;;                                                 by a
;;                                                 ListOf<State>)
;; TEMPLATE: 
;; los-fn : ListOf<State> -> ??
;(define (los-fn los)
;  (cond 
;    [(empty? los)...]
;    [else(...(state-fn (first los))
;             (los-fn (rest los)))]))

;; END OF DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define POSITION-1 (list 1 2))

(define POSITION-2 (list 2 3))

(define POSITION-3 (list 4 5))

(define LOP-1 (list POSITION-1 POSITION-2))


(define ZERO 0)
(define ONE 1)
(define TWO 2)
(define THREE 3)


;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path : Position Position ListOf<Position> -> Maybe<Plan>
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; EXAMPLES:
;; (path (list 1 2) (list 3 4) (list (list 2 2))) ->
;; (("south" 3) ("east" 4) ("north" 4) ("west" 1) ("south" 3) ("west" 1))
;; (path (list 1 1) (list 4 4) (list (list 1 1) (list 2 2) (list 2 1) 
;; (list 3 1))) ->
;; false
;; STRATEGY: FUNCTIONAL COMPOSITION

(define (path src tgt obstacles)
  (cond
    [(equal? src tgt) empty]
    [(or
      (member? src obstacles)
      (member? tgt obstacles)) false]
    [else
     (path-helper (reachables-from 
                   empty 
                   (list (make-state src (list src)))
                   tgt 
                   (append 
                    obstacles 
                    (new-list 
                     (+ (get-maximum src tgt obstacles) TWO)))))]))

;; TESTS
(define-test-suite path-tests 
  (check-equal? 
   (path (list 1 2) (list 3 4) (list (list 2 2)))
   '( ("south" 3) ("east" 4) ("north" 4) 
                  ("west" 1) ("south" 3) ("west" 1))
   "Checking to make sure it returns a correct path.")
  (check-equal? 
   (path (list 1 1) (list 4 4) (list (list 1 1) 
                                     (list 2 2) (list 2 1) (list 3 1)))
   false
   "Impossible path should return false.")
  (check-equal? (path (list 1 1) (list 1 1) empty)
                empty
                "Should return empty, nothing to do, src is tgt.")
  
  (check-equal? (path (list 1 1) (list 5 5) 
                      (list (list 4 5) (list 5 4) (list 5 6)))
                false 
                "test when there are no psossible moves"))

;; path-helper : MaybeListOf<Position> -> Maybe<Plan>
;; GIVEN: a Maybe list of position
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; EXAMPLES:
;; (path-helper false) -> false
;; (path-helper (list (list 1 1) (list 1 2))) -> (("south" 1))
;; STRATEGY: STRUCTURAL DECOMPOSITION on route : MaybeListOf<Position>

(define (path-helper route)
  (if (false? route)
      false
      (create-plan
       (path-to-moves 
        (first route) 
        (remove (first route) route)))))

;; TESTS
(define-test-suite path-helper-tests
  (check-equal? 
   (path-helper false)
   false
   "Testing to make sure it returns false.")
  (check-equal? 
   (path-helper (list (list 1 1) (list 1 2)))
   '(("south" 1))
   "Testing to make sure a maybe plan is returned."))

;; reachables-from: ListOf<Position> States Position 
;;                 ListOf<Position> -> MaybeListOf<Position>
;; GIVEN: a list of positions, representing where we have visited on board
;; a list of states, a positionr epresenting the target position and a list
;; of positions representing the obstacles on the board.
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; WHERE: visited will increase in size as we go through different 
;; combinations on the board, the positions visited from states be added to 
;; the visited and all the unvisited positions with its plan are added 
;; to the list of states and when any of this new position is been visited 
;; it is removed from the states and added to the visited
;; EXAMPLES:
;; (reachables-from (list (list empty)) 
;; (list (make-state (list 1 2) empty)) (list 1 3) (list (list 1 2))) ->
;; ((1 3))
;; STRATEGY: GENERAL RECURSION
;; TERMINATION ARGUMENT: after removing the duplicates i.e set diff with 
;; respect to visited and obstacles the new adjacent positions 
;; if the new given positions has a target in it it returns the path
;; are given
;; if there are no possible adjacent moves then the it will recurse
;; on the rest
;; of states will become until it becomes
;; empty and when it is empty the genereal recursion terminates and returns 
;; false which means there are no possible moves for the source to reach
;; the target as they are completely surrounded by blocks which 
;; is my halting meausre i.e the length of states

(define (reachables-from visited states tgt obsatcles)
  (if (empty? states)
      false
      (local ((define candidates 
                (set-diff (succesors 
                           (state-position (first states)))
                          (append visited obsatcles))))
        (if (empty? candidates)
            (reachables-from 
             (cons (state-position (first states)) visited)
             (rest states) tgt obsatcles)
            (if (equal? (first candidates) tgt)
                (append (state-plan (first states)) (list tgt))
                (reachables-from (cons (state-position (first states))
                                       visited)
                                 (append (convert-to-states 
                                          candidates 
                                          (first states))
                                         (rest states))
                                 tgt obsatcles))))))

;; TESTS
(define-test-suite reachables-from-tests
  (check-equal? 
   (reachables-from (list (list empty)) 
                    (list (make-state (list 1 2) empty)) 
                    (list 1 3) 
                    (list (list 1 2)))
   '((1 3))
   "Should return a single movement, to 1 3")
  (check-equal?
   (reachables-from (list (list empty)) 
                    empty 
                    (list 1 3) 
                    (list (list 1 2)))
   false
   "testing to make sure false is returned, states is empty."))

;; convert-to-states : ListOf<Position> State -> States
;; GIVEN: a list of positions and a state
;; RETURNS: a list of state, where elements within state
;; have been updated to reflect the change in position
;; EXAMPLES:
;;(convert-to-states LOP-1 (make-state (list 1 1) empty)) -> 
;; '( (make-state '(1 2) '((1 2))) (make-state '(2 3) '((2 3))))
;; STRATEGY: STRUCTURAL DECOMPOSITION on state : State

(define (convert-to-states lop state)
  (map
   ;; Position -> State
   ;; GIVEN: a position
   ;; RETURNS: a state with its position field updated
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on state : State
   (lambda (x)
     (make-state x (append (state-plan state) (list x)))) lop))

;; TESTS
(define-test-suite convert-to-states-tests
  (check-equal?
   (convert-to-states LOP-1 (make-state (list 1 1) empty)) 
   (list (make-state '(1 2) '((1 2))) (make-state '(2 3) '((2 3))))
   "Testing to make sure states is created."))

;; succesors : Position -> ListOf<Position>
;; GIVEN: a position
;; RETURNS: a list of position with those that are adjacent
;; to the given position
;; EXAMPLES: (succesors (list 1 1)) -> (list (1 2) (2 1))
;; STRATEGY: HOFC
(define (succesors p)
  (filter
   ;; Number -> Boolean
   ;; GIVEN: a number
   ;; RETURNS: true iff the number is greater
   ;; than zero
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on p : Position
   (lambda (x) (and 
                (> (first x) ZERO)
                (> (second x) ZERO)))
   (find-adjacents p)))

;; TESTS
(define-test-suite succesors-tests
  (check-equal?
   (succesors (list 1 1))
   '((1 2) (2 1))
   "Testing to make sure list of adjacent blocks is created."))

;; find-adjacents : Position -> ListOf<Position>
;; GIVEN: a poistion
;; RETURNS: a list of positions adjacent to the given position
;; EXAMPLES: 
;; (find-adjacents (list 1 1)) -> (list (0 1) (1 0) (1 2) (2 1))
;; DESIGN STRATEGY: Structural Decomposition on p:Position
(define (find-adjacents p)
  (list (list (first p) (+ (second p) ONE))
        (list (+ (first p) ONE) (second p))
        (list (first p) (- (second p) ONE))
        (list (- (first p) ONE) (second p))))

;; get-maximum : Position Position ListOf<Position> -> PosInt
;; GIVEN: a position representing the source position
;; a position representing the target position
;; and a list of positons representing the obstacles
;; RETURNS: the maximum number of a coordinate on the board
;; NOTE: Used to make board finite
;; EXAMPLES:
;; (get-maximum POSITION-1 POSITION-2 LOP-1) -> 3
;; (get-maximum POSITION-1 POSITION-3 LOP-1) -> 5
;; STRATEGY: STRUCTURAL DECOMPOSITION on src : Position
(define (get-maximum src tgt lop)
  (max
   (max (first src) (second src))
   (max (first tgt) (second tgt))
   (get-maximum-from-lop lop)))

;; TESTS
(define-test-suite get-maximum-tests
  (check-equal?
   (get-maximum POSITION-1 POSITION-2 LOP-1)
   3
   "Testing to make sure it returns 3, the max value.")
  (check-equal?
   (get-maximum POSITION-1 POSITION-3 LOP-1)
   5
   "Testing to make sure it returns 5, this sets max value."))

;; get-maximum-from-lop : ListOf<Position> -> PosInt
;; GIVEN: a list of position
;; RETURNS: the maximum x or y value within that list of positions
;; EXAMPLES:
;; (get-maximum-from-lop LOP-1) -> 3
;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
(define (get-maximum-from-lop lop)
  (foldr
   ;; Position PosInt -> PosInt
   ;; GIVEN: a position
   ;; RETURNS: the maximum value between its x and y coord
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on f : Position
   (lambda (f base)
     (max (first f)
          (second f) base))
   ZERO
   lop))

;; TESTS
(define-test-suite get-maximum-from-lop-tests
  (check-equal?
   (get-maximum-from-lop LOP-1)
   3
   "Testing to make sure it returns 3, the max for this set."))

;; new-list : PosInt -> ListOf<Position>
;; GIVEN: a PosInt representing the maximum coordinate of the board
;; RETURNS: a list of positions where the pos int provided is the maximum
;; for x and y
;; EXAMPLES: 
;; (new-list 1) -> ((1 1))
;; (new-list 2) -> ((1 2) (2 1) (2 2))
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (new-list n)
  (set-union
   (build-list n
               ;; PosInt -> Position
               ;; GIVEN: a positive integer
               ;; RETURNS: a list where the x coordinate
               ;; has been increased by that pos int plus 1
               ;; STRATEGY: DOMAIN KNOWLEDGE
               (lambda (i)
                 (list (+ i ONE) n)))
   (build-list n
               ;; PosInt -> Position
               ;; GIVEN: a positive integer
               ;; RETURNS: a list where the y coordinate
               ;; has been increased by that pos int plus 1
               ;; STRATEGY: DOMAIN KNOWLEDGE
               (lambda (i)
                 (list n (+ i ONE))))))

;; TESTS
(define-test-suite new-list-tests
  (check-equal?
   (new-list 2)
   '((1 2) (2 1) (2 2))
   "Testing to make sure a list of positions with max 2 is created.")
  (check-equal?
   (new-list 1)
   '((1 1))
   "Checking to make sure a list of position with max 1 i created."))

;; path-to-moves : Position NEListOf<Position> -> ListOf<Move>
;; GIVEN: a position and a obstacles
;; RETURNS: a list of moves
;; EXAMPLES:
;; (path-to-moves (list 1 2) (list (list 2 3))) -> 
;; (("east" 1))
;; STRATEGY: HOFC

(define (path-to-moves src lop)
  (map convert-to-move
       (cons src (reverse (rest (reverse lop))))
       lop))

;; TESTS
(define-test-suite path-to-moves-tests
  (check-equal? (path-to-moves (list 1 2) (list (list 2 3)))
                '(("east" 1))
                "Checking to make sure list of moves is created."))

;; convert-to-move : Position Position -> Move
;; GIVEN: a position representing the current
;; and a position representing the next position
;; RETURNS: a move resulting to getting to the next position
;; EXAMPLES: (convert-to-move (list 1 1) (list 1 2)) ->
;; ("south" 1)
;; STRATEGY: STRUCTURAL DECOMPOSITION on current and next:Position

(define (convert-to-move current next)
  (if (= (first current) (first next))
      (if (= (+ (second current) ONE) (second next))
          (list "south" ONE)
          (list "north" ONE))
      (if (= (+ (first current) ONE) (first next))
          (list "east" ONE)
          (list "west" ONE))))

;; TESTS
(define-test-suite convert-to-move-tests
  (convert-to-move (list 1 1) (list 1 2))
  '("south" 1)
  "Testing to makre sure it produces a move 1 south.")

;; create-plan : NEListOf<Move> -> Plan
;; GIVEN: a list of moves
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks.
;; EXAMPLES: 
;; (create-plan (list '("south" 1) '("east" 1))) ->
;; (("south" 1) ("east" 1))
;; (create-plan empty) -> ()
;; STRATEGY: HOFC

(define (create-plan lom)
  (reverse (foldl (lambda (next-move plan)
                    (create-plan-helper next-move plan))
                  (list (first lom))
                  (rest lom))))

;; TESTS
(define-test-suite create-plan-tests
  (check-equal?
   (create-plan (list '("south" 1) '("east" 1)))
   '(("south" 1) ("east" 1))
   "Testing to make sure a plan is returned."))

;; create-plan-helper : Move NEListOf<Move> -> Plan
;; GIVEN: a move, a list of moves and a positive integer
;; RETURNS: a plan
;; EXAMPLES: 
;; (create-plan-helper '("south" 1) (list '("north" 1))) -> 
;; (("south" 1) ("north" 1))
;; STRATEGY: STRUCTURAL DECOMPOSITION on lom:ListOf<Move>
(define (create-plan-helper next-move plan)
  (if (equal? (first next-move) (first (first plan)))
      (cons (list (first next-move)
                  (+ (second next-move)
                     (second (first plan))))
            (rest plan))
      (cons next-move plan)))

;; TESTS
(define-test-suite create-plan-helper-tests
  (check-equal? 
   (create-plan-helper '("south" 1) (list '("north" 1)))
   '(("south" 1) ("north" 1))
   "Testing to make sure plan is created."))


;; TESTS
(run-tests path-tests)
(run-tests path-helper-tests)
(run-tests reachables-from-tests)
(run-tests convert-to-states-tests)
(run-tests succesors-tests)
(run-tests path-to-moves-tests)
(run-tests convert-to-move-tests)
(run-tests create-plan-tests)
(run-tests create-plan-helper-tests)
(run-tests get-maximum-tests)
(run-tests get-maximum-from-lop-tests)
(run-tests new-list-tests)