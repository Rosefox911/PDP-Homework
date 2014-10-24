;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pitchers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide
 list-to-pitchers
 pitchers-to-list
 make-move
 make-pitcher
 pitchers-after-moves
 solve
 move-src
 move-tgt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALGORITHM
;; The algorithm we tried to follow for this problem set is 
;; Breadth-first search (BFS).
;; the goal of this algorithm is to traverse the graph a close as possible
;; to the root node.
;; We are working with two constructs, worklist and seen
;; Seen is simply a list of positions we have already visited.
;; We might get to one of these positions through a sequence of moves 
;; we haven't already tried yet, but because it brings us to the same 
;; configuration  of filled pitchers, we don't need to check it again.
;; Worklist are the nodes currently under consideration,
;; this relates to combination that we have not tried, 
;; worklist will also increase till we find the case that solves the problem
;; this would be where we have the goal amount within a pitcher.
;; Both seen and worklist increase in size till the
;; point where we solve the problem.
;; the halting measures for the algorithm are simply two:
;; -- if the worklist is empty, this means there are 
;;    no more moves left to explore
;; -- if the first move in the worklist (a move we have not tried)
;;    leads to a solution, then return the total series of moves.
;; NOTE: a "node" refers to a combination of a pitcher configuration 
;; and a list of moves.

;; NOTE2: When we handle a node, we:
;; -- Consider all the nodes directly reachable from it,
;; -- Discard all nodes which lead to a position already seen,
;; -- Discard all nodes which lead to a position already on the worklist,
;; -- The nodes that remain go on the end of the worklist, and
;; -- The node under consideration is added to the seen list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

(define-struct pitcher (mnt cap))
;; A Pitcher is a (make-pitcher Nat Nat)
;; mnt is a NonNegativeInt representing the amount of liquid in the pitcher
;; cap is a NonNegativeInt representing the total capacity of the pitcher
;WHERE: contents is smaller than capacity or we can say that
;For each ith Pitcher, 0 <= mnt_i <= cap_i

;; TEMPLATE
;; pitcher-fn : Pitcher -> ??
;;(define (pitcher-fn pit)
;;  (...
;;   (pitcher-mnt pit)
;;   (pitcher-cap pit)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Pitchers (LOP) is a non-empty ListOf<Pitcher> in any order, and is one of
;;--  (cons Pitcher empty) (interp: there is a single pitcher in the list,
;; it cannot be empty)
;;-- (cons Pitcher Pitchers) (interp: there are pitchers in the list)

;; TEMPLATE
;; lop-fn : Pitchers -> ??
;;(define (lop-fn lop)
;;  (cond
;;    [(empty? (rest lop)) ...]
;;    [else (...
;;           (pitcher-fn (first lop))
;;           (lip-fn (rest lop)))]))

;TEMPLATE
;pitchers-fn : Pitchers -> ??
;;(define (pitchers-fn pitchers)
;;  (...
;;   (pitcher-fn (first pitchers))
;;   (lop-fn (rest pitchers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Rep is a
;;-- (list Number Number) (interp: a list with two numbers,
;; the first representing the pitcher's current-quantity and
;; the second representing the pitcher's capacity-amount)

;; TEMPLATE
;; rep-fn : Rep -> ??
;(define (rep-fn rep)
;  (... (first rep)
;       (second rep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;A ListRep is a non-empty ListOf<Rep> which is one of
;;-- (cons Rep empty) (interp: a list with a single Rep in it)
;;-- (cons Rep ListRep) (interp: A list with multiple reps in it)

;; TEMPLATE
;; listrep-fn : ListRep -> ??
;(define (listrep-fn lr)
;  (cond
;    [(rep? lr) ...]
;    [else
;     (...
;      (rep-fn (first lr))
;      (listrep-fn (rest lr)))]))

;;ListRep ::= ((contents1 capacity1)
;;             (contents2 capacity2)
;;             ...
;;             (contents_n capacity_n))
;;WHERE:  n >=1, and for each i, 0 <= contents_i <= capacity_i
;;INTERPRETATION: the list of pitchers, from 1 to n, with their contents
;;and capacity
;;EXAMPLE: ((5 10) (7 8)) is a list of two pitchers.  The first
;;currently holds 5 and has capacity 10; the second currently holds 7 and has
;;capacity 8.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct move (src tgt))
;; A Move is a (make-move PosInt PosInt)
;; src represents the source pitcher of the liquid movement
;; tgt represents the target pitcher of the liquid movement
;; WHERE: src and tgt are different
;; INTERP: (make-move i j) means pour from pitcher i to pitcher j.

;; move-fn : Move -> ??
;;(define (move-fn mov)
;;  (...
;;   (move-src mov)
;;   (move-tgt mov)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfMove (LOM) is a list of moves, the order is executing order, 
;;   is one of
;;-- empty (interp: there are no moves in the list)
;;-- (cons Move ListOfMove) (interp: there are moves in the list)
;;                       moves are made from left to right order.

;; TEMPLATE
;; lom-fn : ListOfMove -> ??
;;(define (lom-fn lom)
;;  (cond
;;    [(empty? lom) ...]
;;    [else (...
;;           (move-fn (first lom))
;;           (lom-fn (rest lom)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Maybe<ListOf<Move>> is one of
;; -- false        
;;   (interp: when there is no solution for the problem)
;; -- ListOf<Move> 
;;  (interp: when there is a way to solve it with a list of moves)
;;   moves are made from left to right order.
;;TEMPLATE :
;(define (maybe-lom-fn lom)
;  (cond
;    [(false? lom) ...]
;    [else (...
;           (lom-fn lom))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NEListOf<PosInt> is a non-empty list of the capacities
;;;               of the pitchers in any order and is one of
;; (cons PosInt empty) (interp : when there is only one item in the list)
;; (cons PosInt ListOf<PosInt>)
;; (interp: when there is at list one pitcher that has a capacity)


;;NEListOf<PosInt> is a non-empty list of the capacities 
;;                 of the pitchers and is one of
;;(cons PosInt ListOf<PosInt>)
;;  (interp: when there is at list one pitcher that has a capacity)

;; TEMPLATE:
;; nelp-fn : NeListOf<PosInt> -> ??
;(define (nelp-fn nelp)
;  (....
;   (first nelp)
;   (lopi-fn (rest nelp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;ListOf<PosInt> is a list of the capacities of the pitchers, one of
;; -- empty
;;     (interp: when there is no pitcher)
;; -- (cons PosInt ListOf<PosInt>)
;;     (interp: when there is at list one pitcher that has a capacity)


;;ListOf<PosInt> is a list of the capacities of the pitchers and is one of
;; -- empty
;;       (interp: when there is nothing in the list)
;; -- (cons PosInt ListOf<PosInt>)
;;       (interp: when there is at list one PosInt in the list)
;(define (lopi-fn lopi)
;  (cond
;    [(empty? lopi) ...]
;    [else
;     (first lopi)
;     (lopi (rest lopi))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct node (pitchers moves))
;;A Node is
;; -- (make-node Pitchers ListOf<Move>)
;; interp:
;; Pitchers the configuration of pitchers at this node
;; moves or ListOf<Move> are the moves taken to reach this node, the path

;;TEMPLATE:
;;node-fn : Node -> ??
;;(define (node-fn n)
;;  (...
;;   (pitchers-fn (node-pitcher n))
;;   (lom-fn (node-lom n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;A ListOf<Node> is a list of Node in any order and is one of
;;--empty      interp: An empty list with no Node
;;--(cons Node ListOf<Node>)
;;             interp: A List of Node that has at least one Node in it
;;TEMPLATE:
;;lon-fn : LON ->??
;;(define (lon-fn lon)
;;  (cond
;;    [(empty? lon) ...]
;;    [else (...
;;           (node-fn (first lon))
;;           (lon-fn (rest lon)))]))

;; END OF DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define LOP-1 (list (make-pitcher 1 5) (make-pitcher 5 10) (make-pitcher 6 12)))
;; A list of pitchers (LOP) with 3 pitchers in it.

(define LOP-2 (list (make-pitcher 10 20) (make-pitcher 5 10)))
;; A list of pitchers (LOP) with 2 pitcher in it.

(define LOM-1 (list (make-move 1 2) (make-move 2 3)))
;; a list of move (LOM) with 2 moves in it

(define LOM-2 (list (make-move 1 2)))
;; a list of moves (LOM) with 1 move in it

(define NODE-1 (make-node LOP-1 LOM-1))
;; a node with LOP-1 and LOP-2 in it

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;list-to-pitchers : ListRep -> Pitchers
;; GIVEN: a listrep
;; RETURNS: a list of Pitchers from the list of rep
;; EXAMPLES: See the tests
;; STRATEGY: HOFC
(define (list-to-pitchers lrep)
  (map make-lrep-from-pitcher lrep))

;;make-lrep-from-pitcher : Rep -> Pitcher
;;GIVEN : a single rep
;;RETURNS : create the pitcher from the rep
;;EXAMPLE : See the tests below
;;STRATEGY : Struct decompo on rep [Rep]
(define (make-lrep-from-pitcher rep)
  (make-pitcher (first rep) (second rep)))


;;TEST
(define-test-suite list-to-pitchers-tests
  (check-equal?
   (list-to-pitchers  '((5 10) (7 8)))
   (list (make-pitcher 5 10) (make-pitcher 7 8))
   "Testing to make sure ListRep is converted to list of pitchers."))

;; pitchers-to-list : Pitchers -> ListRep
;; GIVEN: an internal representation of a set of pitchers
;; RETURNS: a ListRep that represents them.
;; EXAMPLES: see tests
;; STRATEGY: HOFC
(define (pitchers-to-list lop)
  (map make-lrep-from-pitchers lop))


;;make-lrep-from-pitchers : Pitcher -> Rep
;;GIVEN : a single pitcher
;;RETURNS : a rep from the given pitcher
;;EXAMPLE : (make-lrep-from-pitchers (make-pitcher 2 3)) ->  (list 2 3)
;;STRATEGY : Struct decompo on p : Pitcher
(define (make-lrep-from-pitchers p)
  (list (pitcher-mnt p) (pitcher-cap p)))

;;TEST
(define-test-suite pitchers-to-list-tests
  (check-equal?
   (pitchers-to-list LOP-1)
   (list (list 1 5) (list 5 10) (list 6 12))
   "Testing to make sure Lrep is created from LOP"))


;;pitchers-after-moves : Pitchers ListOf<Move> -> Pitchers
;;GIVEN: An internal representation of a set of pitchers, and a sequence
;;of moves
;;WHERE: each move on the list refers to pitchers numbered between 1 and
;; the length of the list of pitchers, inclusive. 
;;RETURNS: the internal representation of the set of pitchers that should
;;result after executing the given list of moves, in order, on the given
;;set of pitchers.
;;STRATEGY : HOFC
(define (pitchers-after-moves lop lom)
  (foldl pitchers-after-move lop lom))

;;source-pitcher-after-move : Pitcher Pitcher -> Pitcher
;;GIVEN : a source and a target pitcher
;;RETURNS : a new pitcher as a source pitcher, based on if the
;;         amount can be transferred or not.
;;EXAMPLE : See the tests
;;STRATEGY : Double decompo on pt and ps : Pitcher
(define (source-pitcher-after-move ps pt)
  (if (< (- (pitcher-cap pt) (pitcher-mnt pt)) (pitcher-mnt ps))
      (make-pitcher (- (pitcher-mnt ps) (- (pitcher-cap pt) (pitcher-mnt pt)))
                    (pitcher-cap ps))
      (make-pitcher 0 (pitcher-cap ps))))

;;target-pitcher-after-move : Pitcher Pitcher -> Pitcher
;;GIVEN : a source pitcher and a target pitcher
;;RETURNS : get the target pitcher based on the move from source to the target
;;EXAMPLE : See the tests
;;STRATEGY : Double decompo on pt and ps : Pitcher
(define (target-pitcher-after-move ps pt)
  (if (< (- (pitcher-cap pt) (pitcher-mnt pt)) (pitcher-mnt ps))
      (make-pitcher (pitcher-cap pt) (pitcher-cap pt))
      (make-pitcher (+ (pitcher-mnt ps) (pitcher-mnt pt))
                    (pitcher-cap pt))))

;;get-source-pitcher : Pitchers Move -> Pitcher
;;GIVEN : a list of pitchers and a move
;;RETURNS : the source pitcher that we get from the list
;;EXAMPLE : See the tests
;;STRATEGY :Struct decompo on move : Move
(define (get-source-pitcher lop move)
  (list-ref lop (sub1 (move-src move))))

;;get-target-pitcher : Pitchers Move -> Pitcher
;;GIVEN : a list of pitchers and a move
;;WHERE: every move refers only to pitchers that are in the set of pitchers.
;;RETURNS : the target pitcher that we get from the list
;;EXAMPLE : See the tests
;;STRATEGY :Struct decompo on move : Move
(define (get-target-pitcher lop move)
  (list-ref lop (sub1 (move-tgt move))))

;;pitchers-after-move : Move Pitchers -> Pitchers
;;GIVEN : a list of pitchers and a move
;; WHERE: the move refers to pitcher numbers between 1
;; and the length of pitchers, inclusive
;;RETURNS : a list of pitcher, updated to represent the changes due to the move
;;EXAMPLE :  (pitchers-after-move (make-move 1 2) 
;;           (list (make-pitcher 8 8) (make-pitcher 0 3)))
;;           -> ((make-pitcher 5 8) (make-pitcher 3 3))
;;STRATEGY : HOFC
(define (pitchers-after-move move pitchers)
  (local
    ((define src (get-source-pitcher pitchers move))
     (define tgt (get-target-pitcher pitchers move)))
    (map 
     ;; Pitcher Integer -> Pitcher
     ;; GIVEN: a pitcher and an integer representing its position
     ;; RETURNS: either a pitcher exactly like the original, or
     ;; a pitcher like the original, except its source or target
     ;; have been updated to reflect the move being made.
     ;; STRATEGY: STRUCTURAL DECOMPOSITION on move : Move
     (lambda (pitcher position)
           (cond 
             [(= (add1 position) (move-src move))
                  (source-pitcher-after-move src tgt)]
             [(= (add1 position) (move-tgt move))
                  (target-pitcher-after-move src tgt)]
             [else pitcher]))
         pitchers
         (build-list (length pitchers) identity))))


;;TEST :
(define-test-suite pitchers-after-moves-tests
  (check-equal?
   (pitchers-after-moves LOP-1 LOM-1)
   (list (make-pitcher 0 5) (make-pitcher 0 10) (make-pitcher 12 12))
   "The list after the moves should be like up.")
  (check-equal?
   (pitchers-after-moves LOP-1 empty)
   LOP-1
   "Nothing should be changed.")
  (check-equal?
   (pitchers-after-moves LOP-2 LOM-2)
   (list (make-pitcher 5 20) (make-pitcher 10 10))
   "It will fill as much as needed.")
  (check-equal?
   (pitchers-after-moves empty empty)
   empty
   "It will fill as much as needed."))

;;solve : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;;GIVEN: a list of the capacities of the pitchers and the goal amount
;;RETURNS: a sequence of moves which, when executed from left to right,
;;results in one pitcher (not necessarily the first pitcher) containing
;;the goal amount.  Returns false if no such sequence exists.
;;EXAMPLE:
;; (solve '( 2 3 4) 5) -> false
;; (solve (list 3 4 6) 3) -> empty
;; (solve (list 10 7 3) 5) ->
;;(list
;; (make-move 1 2)
;; (make-move 2 3)
;; (make-move 3 1)
;; (make-move 2 3)
;; (make-move 3 1)
;; (make-move 2 3)
;; (make-move 1 2)
;; (make-move 2 3))
;;STRATEGY: STRUCTURAL DECOMPOSITION on capacities : NEListOf<PosInt>
(define (solve capacities target)
  (if (< (first capacities) target) 
      false
      (search-for target
                  empty
                  (list (make-node (fill-first capacities) empty)))))
;; TESTS
(define-test-suite solve-tests
  (check-equal?
   (solve (list 1 2 3 4) 5)
   false
   "This is unsolvable, should return false.")
  (check-equal?
   (solve (list 3 4 6) 3)
   empty
   "This case has no moves needed to solve it, should return empty list")
  (check-equal?
   (solve (list 10 7 3) 5)
   (list
    (make-move 1 2)
    (make-move 2 3)
    (make-move 3 1)
    (make-move 2 3)
    (make-move 3 1)
    (make-move 2 3)
    (make-move 1 2)
    (make-move 2 3))
   "This should produce a sequence of
movements resulting in the problem being solved."))


;;fill-first : ListOf<PosInt> -> Pitchers
;;GIVEN : a list of capacities
;;RETURNS : a list of pitchers, for begining, the first pitcher
;;should be filled, the rest should be empty
;;EXAMPLE : see the tests
;;STRATEGY: HOFC
(define (fill-first capacities)
  (cons (make-pitcher (first capacities) (first capacities))
        (map 
         ;; PosInt -> Pitcher
         ;; GIVEN: a posint extracted from ListOf<PosInt>
         ;; RETURNS: a pitcher with its capacity updated based on
         ;; the posint passed to it and where its it is empty (transferred).
         ;; STRATEGY: Function Composition
         (lambda (capacity) 
           (make-pitcher 0 capacity))
         (rest capacities))))

;;search-for : PosInt ListOf<Node> ListOf<Node> -> ListOf<Move>
;;GIVEN : 2 lists of nodes, list of visited nodes and "edge" nodes and a goal
;;RETURN: list of moves to reach the goal with these pitchers, 
;; puts the list of moves in order
;;WHERE: worklist is a list of nodes which are solvable iff the original
;; one is solvable and seen is a list of nodes
;; which you've already checked, but are not the solution.
;;EXAMPLE: See the tests
;;STRATEGY: General Recursion
;;TERMINATION ARGUMENT:  each recursive call adds a unique configuration
;; to the seen list. there is a finite number of reachable configurations,
;; and when all of them are on the seen list, the algorithm terminates.
(define (search-for target seen worklist)
  (cond
    [(empty? worklist) false]
    [(target-inside-worklist? target worklist)
     (reverse (node-moves (first worklist)))] 
    [else
     (search-for 
      target 
      (cons (first worklist) seen) 
      (get-new-list worklist seen))]))

;; TESTS
(define-test-suite search-for-tests
  (check-equal? 
   (search-for 5 (list NODE-1) empty)
   false
   "Giving it an empty worklist, should be return false."))


;;get-new-list : ListOf<Node> ListOf<Node> -> ListOf<Node>
;;GIVEN: 2 list of nodes, to work and to check
;;RETURNS: a list of candidates nodes, either on the original worklist 
;; or reachable from it, and not in the seen list
;; so (rest worklist) = "on the original worklist", 
;; (filter-redundant-nodes ..) = "reachable from it,
;; and not on the seen list
;;EXAMPLE: See the tests
;;STRATEGY: STRUCTURAL DECOMPOSITION on worklist : ListOf<Node>
(define (get-new-list worklist seen)
  (append (rest worklist)
          (filter-redundant-nodes worklist seen)))



;;filter-redundant-nodes : ListOf<Node> ListOf<Node> -> ListOf<Node>
;;GIVEN: 2 list of nodes, to work and to check
;;RETURN: filter the visited nodes and return the rest
;;EXAMPLE : See the tests
;;STRATEGY: HOFC
(define (filter-redundant-nodes worklist seen)
  (filter 
   ;; Node -> Boolean
   ;; GIVEN: a node
   ;; RETURNS: true iff the node is NOT a member of either the seen list of
   ;; nodes OR the worklist of nodes, thus creating a list of nodes with
   ;; these "true" elements within it.
   ;; STRATEGY: Function Composition
   (lambda (node)
     (check-for-redundancy node seen worklist)) 
   (get-new-list-after-move 
    (possible-moves 
     (length (node-pitchers (first worklist)))) 
    (first worklist))))


;;possible-moves : PosInt -> ListOf<Move>
;;GIVEN: length of the list of pitchers of this puzzle
;;RETURNS: a list of all the moves possible for N pitchers
;;EXAMPLE: See the tests
;;STRATEGY: HOFC
(define (possible-moves n)
  (foldr append empty (construct-list-of-moves n)))


;;construct-list-of-moves : PosInt -> ListOf<Move>
;;GIVEN: the length of list of pitchers of this puzzle
;;RETURNS: Constructs a list of moves, does recursion on a list of moves
;;where target and source are not equal (filter handles this)
;;creates a list with elements where source and target are NOT equal
;;EXAMPLE: see the tests
;;STRATEGY: HOFC
(define (construct-list-of-moves n)
  (map 
   ;; PosInt -> Move
   ;; GIVEN: a positive integer representing the source pitcher
   ;; RETURNS: a move based on the source and target
   ;; STRATEGY: HOFC
   (lambda (source)     
     (map 
      ;; PosInt -> Move
      ;; GIVEN: a positive integer representing the target pitcher
      ;; RETURNS: a move based on the source and target
      ;; STRATEGY: Function Composition
      (lambda (target)
        (make-move (add1 source) (add1 target)))
      (filter 
       ;; PosInt -> Boolean
       ;; GIVEN: a posint representing the target pitcher
       ;; RETURNS: true iff the target and source are NOT equal
       ;; STRATEGY: Domain Knowledge
       (lambda (target)
         (not (= source target)))
       (build-list n identity))))
   (build-list n identity)))


;;check-for-redundancy : Node ListOf<Node> ListOf<Node> -> Boolean
;;GIVEN: a node, a list of visited nodes, a list of nodes that has to be visited
;;RETURNS: whether this node describes a pitcher configuration
;; which has already been seen
;;EXAMPLE: See the tests
;;STRATEGY: HOFC
(define (check-for-redundancy node seen worklist) 
  (not (or (member (node-pitchers node)
                   (map node-pitchers seen)) 
           (member (node-pitchers node)
                   (map node-pitchers worklist)))))


;;get-new-list-after-move : ListOf<Move> ListOf<Node> -> ListOf<Node>
;;GIVEN: a list of moves and a list of nodes
;;RETURNS: returns a list of nodes where
;; the pitchers have been updated to reflect the move request
;; and the list of moves have been updated to include
;; the latest move requests
;;EXAMPLE: See the tests
;;STRATEGY: HOFC
(define (get-new-list-after-move lom wlst)  
  (map 
   ;; Move -> Node
   ;; GIVEN: a move
   ;; RETURNS: an updated node
   ;; STRATEGY: Function Composition
   (lambda (move)
     (update-list-with-move move wlst))
   lom))


;;update-list-with-move : Move  Node -> Node
;; GIVEN: a move and a node
;; RETURNS: a node with its list of pitchers and list of moves updated
;; Updating list of pitchers with move request
;; updating list of moves, with recent move
;;; EXAMPLE : see the tests
;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
(define (update-list-with-move move wlst)
  (make-node 
   (pitchers-after-move move (node-pitchers wlst))
   (cons move (node-moves wlst))))



;;target-inside-worklist? : PosInt ListOf<Node> -> Boolean
;;GIVEN: a goal and a list of nodes
;;RETURNS: if the target is a member of worklist, true, otherwise false
;;EXAMPLE: see the tests
;;STRATEGY: HOFC
(define (target-inside-worklist? target worklist)
  (member target 
          (map pitcher-mnt (node-pitchers (first worklist)))))

(run-tests list-to-pitchers-tests)
(run-tests pitchers-to-list-tests)
(run-tests pitchers-after-moves-tests)
(run-tests solve-tests)
(run-tests search-for-tests)