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
 search-for)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

(define-struct pitcher (mnt cap))
;; A Pitcher is a (make-pitcher NonNegativeInt NonNegativeInt)
;; mnt is a NonNegativeInt representing the amount of liquid in the pitcher
;; cap is a NonNegativeInt representing the total capacity of the pitcher

;; TEMPLATE
;; pitcher-fn : Pitcher -> ??
;;(define (pitcher-fn pit)
;;  (...
;;   (pitcher-mnt pit)
;;   (pitcher-cap pit)))
;;

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


;; A ListOfMove (LOM) is a
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

;; A Pitchers (LOP) is a ListOf<Pitcher>
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

;; A Rep is a
;;-- (list Number Number) (interp: a list with two numbers,
;; the first representing the pitcher's current-quantity and
;; the second representing the pitcher's capacity-amount)

;; TEMPLATE
;; rep-fn : Rep -> ??
                                        ;(define (rep-fn rep)
                                        ;  (... (first rep)
                                        ;       (second rep)))


;;A ListRep is a ListOf<Rep> which is one of
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


;;Maybe<ListOf<Move>> is one of
;; -- false        (interp: when there is no solution for the problem)
;; -- ListOf<Move> (interp: when there is a way to solve it with a list of moves)
;;                          moves are made from left to right order.
;;TEMPLATE :
                                        ;(define (maybe-lom-fn lom)
                                        ;  (cond
                                        ;    [(false? lom) ...]
                                        ;    [else (...
                                        ;           (lom-fn lom))]))


;;NEListOf<PosInt> is a non-empty list of the capacities of the pitchers and is one of
;;(cons PosInt ListOf<PosInt>)
;;  (interp: when there is at list one pitcher that has a capacity)
;;TEMPLATE:
;(define (nelp-fn nelp)
;  (....
;   (first nelp)
;   (lopi-fn (rest nelp))))

;;NEListOf<PosInt> is a non-empty list of the capacities of the pitchers and is one of
;;(cons PosInt empty) (interp : when there is only one item in the list)
;;(cons PosInt ListOf<PosInt>)
;;  (interp: when there is at list one pitcher that has a capacity)


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


(define-struct node (pitchers moves))
;;A Node is
;; -- (make-node Pitchers ListOf<Move>)
;; interp:
;; Pitchers the configuration of pitchers at this node
;; ListOf<Move> the moves taken to reach this node

;;TEMPLATE:
;;node-fn : Node -> ??
;;(define (node-fn n)
;;  (...
;;   (pitchers-fn (node-pitcher n))
;;   (lom-fn (node-lom n))))

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

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;list-to-pitchers : ListRep -> Pitchers
;; GIVEN: a listrep
;; RETURNS: a list of Pitchers
;; EXAMPLES: TODO
;; STRATEGY: TODO
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
;; STRATEGY: Structural decomposition on lop : Pitchers
(define (pitchers-to-list lop)
  (map make-lrep-from-pitchers lop))



;;make-lrep-from-pitchers : Pitcher -> Rep
;;GIVEN : a single pitcher
;;RETURNS : a rep
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



;;source-pitcher-after-move : Pitcher Pitcher -> Pitcher
;;GIVEN : a source and a target pitcher
;;RETURNS : a new pitcher as a source pitcher, based on if the
;;         amount can be transferred or not.
;;EXAMPLE : See the tests
;;STRATEGY : Struct decompo on pitcher : Pitcher
(define (source-pitcher-after-move ps pt)
  (if (< (- (pitcher-cap pt) (pitcher-mnt pt)) (pitcher-mnt ps))
      (make-pitcher (- (pitcher-mnt ps) (- (pitcher-cap pt) (pitcher-mnt pt)))
                    (pitcher-cap ps))
      (make-pitcher 0 (pitcher-cap ps))))

;;target-pitcher-after-move : Pitcher Pitcher -> Pitcher
;;GIVEN : a source pitcher and a target pitcher
;;RETURNS : get the target pitcher based on the move from source to the target
;;EXAMPLE : See the tests
;;STRATEGY : Struct decompo on pitcher : Pitcher
(define (target-pitcher-after-move ps pt)
  (if (< (- (pitcher-cap pt) (pitcher-mnt pt)) (pitcher-mnt ps))
      (make-pitcher (pitcher-cap pt) (pitcher-cap pt))
      (make-pitcher (+ (pitcher-mnt ps) (pitcher-mnt pt))
                    (pitcher-cap pt))))

;;get-source-pitcher : Pitchers Move -> Pitcher
;;GIVEN : a list of pitchers and a move
;;RETURNS : the source pitcher
;;EXAMPLE :
;;STRATEGY :Struct decompo on move : Move
(define (get-source-pitcher lop move)
  (list-ref lop (move-src move)))

;;get-target-pitcher : Pitchers Move -> Pitcher
;;GIVEN : a list of pitchers and a move
;;RETURNS : the target pitcher
;;EXAMPLE :
;;STRATEGY :Struct decompo on move : Move
(define (get-target-pitcher lop move)
  (list-ref lop (move-tgt move)))

;;pitchers-after-move : Move Pitchers -> Pitchers
;;GIVEN : a list of pitchers and a move
;;RETURNS : a list of pitcher with that move
;;EXAMPLE :
;;STRATEGY : Structural decompo on move : Move
(define (pitchers-after-move move pitchers)
  (local
      ((define src (get-source-pitcher pitchers move))
       (define tgt (get-target-pitcher pitchers move)))
    (map (lambda (pitcher position)
           (cond [(= position (move-src move))
                  (source-pitcher-after-move src tgt)]
                 [(= position (move-tgt move))
                  (target-pitcher-after-move src tgt)]
                 [else pitcher]))
         pitchers
         (build-list (length pitchers) identity))))

;;pitchers-after-moves : Pitchers ListOf<Move> -> Pitchers
;;GIVEN: An internal representation of a set of pitchers, and a sequence
;;of moves
;;WHERE: every move refers only to pitchers that are in the set of pitchers.
;;RETURNS: the internal representation of the set of pitchers that should
;;result after executing the given list of moves, in order, on the given
;;set of pitchers.
;;STRATEGY : HOFC
(define (pitchers-after-moves lop lom)
  (foldl pitchers-after-move lop lom))

(define (fill-first capacities)
  (cons (make-pitcher (first capacities) (first capacities))
        (map (lambda (capacity) (make-pitcher 0 capacity))
             (rest capacities))))

;;solve : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;;GIVEN: a list of the capacities of the pitchers and the goal amount
;;RETURNS: a sequence of moves which, when executed from left to right,
;;results in one pitcher (not necessarily the first pitcher) containing
;;the goal amount.  Returns false if no such sequence exists.
;;EXAMPLE:
;;STRATEGY: Struct decompo on nlep : NEListOf<PosInt>
(define (solve capacities target)
  (if (< (first capacities) target) false
      (search-for target
                  empty
                  (list (make-node (fill-first capacities) empty)))))

(define-test-suite solve-tests
  (check-equal?
   (solve (list 1 2 3 4) 5)
   false
   "This is unsolvable, should return false.")
  (check-equal?
   (solve (list 2) 2)
   empty
   "This case has no moves needed to solve it, should return empty list")
  (check-equal?
   (solve (list 10 7 3) 5)
   (list
    (make-move 0 1)
    (make-move 1 2)
    (make-move 2 0)
    (make-move 1 2)
    (make-move 2 0)
    (make-move 1 2)
    (make-move 0 1)
    (make-move 1 2))
   "This should produce a sequence of
movements resulting in the problem being solved."))


;; making a list of possible moves
(define (possible-moves n)
  (foldr append empty (construct-list-of-moves n)))

(define (construct-list-of-moves n)
  (map 
   ;; PosInt -> 
   (lambda (source)
     (map ;; Constructs a list of moves, does recursion on a list of moves where target and source are not equal (filter handles this)
      ;; PosInt -> Move
      ;; GIVEN: a positive integer representing the target pitcher
      ;; RETURNS: a move based on the source and target
      ;; STRATEGY: STRUCTURAL DECOMPOSITION on move : Move
      (lambda (target)
        (make-move source target))
      (filter ;; creates a list with elements where source and target are NOT equal
       ;; PosInt -> Boolean
       ;; GIVEN: a posint representing the target pitcher
       ;; RETURNS: true iff the target and source are NOT equal
       ;; STRATEGY: Domain Knowledge
       (lambda (target)
         (not (= source target)))
       (build-list n identity))))
   (build-list n identity)))




;; target   - target volume, in any pitcher
;; seen     - list of visited nodes
;; worklist - "edge" nodes
(define (search-for target seen worklist)
  (cond
   [(empty? worklist) false]
   [(target-inside-worklist? target worklist)
    (reverse (node-moves (first worklist)))] ;; puts the list of moves in order
   [else
    (search-for target
                (cons (first worklist) seen) ;; add the nodes in worklist to seen
                (append (rest worklist) ;; appends the rest of worklist to a list we have confirmed we have not seen
                        (filter 
                         ;; Node -> Boolean
                         ;; GIVEN: a node
                         ;; RETURNS: true iff the node is NOT a member of either the seen list of nodes
                         ;; OR the worklist of nodes
                         ;;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
                         (lambda (node)
                                  (not (or (member (node-pitchers node)
                                                   (map node-pitchers seen)) ;; map is recursively going through the nodes within pitchers
                                           (member (node-pitchers node)
                                                   (map node-pitchers worklist))))) ;; map is recursively going through the nodes within worklist
                                (map 
                                 ;; Move -> Node
                                 ;; GIVEN: a move
                                 ;; RETURNS: a node with its list of pitchers and list of moces updated
                                 ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
                                 (lambda (move)
                                       (make-node
                                        (pitchers-after-move move (node-pitchers (first worklist))) ;; Updating list of pitchers with move request
                                        (cons move (node-moves (first worklist))))) ;; updating list of moves, with recent move
                                     (possible-moves (length (node-pitchers (first worklist))))))))])) ;; the length of the list of pitchers is being passed to possible-moves
                                                                                                       ;; possible moves will return a list of possible moves that we can do
                                                                                                       ;; checking that we aren't repeating moves


;; is the target a member of worklist?
(define (target-inside-worklist? target worklist)
  (member target (map pitcher-mnt (node-pitchers (first worklist)))))


;; ;;solve-pitcher : NEListOf<PosInt> PosInt -> Maybe<ListOf<Move>>
;; ;;GIVEN : a list of the capacities of the pitchers and the goal amount
;; ;;RETURNS :  a sequence of moves which, when executed from left to right,
;; ;;results in one pitcher (not necessarily the first pitcher) containing
;; ;;the goal amount.  Returns false if no such sequence exists.
;; ;;EXAMPLE:
;; ;;STRATEGY:
;; (define (solve-pitcher nelp num)
;;   (if (= (first nelp) num)
;;       ;;already reached the goal
;;       empty
;;       (solve-pitcher-helper
;;        (list (make-node (get-current-situation-of-pitchers nelp) empty))
;;        nelp
;;        num
;;        (length nelp))))

;; ;;get-current-situation-of-pitchers : NEListOf<PosInt> -> Pitchers
;; ;;GIVEN:
;; ;;RETURNS : the situation of the pitchers
;; ;;EXAMPLE : (get-current-situation-of-pitchers (list 9 8))
;; ;;    -> (list (make-pitcher 9 9) (make-pitcher 0 8))
;; ;;STRATEGY : Struct decompo on NEListOf<PosInt>
;; (define (get-current-situation-of-pitchers nelp)
;;   (cons (make-pitcher (first nelp) (first nelp))
;;         (get-current-situation-of-lopi (rest nelp))))




;; ;;get-current-situation-of-lopi : NEListOf<PosInt> -> Pitchers
;; ;;GIVEN : a list of capacities of the Pitchers
;; ;;RETURNS : the basic version of the lop
;; ;;EXAMPLE
;; ;;STRATEGY :
;; (define (get-current-situation-of-lopi nelp)
;;   (map
;;    (lambda (n)
;;      (make-pitcher 0 n))
;;    nelp))


;; ;;solve-pitcher-helper: ListOf<Node> NEListOf<PosInt> PosInt PosInt -> Maybe<ListOf<Move>>
;; ;;GIVEN : a list of nodes, a list of capacities and the goal and also the number of pitchers
;; ;;WHERE :
;; ;;EXAMPLE:
;; ;;STRATEGY :
;; ;;HALTER : total number of nodes which is finite and integer
;; ;;         and its decreasing after visting each node
;; (define (solve-pitcher-helper lon lopi goal len)
;;   (local
;;       ;;a list of nodes that are unvisited
;;       ((define unvisited-nodes
;;          (my-set-difference
;;           (passed-nodes (make-node (get-current-situation-of-pitchers lopi) empty) len))
;;          lon))
;;     (cond
;;      [(empty? unvisited-nodes) false]
;;      [else
;;       (solve-pitcher-helper
;;        (append unvisited-nodes lon) unvisited-nodes goal len)])))



;; ;;my-set-difference : ListOf<Node> ListOf<Node> -> ListOf<Node>
;; ;;GIVEN : two list of nodes
;; ;;RETURNS : a list of node considering the first list of nodes,
;; ;;          and checking if a node is not being repeated
;; ;;EXAMPLE :
;; ;;STRATEGY :
;; (define (my-set-difference lon1 lon2)
;;   (cond
;;    [(empty? lon) empty]
;;    [else]))


     (run-tests solve-tests)
    (run-tests list-to-pitchers-tests)
    (run-tests pitchers-to-list-tests)
