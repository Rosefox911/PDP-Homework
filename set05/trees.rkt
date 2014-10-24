;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-roots
 node-to-center
 node-to-sons
 node-to-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

(define-struct world (lon mx my))
;; A World is a (make-world ListOf<Node> Number Number)
;;ListOf<Node>      is a list of nodes which contains the nodes 
;;                  that exist within the world in any order     
;;mx                is the x-coordinate of the mouse
;;my                is the y-coordinate of the mouse

;;template:
;;world-fn : World ->
;(define (world-fn world)
;  (...
;   (world-lon world)
;   (world-mx world)
;   (world-my world)))


(define-struct node (lon selected? x-pos y-pos))
;; A Node is a (make-node ListOf<Node> Boolean Number Number)
;; INTERPRETATION :
;; ListOf<Node>     is a list of nodes which represents the sub-nodes 
;;                  (sons) of the node
;; selected?        is a boolean which represents of the node is selected or not
;; x-pos            is the x coordinate of the center of the node
;; y-pos            is the y coordinate of the center of the node

;;template:
;; node-fn : Node -> ??
;(define (node-fn node)
;  (...
;   (lon-fn (node-lon node))
;   (node-selected? node)
;   (node-x-pos node)
;   (node-y-pos node)))

;; A ListOf<Node> (LON) is either:
;; -- empty           (interp: has no nodes/subnodes within it)
;; -- (cons Node LON) (interp: has nodes/subnodes within it)

;;template:
;; lon-fn : ListOf<Node> -> ??
;(define (lon-fn lon)
;  (cond
;    [(empty? lon) ...]
;    [(cons? lon )
;     (...
;      (node-fn (first lon))
;      (lon-fn (rest lon)))]))


;; A DraggableObjectMouseEvent is a partition of 
;; MouseEvent into the following categories:
;; -- "button-down"         (interp: object is selected)
;; -- "drag"                (interp: drag the object)
;; -- "button-up"           (interp: unselect the object)
;; -- any other mouse event (interp: ignored)

;;template:
;; dome-fn : DraggableObjectMouseEvent -> ??
;(define (dome-fn do)
;  (cond
;    [(mouse=? do "button-down") ...]
;    [(mouse=? do "drag") ...]
;    [(mouse=? do "button-up") ...]
;    [else ...]))


;; A DraggableObjectKeyEvent is a partition of KeyEvent, which is one of
;; -- "t" (interp: t key, at any time creates a ne
;; w root node in the center of the top of the canvas. 
;; The root appears tangent to the top of the canvas and initially has no sons)
;; -- "n" (interp: n key, while a node is selected adds 
;;                 a new son, if there is enough space)
;; -- "d" (interp: d key, while a node is selected 
;;                 deletes the node and its whole subtree.)
;; -- "u" (interp: u key, (whether a node is selected or not)
;;                deletes every node in the upper half of the canvas. 
;;        (If a node is deleted, all of its children are also deleted.)
;; -- any other KeyEvent (interp: ignore)

;;template:
;; draggable-object-kev-fn : DraggableObjectKeyEvent -> ??
;(define (draggable-object-kev-fn kev)
;  (cond 
;    [(key=? kev "t") ...]
;    [(key=? kev "n") ...]
;    [(key=? kev "d") ...]
;    [(key=? kev "u") ...]
;    [else ...]))

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CANVAS-WIDTH 400)
;; Width of the canvas in pixels

(define CANVAS-HEIGHT 400)
;; Height of the canvas in pixels

;;
(define HALF-WIDTH (/ CANVAS-WIDTH 2))
;; half of the width of the canvas

(define HALF-HEIGHT (/ CANVAS-HEIGHT 2))
;; half of the height of the canvas

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; creates an empty-scene with the 
;; CANVAS-WIDTH and CANVAS-HEIGHT constants

(define OBJECT-SIZE 20)
;; the size of the object which represents a node

(define LENGTH 3)
;;the y when creating new child should be 3 times more

(define DOUBLE 2)
;;whenever anything needs to be doubled

(define HALF-OBJECT (/ OBJECT-SIZE 2))
;; Half of the object size

(define UNSELECTED-OBJECT (square OBJECT-SIZE "outline" "green"))
;; the unselected drawing of the object, in this case a square,
;; that represents a node

(define SELECTED-OBJECT (square OBJECT-SIZE "solid" "green"))
;; the selected drawing of the object, in this case a square,
;; that represents a node

(define RED-OBJECT (square OBJECT-SIZE "solid" "red"))
;;the selected square that has no more space for more children

(define INITIAL-NODE (make-node empty false HALF-WIDTH (/ OBJECT-SIZE 2)))
;; the initial node, at the top center of canvas

(define NODE-1 (make-node empty true 50 50))
;; a node with no subnodes, that is selected

(define NODE-2 (make-node empty false 100 100))
;; a node with no subnodes, that is unselected

(define NODE-3 (make-node empty true 150 150))
;; a node with no subnodes, that is unselected

(define NODE-4 (make-node (list NODE-1) false 200 200))
;; a node with a single subnode, that is unselected

(define NODE-5 (make-node empty true -1 -1))
;; a node with no subnodes, that is selected, no space for subnode!

(define NODE-6 (make-node empty true 350 350))
;; node with no subnodes, that is selected

(define INITIAL-LON (list INITIAL-NODE NODE-2))
;; the initial list of nodes, with INITIAL-NODE in it

(define LON-1 (list NODE-1 NODE-2))
;; a list of nodes with 2 elements in it, no subnodes

(define LON-2 (list NODE-3))
;; a list of nodes with 1 element in it, no subnodes

(define LON-3 (list NODE-4))
;; a list of nodes with 2 elements, 1 subnode

(define LON-4 (list LON-3 LON-2))
;; list of lists that 2 nodes are selected

;(define INITIAL-WORLD (make-world INITIAL-LON 0 0))
;; the initial-world, which is empty

(define LON-5 (list (make-node (list NODE-1) false 20 34) INITIAL-NODE))
;; list of nodes with 2 nodes in it, one selected, one unselected

(define LON-6 (list NODE-2))
;; list of nodes with 1 node in it, unselected

(define LON-7 (list NODE-6))
;; list of nodes with 1 node in it, selected

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;initial-world : Any -> World
;;GIVEN: any value
;;RETURNS: an initial world.  The given value is ignored.
;; EXAMPLES: (initial-world 1) -> (make-world empty 0 0)
;; STRATEGY: DOMAIN KNOWLEDGE
(define (initial-world any)
  (make-world empty 0 0))

;; TESTS
(define-test-suite initial-world-tests
  (check-equal? 
   (initial-world 1) 
   (make-world empty 0 0) 
   "Checking to make sure empty world is created"))


;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; EXAMPLES: (run 1) -> the final state of the world
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (run any)
  (big-bang (initial-world any)           
            (on-draw world-to-scene)
            (on-tick world-after-tick .25)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WORLD AFTER TICK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-tick : World - > World
;; GIVEN: a world
;; RETURNS: a world that should follow a clock tick
;; EXAMPLES: (world-after-tick (initial-world 1)) -> (make-world empty 0 0)
;; STRATEGY: DOMAIN KNOWLEDGE
(define (world-after-tick world)
  world)

;; TESTS
(define-test-suite world-after-tick-tests
  (check-equal? 
   (world-after-tick (initial-world 1)) 
   (make-world empty 0 0) 
   "Checking that it returns the same 
    world we do not change anything on tick"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;WORLD TO SCENE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a scene that portrays the given world
;; EXAMPLES: (world-to-scene (initial-world 1)) ->
;; creates a scene with the given world on it
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World
(define (world-to-scene world)
  (draw-lines (world-lon world)
              (draw-lon (world-lon world) EMPTY-CANVAS)))


;; TESTS
(define-test-suite world-to-scene-tests
  (check-equal? 
   (image? 
    (world-to-scene (initial-world 1))) 
   true 
   "Testing to make sure scene is produced"))

;;draw-lon : ListOf<Node> Scene -> Scene
;;GIVEN : a list of nodes and a scene
;;RETURNS : a scene including these nodes on it
;;EXAMPLE : (draw-lon LON-1 EMPTY-CANVAS) ->
;; Scene with the list of nodes on it
;;STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (draw-lon lon s)
  (foldr 
   ;; Node Scene -> Scene
   ;; GIVEN: a node and a scene
   ;; RETURNS: a scene with that node on it and the line
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
   (lambda (node s)
     (place-image
      (choose-node node)
      (node-x-pos node)
      (node-y-pos node)
      (overlay
       (draw-lon (node-lon node) s)
       s))) s lon))

;; TESTS
(define-test-suite draw-lon-tests
  (check-equal? 
   (image? (draw-lon LON-1 EMPTY-CANVAS)) 
   true 
   "Testing to make sure scene is drawn"))


;;choose-node : Node -> Scene
;;GIVEN : a node
;;RETURN : a scene of the node, based on its situation
;;        weather or not its selected and 
;;         also if it can have another child or not
;;EXAMPLE : (choose-node NODE-1) -> the image representation
;; of a node, depending if its selected or unselected.
;;STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (choose-node node)
  (if (node-selected? node)
      (check-for-selected-node-shape node)
      UNSELECTED-OBJECT))

;; TESTS
(define-test-suite choose-node-tests 
  (check-equal? 
   (image? (choose-node NODE-1)) 
   true 
   "Testing to make sure selected node is picked")
  (check-equal? 
   (image? (choose-node NODE-2)) 
   true 
   "Testing to make sure unselected node is picked"))

;;check-for-selected-node-shape : Node -> Scene
;;GIVEN : a node that is selected
;;RETURN : a solid node that is either red or green
;;         based on the node situation
;;EXAMPLE : (check-for-selected-node-shape NODE-1) ->
;; a selected image node if there is enough room on canvas for another node
;; otherwise a red colored node.
;;STRATEGY : STRUCTURAL DECOMPOSITION on : Node
(define (check-for-selected-node-shape node)
  (if (enough-space? node)
      SELECTED-OBJECT
      RED-OBJECT))

;; TESTS
(define-test-suite check-for-selected-node-shape-tests
  (check-equal? 
   (image? (check-for-selected-node-shape NODE-1)) 
   true 
   "Checking to make sure it produced selected node")
  (check-equal? 
   (image? (check-for-selected-node-shape NODE-5)) 
   true 
   "Checking to make sure it red node, no space!"))

;;draw-lines : ListOf<Node> Scene-> Scene
;;GIVEN : a list of nodes
;;RETURN : a scene that draw lines to connect a node with its children
;;EXAMPLE : (draw-lines LON-1 EMPTY-CANVAS) -> draws a scene
;; where a ndoe and its child are connected by a scene-line
;;STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (draw-lines lon s)
  (foldr 
   ;; Node Scene-> Scene
   ;; GIVEN: a node and a scene
   ;; RETURNS: a scene with the subnodes and father nodes connected with lines
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
   (lambda (node s)
     (connect-nodes
      (node-lon node)
      (node-x-pos node)
      (node-y-pos node)
      s)) s lon))

;; TESTS
(define-test-suite draw-lines-tests
  (check-equal? 
   (image?  (draw-lines LON-1 EMPTY-CANVAS)) 
   true 
   "should produce a scene"))

;;connect-nodes : ListOf<Node> Number Number Scene -> Scene
;;GIVEN : a list of nodes, x and y coordinate of the parent node,
;;        a current scene
;;RETURNS : a scene that draw a  line between a node and its children
;;EXAMPLE : (connect-nodes LON-1 50 50 EMPTY-CANVAS) ->  draws a scene
;; where a ndoe and its child are connected by a scene-line
;;STRATEGY : HOFC
(define (connect-nodes lon x y s)
  (foldr 
   ;; Node Scene-> Scene
   ;; GIVEN: a node and a scene
   ;; RETURNS: a scene with the subnodes/father nodes connected
   ;; STRATEGY : Function Composition
   (lambda (node s)
     (connect-with-children node x y s))
   s lon))

;; TESTS
(define-test-suite connect-nodes-tests
  (check-equal? 
   (image? (connect-nodes LON-1 50 50 EMPTY-CANVAS)) 
   true 
   "Checking to make sure it draws the connecting line"))



;;connect-with-children : Node Number Number Scene -> Scene
;;GIVEN : a node, the x and y coordinate of its parent and a scene
;;RETURN : a scene that connects a node with its children with lines
;;EXAMPLE : 
;;  (connect-with-children NODE-1 100 100 EMPTY-CANVAS) -> draws a scene
;; where a ndoe and its child are connected by a scene-line
;;STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (connect-with-children node x y s)
  (scene+line
   (connect-nodes 
    (node-lon node) 
    (node-x-pos node) 
    (node-y-pos node) s)
   (node-x-pos node)
   (node-y-pos node)
   x
   y
   "blue"))

;; TESTS
(define-test-suite connect-with-children-tests
  (check-equal? 
   (image? (connect-with-children NODE-1 100 100 EMPTY-CANVAS)) 
   true 
   "Checking tomake sure it connects line from parent to children"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MOUSE EVENT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-mouse-event : World Number Number DraggableObjectMouseEvent 
;; -> World
;; GIVEN: a world, a number representing the x position 
;; of the mouse when mousevent is triggered
;; a y coordinate given the y position of the mouse when mousevent is triggered
;; and a mouseevent
;; RETURNS: the world that show follow 
;;          the given world after the given mouse event
;; EXAMPLES: 
;; (world-after-mouse-event (make-world LON-1 50 50) 51 51 "button-up") ->
;; (make-world (list (make-node empty false 50 50) 
;; (make-node empty false 100 100)) 50 50)
;; (world-after-mouse-event (make-world LON-1 50 50) 51 51 "button-down") ->
;; (make-world (list (make-node empty true 50 50) 
;; (make-node empty false 100 100)) 51 51)
;; (world-after-mouse-event (make-world LON-1 50 50) 51 51 "drag") ->
;; (make-world (list (make-node empty true 51 51) 
;; (make-node empty false 100 100)) 51 51)
;; (world-after-mouse-event (make-world LON-1 50 50) 51 51 "enter") ->
;; (make-world (list (make-node empty true 50 50) 
;; (make-node empty false 100 100)) 50 50)
;; STRATEGY: STRUCTURAL DECOMPOSITION on mouseevent : DraggableObjectMouseEvent
(define (world-after-mouse-event world x-coord y-coord mouseevent)
  (cond
    [(mouse=? mouseevent "button-up") 
     (world-after-button-up world)]
    [(mouse=? mouseevent "drag") 
     (world-after-drag world x-coord y-coord)]
    [(mouse=? mouseevent "button-down") 
     (world-after-button-down world x-coord y-coord)]
    [else 
     world]))

;; TESTS

(define-test-suite world-after-mouse-event-tests
  (check-equal? 
   (world-after-mouse-event (make-world LON-1 50 50) 51 51 "button-up") 
   (make-world 
    (list 
     (make-node empty false 50 50) (make-node empty false 100 100)) 50 50) 
   "Testing button-up mouse event")
  
  (check-equal? 
   (world-after-mouse-event (make-world LON-1 50 50) 51 51 "button-down")
   (make-world 
    (list 
     (make-node empty true 50 50) (make-node empty false 100 100)) 51 51) 
   "Testing button-down mouse event")
  
  
  (check-equal? 
   (world-after-mouse-event (make-world LON-1 50 50) 51 51 "drag")
   (make-world 
    (list 
     (make-node empty true 51 51) (make-node empty false 100 100)) 51 51) 
   "Testing drag mouse event")
  
  (check-equal? 
   (world-after-mouse-event (make-world LON-1 50 50) 51 51 "enter")
   (make-world 
    (list 
     (make-node empty true 50 50) (make-node empty false 100 100)) 50 50) 
   "Testing else statement with enter"))


;; world-after-button-up : World -> World
;; GIVEN : a world
;; RETURNS : a world where a selected node/nodes should be unselected
;; EXAMPLE : (world-after-button-up (make-world LON-1 0 0)) -> 
;; (make-world 
;; (list (make-node empty false 50 50) (make-node empty false 100 100)) 0 0)
;; STRATEGY : STRUCTURAL DECOMPOSITION on world : World
(define (world-after-button-up world)
  (make-world (button-up-nodes (world-lon world)) 
              (world-mx world) (world-my world)))

(define-test-suite world-after-button-up-tests
  (check-equal? 
   (world-after-button-up (make-world LON-1 0 0)) 
   (make-world 
    (list (make-node empty false 50 50) (make-node empty false 100 100)) 0 0)
   "Testing to make sure selected node is now unselected"))

;; button-up-nodes : ListOf<Node> -> ListOf<Node> 
;; GIVEN : a list of nodes
;; RETURN : unselected if any node is selected
;;          quite simply, just changes selected nodes, to unselected
;; EXAMPLE : (button-up-nodes LON-1) ->
;;      (list (make-node empty false 50 50) (make-node empty false 100 100))
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (button-up-nodes lon)
  (map 
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node that has been updated following a button-up event
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
   (lambda (node)
     (if (node-selected? node)        
         (make-node
          (node-lon node)
          false 
          (node-x-pos node)
          (node-y-pos node))
         (make-node
          (button-up-nodes (node-lon node))
          (node-selected? node)
          (node-x-pos node)
          (node-y-pos node)))) 
   lon))

;; TESTS
(define-test-suite button-up-nodes-tests
  (check-equal? (button-up-nodes LON-1)
                (list (make-node empty false 50 50) 
                      (make-node empty false 100 100))
                "Testing to make sure nodes are unselected"))

;; world-after-drag : World -> World
;; GIVEN : a world
;; RETURNS : a world that selected nodes 
;;          should be dragged based on the mouse movement
;; EXAMPLE : 
;; (world-after-drag (make-world LON-1 50 50) 51 51) ->
;; (make-world (list (make-node empty true 51 51) 
;; (make-node empty false 100 100)) 51 51)
;; STRATEGY : STRUCTURAL DECOMPOSITION on world : World
(define (world-after-drag world x-coord y-coord)
  (make-world
   (drag-nodes (world-lon world) 
               (- x-coord (world-mx world)) 
               (- y-coord (world-my world)))
   x-coord
   y-coord))

;; TESTS
(define-test-suite world-after-drag-tests
  (check-equal? (world-after-drag (make-world LON-1 50 50) 51 51)
                (make-world 
                 (list (make-node empty true 51 51) 
                       (make-node empty false 100 100)) 51 51)
                "Testing to make sure nodes are updated"))


;; drag-nodes: ListOf<Node> Number Number -> ListOf<Node>
;; GIVEN : a list of nodes, x and y coordinate of the mouse
;; RETURN : a list of nodes that moves based on the mouse movement
;; EXAMPLE : 
;; (drag-nodes LON-1 51 51) ->
;; (list (make-node empty true 101 101) (make-node empty false 100 100))
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (drag-nodes lon x-coord y-coord)
  (map 
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node that should follow after the drag mouse event happens
   ;; on that node
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
   (lambda (node)
     (if (node-selected? node)
         (drag-nodes-helper node x-coord y-coord)
         (make-node
          (drag-nodes (node-lon node) x-coord y-coord)
          (node-selected? node)
          (node-x-pos node)
          (node-y-pos node))))
   lon))

;; TESTS
(define-test-suite drag-nodes-tests
  (check-equal? (drag-nodes LON-1 51 51) 
                (list 
                 (make-node empty true 101 101) 
                 (make-node empty false 100 100)) 
                "Testing to make sure nodes are updated"))


;; drag-nodes-helper : Node Number Number -> Node
;; GIVEN : a node whose parent is selected, 
;; x and y coordinate of the mouse
;; RETURN : a node and all its children should move 
;; based on the mouse event's coordinates
;; EXAMPLE : (drag-nodes-helper NODE-1 54 54) ->
;; (make-node empty true 104 104)
;; STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (drag-nodes-helper node x-coord y-coord)
  (make-node
   (move-this-node (node-lon node) x-coord y-coord)
   (node-selected? node)
   (+ (node-x-pos node) x-coord)
   (+ (node-y-pos node) y-coord)))

;; TESTS
(define-test-suite drag-nodes-helper-tests
  (check-equal? (drag-nodes-helper NODE-1 54 54)
                (make-node empty true 104 104) 
                "Testing to make sure node's position is updated"))

; ;move-this-node : ListOf<Node> Number Number -> Node
;; GIVEN : a list of nodes and x and y coordinate of the mouse event
;; RETURNS : the node should move accoringly
;; EXAMPLE : (move-this-node LON-1 51 52) ->
;; (list (make-node empty true 101 102) (make-node empty false 151 152))
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (move-this-node lon dx dy)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node like the original except its position
   ;; has been updated
   ;; STRATEGY: FUNCTIONAL COMPOSITION
   (lambda (node)
     (drag-nodes-helper node dx dy))
   lon))

;; TESTS
(define-test-suite move-this-node-tests
  (check-equal? (move-this-node LON-1 51 52)
                (list 
                 (make-node empty true 101 102) 
                 (make-node empty false 151 152)) 
                "Testing to make sure nodes have moved"))


;; world-after-button-down : World Number Number -> World
;; GIVEN : a world and a x-coordinate and y-coordinate of the mouse event
;; RETURNS : a world that nodes that are in mouse area(x and y) are selected
;; EXAMPLE : (world-after-button-down (make-world LON-1 51 51) 56 56) ->
;; (make-world (list (make-node empty true 50 50) 
;; (make-node empty false 100 100)) 56 56)
;; STRATEGY : STRUCTURAL DECOMPOSITION on world : World
(define (world-after-button-down world x-coord y-coord)
  (make-world 
   (button-down-nodes (world-lon world) x-coord y-coord)
   x-coord
   y-coord))

;; TESTS
(define-test-suite world-after-button-down-tests
  (check-equal? (world-after-button-down (make-world LON-1 51 51) 56 56)
                (make-world (list 
                             (make-node empty true 50 50) 
                             (make-node empty false 100 100)) 56 56) 
                "Testing to make sure nodes (within range) are selected"))

;; button-down-nodes : ListOf<Node> Number Number -> ListOf<Node>
;; GIVEN : a list of nodes and a x-coordinate and y-coordinate 
;; of the mouse event
;; RETURN : selected nodes in the area of the mouse
;;         does map on lon passed to it, then calls select-node
;; EXAMPLE : 
;; (button-down-nodes LON-1 53 52)
;; (list (make-node empty true 50 50) (make-node empty false 100 100))
;;STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (button-down-nodes lon x-coord y-coord)
  (map 
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node like the original except it is now selected
   ;; assuming it meets the criteria
   ;; STRATEGY: FUNCTIONAL COMPOSITION
   (lambda (node)
     (select-node node x-coord y-coord)) 
   lon))

;; TESTS
(define-test-suite button-down-nodes-tests
  (check-equal? 
   (button-down-nodes LON-1 53 52)
   (list (make-node empty true 50 50) 
         (make-node empty false 100 100)) 
   "Testing to make sure nodes are updated"))

;; select-node : Node Number Number -> Node
;; GIVEN : a node and a x-coordinate and y-coordinate of the mouse
;; RETURNS : select the node
;; Gets passed a node by the map happening in extract-nodes
;; checks to see if it is selectable (i.e. within the object)
;; then creates a new node, like the original, except selected
;; EXAMPLE : (select-node NODE-1 51 51) -> (make-node empty true 50 50)
;; STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (select-node node x-coord y-coord)
  (make-node
   (button-down-nodes
    (node-lon node) 
    x-coord 
    y-coord)
   (inside-object?
    (node-x-pos node) 
    (node-y-pos node) 
    x-coord 
    y-coord)
   (node-x-pos node)
   (node-y-pos node)))

;; TESTS
(define-test-suite select-node-tests
  (check-equal? (select-node NODE-1 51 51)
                (make-node empty true 50 50) 
                "Testing to make sure node-selected? is updated"))

;; inside-object? : Number Number Number Number -> Boolean
;; GIVEN :a x-coordinate and y-coordinate of the node
;;       and a x-coordinate and y-coordinate of the mouse
;; RETURNS : true if mouse is in the area of the node
;; gets passed the node's center x and y position,
;; as well as the x and y position the mouse event
;; happened on
;; returns true iff the mouse event happened within the node
;; EXAMPLE :
;; (inside-object? 50 50 54 54) -> true
;; (inside-object? 50 50 71 71) -> false
;;STRATEGY : FUNCTIONAL COMPOSITION
(define (inside-object? node-x node-y mouse-x mouse-y)
  (and
   (<=
    (- node-x HALF-OBJECT)
    mouse-x
    (+ node-x HALF-OBJECT))
   (<=
    (- node-y HALF-OBJECT)
    mouse-y
    (+ node-y HALF-OBJECT))))

;;TESTS:
(define-test-suite inside-object?-tests
  (check-equal? (inside-object? 50 50 54 54) true 
                "Testing where it is inside object")
  (check-equal? (inside-object? 50 50 71 71) false 
                "Testing where it is outside object"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;KEY EVENT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-key-event : World DraggableObjectKeyEvent -> World
;; GIVEN: a world and a keyevent
;; RETURNS: the world that should follow the given world after the given
;; key event.
;; EXAMPLE : 
;; (world-after-key-event (make-world LON-1 50 50) "t") ->
;; (make-world (list (make-node empty false 200 10)
;; (make-node empty true 50 50) (make-node empty false 100 100)) 50 50)
;; (world-after-key-event (make-world LON-1 50 50) "n")
;; (make-world (list (make-node 
;; (list (make-node empty false 50 110)) true 50 50) 
;; (make-node empty false 100 100)) 50 50)
;; (world-after-key-event (make-world LON-1 50 50) "d") ->
;; (make-world (list (make-node empty false 100 100)) 50 50)
;; (world-after-key-event (make-world LON-1 50 50) "u") ->
;; (make-world empty 50 50)
;; (world-after-key-event (make-world LON-1 50 50) "p") ->
;; (make-world (list (make-node empty true 50 50) 
;; (make-node empty false 100 100)) 50 50)
;;STRATEGY : STRUCTURAL DECOMPOSITION on kev : DraggableObjectKeyEvent
(define (world-after-key-event w kev)
  (cond
    [(key=? kev "t") (create-fresh-node w)]
    [(key=? kev "n") (add-new-son w)]
    [(key=? kev "d") (delete-nodes-subtrees w)]
    [(key=? kev "u") (delete-upper-part-nodes w)]
    [else w]))

;; TESTS
(define-test-suite world-after-key-event-tests
  (check-equal? (world-after-key-event (make-world LON-1 50 50) "t")
                (make-world 
                 (list 
                  (make-node empty false 200 10) 
                  (make-node empty true 50 50) 
                  (make-node empty false 100 100)) 50 50)
                "Testing t key event")
  
  (check-equal? (world-after-key-event (make-world LON-1 50 50) "n")
                (make-world 
                 (list 
                  (make-node 
                   (list 
                    (make-node empty false 50 110)) true 50 50) 
                  (make-node empty false 100 100)) 50 50) 
                "Testing with n key event")
  
  (check-equal? (world-after-key-event (make-world LON-1 50 50) "d")
                (make-world 
                 (list 
                  (make-node empty false 100 100)) 50 50) 
                "Testing with d key event")
  
  (check-equal? (world-after-key-event (make-world LON-1 50 50) "u")
                (make-world empty 50 50) 
                "Testing with u key event")
  
  (check-equal? (world-after-key-event (make-world LON-1 50 50) "p")
                (make-world 
                 (list 
                  (make-node empty true 50 50) 
                  (make-node empty false 100 100)) 50 50) 
                "Testing else clause"))

;;create-fresh-node : World -> World
;;GIVEN : a world
;;RETURNS : a world after pressing t
;;EXAMPLE : (create-fresh-node (make-world empty 50 50)) ->
;; (make-world (list (make-node empty false 200 10)) 50 50)
;;STRATEGY : STRUCTURAL DECOMPOSITION on w : World
(define (create-fresh-node w)
  (make-world 
   (cons INITIAL-NODE (world-lon w))
   (world-mx w)
   (world-my w)))

;; TESTS
(define-test-suite create-fresh-node-tests
  (check-equal? (create-fresh-node (make-world empty 50 50))
                (make-world (list (make-node empty false 200 10)) 50 50) 
                "Testing to make sure new node is created"))


;;add-new-son : World -> World
;;GIVEN : a world
;;RETURNS : a world, following pressing n which will
;;          add a new node to the selected node
;;EXAMPLE : (add-new-son (make-world LON-3 50 50)) ->
;; (make-world (list (make-node (list (make-node 
;; (list (make-node empty false 50 110)) true 50 50)) false 200 200)) 50 50)
;; (add-new-son (make-world LON-1 50 50)) -> 
;; (make-world (list (make-node (list 
;; (make-node empty false 50 110)) true 50 50) 
;; (make-node empty false 100 100)) 50 50)
;; (add-new-son (make-world LON-6 50 50)) ->
;; (make-world (list (make-node empty false 100 100)) 50 50)
;;STRATEGY :STRUCTURAL DECOMPOSITION on w : World
(define (add-new-son w)
  (if (check-any-node-selected? (world-lon w))
      (make-world (create-new-child (world-lon w)) 
                  (world-mx w) (world-my w))
      w))

;; TESTS
(define-test-suite add-new-son-tests
  (check-equal? (add-new-son (make-world LON-3 50 50))
                (make-world 
                 (list 
                  (make-node 
                   (list 
                    (make-node 
                     (list 
                      (make-node empty false 50 110)) 
                     true 50 50)) 
                   false 200 200)) 
                 50 50) "Testing to make sure new son is created")
  (check-equal? (add-new-son (make-world LON-6 50 50))
                (make-world (list (make-node empty false 100 100)) 50 50) 
                "Testing to make sure world is returned like before"))

;; one-node-selected? : Node -> Boolean
;; GIVEN : a node
;; RETURN : true iff a node is selected
;; EXAMPLE : (one-node-selected? NODE-1) -> true
;; (one-node-selected? NODE-2) -> false
;; STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (one-node-selected? node)
  (or
   (node-selected? node)
   (check-any-node-selected? (node-lon node))))

;; TESTS
(define-test-suite one-node-selected?-tests
  (check-equal? 
   (one-node-selected? NODE-1)
   true 
   "Testing to make selected node returns true")
  (check-equal? (one-node-selected? NODE-2)
                false
                "Testing to make sure unselected node returns false"))

;; check-any-node-selected? : Listof<Node> -> Boolean
;; GIVEN : a list of nodes
;; RETURNS : True iff one node in the list is selected
;; EXAMPLE : (check-any-node-selected? LON-1) -> true
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (check-any-node-selected? lon)
  (ormap one-node-selected? lon))

;; TESTS
(define-test-suite check-any-node-selected?-tests
  (check-equal? 
   (check-any-node-selected? LON-1)
   true
   "Testing to make sure it returns true, this has a node that is selected"))

;; create-new-child : ListOf<Node> -> ListOf<Node>
;; GIVEN : a list of nodes
;; RETURN : add a new child to the selected nodes
;; EXAMPLE : 
;; (create-new-child LON-1) ->
;; (list (make-node (list (make-node empty false 50 110)) true 50 50) 
;; (make-node empty false 100 100))
;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
(define (create-new-child lon)
  (map
   ;; Node -> Node
   ;; GIVEN: a node
   ;; RETURNS: a node with a new child node attached to it
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
   (lambda (node)
     (if (node-selected? node)
         (create-new-child-helper node)
         (make-node 
          (create-new-child (node-lon node)) 
          (node-selected? node) 
          (node-x-pos node)
          (node-y-pos node)))) 
   lon))

;; TESTS
(define-test-suite create-new-child-tests
  (check-equal? 
   (create-new-child LON-1)
   (list 
    (make-node 
     (list 
      (make-node empty false 50 110)) true 50 50) 
    (make-node empty false 100 100))))

;; create-new-child-helper : Node -> Node
;; GIVEN : a node whose parent is selected
;; RETURN : a node with a new child
;; EXAMPLE :
;; (create-new-child-helper NODE-1) ->
;; (make-node (list (make-node empty false 50 110)) true 50 50)
;; (create-new-child-helper NODE-2) ->
;; (make-node (list (make-node empty false 100 160)) false 100 100)
;; (create-new-child-helper NODE-5) ->
;; (make-node empty true -1 -1)
;; STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (create-new-child-helper node)
  (if (enough-space? node)
      (make-node  
       (cons  (born-new-child
               (node-lon node)
               (node-x-pos node) 
               (node-y-pos node))
              (create-new-child (node-lon node)))
       (node-selected? node)
       (node-x-pos node)
       (node-y-pos node))
      node))

;; TESTS
(define-test-suite create-new-child-helper-tests
  (check-equal? 
   (create-new-child-helper NODE-1)
   (make-node (list (make-node empty false 50 110)) true 50 50) 
   "Testing to make sure new node is created")
  (check-equal? 
   (create-new-child-helper NODE-2)
   (make-node (list (make-node empty false 100 160)) false 100 100) 
   "Testing to make sure new node is created")
  (check-equal? 
   (create-new-child-helper NODE-5) (make-node empty true -1 -1)
   "Testing to make sure no new node is created"))

;;born-new-child : ListOf<Node> Number Number -> ListOf<Node>
;;GIVEN : a list of node for one node, and its x-pos and y-pos
;;RETURN : a new list of node with new node
;;EXAMPLE : (born-new-child LON-1 50 51) ->
;; (make-node empty false 10 111)
;;STRATEGY : Function Composition
(define (born-new-child lon x y)
  (make-node empty 
             false 
             (check-min-x lon x)
             (get-y y)))

;; TESTS
(define-test-suite born-new-child-tests
  (check-equal?
   (born-new-child LON-1 50 51)
   (make-node empty false 10 111) 
   "Checking to make sure born-new-child is working on LON-1"))

;; enough-space? : Node -> Boolean
;; GIVEN : a node
;; RETURN : whether or not it can have a new child, based on
;; if there is enough space on canvas
;; EXAMPLE :
;; (enough-space? NODE-1) -> true
;; (enough-space? NODE-5) ->false
;; STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (enough-space? node)
  (and (< (get-y (node-y-pos node)) CANVAS-HEIGHT)
       (>= (- (get-min-x (node-lon node) (node-x-pos node)) 
              (* OBJECT-SIZE DOUBLE)) (/ OBJECT-SIZE DOUBLE))
       (< (get-min-x (node-lon node) (node-x-pos node)) 
          (- CANVAS-WIDTH (/ OBJECT-SIZE DOUBLE)))))

;; TESTS
(define-test-suite enough-space?-tests
  (check-equal? (enough-space? NODE-1) 
                true 
                "Testing to make sure we have enough room")
  (check-equal? (enough-space? NODE-5) 
                false 
                "Testing to make sure it is false, NOT enough room"))

;; get-min-x : ListOf<Node> Number -> Number
;; GIVEN : a list of nodes and the parent's x
;; RETURN : min x among them
;; EXAMPLE : (get-min-x LON-1 50) -> 50
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (get-min-x lon x)
  (foldr
   ;; Node Number-> Number
   ;; GIVEN: a node and the parent x-coordinate
   ;; RETURNS: a number representing the minimum x coordinaet among
   ;; the node and subnodes
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
   (lambda (node x) 
     (min x (node-x-pos node))) 
   x
   lon))

;; TESTS
(define-test-suite get-min-x-tests
  (check-equal? 
   (get-min-x LON-1 50)     
   50 
   "Testing to make sure result is lowest possible"))

;; check-min-x : ListOf<Node> Number -> Number
;; GIVEN : a list of nodes and the x-coordinate of the parent
;; RETURN : get the x that is fine as a child
;; EXAMPLE : (check-min-x LON-1 50) -> 10
;; STRATEGY : FUNCTION COMPOSITION
(define (check-min-x lon x)
  (if (= (length lon) 0) 
      x
      (- (get-min-x lon x) (* OBJECT-SIZE 2))))

;; TESTS
(define-test-suite check-min-x-tests
  (check-equal? 
   (check-min-x LON-1 50) 
   10 
   "Testing to make sure result is 10"))

;; get-y : Number -> Number
;; GIVEN : the parent's y-pos
;; RETURNS : what the child y-pos should be
;; EXAMPLE : (get-y 10) -> 70
;; STRATEGY : DOMAIN KNOWLEDGE
(define (get-y y)
  (+ (* OBJECT-SIZE LENGTH) y))

;; TESTS
(define-test-suite get-y-tests
  (check-equal? 
   (get-y 10) 
   70 
   "Testing to make sure result is 70"))

;; get-selected-nodes : ListOf<Node> -> ListOf<Node>
;; GIVEN : A list of nodes
;; RETURN : this will filter the selected nodes, return
;;         those that match the condition
;; EXAMPLE : (get-selected-nodes LON-1) ->
;; (list (make-node empty true 50 50))
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (get-selected-nodes lon)
  (filter 
   ;; Node -> Boolean
   ;; GIVEN: a node
   ;; RETURNS: a boolean determinig if the node is selected or not
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on node: Node
   (lambda (node) 
     (node-selected? node)) 
   lon))  

;; TESTS
(define-test-suite get-selected-nodes-tests
  (check-equal? 
   (get-selected-nodes LON-1)
   (list (make-node empty true 50 50)) 
   "Testing to make sure it extracts selected node"))

;; delete-upper-part-nodes : World -> World
;; GIVEN : a world
;; RETURNS : a new world where nodes in upper half of the canvas
;;         and its children are deleted
;; EXAMPLE :
;; (delete-upper-part-nodes (make-world LON-6 50 50)) ->
;; (make-world empty 50 50)
;; (delete-upper-part-nodes (make-world LON-7 50 50)) ->
;; (make-world (list (make-node empty true 350 350)) 50 50)
;; STRATEGY :STRUCTURAL DECOMPOSITION on w : World
(define (delete-upper-part-nodes w)
  (if (nodes-in-upper-part? (world-lon w))
      (make-world (delete-upper-nodes (world-lon w)) (world-mx w) (world-my w))
      w))

;; TESTS
(define-test-suite delete-upper-part-nodes-tests
  (check-equal? 
   (delete-upper-part-nodes (make-world LON-6 50 50))
   (make-world empty 50 50) 
   "Testing to make sure world is empty")
  (check-equal? 
   (delete-upper-part-nodes (make-world LON-7 50 50))
   (make-world (list (make-node empty true 350 350)) 50 50) 
   "Testing to make sure world is NOT empty"))

;; check-all-node-posn? : Node -> Boolean
;; GIVEN : a node
;; RETURNS : whether or not the node is in the upper half of the canvas
;; EXAMPLE : 
;; (check-all-node-posn? NODE-1) -> true
;; (check-all-node-posn? NODE-6) -> false
;; STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (check-all-node-posn? node)
  (or
   (<= (node-y-pos node) HALF-HEIGHT)
   (nodes-in-upper-part? (node-lon node))))

;; TESTS
(define-test-suite check-all-node-posn?-tests
  (check-equal? 
   (check-all-node-posn? NODE-1) 
   true 
   "Testing to make sure node is in upper part of canvas")
  (check-equal? 
   (check-all-node-posn? NODE-6) 
   false 
   "testing to make sure result is false, node is NOT in upper part of canvas"))


;; check-all-node-posn : ListOf<Node> -> Boolean
;; GIVEN : a list of nodes
;; RETURNS : true if one node is in the upper part
;; EXAMPLE : (nodes-in-upper-part? LON-5) -> true
;; STRATEGY : HIGHER ORDER FUNCTION COMPOSITION
(define (nodes-in-upper-part? lon)
  (ormap check-all-node-posn? lon))

;; TESTS
(define-test-suite nodes-in-upper-part?-tests
  (check-equal? 
   (nodes-in-upper-part? LON-5) 
   true 
   "Testing to make sure result is true, this LON is IN upper part"))

;;delete-upper-nodes : ListOf<Nodes> -> ListOf<Nodes>
;;GIVEN : a list of nodes
;;RETURN : a list of nodes, that any node that is in upper part (if any)
;;         with its children will be deleted
;;EXAMPLE :(delete-upper-nodes LON-5) -> empty
;;STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
(define (delete-upper-nodes lon)
  (map 
   delete-upper-nodes-helper     
   (filter
    ;; Node -> LON
    ;; GIVEN: a node
    ;; RETURNS: a lon except with all nodes below the HALF-HEIGHT deleted
    ;; STRATEGY: STRUCTURAL DECCOMPOSITION on node : Node
    (lambda (node) 
      (not (<= (node-y-pos node) HALF-HEIGHT)))
    lon)))


;; TESTS
(define-test-suite delete-upper-nodes-tests
  (check-equal? 
   (delete-upper-nodes LON-5) 
   empty 
   "Testing to make sure all nodes in LON-5 are deleted"))

;; delete-upper-nodes-helper : Node -> Node
;;GIVEN : a node
;;RETURN : a node that if it any of its children are selected,
;;        they and their children will be deleted
;;EXAMPLE : See the tests
;;STRATEGY : STRUCTURAL DECOMPOSITION on node : Node
(define (delete-upper-nodes-helper node)
  (make-node (delete-upper-nodes (node-lon node))
             (node-selected? node)
             (node-x-pos node)
             (node-y-pos node)))

;; TESTS
(define-test-suite delete-upper-nodes-helper-tests
  (check-equal? 
   (delete-upper-nodes-helper NODE-4) 
   (make-node empty false 200 200) 
   "Testing to make sure all subnodes are deleted"))

;;delete-nodes-subtrees : World -> World
;;GIVEN : a world
;;RETURNS : a world after selected nodes are deletes
;;EXAMPLE : 
;; (delete-nodes-subtrees (make-world LON-3 50 50)) ->
;; (make-world (list (make-node empty false 200 200)) 50 50)
;;STRATEGY: STRUCTURAL DECOMPOSITION on w : World
(define (delete-nodes-subtrees w)
  (make-world 
   (delete-selected-nodes (world-lon w))           
   (world-mx w) 
   (world-my w)))

;; TESTS
(define-test-suite delete-nodes-subtrees-tests
  (check-equal? 
   (delete-nodes-subtrees (make-world LON-3 50 50))
   (make-world (list (make-node empty false 200 200)) 50 50) 
   "Testing to make sure all subnodes are deleted"))

;;delete-selected-nodes : ListOf<Node> -> ListOf<Node>
;;GIVEN : a list of nodes
;;RETURNS : a list of nodes where selected nodes are deleted
;;EXAMPLE : (delete-selected-nodes LON-5) ->
;; (list (make-node empty false 20 34) (make-node empty false 200 10))
;;STRATEGY : Higher Order Function composition
(define (delete-selected-nodes lon)
  (map 
   delete-nodes-helper     
   (filter 
    ;; Node -> Boolean
    ;; GIVEN: a node
    ;; RETURNS : True if the node is not selected
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
    (lambda (node) 
      (not (node-selected? node))) 
    lon)))

;; TESTS
(define-test-suite deleted-selected-nodes-tests
  (check-equal? 
   (delete-selected-nodes LON-5)
   (list 
    (make-node empty false 20 34) 
    (make-node empty false 200 10)) 
   "Testing to make sure selected nodes in LON are deleted"))

;; delete-nodes-helper : Node -> Node
;;GIVEN : a node
;;RETURN : a node that if any of its children are selected,
;;        node and its children will be deleted
;;EXAMPLE : (delete-nodes-helper NODE-4) ->
;; (make-node empty false 200 200)
;;STRATEGY : Structural decomposition on node : Node
(define (delete-nodes-helper node)
  (make-node 
   (delete-selected-nodes (node-lon node))            
   (node-selected? node) 
   (node-x-pos node) 
   (node-y-pos node))) 


;; TESTS
(define-test-suite delete-nodes-helper-tests
  (check-equal? 
   (delete-nodes-helper NODE-4)
   (make-node empty false 200 200)
   "Testing to make sure subnodes are deleted"))

;; world-to-roots : World -> ListOf<Node>
;; GIVEN: a World
;; RETURNS: a list of all the root nodes in the given world.
;; EXAMPLES:
;; (world-to-roots (make-world LON-4 50 50)) ->
;; (list (list (make-node (list (make-node empty true 50 50)) false 200 200))
;; (list (make-node empty true 150 150)))
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World
(define (world-to-roots world)
  (world-lon world))

;; TESTS
(define-test-suite world-to-roots-tests
  (check-equal? 
   (world-to-roots (make-world LON-4 50 50))
   (list 
    (list 
     (make-node 
      (list 
       (make-node empty true 50 50)) 
      false 200 200)) 
    (list 
     (make-node empty true 150 150))) 
   "Testing to make sure LON is extracted from world"))

;; node-to-center : Node -> Posn
;; GIVEN: a node
;; RETURNS: the center of the given node as it is to be displayed on the scene.
;; EXAMPLES: (node-to-center NODE-1) -> (make-posn 50 50)
;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
(define (node-to-center node)
  (make-posn 
   (node-x-pos node) 
   (node-y-pos node)))

;; TESTS
(define-test-suite node-to-center-tests
  (check-equal? 
   (node-to-center NODE-1)
   (make-posn 50 50) 
   "Testing to make sure node's x and y position
 are extracted and posn is created"))

;; node-to-sons : Node -> ListOf<Node>
;; GIVEN: a node
;; returns: a list of its subnodes if there are any, otherwise empty
;; EXAMPLES:
;; (node-to-sons NODE-3) -> empty
;; (node-to-sons NODE-4) -> (list (make-node empty true 50 50))
;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
(define (node-to-sons node)
  (node-lon node))

;; TESTS
(define-test-suite node-to-sons-tests
  (check-equal? 
   (node-to-sons NODE-3) 
   empty 
   "Testing to make sure empty is extracted from NODE-3's LON")
  (check-equal? 
   (node-to-sons NODE-4) 
   (list (make-node empty true 50 50)) 
   "Testing to make sure a list os extracted from NODE-4's LON"))

;; node-to-selected? : Node -> Boolean
;; GIVEN: a node
;; RETURNS: true iff the node is selected
;; EXAMPLES:
;; (node-to-selected? NODE-1) -> true
;; (node-to-selected? NODE-2) -> false
;; STRATEGY: STRUCTURAL DECOMPOSITION on node : Node
(define (node-to-selected? node)
  (node-selected? node))

;; TESTS
(define-test-suite node-to-selected?-tests
  (check-equal? 
   (node-to-selected? NODE-1)
   true 
   "Testing to make sure node that is selected, returns true")
  (check-equal? 
   (node-to-selected? NODE-1)
   true 
   "Testing to make sure node that is NOT selected, returns false"))

;; TESTS
(run-tests initial-world-tests)
(run-tests world-after-tick-tests)
(run-tests world-to-scene-tests)
(run-tests draw-lon-tests)
(run-tests choose-node-tests)
(run-tests check-for-selected-node-shape-tests)
(run-tests draw-lines-tests)
(run-tests connect-nodes-tests)
(run-tests connect-with-children-tests)
(run-tests world-after-button-up-tests)
(run-tests world-after-button-down-tests)
(run-tests world-after-mouse-event-tests)
(run-tests button-up-nodes-tests)
(run-tests world-after-drag-tests)
(run-tests drag-nodes-tests)
(run-tests drag-nodes-helper-tests)
(run-tests move-this-node-tests)
(run-tests world-after-button-down-tests)
(run-tests button-down-nodes-tests)
(run-tests select-node-tests)
(run-tests inside-object?-tests)
(run-tests world-after-key-event-tests)
(run-tests create-fresh-node-tests)
(run-tests add-new-son-tests)
(run-tests one-node-selected?-tests)
(run-tests check-any-node-selected?-tests)
(run-tests create-new-child-tests)
(run-tests create-new-child-helper-tests)
(run-tests born-new-child-tests)
(run-tests enough-space?-tests)
(run-tests get-min-x-tests)
(run-tests check-min-x-tests)
(run-tests get-y-tests)
(run-tests get-selected-nodes-tests)
(run-tests delete-upper-part-nodes-tests)
(run-tests check-all-node-posn?-tests)
(run-tests nodes-in-upper-part?-tests)
(run-tests delete-upper-nodes-tests)
(run-tests delete-nodes-subtrees-tests)
(run-tests deleted-selected-nodes-tests)
(run-tests delete-nodes-helper-tests)
(run-tests delete-upper-nodes-helper-tests)
(run-tests world-to-roots-tests)
(run-tests node-to-center-tests)
(run-tests node-to-sons-tests)
(run-tests node-to-selected?-tests)