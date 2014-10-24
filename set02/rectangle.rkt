;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

;; run with (run 0)

(provide run)
(provide initial-world)
(provide world-selected?)
(provide world-to-center)
(provide world-after-mouse-event)

;; DATA DEFINITIONS

(define-struct world
  (coordinates world-selected? circle-offset))
;; A World is a (make-world Posn Boolean Posn)
;; Interpretation:
;; coordinates is the posn that holds 
;; the x and y coordinates in the middle of the rectangle
;; world-selected? determines whether 
;; or not the rectangle is selected.
;; circle-offset is the posn that holds the the x coordinate
;; offset from rectangle's x coordinate to the 
;; INDICATOR circle's x coordinate and
;; the y coordinate offset from rectangle's y coordinate
;; to the INDICATOR circle's y coordinate.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (...
;   (world-coordinates w)
;   (world-y-pos w)
;   (world-world-selected? w)
;   (world-circle-offset w)))

;; template:
;; posn-fn : Posn -> ??
;(define (posn-fn po)
;  (...
;   (posn-x posn)
;   (posn-y posn)))

;; A DraggableRectangleMouseEvent is a partition of 
;; MouseEvent into the following categories:
;; -- "button-down" (interp: rectangle is selected)
;; -- "drag" (interp: drag the rectangle)
;; -- "button-up" (interp: unselect the rectangle)
;; -- any other mouse event (interp: ignored)

;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))

;; NOTE: None of the draw-* function check that the inputs entered are correct
;; This is done by the function that passes the values to them.

;; END OF DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS
(define RECTANGLE-DRAWING (rectangle 100 60 "solid" "green"))
;; defines the size and color 
;; of the rectangle when it is NOT selected.

(define SELECTED-RECTANGLE-DRAWING (rectangle 100 60 "outline" "green"))
;; defines the size and color 
;; of the rectangle when it IS selected.

(define INDICATOR (circle 5 "solid" "red"))
;; defines the size and color of the indicator circle.
;; Used to indicate where the mouse grabbed the rectangle.

(define CANVAS-WIDTH 400)
;; defines the width of the canvas (scene) 
;; upon which we will place the rectangle.

(define CANVAS-HEIGHT 300)
;; defines the height of the canvas (scene) 
;; upon which we will place the rectangle.

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; creates an empty-scene with the 
;; CANVAS-WIDTH and CANVAS-HEIGHT constants

(define HALF-RECTANGLE-WIDTH (/ 100 2))
;; defines half of the width dimension for the rectangle.

(define HALF-RECTANGLE-HEIGHT (/ 60 2))
;; defines half of the height dimension for the rectangle.

(define INITIAL-X 200)
;; defines the initial x coordinate of the rectangle on the canvas.

(define INITIAL-Y 150)
;; defines the initial y coordinate of the rectangle on the canvas.


(define MIN-X-COORDINATE 50)
;; The minimum the rectangle's center x coordinate 
;; can be so it stays within canvas.

(define MAX-X-COORDINATE 350)
;; The maximum the rectangle's center x coordinate 
;; can be so it stays within canvas.

(define MIN-Y-COORDINATE 30)
;; The minimum the rectangle's center y coordinate 
;; can be so it stays within canvas.

(define MAX-Y-COORDINATE 270)
;; The maximum the rectangle's center x coordinate 
;; can be so it stays within canvas.

(define STARTING-WORLD-SELECTED 
  (make-world (make-posn INITIAL-X INITIAL-Y) true (make-posn 0 0)))
;; Initial-world that is selected, with 0 circle offset

(define STARTING-WORLD-UNSELECTED 
  (make-world (make-posn INITIAL-X INITIAL-Y) false (make-posn 0 0)))
;; Initial-world that is unselected, with 0 circle offset


;; END OF CONSTANTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-selected? : World -> Boolean
;; GIVEN a world
;; RETURNS: true iff the rectangle is selected.
;; EXAMPLES:
;; (world-selected? STARTING-WORLD-SELECTED) -> true
;; (world-selected? STARTING-WORLD-UNSELECTED)-> false
;; STRATEGY: STRUCTURCAL DECOMPOSITION on world : World

(define (world-selected? world)
  (world-world-selected? world))

;; TESTS
(define-test-suite world-selected?-tests
 (check-equal? 
  (world-selected? STARTING-WORLD-SELECTED) 
  true "Checks if the world is selected.")
  (check-equal? 
   (world-selected? STARTING-WORLD-UNSELECTED) 
   false "Checks if the world is selected."))
  (run-tests world-selected?-tests)

;; draw-indicator : Number Number Posn -> Scene
;; GIVEN: a number representing the x coordinate of the middle of the rectangle
;; a number representing the y coordinate of the middle of the rectangle
;; a posn indicating the offset of the 
;; indiactor circle from the center of the rectangle
;; RETURNS: a scene with the indicator circle drawn on it
;; EXAMPLES:
;; (draw-indicator 80 80 (make-posn 10 10)) -> Scene with indicator circle
;; drawn on it
;; STRATEGY: STRUCTURAL DECOMPOSITION on indicatorposn : Posn

(define (draw-indicator rect-x rect-y indicatorposn)
  (place-image INDICATOR
               (-  rect-x (posn-x indicatorposn))
               (- rect-y (posn-y indicatorposn))
               EMPTY-CANVAS))

;; TESTS
(define-test-suite draw-indicator-tests
  (check-equal? 
   (image? (draw-indicator 80 80 (make-posn 10 10))) 
   true "Draw-indicator drew an image"))
  (run-tests draw-indicator-tests)

;; draw-selected : Posn Posn -> Scene
;; GIVEN: a posn representing the coordinates
;; of the middle of a rectangle and a posn representing
;; the offset of the INDICATOR circle from this point.
;; RETURNS: a scene that portrays the world
;; EXAMPLES:
;; (draw-selected (make-posn 50 50) (make-posn 25 25)) ->
;; (place-image SELECTED-RECTANGLE-DRAWING (posn-x (make-posn 50 50))
;; (posn-y (make-posn 50 50)) (place-image INDICATOR 
;; (posn-x (make-posn 25 25)) (posn-y (make-posn 25 25)) EMPTY-CANVAS))
;; STRATEGY: STRUCTURAL DECOMPOSITION on rectangleposn : Posn

(define (draw-selected rectangleposn indicatorposn)
  (place-image SELECTED-RECTANGLE-DRAWING 
               (posn-x rectangleposn) 
               (posn-y rectangleposn) 
               (draw-indicator (posn-x rectangleposn) 
                               (posn-y rectangleposn) 
                               indicatorposn)))

;; TESTS
(define-test-suite draw-selected-tests
 (check-true (image? (draw-selected (make-posn 50 50) (make-posn 25 25))))
             "Testing to make sure draw-selected produces image")
(run-tests draw-selected-tests)

;; draw-unselected : Posn -> Scene
;; GIVEN: a posn representing the coordinates 
;; of the middle of the rectangle
;; Returns: a scene that portrays the world
;; EXAMPLES:
;; (draw-unselected (make-posn 50 50) (make-posn 25 25))
;; STRATEGY: STRUCTURAL DECOMPOSITION on posn : Posn
(define (draw-unselected posn)
  (place-image RECTANGLE-DRAWING 
               (posn-x posn) 
               (posn-y posn) 
               EMPTY-CANVAS))

;; TESTS
(define-test-suite draw-unselected-tests
 (check-true (image? (draw-unselected (make-posn 50 50))))
             "Testing to make sure draw-unselected produces image")
(run-tests draw-unselected-tests)

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a scene that portrays the given world
;; EXAMPLES:
;; (world-to-scene (initial-world 50)) -> 
;; Places the unselected rectangle on the canvas at the INITIAL-X AND INITIAL-Y
;; (world-to-scene STARTING-WORLF-SELECTED) -> 
;; Places the selected rectangle on the canvas at INTIAL-X AND INITIAL-Y 
;; as well as the INDICATOR circle
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-to-scene world)
  (if (equal? (world-selected? world) false) 
      (draw-unselected (world-coordinates world)) 
      (draw-selected (world-coordinates world) 
                     (world-circle-offset world))))


;; TESTS
(define-test-suite world-to-scene-tests
 (check-true (image? (world-to-scene 
                      (make-world (make-posn 150 150) false (make-posn 0 0))))
             "world-to-scene produces unselected rectangle")
  (check-true (image? (world-to-scene 
                      STARTING-WORLD-SELECTED))
             "world-to-scene produces selected rectangle"))
(run-tests world-to-scene-tests)

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: the initial world.
;; EXAMPLES:
;; (initial-world 10) -> 
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; (initial-world "a") -> 
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; STRATEGY: FUNCTIONAL COMPOSITION
;; NOTE: This function ignores its argument.

(define (initial-world any)
(make-world 
 (make-posn INITIAL-X INITIAL-Y) 
 false
 (make-posn 0 0)))

;; TESTS
(define-test-suite initial-world-tests
 (check-equal? (initial-world 10)  (make-world 
                                    (make-posn 200 150) 
                                    false (make-posn 0 0)) 
               "initial-world returns the default world, defined by contacts.")
  (check-equal? (initial-world "a")  (make-world 
                                    (make-posn 200 150) 
                                    false (make-posn 0 0)) 
               "initial-world returns the default world, defined by contacts."))
(run-tests initial-world-tests)


;; fix-coordinates : Posn -> Posn
;; GIVEN: a Posn
;; RETURNS: a Posn that has been verified to be within the canvas
;; if it is not, it will be set to the border min/max of that coordinate
;; EXAMPLES:
;; (fix-coordinates (make-posn 45 30)) -> (make-posn 50 30)
;; (fix-coordinates (make-posn 45 29)) -> (make-posn 50 30)
;; (fix-coordinates (make-posn 400 30)) -> (make-posn 350 30)
;; (fix-coordinates (make-posn 400 300)) -> (make-posn 350 270)
;; STRATEGY: Structural Decomposition on posn : Posn

(define (fix-coordinates posn)
  (make-posn
   [cond
     [(< (posn-x posn) MIN-X-COORDINATE) MIN-X-COORDINATE]
     [(> (posn-x posn) MAX-X-COORDINATE) MAX-X-COORDINATE]
     [else (posn-x posn)]]
   [cond
     [(< (posn-y posn) MIN-Y-COORDINATE) MIN-Y-COORDINATE]
     [(> (posn-y posn) MAX-Y-COORDINATE) MAX-Y-COORDINATE]
     [else (posn-y posn)]]))

;; TESTS
(define-test-suite fix-coordinates-tests
  (check-equal? 
   (fix-coordinates (make-posn 45 30)) 
   (make-posn 50 30) "x coordinate should be corrected")
  (check-equal? 
   (fix-coordinates (make-posn 45 29)) 
   (make-posn 50 30) "both coordinate should be corrected")
  (check-equal? 
   (fix-coordinates (make-posn 400 30)) 
   (make-posn 350 30) "x coordinate should be corrected")
  (check-equal? 
   (fix-coordinates (make-posn 400 300)) 
   (make-posn 350 270) "both coordinates should be corrected"))
(run-tests fix-coordinates-tests)


;; world-to-center : World -> Posn
;; GIVEN: a world
;; RETURNS: the coordinates of the center of the rectangle as a Posn
;; EXAMPLES:
;; (world-to-center STARTING-WORLD-SELECTED)
;; -> (make-posn 200 150)
;; (world-to-center STARTING-WORLD-UNSELECTED)
;; -> (make-posn 200 150)
;; STRATEGY: Structural Decomposition on world : World
  
(define (world-to-center world)
  (world-coordinates world))

;; TESTS
(define-test-suite world-to-center-tests
 (check-equal? 
  (world-to-center STARTING-WORLD-SELECTED) 
  (make-posn 200 150) 
  "world-to-center produces a posn representing coordinates of rectangle")
  (check-equal? 
   (world-to-center STARTING-WORLD-UNSELECTED) 
   (make-posn 200 150) 
   "world-to-center produces a posn representing coordinates of rectangle"))
(run-tests world-to-center-tests)

;; calc-offset : Number Number Posn -> Posn
;; GIVEN: a number representing the x coordinate 
;; of the proposed world. A number representing the y coordinate
;; of the proposed world. A posn representing the
;; current position of the middle of the rectangle on the canvas.
;; RETURNS: a posn representing the offset of the 
;; INDICATOR circle from the midde of the rectangle
;; EXAMPLES:
;; (calc-offset 201 150 (make-posn 200 150)) -> (make-posn -1 0)
;; (calc-offset 60 150 (make-posn 70 150)) -> (make-posn 10 0)
;; (calc-offset 200 150 (make-posn 50 30)) -> (make-posn -150 -120)
;; (calc-offset 200 150 (make-posn 50 30)) -> (make-posn 50 30)
;; STRATEGY: Structural Decomposition on posn : Posn

(define (calc-offset x y posn)
  (make-posn (- (posn-x posn) x) (- (posn-y posn) y)))

;; TESTS
(define-test-suite calc-offset-tests
  (check-equal? 
   (calc-offset 201 150 (make-posn 200 150)) 
   (make-posn -1 0) 
   "Testing to make sure calc-offset can handle negative offset")
  (check-equal? 
   (calc-offset 60 150 (make-posn 70 150)) 
   (make-posn 10 0) 
   "Testing to make sure calc-offset can handle positive offset")
  (check-equal? 
   (calc-offset 200 150 (make-posn 50 30)) 
   (make-posn -150 -120) 
   "Testing to make sure calc-offset can handle large offset")
  (check-equal? 
   (calc-offset 50 30 (make-posn 50 30)) 
   (make-posn 0 0) 
   "Testing to make sure calc-offset can handle 0 offset"))
(run-tests calc-offset-tests)

;; inside-rectangle? : Posn Number Number -> Boolean
;; GIVEN: a posn, a number representing a x coordinate on the canvas,
;; and a number representing a y coordinate on the canvas.
;; RETURNS: true iff the given coordinate is inside 
;; the bounding box of the rectangle.
;; EXAMPLES:
;;(inside-rectangle? (world-coordinates STARTING-WORLD-SELECTED) 150 150) -> 
;; true
;;(inside-rectangle? (world-coordinates STARTING-WORLD-SELECTED) 300 300) -> 
;; false
;;(inside-rectangle? (world-coordinates STARTING-WORLD-SELECTED) 150 200) -> 
;; false
;;(inside-rectangle? (world-coordinates STARTING-WORLD-SELECTED) 200 150) -> 
;; true
;;(inside-rectangle? (world-coordinates STARTING-WORLD-SELECTED) 250 180) -> 
;; true
;;(inside-rectangle? (world-coordinates STARTING-WORLD-SELECTED) 251 180) -> 
;; false
;; STRATEGY: STRUCTURAL DECOMPOSITION on coordinates : Posn

(define (inside-rectangle? coordinates x y)
  (and
    (<= 
      (- (posn-x coordinates) HALF-RECTANGLE-WIDTH)
      x
      (+ (posn-x coordinates) HALF-RECTANGLE-WIDTH))
    (<= 
      (- (posn-y coordinates) HALF-RECTANGLE-HEIGHT)
      y
      (+ (posn-y coordinates) HALF-RECTANGLE-HEIGHT))))

;; TESTS
(define-test-suite inside-rectangle?-tests
  (check-equal? (inside-rectangle? 
                 (world-coordinates STARTING-WORLD-SELECTED) 150 150) 
                true "Given coordinate should be inside rectangle")
  (check-equal? (inside-rectangle? 
                 (world-coordinates STARTING-WORLD-SELECTED) 300 300) 
                false "Given coordinate should NOT inside rectangle")
  (check-equal? (inside-rectangle? 
                 (world-coordinates STARTING-WORLD-SELECTED) 150 200) 
                false "Given coordinate should NOT inside rectangle")
  (check-equal? (inside-rectangle? 
                 (world-coordinates STARTING-WORLD-SELECTED) 200 150) 
                true "Given coordinate should be inside rectangle")
  (check-equal? (inside-rectangle? 
                 (world-coordinates STARTING-WORLD-SELECTED) 250 180) 
                true "Given coordinate should be inside rectangle (at edge)")
  (check-equal? (inside-rectangle? 
                 (world-coordinates STARTING-WORLD-SELECTED) 251 180) 
                false "Given coordinate should NOT be inside rectangle (1px)"))
(run-tests inside-rectangle?-tests)

;; world-after-button-down : World Number Number -> World
;; GIVEN: a world, a number representing 
;; the x coordinate of the proposed world 
;; a number representing the y coordinate of the proposed world.
;; RETURNS: The world that follows a button-down mouseevent 
;; at a given location.
;; if the button-down event is inside the rectangle, 
;; returns a world that is now selected which 
;; has the offset of circle-x-offset and circle-y-offset calculated.
;; EXAMPLES:
;; (world-after-button-down STARTING-WORLD-UNSELECTED 200 150) -> 
;; (make-world (make-posn 200 150) true (make-posn 0 0))
;; (world-after-button-down STARTING-WORLD-UNSELECTED 250 180) -> 
;; (make-world (make-posn 200 150) true (make-posn -50 -30))
;; (world-after-button-down STARTING-WORLD-UNSELECTED 150 120) -> 
;; (make-world (make-posn 200 150) true (make-posn 50 30))
;; STRATEGY: Structural Decomposition on world : World
(define (world-after-button-down world x y)
  (if (inside-rectangle? (world-coordinates world) x y) 
      (make-world (world-coordinates world) 
                  true 
                  (calc-offset x y (world-coordinates world))) 
      world))

;; TESTS
(define-test-suite world-after-button-down-tests
  (check-equal? 
   (world-after-button-down STARTING-WORLD-UNSELECTED 200 150) 
   (make-world (make-posn 200 150) true (make-posn 0 0)) 
   "world-after-button-down can create offset of 0")
  (check-equal? 
   (world-after-button-down STARTING-WORLD-UNSELECTED 250 180) 
   (make-world (make-posn 200 150) true (make-posn -50 -30)) 
               "world-after-button-down can create negative offset")
  (check-equal? 
   (world-after-button-down STARTING-WORLD-UNSELECTED 150 120) 
   (make-world (make-posn 200 150) true (make-posn 50 30))
               "world-after-button-down can create positive offset"))
(run-tests world-after-button-down-tests)


;; center-from-mouse : Number Number Posn -> Posn
;; GIVEN: A number representing the 
;; proposed x coordinate of the world
;; A number representing the y coordinate of the world
;; A posn representing the offset from the INDICATOR circle
;; to coordinates of the middle of the rectangle
;; RETURNS: a posn representing the offset from the middle of the rectangle
;; This is done so the mouse, when dragged, stays with the INDICATOR circle,
;; not the center of the rectangle
;; EXAMPLES:
;; (center-from-mouse 50 30 (make-posn -1 -1)) -> (make-posn 49 29)
;; (center-from-mouse 350 270 (make-posn -10 -10)) -> (make-posn 340 260)
;; (center-from-mouse 340 260 (make-posn 10 10)) -> (make-posn 350 270)
;; STRATEGY: STRUCTURAL DECOMPOSITION on posn : Posn
(define (center-from-mouse x y offset)
  (make-posn (+ (posn-x offset) x) (+ (posn-y offset) y)))

;; TESTS
(define-test-suite center-from-mouse-tests
(check-equal? 
 (center-from-mouse 50 30 (make-posn -1 -1)) 
 (make-posn 49 29) 
 "should create a posn that subtracts 1 for each coordinate")
(check-equal? 
 (center-from-mouse 350 270 (make-posn -10 -10)) 
 (make-posn 340 260) 
 "should create a posn that subtracts 10 for each coordinate")
(check-equal? 
 (center-from-mouse 340 260 (make-posn 10 10)) 
 (make-posn 350 270) 
 "should create a posn that adds 10 to each coordinate"))
(run-tests center-from-mouse-tests)


;; world-after-drag : World Number Number -> World
;; GIVEN: a world, a number representing
;; the x coordinate of the proposed world
;; a number representing the y coordinate of the proposed world.
;; RETURNS: The world that follows a drag mouseevent.
;; If the world is selected, 
;; then return a world just like the given one,
;; except that is now centered on the mouse position
;; EXAMPLES:
;; (world-after-drag STARTING-WORLD-SELECTED 200 150) -> 
;; (make-world (make-posn 200 150) true (make-posn 0 0)) 
;; (world-after-drag STARTING-WORLD-SELECTED 210 160) -> 
;; (make-world (make-posn 210 160) true (make-posn 0 0)) 
;; (world-after-drag STARTING-WORLD-UNSELECTED 200 150) -> 
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-after-drag world x y)
  (if (world-selected? world) 
      (make-world 
       (fix-coordinates 
        (center-from-mouse x y 
                           (world-circle-offset world)))
       (world-world-selected? world) 
       (world-circle-offset world)) 
      world))

;; TEST
(define-test-suite world-after-drag-tests
  (check-equal? 
   (world-after-drag STARTING-WORLD-SELECTED 200 150) 
   (make-world (make-posn 200 150) true (make-posn 0 0)) 
   "world-after-drag should not change the position")
  (check-equal? 
   (world-after-drag STARTING-WORLD-SELECTED 210 160) 
   (make-world (make-posn 210 160) true (make-posn 0 0)) 
   "world-after-drag should change rectangle position") 
  (check-equal? 
   (world-after-drag STARTING-WORLD-UNSELECTED 200 150) 
   (make-world (make-posn 200 150) false (make-posn 0 0)) 
   "world-after-drag should not change position because it is not selected"))
(run-tests world-after-drag-tests)

;; world-after-button-up : World Number Number -> World
;; GIVEN: a world, a number representing 
;; the x coordinate of the proposed world
;; a number representing the y coordinate of the proposed world.
;; RETURNS: The world that follows a drag mouseevent.
;; If the world is selected, 
;; then return a world just like the given one,
;; except that it is no longer selected.
;; the circle-x-offset and circle-y-offset will be reset to 0
;; EXAMPLES:
;; (world-after-button-up STARTING-WORLD-SELECTED 210 150) -> 
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; (world-after-button-up STARTING-WORLD-UNSELECTED 200 150) -> 
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; STRATEGY: Structural Decomposition on world : World

(define (world-after-button-up world x y)
  (if (world-selected? world) 
      (make-world 
       (world-coordinates world) 
       false 
       (make-posn 0 0)) 
      world))



;; TESTS
;; TODO
(define-test-suite world-after-button-up-tests
(check-equal? 
 (world-after-button-up STARTING-WORLD-SELECTED 210 150) 
 (make-world (make-posn 200 150) false (make-posn 0 0)) 
 "changes selected from true to false")
(check-equal? 
 (world-after-button-up STARTING-WORLD-UNSELECTED 200 150) 
 (make-world (make-posn 200 150) false (make-posn 0 0))
 "does nothing, because it is already unselected"))
(run-tests world-after-button-up-tests)




;; world-after-mouse-event : World Number Number MouseEvent -> World
;; GIVEN: a world, a number representing 
;; the x coordinate of the middle of the rectangle in the proposed world
;; a number representing the y coordinate of the middle of 
;; the rectangle in the proposed world, and a mousevent.
;; RETURNS: the world that follows the given mouseevent.
;; EXAMPLES:
;; (world-after-mouse-event STARTING-WORLD-UNSELECTED 70 70 "button-down") -> 
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; (world-after-mouse-event STARTING-WORLD-SELECTED 70 70 "drag") -> 
;; (make-world (make-posn 70 70) true (make-posn 0 0))
;; (world-after-mouse-event STARTING-WORLD-SELECTED 70 70 "button-up") ->
;; (make-world (make-posn 200 150) false (make-posn 0 0))
;; STRATEGY: STRUCTURAL DECOMPOSITION on mouseevent : MouseEvent

(define (world-after-mouse-event world x y mouseevent)
  (cond
    [(mouse=? mouseevent "button-down") 
     (world-after-button-down world x y)]
    [(mouse=? mouseevent "drag") 
     (world-after-drag world x y)]
    [(mouse=? mouseevent "button-up") 
     (world-after-button-up world x y)]
    [else world]))

;; TESTS
(define-test-suite world-after-mouse-event-tests
 (check-equal? 
  (world-after-mouse-event 
   STARTING-WORLD-UNSELECTED 70 70 "button-down") 
  (make-world (make-posn 200 150) false (make-posn 0 0)) 
  "world after button-down")
 (check-equal? 
  (world-after-mouse-event 
   STARTING-WORLD-SELECTED 70 70 "drag") 
  (make-world (make-posn 70 70) true (make-posn 0 0)) 
  "world-after drag")
 (check-equal? 
  (world-after-mouse-event 
   STARTING-WORLD-SELECTED 70 70 "button-up") 
  (make-world (make-posn 200 150) false (make-posn 0 0)) 
  "world-after button-up")
(check-equal? 
  (world-after-mouse-event 
   STARTING-WORLD-SELECTED 70 70 "enter") 
  (make-world (make-posn 200 150) true (make-posn 0 0)) 
  "world-after a mousevent not defined"))
(run-tests world-after-mouse-event-tests)

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: ignores its argument and starts the interactive program.
;; RETURNS: the final state of the world.

(define (run any)
  (big-bang (initial-world 0)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))