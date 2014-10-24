;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname balls-in-box) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide
 run
 initial-world
 world-after-mouse-event
 world-after-key-event
 world-after-tick
 world-balls
 ball-x-pos
 ball-y-pos
 ball-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Velocity is a PosInt, denoting a ball's speed and direction
;; A Velocity is either:
;; -- positive (interp: the ball is moving to the right)
;; -- negative (interp: the ball is moving to the left)
;; -- zero     (interp: the ball is not moving)

;; velocity-fn : Velocity -> ??
;(define (velovity-fn vel)
;  (cond 
;    [(positive? vel) 
;     ...]
;    [(negative? vel) 
;     ...]
;    [(zero? vel)
;     ...]))

(define-struct ball (coordinates selected? offset velocity))
;; A Ball is a (make-ball Posn Boolean Posn Velocity)
;; Interpretation:
;; coordinates is the posn that holds the x and y coordinates
;; of the middle of the ball
;; selected? determined whether or not the ball is selected.
;; offset represents the offset from the ball's center
;; velocity represents the ball's current velocity

;; ball-fn : Ball -> ??
;(define (ball-fn b)
;  (...
;   (ball-coordinates b)
;   (ball-selected? b)
;   (ball-offset b)
;   (ball-velocity b)))

;; A ListOf<Ball> (LOB) is either:
;; -- empty           (interp: no balls in list)
;; -- (cons Ball LOB) (interp: balls in list)

(define-struct world (balls initial-velocity paused?))
;; A World is a (make-world ListOf<Ball> Velocity Boolean)
;; Interpretation:
;; balls is a list of the balls in the world
;; initial-velocity is the velocity given to new balls added
;; paused? indicates whether the world is currently paused

;; posn-fn : Posn -> ??
;(define (posn-fn po)
;  (...
;   (posn-x posn)
;   (posn-y posn)))

;; world-fn : World -> ??
;; (define (world-fn w)
;;   (world-balls w)
;;   (world-initial-velocity w)
;;   (world-paused? w))

;; A DraggableCircleMouseEvent is a partition of 
;; MouseEvent into the following categories:
;; -- "button-down" (interp: circle is selected)
;; -- "drag" (interp: drag the circle)
;; -- "button-up" (interp: unselect the circle)
;; -- any other mouse event (interp: ignored)

;; mev-fn : DraggableCircleMouseEvent -> ??
;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))

;; A DraggableCircleKeyEvent is a KeyEvent, which is one of
;; -- "n" (interp: n key, creates a circle in center of canvas)
;; -- "d" (interp: d key, deletes a selected circle from canvas)
;; -- " " (interp: space, pauses or unpauses the world)
;; -- any other KeyEvent (interp: ignore)

;; draggable-circle-kev-fn : DraggableCircleKeyEvent -> ??
;(define (draggable-circle-kev-fn kev)
;  (cond 
;    [(key=? kev "n") 
;     ...]
;    [(key=? kev "d") 
;     ...]
;    [(key=? kev " ")
;     ...]
;    [else 
;     ...]))

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define CANVAS-WIDTH 400)
;; Width of the canvas

(define CANVAS-HEIGHT 300)
;; Height of the canvas

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
;; creates an empty-scene with the 
;; CANVAS-WIDTH and CANVAS-HEIGHT constants

(define BALL-RADIUS 20)
;; radius of the ball

(define MIN-X-COORDINATE (+ 0 BALL-RADIUS))
;; The minimum the circle's center x coordinate 
;; can be so it stays within canvas.

(define MIN-Y-COORDINATE (+ 0 BALL-RADIUS))
;; The minimum the circle's center y coordinate 
;; can be so it stays within canvas.

(define MAX-X-COORDINATE (- CANVAS-WIDTH BALL-RADIUS))
;; The maximum the circle's center x coordinate 
;; can be so it stays within canvas.

(define MAX-Y-COORDINATE (- CANVAS-HEIGHT BALL-RADIUS))
;; The maximum the circle's center x coordinate 
;; can be so it stays within canvas.


(define INITIAL-POSN
  (make-posn (/ CANVAS-WIDTH 2)
             (/ CANVAS-HEIGHT 2)))
;; The initial POSN for the circle

(define INITIAL-OFFSET (make-posn 0 0))
;; The initial offset from the circle's middle point

(define BALL-COLOR "blue")
;; The color of the ball

(define SELECTED-BALL-IMAGE
  (circle BALL-RADIUS "solid" BALL-COLOR))
;; The drawing of the ball

(define UNSELECTED-BALL-IMAGE
  (circle BALL-RADIUS "outline" BALL-COLOR))
;; a drawing of an unselected ball

(define DEFAULT-VELOCITY 8)
;; Default velocity of ball

;; FOR TESTING
(define BALL-1 (make-ball (make-posn 100 100) false 
                          INITIAL-OFFSET DEFAULT-VELOCITY))
;; Ball that is unselected

(define BALL-2 (make-ball (make-posn 110 110) true  
                          INITIAL-OFFSET DEFAULT-VELOCITY))
;; Ball that is selected

(define BALL-3 (make-ball (make-posn 50 50) true  
                          INITIAL-OFFSET DEFAULT-VELOCITY))
;; Ball that is selected

(define BALL-4 (make-ball (make-posn (- MAX-X-COORDINATE 4) 100)
                          false INITIAL-OFFSET DEFAULT-VELOCITY))
;; Ball that will bounce on the next tick

(define WORLD-1 (make-world (list BALL-1 BALL-2 BALL-3) DEFAULT-VELOCITY false))
;; a world with BALL-1, BALL-2 and BALL-3, moving at DEFAULT-VELOCITY, unpaused


(define WORLD-2 (make-world (list BALL-1 BALL-2 BALL-3) DEFAULT-VELOCITY true))
;; a world with BALL-1, BALL-2 and BALL-3, moving at DEFAULT-VELOCITY, paused

(define LOB-1 (list BALL-1 BALL-2 BALL-3))
;; a ListOfBalls


;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; within-bounds : Integer Integer Integer -> Integer
;; GIVEN: an integer representing the minimum value,
;; an integer representing the actual value, and an integer representing
;; the maximum value
;; RETURNS: the original value, if it is within the minimum and maximum.
;; otherwise, it will return the minimum or maximum value.
;; EXAMPLES: (within-bounds MIN-X-COORDINATE 50 MAX-X-COORDINATE) -> 50
;; STRATEGY: DOMAIN KNOWLEDGE

(define (within-bounds minimum value maximum)
  (min (max value minimum) maximum))

;; TESTS
(define-test-suite within-bounds-tests
  (check-equal? 
   (within-bounds MIN-X-COORDINATE 50 MAX-X-COORDINATE)
   50
   "Testing where value is correct and should be returned"))
(run-tests within-bounds-tests)

;; run : PosInt PosReal -> World
;; GIVEN: a ball speed and a frame rate, in seconds per tick.
;; EFFECT: runs the world.
;; RETURNS: the final state of the world.
;; EXAMPLES: (run 8 .25) creates and runs a world in which each
;; ball travels at 8 pixels per tick and each tick is 0.25 secs.
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (run speed rate)
   (big-bang (initial-world speed)           
             (on-draw world-to-scene)
             (on-tick world-after-tick rate)
             (on-key world-after-key-event)
             (on-mouse world-after-mouse-event)))

;; move-ball : Posn PosInt -> Ball
;; GIVEN: the center coordinate of a ball and its velocity
;; RETURNS: the ball after it moves at the given velocity
;; EXAMPLES:
;; (move-ball (make-posn 92 100) 8) -> BALL-1
;; STRATEGY: STRUCTURE DECOMPOSITION on center : Posn
(define (move-ball center velocity)
  (make-ball (make-posn (within-bounds MIN-X-COORDINATE
                                       (+ velocity (posn-x center))
                                       MAX-X-COORDINATE)
                        (posn-y center))
             false
             INITIAL-OFFSET
             (if (or (> (+ velocity (posn-x center)) MAX-X-COORDINATE)
                     (< (+ velocity (posn-x center)) MIN-X-COORDINATE))
                 (- velocity)
                 velocity)))

;; TESTS
(define-test-suite move-ball-tests
  (check-equal? (move-ball (make-posn 92 100) 8) BALL-1
                "Testing regular motion")
  (check-equal? (move-ball (make-posn (- MAX-X-COORDINATE 4) 100) 8)
                (make-ball 
                 (make-posn MAX-X-COORDINATE 100) false INITIAL-OFFSET -8)
                "Testing bounces"))
(run-tests move-ball-tests)

;; ball-after-tick : Ball -> Ball
;; GIVEN: a ball
;; RETURNS: the ball that should follow a tick
;; EXAMPLES:
;; (ball-after-tick BALL-1) ->
;; (make-ball (make-posn 108 100) false INITIAL-OFFSET DEFAULT-VELOCITY))
;; (ball-after-tick BALL-2) ->
;; (make-ball (make-posn 110 110) true  INITIAL-OFFSET DEFAULT-VELOCITY))
;; (ball-after-tick BALL-4) ->
;; (make-ball (make-posn MAX-X-COORDINATE 100) false INITIAL-OFFSET -8)
;; STRATEGY: STRUCTURE DECOMPOSITION on ball : Ball
(define (ball-after-tick ball)
  (if (ball-selected? ball) ball
      (move-ball (ball-coordinates ball)
                 (ball-velocity ball))))
 
;; TESTS
(define-test-suite ball-after-tick-tests
  (check-equal? (ball-after-tick BALL-1)
                (make-ball (make-posn 108 100)
                           false INITIAL-OFFSET DEFAULT-VELOCITY)
                "Testing that an unselected ball moves")
  (check-equal? (ball-after-tick BALL-2) BALL-2
                "Testing that a selected ball doesn't move")
  (check-equal? (ball-after-tick BALL-4)
                (make-ball (make-posn MAX-X-COORDINATE 100)
                           false INITIAL-OFFSET -8)
                "Testing bouncing at the canvas edge"))
(run-tests ball-after-tick-tests)

;; world-after-tick : World -> World
;; GIVEN: a world
;; RETURNS: the world that should follow a click tick
;; EXAMPLES: 
;; (world-after-tick empty) -> empty
;; STRATEGY: DOMAIN KNOWLEDGE
(define (world-after-tick world)
  (if (world-paused? world) world
      (make-world (map ball-after-tick (world-balls world))
                  (world-initial-velocity world)
                  false)))

;; TESTS
(define-test-suite world-after-tick-tests
  (check-equal? 
   (world-after-tick WORLD-1) 
   (make-world
    (cons (make-ball (make-posn 108 100) false (make-posn 0 0) 8)
          (list BALL-2 BALL-3))
    DEFAULT-VELOCITY
    false) "Testing that balls move properly")
  (check-equal? 
   (world-after-tick WORLD-2)
   (make-world
    (list
     (make-ball (make-posn 100 100) false (make-posn 0 0) 8)
     (make-ball (make-posn 110 110) true (make-posn 0 0) 8)
     (make-ball (make-posn 50 50) true (make-posn 0 0) 8))
    8
    true) 
   "Testing where world is paused."))
(run-tests world-after-tick-tests)


;; initial-world : PosInt -> World
;; GIVEN: An argument, which is ignored.
;; RETURNS: a world with no balls, but with the property that any
;; balls created in that world will travel at the given speed.
;; EXAMPLES: 
;; (initial-world 1) -> (make-world empty 1 false)
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (initial-world speed)
  (make-world empty speed false))

;; TESTS
(define-test-suite initial-world-tests
  (check-equal? (world-balls (initial-world 10))
                empty
                "Testing whether the initial world is empty"))
(run-tests initial-world-tests)

;; place-image-at :  Image Posn Scene -> Scene
;; GIVEN: a ball's image, 
;; a posn representing the coordinsates of its center
;; a scene represents the canvas where the ball will be drawn
;; RETURNS: a scene with the ball's image on it
;; EXAMPLES:
;; (place-image-at SELECTED-BALL-IMAGE (make-posn 50 50) EMPTY-CANVAS) ->
;; a scene with a ball on it
;; STRATEGY: STRUCTURAL DECOMPOSITION on posn : Posn

(define (place-image-at image posn scene)
  (place-image image (posn-x posn) (posn-y posn) scene))

; TESTS
(define-test-suite place-image-at-tests
  (check-equal? 
   (image? 
    (place-image-at SELECTED-BALL-IMAGE (make-posn 50 50) EMPTY-CANVAS))
   true 
   "Testing with a selected ball"))
(run-tests place-image-at-tests)


;; ball-image : Ball -> Image
;; GIVEN: a ball
;; RETURNS: an image representing a selected, or unselected ball
;; EXAMPLES: (ball-image BALL-1) ->
;; an unselected ball image
;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball

(define (ball-image ball)
  (if (ball-selected? ball)
      SELECTED-BALL-IMAGE
      UNSELECTED-BALL-IMAGE))

;; TESTS
(define-test-suite ball-image-tests
  (check-equal? 
   (image? (ball-image BALL-1)) 
   true 
   "Testing with an unselected ball"))
(run-tests ball-image-tests)

;; draw-ball : Ball Scene -> Scene
;; GIVEN: a ball and a scene
;; RETURNS: the ball on the scene
;; EXAMPLES: (draw-ball BALL-1 EMPTY-CANVAS) ->
;; an unselected ball on a scene
;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball


(define (draw-ball ball scene)
  (place-image-at (ball-image ball)
                  (ball-coordinates ball)
                  scene))

;; TESTS
(define-test-suite draw-ball-tests
  (check-equal? 
   (image? (draw-ball BALL-1 EMPTY-CANVAS)) 
   true 
   "Testing with an unselected ball"))
(run-tests draw-ball-tests)

;; draw-balls : ListOfBalls Scene : Scene
;; GIVEN: a ListOfBalls
;; RETURNS: a scene with the ListOfBalls on it
;; EXAMPLES: 
;; (draw-balls LOB-1 EMPTY-CANVAS) -> 
;; a scene with a ListOfBalls on it
;; STRATEGY: HIGHER ORDER FUNCTIONAL COMPOSITION

(define (draw-balls balls canvas)
  (foldr draw-ball canvas balls))

;; TESTS
(define-test-suite draw-balls-tests
  (check-equal? 
   (image? (draw-balls LOB-1 EMPTY-CANVAS)) 
   true 
   "Testing with a list of balls, 2 unselected, 1 selected"))
(run-tests draw-balls-tests)


;; world-counter : World -> Image
;; GIVEN: a world
;; RETURNS: an image representing the number of balls in the world
;; EXAMPLES: (world-counter WORLD-1) -> an image
;; representing the number of balls in the world
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-counter world)
  (text (number->string (length (world-balls world))) 20 "black"))

;; TESTS
(define-test-suite world-counter-tests
  (check-equal? 
   (image? (world-counter WORLD-1)) 
   true 
   "Testing to make sure image is produced"))
(run-tests world-counter-tests)

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a scene that portrays the given world.
;; EXAMPLES: (world-to-scene WORLD-1) ->
;; a scene representing WORLD-1
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World
(define (world-to-scene world)
  (draw-balls (world-balls world)
              (place-image-at (world-counter world)
                              INITIAL-POSN
                              EMPTY-CANVAS)))

;; TESTS
(define-test-suite world-to-scene-tests
  (check-equal? 
   (image? (world-to-scene WORLD-1)) 
   true 
   "Testing with WORLD-1 that a scene is created"))
(run-tests world-to-scene-tests)


;; keep-unselected : ListOfBalls -> ListOfBalls
;; GIVEN: a ListOfBalls
;; RETURNS: a ListOfBalls like the original, 
;; except those that were selected are removed
;; and those that are unselected are kept.
;; EXAMPLES: (keep-unselected LOB-1) ->
;; (cons (make-ball (make-posn 100 100) false (make-posn 0 0) 8) empty)))
;; STRATEGY: HIGHER ORDER FUNCTIONAL COMPOSITION

(define (keep-unselected balls)
  (filter 
   ;; Ball -> Boolean
   ;; RETURNS: Whether the ball is NOT selected
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on book : Book
   (lambda (ball)
     (not (ball-selected? ball)))
   balls))

;; TESTS
(define-test-suite keep-unselected-tests
  (check-equal? (keep-unselected LOB-1)
                (cons 
                 (make-ball 
                  (make-posn 100 100) 
                  false 
                  (make-posn 0 0) 
                  DEFAULT-VELOCITY)
                 empty)))

(run-tests keep-unselected-tests)

;; ball-x-pos : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the x coordinate of its center
;; EXAMPLES: (ball-x-pos BALL-1) -> 100
;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball
(define (ball-x-pos ball)
  (posn-x (ball-coordinates ball)))

;; TESTS
(define-test-suite ball-x-pos-tests
  (check-equal? 
   (ball-x-pos BALL-1)     
   100))
(run-tests ball-x-pos-tests)

;; ball-y-pos : Ball -> Integer
;; GIVEN: a ball
;; RETURNS: the y coordinate of its center
;; EXAMPLES: (ball-y-pos BALL-2) -> 110
;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball
(define (ball-y-pos ball)
  (posn-y (ball-coordinates ball)))

;; TESTS
(define-test-suite ball-y-pos-tests
  (check-equal? 
   (ball-y-pos BALL-2)
   110))
(run-tests ball-y-pos-tests)

;; in-circle? : Integer Integer Ball -> Boolean
;; GIVEN: an integer representing the x coordinate
;; that the mouse event occured on,
;; an integer representing the y coordinate
;; that the mouse event occured on,
;; and a Ball
;; RETURNS: true iff the mouse event happened within the circle's coordinates
;; EXAMPLES:
;; (in-circle? 200 200 BALL-1) -> false
;; (in-circle? 100 100 BALL-1) -> true
;; (in-circle? 120 120 BALL-1) -> true
;; STRATEGY: FUNCTIONAL COMPOSITION

(define (in-circle? x y ball)
  (>= (expt BALL-RADIUS 2)
      (+ (expt (- (ball-x-pos ball) x) 2)
         (expt (- (ball-y-pos ball) y) 2))))

;; TESTS
(define-test-suite in-circle?-tests
  (check-equal? 
   (in-circle? 200 200 BALL-1) false "Testing outside the circle") 
  (check-equal? 
   (in-circle? 100 100 BALL-1) true "Testing inside circle")
  (check-equal? 
   (in-circle? 100 120 BALL-1) true "Testing at edge of circle"))
(run-tests in-circle?-tests)

;; add-ball : World -> World
;; GIVEN: a world
;; RETURNS: the given world after a "n" key event
;; EXAMPLES: (add-ball WORLD-1) ->
;; same world as before, except a new ball is added to the list
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (add-ball world)
  (make-world (cons (make-ball INITIAL-POSN
                               false
                               INITIAL-OFFSET
                               (world-initial-velocity world))
                    (world-balls world))
              (world-initial-velocity world)
              (world-paused? world)))

;; TESTS
(define-test-suite add-ball-tests
  (check-equal? (add-ball WORLD-1) 
                (make-world
                 (cons
                  (make-ball 
                   (make-posn 200 150) 
                   false (make-posn 0 0) DEFAULT-VELOCITY)
                  (cons
                   (make-ball 
                    (make-posn 100 100) 
                    false (make-posn 0 0) DEFAULT-VELOCITY)
                   (cons 
                    (make-ball 
                     (make-posn 110 110)
                     true (make-posn 0 0) DEFAULT-VELOCITY)
                    (cons 
                     (make-ball (make-posn 50 50) true 
                                (make-posn 0 0) DEFAULT-VELOCITY)
                     empty))))
                 (world-initial-velocity WORLD-1)
                 (world-paused? WORLD-1)))
  "Testing to make sure ball is added to world")
(run-tests add-ball-tests)

;; world-after-d : World -> World
;; GIVEN: a world
;; RETURNS: the given world after a "d" key event
;; EXAMPLES: (world-after-d WORLD-1) ->
;; same world as before, except all selected balls are removed from list
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-after-d world)
  (make-world (keep-unselected (world-balls world))
              (world-initial-velocity world)
              (world-paused? world)))

;; TESTS
(define-test-suite world-after-d-tests
  (check-equal? 
   (world-after-d WORLD-1) 
   (make-world (cons (make-ball (make-posn 100 100) false
                                (make-posn 0 0) DEFAULT-VELOCITY)
                     empty)
               (world-initial-velocity WORLD-1)
               (world-paused? WORLD-1))))
(run-tests world-after-d-tests)

;; world-toggle-pause : World -> World
;; GIVEN: A World
;; RETURNS: That world, with the pause status toggled
;; EXAMPLES: (world-toggle-pause WORLD-1) -> an identical world,
;; which will not change as simulation ticks pass
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-toggle-pause world)
  (make-world (world-balls world)
              (world-initial-velocity world)
              (not (world-paused? world))))

(define-test-suite world-toggle-pause-tests
  (check-equal?
   (world-toggle-pause WORLD-1)
   (make-world (world-balls WORLD-1)
               (world-initial-velocity WORLD-1)
               (not (world-paused? WORLD-1)))))
(run-tests world-toggle-pause-tests)

;; world-after-key-event : World DraggableCircleKeyEvent -> World
;; GIVEN: a world and a keyevent
;; RETURNS: the world that should follow the given world after the given
;; key event.
;; EXAMPLES: (world-after-key-event WORLD-1 "n") ->
;; creates a new ball within the world
;; (world-after-key-event WORLD-1 "d") ->
;; Assuming there is a ball selected, remove it from the list
;; (world-after-key-event WORLD-1 "up") ->
;; returns world
;; STRATEGY: STRUCTURAL DECOMPOSITION on keyevent : DraggableCircleKeyEvent

(define (world-after-key-event world keyevent)
  (cond 
    [(key=? keyevent "n") (add-ball world)]
    [(key=? keyevent "d") (world-after-d world)]
    [(key=? keyevent " ") (world-toggle-pause world)]
    [else world]))

;; TESTS
(define-test-suite world-after-key-event-tests
  (check-equal? (world-after-key-event WORLD-1 "n")
                (make-world
                 (cons
                  (make-ball 
                   (make-posn 200 150) false (make-posn 0 0) DEFAULT-VELOCITY)
                  (cons
                   (make-ball 
                    (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
                   (cons 
                    (make-ball 
                     (make-posn 110 110) true (make-posn 0 0) DEFAULT-VELOCITY) 
                    (cons 
                     (make-ball (make-posn 50 50) true 
                                (make-posn 0 0) DEFAULT-VELOCITY) empty))))
                 (world-initial-velocity WORLD-1)
                 (world-paused? WORLD-1))
                "Testing creating a new ball")
  (check-equal? (world-after-key-event WORLD-1 "d")
                (make-world 
                 (cons 
                  (make-ball 
                   (make-posn 100 100) 
                   false 
                   (make-posn 0 0) 
                   DEFAULT-VELOCITY) 
                  empty)
                 (world-initial-velocity WORLD-1)
                 (world-paused? WORLD-1))
                "Testing deleting a ball")
  (check-equal? (world-after-key-event WORLD-1 "up")
                (make-world
                 (cons
                  (make-ball 
                   (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
                  (cons 
                   (make-ball 
                    (make-posn 110 110) true (make-posn 0 0) DEFAULT-VELOCITY) 
                   (cons 
                    (make-ball (make-posn 50 50) true 
                               (make-posn 0 0) DEFAULT-VELOCITY) empty)))
                 (world-initial-velocity WORLD-1)
                 (world-paused? WORLD-1))
                "Testing the else clause")
   (check-equal? (world-after-key-event WORLD-1 " ")
                (make-world 
                 (world-balls WORLD-1)
                 (world-initial-velocity WORLD-1)
                 (not (world-paused? WORLD-1)))
                "Testing the pause toggling"))
(run-tests world-after-key-event-tests)


;; calc-off-set Integer Integer Posn -> Posn
;; GIVEN: an integer representing the x coordinate of the mouse event,
;; an integer representing the y coordinate of the mouse event,
;; and a posn representing the coordinates of center of the circle
;; RETURNS: a Posn with the offset from the center of the circle to 
;; the codinates where the mouse event occured
;; EXAMPLES:
;; (calc-offset 50 50 (make-posn 60 60)) -> (make-posn 10 10)
;; STRATEGY: STRUCT DECOMPOSITION on posn : Posn
(define (calc-offset x y posn)
  (make-posn (- (posn-x posn) x)
             (- (posn-y posn) y)))

;; TESTS
(define-test-suite calc-offset-tests
  (check-equal? 
   (calc-offset 50 50 
                (make-posn 60 60)) 
   (make-posn 10 10) 
   "Testing with positive offet"))

(run-tests calc-offset-tests)

;; select-balls : Integer Integer ListOfBalls -> ListOfBalls
;; GIVEN: an integer representing the x coordinate 
;; of the location of the mouse event,
;; an integer representing the y coordinate of the location
;; of the mouse event, and a ListOfBalls
;; RETURNS: a ListOfBalls like the original, except
;; balls that are within the range of the mouse event's coordinates
;; are now selected
;; EXAMPLES:
;; (select-balls 100 100 LOB-1) ->
;; ListOfBalls like original, except those within the range are now selected
;; STRATEGY: HIGHER ORDER FUNCTIONAL COMPOSITION

(define (select-balls x y balls)
  (map 
   ;; Ball -> Ball
   ;; RETURNS: Marks the ball as selected if the mouse
   ;; click coordinates fall within that ball
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball
   (lambda (ball)
     (make-ball (ball-coordinates ball)
                (in-circle? x y ball)
                (calc-offset x y (ball-coordinates ball))
                (ball-velocity ball)))
   balls))

;; TESTS
(define-test-suite select-balls-tests
  (check-equal? (select-balls 100 100 LOB-1)
                (cons
                 (make-ball 
                  (make-posn 100 100) true (make-posn 0 0) DEFAULT-VELOCITY)
                 (cons 
                  (make-ball 
                   (make-posn 110 110) 
                   true 
                   (make-posn 10 10) 
                   DEFAULT-VELOCITY) 
                  (cons 
                   (make-ball (make-posn 50 50) false 
                              (make-posn -50 -50) DEFAULT-VELOCITY) empty))) 
                "Testing with LOB-1, making sure balls are selected"))
(run-tests select-balls-tests)

;; world-after-button-down : Integer Integer World -> World
;; GIVEN: a, integer representing the x coordinate of the mouse event
;; an integer representing the y coordinate of the mouse event, and
;; a world
;; RETURNS: the world that should follow a button-down mouse event
;; EXAMPLES: (world-after-button-down 100 100 WORLD-1) ->
;; world like original, except those within a 20 point range of the mouse event
;; are now selected
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-after-button-down x y world)
  (make-world (select-balls x y (world-balls world))
              (world-initial-velocity world)
              (world-paused? world)))

;; TESTS
(define-test-suite world-after-button-down-tests
  (check-equal? 
   (world-after-button-down 100 100 WORLD-1)
   (make-world
    (cons
     (make-ball (make-posn 100 100) true (make-posn 0 0) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 110 110) true (make-posn 10 10) DEFAULT-VELOCITY) 
      (cons 
       (make-ball 
        (make-posn 50 50) false (make-posn -50 -50) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1))
   "Testing world after button-down"))
(run-tests world-after-button-down-tests)

;; fix-coordinates : Posn -> Posn
;; GIVEN: a Posn
;; RETURNS: a Posn that has been verified to be within the canvas
;; if it is not, it will be set to the border min/max of that coordinate
;; EXAMPLES:
;; (fix-coordinates (make-posn 500 500)) -> (make-posn 380 280)
;; STRATEGY: FUNCTIONAL COMPOSITION

(define (fix-coordinates posn)
  (make-posn (within-bounds MIN-X-COORDINATE
                            (posn-x posn)
                            MAX-X-COORDINATE)
             (within-bounds MIN-Y-COORDINATE
                            (posn-y posn)
                            MAX-Y-COORDINATE)))

;; TESTS
(define-test-suite fix-coordinates-tests
  (check-equal? 
   (fix-coordinates 
    (make-posn 500 500))
   (make-posn 380 280)
   "Testing to make sure both X and Y values are corrected"))
(run-tests fix-coordinates-tests)

;; add-offset : Integer Integer Posn -> Posn
;; GIVEN: an integer representing the x coordinate of the mouse event
;; an integer representing the y coordinate of the mouse event
;; and a posn representing the offset
;; RETURNS: a posn that represents the current offset based on
;; the x and y coordinates of the mouse event.
;; EXAMPLES:
;; (add-offset 50 50 (make-posn 60 60)) ->  (make-posn 110 110)
;; STRATEGY: STRUCTURAL DECOMPOSITION on offset : Posn

(define (add-offset x y offset)
  (make-posn (+ x (posn-x offset))
             (+ y (posn-y offset))))

;; TESTS
(define-test-suite add-offset-tests
  (check-equal? 
   (add-offset 50 50 (make-posn 60 60))  
   (make-posn 110 110)))
(run-tests add-offset-tests)


;; ball-after-drag : Integer Integer Ball -> Ball
;; GIVEN: an integer representing the mouse event's current x coordinate,
;; an integer representing the mouse event's current y coordinate,
;; and a ball
;; RETURNS: a ball like the original, but with its coordinates and offset
;; modified based on the mouse events coordinates
;; EXAMPLES:
;; (ball-after-drag 120 120 BALL-1) -> 
;; (make-ball (make-posn 120 120) false (make-posn 0 0))
;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball

(define (ball-after-drag x y ball)
  (make-ball (fix-coordinates (add-offset x y (ball-offset ball)))
             (ball-selected? ball)
             (ball-offset ball)
             (ball-velocity ball)))

;; TESTS
(define-test-suite ball-after-drag-tests
  (check-equal? 
   (ball-after-drag 120 120 BALL-1)
   (make-ball (make-posn 120 120) false (make-posn 0 0) DEFAULT-VELOCITY)
   "Testing ball-after-drag with BALL-1, inside canvas"))
(run-tests ball-after-drag-tests)

;; balls-after-drag : Integer Integer ListOfBalls : ListOfBalls
;; GIVEN: an integer representing the mouse event's x coordinate,
;; an integer representing the mouse event's y coordinate,
;; and a ListOfBalls
;; RETURNS: a ListOfBalls like the original, except the ListOfBalls
;; that should follow a drag
;; EXAMPLES:
;; (balls-after-drag 120 120 LOB-1) ->
;; ListOfBalls like original except those that were selected
;; have had their coordinates and offset modified to correspond with the
;; drag mouse event's coordinates
;; STRATEGY: HIGHER ORDER STRUCTURAL DECOMPOSITION

(define (balls-after-drag x y balls)
  (map 
   ;; Ball -> Ball
   ;; RETURNS: Moves selected balls according to the mouse
   ;; dragging, and leaves unselected balls as-is.
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball
   (lambda (ball)
     (if (ball-selected? ball)
         (ball-after-drag x y ball)
         ball))
   balls))

;; TESTS
(define-test-suite balls-after-drag-tests
  (check-equal? 
   (balls-after-drag 120 120 LOB-1)
   (cons
    (make-ball (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
    (cons 
     (make-ball (make-posn 120 120) true (make-posn 0 0) DEFAULT-VELOCITY) 
     (cons 
      (make-ball 
       (make-posn 120 120) true (make-posn 0 0) DEFAULT-VELOCITY) empty)))
   "Tests ListOfBalls after drag"))
(run-tests balls-after-drag-tests)

;; world-after-drag : Integer Integer World -> World
;; GIVEN: an integer representing the mouse event's x coordinate,
;; an integer representing the mouse event's y coordinate
;; and a world
;; RETURNS: the world that should follow drag mouse event
;; EXAMPLES:
;; (world-after-drag 100 100 WORLD-1) ->
;; Same world as before, except those that were selected's
;; coordinates as well as offset have been modified 
;; to correspond with that of the moust event's
;; STRATEGY: STRUCTURAL DECOMPOSITION on world : World

(define (world-after-drag x y world)
  (make-world (balls-after-drag x y (world-balls world))
              (world-initial-velocity world)
              (world-paused? world)))

;; TESTS
(define-test-suite world-after-drag-tests
  (check-equal? 
   (world-after-drag 100 100 WORLD-1)
   (make-world
    (cons
     (make-ball (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 100 100) true (make-posn 0 0) DEFAULT-VELOCITY) 
      (cons 
       (make-ball 
        (make-posn 100 100) true (make-posn 0 0) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1))
   "Testing world-after-drag with WORLD-1"))
(run-tests world-after-drag-tests)

;; world-after-button-up : World -> World
;; GIVEN: a world
;; RETURNS: the world that should follow a button-up mouse event
;; EXAMPLES: 
;; (world-after-button-up WORLD-1) ->
;; same world as before, except those balls that were selected,
;; are now unselected.
;; STRATEGY: FUNCTIONAL COMPOSITION

(define (world-after-button-up world)
  (make-world (deselect-balls (world-balls world))
              (world-initial-velocity world)
              (world-paused? world)))

;; deselect-balls : ListOf<Ball> -> ListOf<Ball>
;; GIVEN: a list of balls
;; RETURNS: a list of balls, all marked as not selected
;; EXAMPLES:
;; (deselect-balls LOB-1) ->
;; same positions as before, except all the balls are deselected
;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION

(define (deselect-balls balls)
  (map 
   ;; Ball -> Ball
   ;; RETURNS: The ball, deselected.
   ;; STRATEGY: STRUCTURAL DECOMPOSITION on ball : Ball
   (lambda (ball)
         (make-ball (ball-coordinates ball)
                    false
                    INITIAL-OFFSET
                    (ball-velocity ball)))
       balls))

;; TESTS
(define-test-suite world-after-button-up-tests
  (check-equal? 
   (world-after-button-up WORLD-1)
   (make-world
    (cons
     (make-ball (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 110 110) false (make-posn 0 0) DEFAULT-VELOCITY) 
      (cons 
       (make-ball 
        (make-posn 50 50) false (make-posn 0 0) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1))
   "Tests world-after-button-up with WORLD-1"))
(run-tests world-after-button-up-tests)

;; world-after-mouse-event : World Integer Integer 
;; DraggableCircleMouseEvent -> World
;; GIVEN: a world, an integer representing the x position 
;; of the mouse when mousevent is triggered
;; a y coordinate given the y position of the mouse when mousevent is triggered
;; and a mouseevent
;; RETURNS: the world that should follow the given world after the given
;; mouse event.
;; EXAMPLES:
;; (world-after-mouse-event WORLD-1 50 50 "button-down") ->
;; same world as before, only the world that follows a button-down mouse event
;; (world-after-mouse-event WORLD-1 60 60 "drag") ->
;; same world as before, only the world that follows a drag mouse event
;; (world-after-mouse-event WORLD-1 50 50 "button-up") ->
;; same world as before, only the world that follows a button-up mouse event
;; (world-after-mouse-event WORLD-1 50 50 "enter") ->
;; same world as before, this case is not handled
;; STRATEGY: STRUCTURAL DECOMPOSITION on mouseevent : DraggableCircleMouseEvent
   
(define (world-after-mouse-event world x y mouseevent)
  (cond
    [(mouse=? mouseevent "button-down") (world-after-button-down x y world)]
    [(mouse=? mouseevent "drag") (world-after-drag x y world)]
    [(mouse=? mouseevent "button-up") (world-after-button-up world)]
    [else world]))

;; TESTS
(define-test-suite world-after-mouse-event-tests
  (check-equal? 
   (world-after-mouse-event WORLD-1 50 50 "button-down") 
   (make-world
    (cons
     (make-ball (make-posn 100 100) false (make-posn 50 50) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 110 110) false (make-posn 60 60) DEFAULT-VELOCITY) 
      (cons                                                      
       (make-ball (make-posn 50 50) true 
                  (make-posn 0 0) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1)) 
   "Testing what happens on button-down mouse event")
  (check-equal? 
   (world-after-mouse-event WORLD-1 60 60 "drag")
   (make-world
    (cons
     (make-ball (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 60 60) true (make-posn 0 0) DEFAULT-VELOCITY) 
      (cons 
       (make-ball (make-posn 60 60) true 
                  (make-posn 0 0) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1))
   "Testing what happens on drag mouse event")
  (check-equal? 
   (world-after-mouse-event WORLD-1 50 50 "button-up") 
   (make-world
    (cons
     (make-ball (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 110 110) false (make-posn 0 0) DEFAULT-VELOCITY) 
      (cons 
       (make-ball (make-posn 50 50) false 
                  (make-posn 0 0) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1))
   "Testing what happens on button-up mouse event")
  (check-equal? 
   (world-after-mouse-event WORLD-1 50 50 "enter") 
   (make-world
    (cons
     (make-ball (make-posn 100 100) false (make-posn 0 0) DEFAULT-VELOCITY)
     (cons 
      (make-ball (make-posn 110 110) true (make-posn 0 0) DEFAULT-VELOCITY) 
      (cons 
       (make-ball (make-posn 50 50) true 
                  (make-posn 0 0) DEFAULT-VELOCITY) empty)))
    (world-initial-velocity WORLD-1)
    (world-paused? WORLD-1))
   "Testing what happens when else clause is triggered"))
(run-tests world-after-mouse-event-tests)
