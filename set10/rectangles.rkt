#lang scheme
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)
(provide
 run
 make-world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; A MyMouseEvent is a partition of
;; MouseEvent into the following categories:
;; -- "button-down" (interp: object is selected)
;; -- "drag" (interp: drag the object)
;; -- "button-up" (interp: unselect the object)
;; -- any other mouse event (interp: ignored)

;; mev-fn : MyMouseEvent -> ??
;(define (mev-fn mev)
;  (cond
;    [(mouse=? mev "button-down") ...]
;    [(mouse=? mev "drag") ...]
;    [(mouse=? mev "button-up") ...]
;    [else ...]))


;; A MyKeyEvent is a KeyEvent, which is one of
;; -- "r" (interp: r key, creates a new rectangle based on
;;         the target's position)
;; -- any other KeyEvent (interp: ignore)

;; kev-fn : MyKeyEvent -> ??
;(define (kev-fn kev)
;  (cond
;    [(key=? kev "n")
;     ...]
;    [(key=? kev "d")
;     ...]
;    [else
;     ...]))

;; Direction is one of the following:
;; -- "left" (interp: the shape is currently moving left (west on canvas))
;; -- "right" (interp: the shape is currently moving right (east on canvas))
;; NOTE: In practice, the following should happen depending on direction:
;; "left" means the x coordinate is being reduced
;; "right" means the x coordinate is being increased

;; direction-fn : Direction -> ??
;(define (direction-fn dir)
;  [cond
;    [(string=? dir "left")
;     ...]
;    [(string=? dir "right")
;     ...]])

;; A ListOf<Shape<%>> is a
;; -- empty                       (interp: a ListOf<Shape<%>> which is empty)
;; --(cons Shape<%> ListOf<Shape<%>>)  (interp: a ListOf<Shape<%>>
;;                                with Rectangles in it)
;; TEMPLATE
;; listofrectangle-fn : ListOf<Shape<%>> -> ??
;(define (listofrectangle-fn lor)
;  (cond
;    [(empty? lor) ...]
;    [else
;     (...
;      (first lor)
;      (listofrectangle-fn (rest lor)))]))

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE

(define World<%>
  (interface ()

    ;; -> Void
    ;; EFFECT: updates this World to its state following a tick
    on-tick

    ;; Integer Integer MouseEvent -> Void
    ;; EFFECT: updates this World to its state following the given
    ;; MouseEvent;
    on-mouse

    ;; KeyEvent -> Void
    ;; EFFECT: updates this World to its state following the given
    ;; Key event.
    on-key

    ;; Scene -> Scene
    ;; RETURNS: a Scene like the given one, but with this object drawn
    ;; on it.
    add-to-scene

    ;; -> Number
    ;; RETURN the x and y coordinates of the target
    get-x
    get-y

    ;; -> Boolean
    ;; RETURNS: Is the target selected?
    get-selected?

    ;; -> ListOf<Shape<%>>
    ;; RETURNS: the list of shapes in the world
    get-shapes
    ))

(define Shape<%>
  (interface ()

    ;; -> Void
    ;; EFFECT: updates this Shape to its state following a tick
    on-tick

    ;; Integer Integer MouseEvent -> Void
    ;; EFFECT: updates this Shape to its state following the given
    ;; MouseEvent;
    on-mouse

    ;; KeyEvent -> Void
    ;; EFFECT: updates this Shape to its state following the given
    ;; Key event.
    on-key

    ;; Scene -> Scene
    ;; RETURNS: a Scene like the given one, but with this object drawn
    ;; on it.
    add-to-scene

    ;; -> Number
    ;; RETURN: the x and y coordinates of the center of this shape.
    get-x
    get-y

    ;; -> Boolean
    ;; Is this shape currently selected?
    is-selected?

    ;; -> String
    ;; RETURNS: either "red" or "green", depending on the color in
    ;; which this shape would be displayed if it were displayed now.
    get-color
    ))

;; END OF INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASSES
;; A World is a (new World% [x PosInt] [y PosInt]
;;                          [selected? Boolean] [Rectangles ListOf<Shape<%>>]
;;                          [speed PosInt]
;; Interpretation: Represents a world, containing:
;; x representing the x coordinate of the center of the target
;; y representing the y coordinate of the center of the target
;; selected? is a boolean representing whether the target is selected or not
;; rectangles is a ListOf<Shape<%>> representing the list of all rectangles
;; in the world
;; speed represents the amount of pixels traveled by the rectangle on each tick
;; World% -- a class that satisfies the World<%> interface (shown below).


(define World%
  (class* object% (World<%>)
    (init-field x ;; a PosInt, the x coordinate, of the target
                y ;; A PosInt, the y coordinate, of the target
                selected?) ;; A Boolean representing if the
    ;; target is selected or not
    (init-field rectangles) ;; A ListOf<Shape<%>> representing all of
    ;; the rectangles on the board
    (init-field speed) ;; A PosInt, the amount of pixeled traveled by a
    ;; rectangle on each tick

    ;; offset-x and offset-y together form the vector from the 
    ;; mouse location to the or target's center
    (field (x-offset ZERO))
    (field (y-offset ZERO))

    (field [CIRC-IMG CIRCLE-CONSTANT]) ;; A drawing of the circle based on
    ;; radius given by CIRCLE-CONSTANT

    (super-new) ;; creating super class

    ;; on-tick : -> Void
    ;; EFFECT: Updates this world to its state following a tick
    ;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (on-tick)
      (for-each
       ;; Rectangle% -> Rectangle%
       ;; GIVEN: a rectangle
       ;; RETURNS: the rectangle that should follow a clock tick
       ;; STRATEGY: FUNCTIONAL COMPOSITION
       (lambda (rectangle)
         (send rectangle on-tick))
       rectangles))

    ;; on-mouse : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates this World to its state following the given MouseEvent
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on mev : MouseEvent
    (define/public (on-mouse mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (send this world-after-button-down mx my mev)]
        [(mouse=? mev "drag")
         (send this world-after-drag mx my mev)]
        [(mouse=? mev "button-up")
         (send this world-after-button-up mx my mev)]
        [else this]))

    ;; world-after-button-down : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates the world following a mouse-down mouseevent.
    ;; STRATEGY: FUNCTIONAL COMPOSITION
    (define/public (world-after-button-down mx my mev)
      (if (send this in-circle? mx my)
          (begin
            (set! x-offset (- x mx))
            (set! y-offset (- y my))
 (set! selected? true)
            (send this rect-on-mouse mx my mev))
          (send this rect-on-mouse mx my mev)))

    ;; in-circle : PosInt PosInt -> Boolean
    ;; GIVEN: a PosInt representing the mouse event's x coordinate
    ;; and a PosInt representing the mouse event's y coordinate
    ;; RETURNS: A boolean determining if the mouse event happened
    ;; inside the target circle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (in-circle? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr CIRCLE-RADIUS)))

    ;; world-after-drag : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates the world following a drag mouseevent.
    ;; STRATEGY: FUNCTIONAL COMPOSITION
    (define/public (world-after-drag mouse-x mouse-y mev)
      (if selected?
          (begin
            (set! x (send this x-helper mouse-x))
            (set! y (send this y-helper mouse-y))
            (send this rect-on-mouse mouse-x mouse-y mev))
          (send this rect-on-mouse mouse-x mouse-y mev)))

    ;; x-helper : PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; RETURNS: a postive integer that is either the next position of
    ;; the target
    ;; or the minimum/maximum position ensuring that the target's x coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (x-helper mouse-x)
      (max CIRCLE-RADIUS
           (min (- CANVAS-WIDTH CIRCLE-RADIUS)
                (+ x-offset mouse-x))))

    ;; y-helper : PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next position of the target
    ;; or the minimum/maximum position ensuring that the target's y coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (y-helper mouse-y)
      (max CIRCLE-RADIUS
           (min (- CANVAS-HEIGHT CIRCLE-RADIUS)
                (+ y-offset mouse-y))))

    ;; world-after-button-up : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates the world following a mouse-up mouseevent.
    ;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (world-after-button-up mx my mev)
      (if selected?
          (begin
            (set! selected? false)
            (send this rect-on-mouse mx my mev))
          (send this rect-on-mouse mx my mev)))

    ;; rect-on-mouse : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates the world by sending ListOf<Shape<%>>
    ;; to the rectangle class' on-mouse field
    ;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (rect-on-mouse mx my mev)
      (for-each
       ;; Rectangle% -> Rectangle%
       ;; GIVEN: a rectangle
       ;; RETURNS: the rectangle that should
       ;; follow a mouse event
       ;; STRATEGY: FUNCTIONAL COMPOSITION
       (lambda (rectangle)
         (send rectangle on-mouse mx my mev))
       rectangles))
    
    ;; on-key : KeyEvent -> World<%>
    ;; GIVEN: a KeyEvent
    ;; RETURNS: the world that should follow the given KeyEvent
    ;; if the keyevent is r, a new rectangle is created on the target's
    ;; current position. Otherwise, the same world is returned.
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on kev : KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev "r")
         (set! rectangles 
               (if (send this no-room?)
                   rectangles
                   (cons
                    (send this make-rectangle-fn) rectangles)))]
        [else this]))
    
    ;; no-room : -> Boolean
    ;; Returns: true if there is no room to create a
    ;; rectangle which is completely inside the canvas
    ;; STRATEGY: Domain Knowledge
    (define/public (no-room?)
      (local ((define target-x x)
              (define target-y y))
        (or (<= (- CANVAS-WIDTH target-x)
                RECTANGLE-HALF-WIDTH)
            (<= target-x
                RECTANGLE-HALF-WIDTH)
            (<= target-y
                RECTANGLE-HALF-HEIGHT)
            (<= (- CANVAS-HEIGHT target-y)
                RECTANGLE-HALF-HEIGHT))))
    
    ;; make-rectangle : -> Shape<%>
    ;; RETURNS: a Rectangle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (make-rectangle-fn)
      (new Rectangle%
        [x x]
        [y y]
        [selected? false]
        [direction "right"]
        [speed speed]))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the original, except
    ;; with the target and rectangles drawn on it
    ;; STRATEGY: HOFC
    (define/public (add-to-scene scene0)
      (local
        ((define scene-with-circle (place-image CIRC-IMG  x y scene0)))
        (foldr
         ;; Rectangle% Scene -> Rectangle%
         ;; GIVEN: a rectangle and a scene
         ;; RETURNS: a scene like the original, except with a rectangle on it
         ;; STRATEGY: FUNCTIONAL COMPOSITION
         (lambda (rectangle scene)
           (send rectangle add-to-scene scene))
         scene-with-circle
         rectangles)))

    ;; get-x : -> PosInt
    ;; RETURNS: the x coordinate of the target circle
    ;; within the world class
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (get-x)
      x)

    ;; get-y : -> PosInt
    ;; RETURNS: the y coordinate of the target circle
    ;; within the world class
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (get-y)
      y)

    ;; get-selected? : -> Boolean
    ;; RETURNS: the selected? field of the target circle
    ;; within the world class
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (get-selected?)
      selected?)

    ;; get-shapes : -> ListOf<Shape<%>>
    ;; RETURNS: the list of rectangles within the world class
    (define/public (get-shapes)
      rectangles)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Rectangle is a (new Rectangle% [x PosInt] [y PosInt]
;; [selected? Boolean] [direction Direction]
;; [speed PosInt]
;; Interpretation: Represents a rectangle, containing:
;;  x which is a positive integer, representing the rectangle's x coordinate
;;  y which is a positive integer, representing the rectangle's y coordinate
;;  selected? which is a boolean, representing whether the rectangle is
;; selected or not
;;  direction which is a Direction, representing the direction the
;; rectangle is traveling it.
;;  speed which is a positive integer, representing the pixels the rectangle
;; is moving on each clock tick
;; Rectangle% -- a class that satisfies the Shape<%> interface (shown below)
(define Rectangle%
  (class* object% (Shape<%>)
    (init-field x ;; a PosInt, the x coordinate of the rectangle
                y ;; a PosInt, the y coordinate of the rectangle
                selected?  ;; a Boolean, representing if the rectangle is
                ;; selected or not. (true = selected, false = not)
                direction ;; a Direction, representing the direction the
                ;; direction the rectangle is moving in
                speed) ;; a PosInt, representing the amount of pixels
    ;; the rectangle will move on each clock tick

    (field (RECTANGLE-IMG RECTANGLE-CONSTANT)) ;; Drawing the rectangle based
    ;; on the RECTANGLE-CONSTANT
    ;; constant defined
    (field (RECTANGLE-COLOR "green"))

    ;; offset-x and offset-y together form the vector from the 
    ;; mouse location to the rectangle center
    (field (x-offset ZERO))
    (field (y-offset ZERO))

    (super-new) ;; creating super class

    ;; on-tick : -> Void
    ;; EFFECT: Updates this Shape to its state following a clock tick
    ;; STRATEGY: FUNCTION COMPOSITION
    (define/public (on-tick)
      (if
       selected?
       this
       (send this rectangle-on-tick)))

    ;; rectangle-on-tick: -> Void
    ;; EFFECT: Updates this Shape to its state following a clock tick
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on direction : Direction
    (define/public (rectangle-on-tick)
      (cond
        [(string=? direction "right")
         (send this rectangle-on-tick-right-direction)]
        [(string=? direction "left")
         (send this rectangle-on-tick-left-direction)]))

    ;; rectangle-on-tick-right-direction : -> Void
    ;; EFFECT: Updates this Shape to its state following a right direction
    ;; move
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-on-tick-right-direction)
      (if
       (>= (min (+ x speed) (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH))
           (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH))
       (begin
         (set! x (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH))
         (set! direction "left"))
       (set! x (+ x speed))
            ))

    ;; rectangle-on-tick-left-direction : -> Void
    ;; EFFECT: Updates this Shape to its state following a left direction
    ;; move
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-on-tick-left-direction)
      (if
       (<= (max (- x speed) RECTANGLE-HALF-WIDTH) RECTANGLE-HALF-WIDTH)
       (begin (set! x RECTANGLE-HALF-WIDTH)
            (set! direction "right"))
       (set! x (- x speed))))

    ;; on-key : KeyEvent -> Void
    ;; EFFECT: Updates this Shape to its state following a given
    ;; key event
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (on-key kev)
      this)

    ;; on-mouse PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates this Shape to its state following the given
    ;; MouseEvent
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on mev : MyMouseEvent
    (define/public (on-mouse mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (send this rectangle-after-button-down mx my)]
        [(mouse=? mev "drag")
         (send this rectangle-after-drag mx my)]
        [(mouse=? mev "button-up")
         (send this rectangle-after-button-up)]
        [else this]))

    ;; rectangle-after-button-down : PosInt PosInt -> Void
    ;; EFFECT: Updates this Shape to its state following a button-down
    ;; mouseevent
    ; STRATEGY: FUNCTION COMPOSITION
    (define/public (rectangle-after-button-down mx my)
      (if (send this in-rectangle? mx my)
          (begin
            (set! x-offset (- x mx))
            (set! y-offset (- y my))
            (set! selected? true))
          this))

    ;; rectangle-after-drag : PosInt PosInt -> Void
    ;; EFFECT: Updates this Shape to its state following a drag
    ;; mouseevent
    ;; STRATEGY: FUNCTION COMPOSITION
    (define/public (rectangle-after-drag mx my)
      (if selected?
          (begin
               (set! x (send this x-helper mx))
               (set! y (send this y-helper my)))
          this))

    ;; x-helper : PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; RETURNS: a postive integer that is either the next positive of the target
    ;; or the minimum/maximum position ensuring that the target's x coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
   (define/public (x-helper mouse-x)
      (max RECTANGLE-HALF-WIDTH
           (min (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH)
                (+ x-offset mouse-x))))

    ;; y-helper : PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next positive of the target
    ;; or the minimum/maximum position ensuring that the target's y coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (y-helper mouse-y)
      (max RECTANGLE-HALF-HEIGHT
           (min (- CANVAS-HEIGHT RECTANGLE-HALF-HEIGHT)
                (+ y-offset mouse-y))))


    ;; in-rectangle? : PosInt PosInt -> Boolean
    ;; GIVEN: a PosInt representing the mouse event's x coordinate
    ;; and a PosInt representing the mouse event's y coordinate
    ;; RETURNS: A boolean determining if the mouse event happened
    ;; inside the rectangle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (in-rectangle? mx my)
      (and
       (<=
        (- x RECTANGLE-HALF-WIDTH)
        mx
        (+ x RECTANGLE-HALF-WIDTH))
       (<=
        (- y RECTANGLE-HALF-HEIGHT)
        my
        (+ y RECTANGLE-HALF-HEIGHT))))

    ;; rectangle-after-button-up : -> Void
    ;; ;; EFFECT: Updates this Shape to its state following a button-up
    ;; mouseevent
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-after-button-up)
      (set! selected? false))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the original, except with a rectangle drawn on it
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (add-to-scene scene)
      (place-image RECTANGLE-IMG x y scene))

    ;; get-x : -> PosInt
    ;; RETURNS: the x coordinate of the rectangle
    ;; within the rectangle class
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (get-x)
      x)

    ;; get-y : -> PosInt
    ;; RETURNS: the y coordinate of the rectangle
    ;; within the rectangle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (get-y)
      y)

    ;; is-selected?  : -> Boolean
    ;; RETURNS: the selected? field of the rectangle
    ;; within the rectangle class
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (is-selected?)
      selected?)

    ;; get-color : -> String
    ;; RETURNS: the color of the current rectangle
    ;; Strategy: Domain Knowledge
    (define/public (get-color)
      RECTANGLE-COLOR)
    ))


;; END OF CLASSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 500)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


(define CIRCLE-INITIAL-X 200)
(define CIRCLE-INITIAL-Y 250)

(define RECTANGLE-WIDTH 30)
(define RECTANGLE-HEIGHT 20)

(define RECTANGLE-HALF-WIDTH (/ RECTANGLE-WIDTH 2))
(define RECTANGLE-HALF-HEIGHT (/ RECTANGLE-HEIGHT 2))

(define ZERO 0)

(define RECTANGLE-CONSTANT (rectangle 30 20 "outline" "green"))

(define CIRCLE-CONSTANT (circle 5 "outline" "red"))

(define CIRCLE-RADIUS 5)

(define CONSTANT-SPEED .25)

(define MIN-X-COORDINATE 15)
;; The minimum the rectangle's center x coordinate
;; can be so it stays within canvas.

(define MAX-X-COORDINATE 375)
;; The maximum the rectangle's center x coordinate
;; can be so it stays within canvas.

(define MIN-Y-COORDINATE 10)
;; The minimum the rectangle's center y coordinate
;; can be so it stays within canvas.

(define MAX-Y-COORDINATE 490)
;; The maximum the rectangle's center x coordinate
;; can be so it stays within canvas.

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-world : PosInt -> World<%>
;; GIVEN: a PosInt representing the amount of pixels the rectangle will
;; travel on reach clock tick
;; RETURNS: a world with no shapes, but in which any shapes
;; created in the future will travel at the given speed.
;; STRATEGY: DOMAIN KNOWLEDGE
(define (make-world speed)
  (new World%
       [x CIRCLE-INITIAL-X]
       [y CIRCLE-INITIAL-Y]
       [selected? false]
       [rectangles empty]
       [speed speed]))

;; run : PosNum PosInt -> World<%>
;; GIVEN: a PosNum representing a frame rate (in seconds/tick)
;; and a PosInt, representing a shape-speed (in pixels/tick),
;; RETURNS: Returns the final state of the world.
(define (run frame-rate speed)
  (big-bang
   (make-world speed)
   (on-tick (lambda (w) (send w on-tick) w) frame-rate)
   (on-draw (lambda (w) (send w add-to-scene EMPTY-CANVAS)))
   (on-key (lambda (w kev) (send w on-key kev) w))
   (on-mouse (lambda (w mx my mev) (send w on-mouse mx my mev) w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

;; test-world: World% -> 
;; (list Number Number Boolean ListOf<Shapes<%>>)
;; Given : a world
;; Returns: a list containing the list 
;; of fields of World%
;; Strategy: Domain Knowledge
(define (test-world w)
  (list (send w get-x)
        (send w get-y)
        (send w get-selected?)
        (length (send w get-shapes))))

(define-test-suite world-tests

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (define world-scene (send world1 add-to-scene EMPTY-CANVAS))
   (check-equal?
    world-scene
    (place-image (rectangle 30 20 "outline" "green")
                 200 250
                 (place-image
                  (circle CIRCLE-RADIUS "outline" "red")
                  200 250
                  EMPTY-CANVAS)) "test-1 for add-to-scene"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-tick)
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-2 for key-event"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "x")
   (send world1 on-tick)
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 0) "test-3 for other key-event"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "enter")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-4 for other mouse-event"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 true 1) "test-5 for button-down mouse-event"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 10 13 "button-down")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-6 for button-down mouse-event
 when it is outside the target"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 10 13 "button-up")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-7 for button-up
 mouse-event
 when it is outside the target"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (send world1 on-mouse 395 250 "drag")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 395 250 true 1)"test-8 for drag
 mouse-event until the canvas edge"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 395 250 "drag")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-9 for drag
 mouse-event when there is nothing selected"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (send world1 on-mouse 395 250 "drag")
   (send world1 on-key "r")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 395 250 true 1)"test-10 for
 key event when target is at the edge 
of the canvas i.e no roo for rectangle"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (send world1 on-mouse 200 250 "button-up")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-11 for button-up 
mouse-event")))

(run-tests world-tests)

;; rectangle tests
;; world1 : (make-world : PosInt -> World%) -> World%
;; Given : a make-world
;; Returns: a world 
(define world1 (make-world 8))

;; test-rectangle: Rectangle% -> 
;; (list Number Number Boolean String)
;; Given : a rectangle
;; Returns: a list containing the list 
;; of fields of Rectangle%
;; Strategy: Domain Knowledge
(define (test-rectangle r)
  (list (send r get-x)
        (send r get-y)
        (send r is-selected?)
        (send r get-color)))

(define-test-suite rectangle-tests

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "button-down")
   (send world1 on-tick)
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 200 250 #t "green") "test-12 for button down
 in rectangle"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "button-down")
   (send rectangle1 on-mouse 390 250 "drag")
   (send rectangle1 on-mouse 390 250 "button-up")
   (send world1 on-tick)
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 377 250 #f "green")
    "test-13 for mouse-events in rectangle"))

    (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "button-down")
   (send rectangle1 on-mouse 390 250 "drag")
   (send rectangle1 on-mouse 390 250 "button-up")
   (send world1 on-tick)
   (send world1 on-tick)
   (define rectangle2 (first (send world1 get-shapes)))
   (send rectangle2 on-mouse 377 250 "button-down")
   (send rectangle2 on-mouse 15 250 "drag")
   (send rectangle2 on-mouse 15 250 "button-up")
   (send world1 on-tick)
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 23 250 #f "green")"test-14 for dagging at 
the edges of the canvas"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "enter")
   (send world1 on-tick)
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 216 250 #f "green") 
    "test-15 for other mose events"))

  (test-begin
   (define world1 (make-world 8))
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-key "r")
   (send world1 on-tick)
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 216 250 #f "green") 
    "test-16 key-event")))

(run-tests rectangle-tests)
