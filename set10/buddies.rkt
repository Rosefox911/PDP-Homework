#lang scheme
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)
(require racket/format)
(provide
 make-world
 run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLISH/SUBSCRIBE DESIGN
;;
;; In this problem, we use the publish/subscribe pattern twice:
;; - To notify rectangles of new potential buddies
;; - To notify buddies that they need to be highlighted
;;
;; Each time a rectangle is created, the first notification is sent out to all
;; currently existing rectangles (with the add-stranger method), and then the
;; new rectangle is implicitly subscribed to this notification, by being added
;; to the list of rectangles. Having each rectangle notified of all other
;; potential buddies enables the rectangles to handle buddy detection
;; independantly of the world object.
;;
;; Each time rectangles become buddies, they are subscribed to the second
;; notification (with the add-buddy method). When a rectangle is selected, it
;; notifies its buddies of this with the highlight method, which changes each
;; rectangle's highlighted state so it can be displayed correctly without
;; needing to query its buddies each time the scene is redrawn.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;    [(key=? kev "r")
;     ...]
;    [else
;     ...]))

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
;; A World is a (new World% [x PosInt] [y PosInt] [mx-pos PosInt]
;;                          [my-pos PosInt]
;;                          [selected? Boolean] [Rectangles ListOf<Shape<%>>]
;;                          [speed PosInt]
;; Interpretation: Represents a world, containing:
;; x representing the x coordinate of the center of the target
;; y representing the y coordinate of the center of the target
;; mx-pos representing the mouse event's x coordinate
;; my-pos representing the mouse event's y coordinate
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

    ;; offset-x and offset-y together form the vector from the 
    ;; mouse location to the or target's center
    (init-field (x-offset ZERO))
    (init-field (y-offset ZERO))

    (field [CIRC-IMG CIRCLE-CONSTANT]) ;; A drawing of the circle based on
    ;; radius given by CIRCLE-CONSTANT

    (super-new) ;; creating super class

    ;; on-tick : -> Void
    ;; EFFECT: Updates this world to its state following a tick
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (on-tick)
      this)

    ;; on-mouse : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: updates this world to its state following the given
    ;; MouseEvent
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on mev : MyMouseEvent
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
    ;; EFFECT: updates this world to its state following the given
    ;; MouseEvent
    ; STRATEGY: FUNCTIONAL COMPOSITION
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
    ;; EFFECT: updates this world to its state following the given
    ;; a drag mouse event
    ;; STRATEGY: FUNCTIONAL COMPOSITION
    (define/public (world-after-drag mouse-x mouse-y mev)
      (if selected?
          (begin
            (set! x (send this x-helper mouse-x mouse-y))
            (set! y (send this y-helper mouse-x mouse-y))
            (send this rect-on-mouse mouse-x mouse-y mev))
          (send this rect-on-mouse mouse-x mouse-y mev)))

    ;; x-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next position of
    ;; the target
    ;; or the minimum/maximum position ensuring that the target's x coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (x-helper mouse-x mouse-y)
      (max CIRCLE-RADIUS
           (min (- CANVAS-WIDTH CIRCLE-RADIUS)
                (+ x-offset mouse-x))))

    ;; y-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next position of 
    ;; the target
    ;; or the minimum/maximum position ensuring that the target's y coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (y-helper mouse-x mouse-y)
      (max CIRCLE-RADIUS
           (min (- CANVAS-HEIGHT CIRCLE-RADIUS)
                (+ y-offset mouse-y))))

    ;; world-after-button-up : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates this world to its state following the given
    ;; a button-up mouseevent
    ;; STRATEGY: FUNCTIONAL COMPOSITION
    (define/public (world-after-button-up mx my mev)
      (if selected?
          (begin
            (set! selected? false)
            (send this rect-on-mouse mx my mev))
          (send this rect-on-mouse mx my mev)))

    ;; rect-on-mouse : PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: Updates the world by passing its ListOf<Shape<%>> to
    ;; the Shape's on-mouse method which will update the rectangles
    ;; based on the given mouseevent
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

    ;; on-key : KeyEvent -> Void
    ;; EFFECT: updates this World to its state following the given
    ;; Key event.
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on kev : KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev "r")
         (set! rectangles
               (if (send this no-room?)
                   rectangles
                   (local ((define new (send this make-rectangle-fn)))
                     (begin
                       (for-each
                        (lambda (old) (send old add-stranger new))
                        rectangles)
                       (cons new rectangles)))))]
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

    ;; make-rectangle-fn : -> Shape<%>
    ;; RETURNS: a Rectangle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (make-rectangle-fn)
      (new Rectangle%
        [x x]
        [y y]
        [selected? false]
        [strangers rectangles]))

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
;; [rmx PosInt] [rmy PosInt]
;; [selected? Boolean] [direction Direction]
;; [speed PosInt]
;; Interpretation: Represents a rectangle, containing:
;;  x which is a positive integer, representing the rectangle's x coordinate
;;  y which is a positive integer, representing the rectangle's y coordinate
;;  rmx which is a positive integer, representing the mouse event's
;; current x coordinate
;;  rmy which is a potivie integer, representing the mouse event's
;; current y coordinate
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
                strangers ;; a ListOf<Shape<%>>, the rectangles that
                ;; aren't yet our buddies
                )
    ;; offset-x and offset-y together form the vector from the 
    ;; mouse location to the or rectangle's center
    (field (x-offset ZERO))
    (field (y-offset ZERO))

    ;; a ListOf<Shape<%>> containing
    ;; the list of "buddied up" (attached) rectangles
    (field (buddies empty))

    ;; Determines if one of our buddies selected
    (field (highlighted? false))

    (super-new) ;; creating super class

    ;; on-tick : -> Void
    ;; EFFECT: updates this Shape to its state following a tick
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (on-tick)
      this)

    ;; on-key : KeyEvent -> Void
    ;; EFFECT: updates this Shape to its state following a tick
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (on-key kev)
      this)

    ;; on-mouse PosInt PosInt MyMouseEvent -> Void
    ;; EFFECT: updates this Shape to its state following the given
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
    ;; EFFECT: Updates this Shape to its state following the given
    ;; button-down mouse event
    ; STRATEGY: FUNCTION COMPOSITION
    (define/public (rectangle-after-button-down mx my)
      (if (send this in-rectangle? mx my)
          (begin
            (set! x-offset (- x mx))
            (set! y-offset (- y my))
            (set! selected? true)
            (for-each
             (lambda (b) (send b rectangle-highlight))
             buddies))
          this))

    ;; rectangle-highlight : -> Void
    ;; EFFECT: Updates this Shape's highlighted interface to true
    ;; meaning it is now a buddy
    (define/public (rectangle-highlight)
      (set! highlighted? true))

    ;; rectangle-after-drag : PosInt PosInt -> Void
    ;; EFFECT: updates this Shape to its state following the given
    ;; drag mouse event
    ;; STRATEGY: FUNCTION COMPOSITION
    (define/public (rectangle-after-drag mx my)
      (if selected?
          (begin
               (set! x (send this x-helper mx))
               (set! y (send this y-helper my))
               (for-each (lambda (that)
                           (send this add-buddy that)
                           (send that add-buddy this)
                           (send that rectangle-highlight))
                         (filter 
                          (lambda (other) (send this overlapping? other))
                          strangers)))
          this))

    ;; overlapping? : Shape<%> -> Boolean
    ;; GIVEN: a shape 
    ;; RETURNS: a boolean, determing if it is overlapping with
    ;; another member of the buddies list or not
    ;; STRATEGY: FUNCTIONAL COMPOSITION
    (define/public (overlapping? other)
      (and (<= (abs (- x (send other get-x))) RECTANGLE-WIDTH)
           (<= (abs (- y (send other get-y))) RECTANGLE-HEIGHT)))

    ;; add-buddy : Shape<%> -> Void
    ;; EFFECT: Updates the shape by removing the rectangle from 
    ;; strangers (not buddies)
    ;; and adds it to buddies
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on new : ListOf<Shape<%>>
    (define/public (add-buddy new)
      (set! strangers (remove new strangers))
      (set! buddies (cons new buddies)))

    ;; add-stranger : Shape<%> -> Void
    ;; EFFECT: Updates the shape by adding a rectangle to its
    ;; strangers (not buddies)
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on new : ListOf<Shape<%>>
    (define/public (add-stranger new)
          (set! strangers (cons new strangers)))

    ;; x-helper : PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; RETURNS: a postive integer that is either the next 
    ;; positive of the target
    ;; or the minimum/maximum position ensuring that the target's x coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (x-helper mx)
      (min (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH)
           (max RECTANGLE-HALF-WIDTH
                (+ mx x-offset))))

    ;; y-helper : PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either 
    ;; the next positive of the target
    ;; or the minimum/maximum position ensuring that the target's y coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (y-helper my)
      (min (- CANVAS-HEIGHT RECTANGLE-HALF-HEIGHT)
           (max RECTANGLE-HALF-HEIGHT
                (+ my y-offset))))

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
    ;; EFFECT: Updates this Shape to its state following the given
    ;; button-up mouse event
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-after-button-up)
      (set! selected? false)
      (set! highlighted? false))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the original, except with a rectangle drawn on it
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (add-to-scene scene)
      (place-image (if (or selected? highlighted?)
                       RECTANGLE-HIGHLIGHTED
                       RECTANGLE-REGULAR)
                   x y scene))

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

    ;; is-selected? : -> Boolean
    ;; RETURNS: a boolean determining if the rectangle is slected
    ;; or not
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (is-selected?)
      selected?)

    ;; get-color: -> String
    ;; RETURNS: either "red" or "green", depending on the color in
    ;; which this shape would be displayed if it were displayed now.
    ;; STRATEGY: Domain Knowledge
    (define/public (get-color)
      (if (or selected? highlighted?)
       RED
       GREEN))
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

(define RED "red")
(define GREEN "green")

(define RECTANGLE-HIGHLIGHTED (rectangle 30 20 "outline" "red"))
(define RECTANGLE-REGULAR (rectangle 30 20 "outline" "green"))

(define CIRCLE-CONSTANT (circle 5 "outline" "red"))

(define CIRCLE-RADIUS 5)

(define CONSTANT-SPEED .25)

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-world : -> World%
;; GIVEN: no arguments
;; RETURNS: A World% with no rectangles.
;; STRATEGY: DOMAIN KNOWLEDGE
(define (make-world)
  (new World%
       [x CIRCLE-INITIAL-X]
       [y CIRCLE-INITIAL-Y]
       [selected? false]
       [rectangles empty]))

;; run : PosNum -> World%
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates and runs a world that runs at the given rate.
;; RETURNS: the final world.
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (run frame-rate)
  (big-bang
   (make-world)
   (on-draw (lambda (w) (send w add-to-scene EMPTY-CANVAS)))
   (on-key (lambda (w kev) (send w on-key kev) w))
   (on-mouse (lambda (w mx my mev) (send w on-mouse mx my mev) w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (define world1 (make-world))
   (send world1 on-key "r")
   (define test-world (send world1 add-to-scene EMPTY-CANVAS))
   (check-equal?
    test-world
    (place-image (rectangle 30 20 "outline" "green")
                 200 250
                 (place-image
                  (circle CIRCLE-RADIUS "outline" "red")
                  200 250
                  EMPTY-CANVAS)) "test-1 for add-to-scene"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-tick)
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-2 for key-event"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-key "r")
   (send world1 on-tick)
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 2) "test-3 for key-event"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "x")
   (send world1 on-tick)
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 0) "test-4 for other key-event"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "enter")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-5 for other mouse-event"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 true 1) "test-6 for button-down mouse-event"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 10 13 "button-down")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-7 for button-down mouse-event
 when it is outside the target"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 10 13 "button-up")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-8 for button-up
 mouse-event
 when it is outside the target"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (send world1 on-mouse 395 250 "drag")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 395 250 true 1) "test-9 for drag
 mouse-event until the canvas edge"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 395 250 "drag")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-10 for drag
 mouse-event when there is nothing selected"))

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (send world1 on-mouse 395 250 "drag")
   (send world1 on-key "r")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 395 250 true 1)) "test-11 for
 key event when target is at the edge 
of the canvas i.e no roo for rectangle")

  (test-begin
   (define world1 (make-world))
   (send world1 on-key "r")
   (send world1 on-mouse 200 250 "button-down")
   (send world1 on-mouse 200 250 "button-up")
   (define test-world1  (test-world world1))
   (check-equal?
    test-world1
    (list 200 250 false 1) "test-12 for button-up 
mouse-event")))

(run-tests world-tests)

;; world1 : (make-world : -> World%) -> World%
;; Given : a make-world
;; Returns: a world 
(define world1 (make-world))

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
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 200 250 #t "red") "test-13 for button down in rectangle"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "button-down")
   (define test-rectangle1 (send rectangle1 add-to-scene EMPTY-CANVAS))
   (check-equal?
    test-rectangle1
    (place-image
     (rectangle 30 20 "outline" "red")
     200 250
     EMPTY-CANVAS) "test-14 for add to scene in rectangle"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "button-down")
   (send rectangle1 on-mouse 390 250 "drag")
   (send rectangle1 on-mouse 390 250 "button-up")
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 385 250 #f "green") "test-15 for mouse-events in rectangle"))

    (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "button-down")
   (send rectangle1 on-mouse 390 250 "drag")
   (send rectangle1 on-mouse 390 250 "button-up")
   (send world1 on-tick)
   (define rectangle2 (first (send world1 get-shapes)))
   (send rectangle2 on-mouse 377 250 "button-down")
   (send rectangle2 on-mouse 15 250 "drag")
   (send rectangle2 on-mouse 15 250 "button-up")
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 23 250 #f "green") "test-16 for dagging at 
the edges of the canvas"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-mouse 200 250 "enter")
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 200 250 #f "green") "test-17 for other mose events"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-key "r")
   (send world1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 200 250 #f "green") "test-18 key-event"))

  (test-begin
   (send world1 on-key "r")
   (define rectangle1 (first (send world1 get-shapes)))
   (send rectangle1 on-key "r")
   (send world1 on-tick)
   (send rectangle1 on-tick)
   (define test-rectangle1 (test-rectangle rectangle1))
   (check-equal?
    test-rectangle1
    (list 200 250 #f "green") "test-19 rectangle on-tick")))

(run-tests rectangle-tests)

