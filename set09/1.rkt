#lang racket
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
;; -- "left" (interp: the cat is currently moving left (west on canvas))
;; -- "right" (interp: the cat is currently moving right (east on canvas))
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

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE

(define World<%>
  (interface ()
    
    ;; -> World<%>
    ;; RETURNS: the World<%> that should follow this one after a tick
    on-tick                             
    
    ;; Integer Integer MyMouseEvent -> World<%>
    ;; GIVEN: an integer representing the x coordinate where the mouse event 
    ;; happened, an integer representing the y coordinate where the mouse event
    ;; happened, and a mouseevent
    ;; RETURNS: the World<%> that should follow this one after the
    ;; given MouseEvent
    on-mouse
    
    ;; KeyEvent -> World<%>
    ;; GIVEN: KeyEvent
    ;; RETURNS: the World<%> that should follow this one after the
    on-key
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the given one, but with this shape drawn
    ;; on it.
    add-to-scene  
    
    ;; -> Integer
    ;; RETURNS: the x and y coordinates of the target, respectively
    get-x
    get-y
    
    ;; -> Boolean
    ;; RETURNS: true iff the the target is selected
    get-selected?
    
    ;; -> ListOf<Shape<%>>
    ;; RETURNS: a list of shapes
    get-shapes
    ))

(define Shape<%>
  (interface ()
    
    ;; -> Shape<%>
    ;; RETURNS: the Shape<%> that should follow this one after a tick
    on-tick                             
    
    ;; Integer Integer MyMouseEvent -> Shape<%>
    ;; GIVEN: an integer representing the x coordinate where the mouse event 
    ;; happened, an integer representing the y coordinate where the mouse event
    ;; happened, and a mouseevent
    ;; RETURNS: the Shape<%> that should follow this one after the
    ;; given MouseEvent
    on-mouse
    
    ;; KeyEvent -> Shape<%>
    ;; GIVEN: a key event
    ;; RETURNS: the Shape<%> that should follow this one after the
    ;; given KeyEvent
    on-key
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a Scene like the given one, but with this object drawn
    ;; on it.
    add-to-scene
    
    ;; -> Integer
    ;; RETURNS: the x and y coordinates of the center of the shape.
    get-x
    get-y
    
    ;; -> Boolean
    ;; RETURNS: true iff the shape is selected
    is-selected?
    ))


;; END OF INTERFACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASSES
;; A World is a (new World% [x PosInt] [y PosInt] [mx-pos PosInt] 
;;                          [my-pos PosInt] 
;;                          [selected? Boolean] [Rectangles ListOf<Rectangle%>]
;;                          [speed PosInt]
;; Interpretation: Represents a world, containing:
;; x representing the x coordinate of the center of the target
;; y representing the y coordinate of the center of the target
;; mx-pos representing the mouse event's x coordinate
;; my-pos representing the mouse event's y coordinate
;; selected? is a boolean representing whether the target is selected or not
;; rectangles is a ListOf<Rectangle%> representing the list of all rectangles
;; in the world
;; speed represents the amount of pixels traveled by the rectangle on each tick
;; World% -- a class that satisfies the World<%> interface (shown below).
(define World%
  (class* object% (World<%>)
    (init-field x ;; a PosInt, the x coordinate, of the target
                y ;; A PosInt, the y coordinate, of the target
                mx-pos ;; A PosInt, the x coordinate, of the mouse event
                my-pos ;; A PosInt, the y coordinate, of the mouse event
                selected?) ;; A Boolean representing if the 
    ;; target is selected or not 
    (init-field rectangles) ;; A ListOf<Rectangle%> representing all of
    ;; the rectangles on the board
    (init-field speed) ;; A PosInt, the amount of pixeled traveled by a
    ;; rectangle on each tick
    
    (field [CIRC-IMG CIRCLE-CONSTANT]) ;; A drawing of the circle based on
    ;; radius given by CIRCLE-CONSTANT
    
    (super-new) ;; creating super class
    
    ;; on-tick : -> World<%>
    ;; RETURNS: A world that should follow the given world after
    ;; a clock tick
    ;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (on-tick)
      (new World%
           [x x]
           [y y]
           [mx-pos mx-pos]
           [my-pos my-pos]
           [selected? selected?]
           [rectangles 
            (map
             ;; Rectangle% -> Rectangle%
             ;; GIVEN: a rectangle
             ;; RETURNS: the rectangle that should follow a clock tick
             ;; STRATEGY: FUNCTIONAL COMPOSITION
             (lambda (rectangle)
               (send rectangle on-tick))
             rectangles)]
           [speed speed]))
    
    ;; on-mouse : PosInt PosInt MyMouseEvent -> World<%>
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; a positive integer representing the mouse event's y coordinate
    ;; and a mouseevent
    ;; RETURNS: A world that should follow the given MouseEvent
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
    
    ;; world-after-button-down : PosInt PosInt MyMouseEvent -> World<%>
    ;; GIVEN: a positive integer representing the mouse event's x coordinate,
    ;; a positive integer representing the mouse event's y coordinate,
    ;; and a mouse event, in this case, a button-down.
    ;; RETURNS: a world that should follow a button-down mouse event
    ; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (world-after-button-down mx my mev)
      (if (send this in-circle? mx my)
          (new World%
               [x x]
               [y y]
               [mx-pos mx]
               [my-pos my]
               [selected? true]
               [rectangles (map
                            ;; Rectangle% -> Rectangle%
                            ;; GIVEN: a rectangle
                            ;; RETURNS: the rectangle that should follow
                            ;; a mouse event
                            ;; STRATEGY: FUNCTIONAL COMPOSITION
                            (lambda (rectangle)
                              (send rectangle on-mouse mx my mev))
                            rectangles)]
               [speed speed])
          (new World%
               [x x]
               [y y]
               [mx-pos mx-pos]
               [my-pos my-pos]
               [selected? selected?]
               [rectangles (map
                            ;; Rectangle% -> Rectangle%
                            ;; GIVEN: a rectangle
                            ;; RETURNS: the rectangle that should follow 
                            ;; a mouse event
                            ;; STRATEGY: FUNCTIONAL COMPOSITION
                            (lambda (rectangle)
                              (send rectangle on-mouse mx my mev))
                            rectangles)]
               [speed speed])))
    
    ;; in-circle : PosInt PosInt -> Boolean
    ;; GIVEN: a PosInt representing the mouse event's x coordinate
    ;; and a PosInt representing the mouse event's y coordinate
    ;; RETURNS: A boolean determining if the mouse event happened
    ;; inside the target circle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (in-circle? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr CIRCLE-RADIUS)))
    
    ;; world-after-drag : PosInt PosInt MyMouseEvent -> World<%>
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; a positive integer representing the mouse event's y coordinate
    ;; and a mouse event, in this case, a drag
    ;; RETURNS: a world that should follow a drag mouse event
    ;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (world-after-drag mouse-x mouse-y mev)
      (if selected? 
          (new World%
               [x (send this x-helper mouse-x mouse-y)]
               [y (send this y-helper mouse-x mouse-y)]
               [mx-pos (send this mx-pos-helper mouse-x mouse-y)]
               [my-pos (send this my-pos-helper mouse-x mouse-y)]
               [selected? true]
               [rectangles (map
                            ;; Rectangle% -> Rectangle%
                            ;; GIVEN: a rectangle
                            ;; RETURNS: the rectangle that should follow
                            ;; a mouse event
                            ;; STRATEGY: FUNCTIONAL COMPOSITION
                            (lambda (rectangle)
                              (send rectangle on-mouse mouse-x mouse-y mev))
                            rectangles)]
               [speed speed])
          (new World%
               [x x]
               [y y]
               [mx-pos mx-pos]
               [my-pos my-pos]
               [selected? selected?]
               [rectangles (map
                            ;; Rectangle% -> Rectangle%
                            ;; GIVEN: a rectangle
                            ;; RETURNS: the rectangle that should
                            ;; follow a mouse event
                            ;; STRATEGY: FUNCTIONAL COMPOSITION
                            (lambda (rectangle)
                              (send rectangle on-mouse mouse-x mouse-y mev))
                            rectangles)]
               [speed speed])))
    
    ;; x-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next position of 
    ;; the target
    ;; or the minimum/maximum position ensuring that the target's x coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (x-helper mouse-x mouse-y)
      (if (<= (+ x (- mouse-x mx-pos)) CIRCLE-RADIUS)
          CIRCLE-RADIUS
          (if (<= (- (- CANVAS-WIDTH x) (- mouse-x mx-pos))
                  CIRCLE-RADIUS)
              (- CANVAS-WIDTH CIRCLE-RADIUS)
              (+ x (- mouse-x mx-pos)))))
    
    ;; y-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next position of the target
    ;; or the minimum/maximum position ensuring that the target's y coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (y-helper mouse-x mouse-y)
      (if (<= (+ y (- mouse-y my-pos)) CIRCLE-RADIUS)
          CIRCLE-RADIUS
          (if (<= (- (- CANVAS-HEIGHT y) (- mouse-y my-pos))
                  CIRCLE-RADIUS)
              (- CANVAS-HEIGHT CIRCLE-RADIUS)
              (+ y (- mouse-y my-pos)))))
    
    ;; mx-pos-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a posint representing the mouse event's x coordinate
    ;; and a posint representing the mouse e vent's y coordinate.
    ;; RETURNS: a posint, representing what mx-pos should be
    ;; it will either be the mx position passed, or another position
    ;; ensuring that the object does not go outside of the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (mx-pos-helper mx my)
      (if (or (<= (+ x (- mx mx-pos)) CIRCLE-RADIUS)
              (<= (- (- CANVAS-WIDTH x) (- mx mx-pos))
                  CIRCLE-RADIUS))
          mx-pos
          mx))
    
    ;; my-pos-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a posint representing the mouse event's x coordinate
    ;; and a posint representing the mouse e vent's y coordinate.
    ;; RETURNS: a posint, representing what my-pos should be
    ;; it will either be the my position passed, or another position
    ;; ensuring that the object does not go outside of the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (my-pos-helper mx my)
      (if (or (<= (+ y (- my my-pos)) CIRCLE-RADIUS)
              (<= (- (- CANVAS-HEIGHT y) (- my my-pos))
                  CIRCLE-RADIUS))
          my-pos
          my))
    
    ;; world-after-button-up : PosInt PosInt MyMouseEvent -> World<%>
    ;; GIVEN: a posint representing the mouse event's x coordinate,
    ;; a posint representing the mouse event's y coordinate and a
    ;; mouse event, in this case, a button-up
    ;; RETURNS: the world that should follow the given mouse event,
    ;; in this case, a button-up
    ;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
    (define/public (world-after-button-up mx my mev)
      (if selected?
          (new World%
               [x x]
               [y y]
               [mx-pos ZERO]
               [my-pos ZERO]
               [selected? false]
               [rectangles (map
                            ;; Rectangle% -> Rectangle%
                            ;; GIVEN: a rectangle
                            ;; RETURNS: the rectangle that should 
                            ;; follow a mouse event
                            ;; STRATEGY: FUNCTIONAL COMPOSITION
                            (lambda (rectangle)
                              (send rectangle on-mouse mx my mev))
                            rectangles)]
               [speed speed])
          (new World%
               [x x]
               [y y]
               [mx-pos mx-pos]
               [my-pos my-pos]
               [selected? selected?]
               [rectangles (map
                            ;; Rectangle% -> Rectangle%
                            ;; GIVEN: a rectangle
                            ;; RETURNS: the rectangle that should 
                            ;; follow a mouse event
                            ;; STRATEGY: FUNCTIONAL COMPOSITION
                            (lambda (rectangle)
                              (send rectangle on-mouse mx my mev))
                            rectangles)]
               [speed speed])))
    
    ;; on-key : KeyEvent -> World<%>
    ;; GIVEN: a KeyEvent
    ;; RETURNS: the world that should follow the given KeyEvent
    ;; if the keyevent is r, a new rectangle is created on the target's
    ;; current position. Otherwise, the same world is returned.
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on kev : KeyEvent
    (define/public (on-key kev)
      (cond
        [(key=? kev "r")
         (new World%
              [x x]
              [y y]
              [mx-pos mx-pos]
              [my-pos my-pos]
              [selected? selected?]
              [rectangles (local ((define target-x x)
                                  (define target-y y))
                            (if (or (<= (- CANVAS-WIDTH target-x)
                                        RECTANGLE-HALF-WIDTH)
                                    (<= target-x 
                                        RECTANGLE-HALF-WIDTH)
                                    (<= target-y
                                        RECTANGLE-HALF-HEIGHT)
                                    (<= (- CANVAS-HEIGHT target-y) 
                                        RECTANGLE-HALF-HEIGHT))
                                rectangles
                                (cons 
                                 (send this make-rectangle-fn) rectangles)))]
              [speed speed])]
        [else this]))
    
    ;; make-rectangle : -> Shape<%>
    ;; RETURNS: a Rectangle
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (make-rectangle-fn)
      (new Rectangle% 
           [x x] 
           [y y]
           [rmx ZERO]
           [rmy ZERO]
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
    
    ;; get-shapes : -> ListOf<Rectangle%>
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
                rmx ;; a PosInt, the x coordinate of the mouse event
                rmy ;; a PosInt, the y coordinate of the mouse event
                selected?  ;; a Boolean, representing if the rectangle is 
                ;; selected or not. (true = selected, false = not)
                direction ;; a Direction, representing the direction the
                ;; direction the rectangle is moving in
                speed) ;; a PosInt, representing the amount of pixels
    ;; the rectangle will move on each clock tick
    
    (field (RECTANGLE-IMG RECTANGLE-CONSTANT)) ;; Drawing the rectangle based 
    ;; on the RECTANGLE-CONSTANT  
    ;; constant defined
    
    (super-new) ;; creating super class
    
    ;; on-tick : -> Shape<%>
    ;; RETURNS: A world that should follow the given world after
    ;; a clock tick
    ;; STRATEGY: FUNCTION COMPOSITION
    (define/public (on-tick)
      (if 
       selected?
       this
       (send this rectangle-on-tick speed)))
    
    ;; rectangle-on-tick PosInt -> Shape<%>
    ;; GIVEN: a positive integer, representing the amount of pixels
    ;; the rectangle with travel in a given direction, on each clock tick
    ;; RETURNS: a Rectangle that has been moved
    ;; STRATEGY: STRUCTURAL DECOMPOSITION on direction : Direction
    (define/public (rectangle-on-tick speed)
      (cond
        [(string=? direction "right") 
         (send this rectangle-on-tick-right-direction speed)]
        [(string=? direction "left") 
         (send this rectangle-on-tick-left-direction speed)]))
    
    ;; rectangle-on-tick-right-direction : PosInt -> Shape<%>
    ;; GIVEN: a positive integer, representing the amount of pixels
    ;; the rectangle with travel in a given direction, on each clock tick
    ;; RETURNS: a Rectangle that has been moved in the right direction
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-on-tick-right-direction speed)  
      (if
       (>= (min (+ x speed) (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH)) 
           (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH))
       (new Rectangle% 
            [x (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH)]
            [y y] 
            [rmx rmx] 
            [rmy rmy] 
            [selected? selected?] 
            [direction "left"] 
            [speed speed])
       (new Rectangle% 
            [x (+ x speed)] 
            [y y] 
            [rmx rmx] 
            [rmy rmy]
            [selected? selected?] 
            [direction direction] 
            [speed speed])))
    
    ;; rectangle-on-tick-left-direction : PosInt -> Shape<%>
    ;; GIVEN: a positive integer, representing the amount of pixels
    ;; the rectangle with travel in a given direction, on each clock tick
    ;; RETURNS: a Rectangle that has been moved in the right direction
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-on-tick-left-direction speed)  
      (if
       (<= (max (- x speed) RECTANGLE-HALF-WIDTH) RECTANGLE-HALF-WIDTH)
       (new Rectangle% [x RECTANGLE-HALF-WIDTH] [y y] [rmx rmx] [rmy rmy]
            [selected? selected?] [direction "right"] [speed speed])
       (new Rectangle% [x (- x speed)] [y y] [rmx rmx] [rmy rmy]
            [selected? selected?] [direction direction] [speed speed])))
    
    ;; on-key : KeyEvent -> Shape<%>
    ;; GIVEN: a KeyEvent
    ;; RETURNS: the rectangle that should follow the given key event
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (on-key kev)
      this)
    
    ;; on-mouse PosInt PosInt MyMouseEvent -> Shape<%>
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; a positive integer representing the mouse event's y coordinate
    ;; and a mouseevent
    ;; RETURNS: A rectangle that should follow the given MouseEvent
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
    
    ;; rectangle-after-button-down : PosInt PosInt -> Shape<%>
    ;; GIVEN: a positive integer representing the mouse event's x coordinate,
    ;; and a positive integer representing the mouse event's y coordinate. 
    ;; RETURNS: a rectangle that should follow a button-down mouse event
    ; STRATEGY: FUNCTION COMPOSITION
    (define/public (rectangle-after-button-down mx my)
      (if (send this in-rectangle? mx my)
          (new Rectangle% [x x] 
               [y y] 
               [rmx mx] 
               [rmy my] 
               [selected? true] 
               [direction direction]
               [speed speed])
          this))
    
    ;; rectangle-after-drag : PosInt PosInt -> Shape<%>
    ;; GIVEN: a positive integer representing the mouse event's x coordinate,
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a rectangle that should follow a drag mouse event
    ;; STRATEGY: FUNCTION COMPOSITION
    (define/public (rectangle-after-drag mx my)
      (if selected?
          (new Rectangle% 
               [x (send this x-helper mx my)] 
               [y (send this y-helper mx my)] 
               [rmx (send this rmx-helper mx my)]
               [rmy (send this rmy-helper mx my)] 
               [selected? true] 
               [direction direction]
               [speed speed])
          this))
    
    
    
    ;; x-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next positive of the target
    ;; or the minimum/maximum position ensuring that the target's x coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
   (define/public (x-helper mx my)
      (if (<= (+ x (- mx rmx)) RECTANGLE-HALF-WIDTH)
          RECTANGLE-HALF-WIDTH
          (if (<= (- (- CANVAS-WIDTH x) (- mx rmx))
                  RECTANGLE-HALF-WIDTH)
              (- CANVAS-WIDTH RECTANGLE-HALF-WIDTH)
              (+ x (- mx rmx)))))
    
    ;; y-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a positive integer representing the mouse event's x coordinate
    ;; and a positive integer representing the mouse event's y coordinate
    ;; RETURNS: a postive integer that is either the next positive of the target
    ;; or the minimum/maximum position ensuring that the target's y coordinate
    ;; does not go outside the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (y-helper mx my)
      (if (<= (+ y (- my rmy)) RECTANGLE-HALF-HEIGHT)
          RECTANGLE-HALF-HEIGHT
          (if (<= (- (- CANVAS-HEIGHT y) (- my rmy))
                  RECTANGLE-HALF-HEIGHT)
              (- CANVAS-HEIGHT RECTANGLE-HALF-HEIGHT)
              (+ y (- my rmy)))))
    
    ;; rmx-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a posint representing the mouse event's x coordinate
    ;; and a posint representing the mouse event's y coordinate.
    ;; RETURNS: a posint, representing what my-pos should be
    ;; it will either be the my position passed, or another position
    ;; ensuring that the object does not go outside of the canvas
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rmx-helper mx my)
      (if (or (<= (+ x (- mx rmx)) RECTANGLE-HALF-WIDTH)
              (<= (- (- CANVAS-WIDTH x) (- mx rmx))
                  RECTANGLE-HALF-WIDTH))
          rmx
          mx))
    
    ;; rmy-helper : PosInt PosInt -> PosInt
    ;; GIVEN: a posint representing the mouse event's x coordinate,
    ;; a posint representing the mouse event's y coordinate and a
    ;; mouse event, in this case, a button-up
    ;; RETURNS: the world that should follow the given mouse event,
    ;; in this case, a button-up
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rmy-helper mx my)
      (if (or (<= (+ y (- my rmy)) RECTANGLE-HALF-HEIGHT)
              (<= (- (- CANVAS-HEIGHT y) (- my rmy))
                  RECTANGLE-HALF-HEIGHT))
          rmy
          my))
    
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
    
    ;; rectangle-after-button-down : -> Shape<%>
    ;; RETURNS: a rectangle that should follow a button up mouse event
    ;; STRATEGY: DOMAIN KNOWLEDGE
    (define/public (rectangle-after-button-up)
     
      (new Rectangle% [x x] [y y] [rmx rmx] [rmy rmy] [selected? false]
           [direction direction] [speed speed])
     )
    
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
       [mx-pos ZERO]
       [my-pos ZERO]
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
   (on-tick (lambda (w) (send w on-tick)) frame-rate)
   (on-draw (lambda (w) (send w add-to-scene EMPTY-CANVAS)))
   (on-key (lambda (w kev) (send w on-key kev)))
   (on-mouse (lambda (w mx my mev) (send w on-mouse mx my mev)))))


;; TESTS
(define-test-suite world-tests
  (local 
    ((define DEFAULT-WORLD (make-world 1))
     
     (define WORLD-1
       (new World%
            [x 50]
            [y 50]
            [mx-pos 0]
            [my-pos 0]
            [selected? false]
            [rectangles 
             (list
              (new Rectangle% 
                   [x 10] 
                   [y 50] 
                   [rmx 0] 
                   [rmy 0]
                   [selected? false] 
                   [direction "right"] 
                   [speed 10]))]
            [speed 10]))
     
     (define WORLD-2
       (new World%
            [x 500]
            [y 500]
            [mx-pos 0]
            [my-pos 0]
            [selected? false]
            [rectangles 
             (list
              (new Rectangle% 
                   [x 10] 
                   [y 50] 
                   [rmx 0] 
                   [rmy 0]
                   [selected? false] 
                   [direction "right"] 
                   [speed 10]))]
            [speed 10]))
     
     (define WORLD-AFTER-BUTTON-DOWN 
       (send DEFAULT-WORLD world-after-button-down 200 250 "button-down"))
     (define WORLD-AFTER-DRAG 
       (send WORLD-AFTER-BUTTON-DOWN world-after-drag 200 250 "drag"))
     (define WORLD-AFTER-BUTTON-UP 
       (send WORLD-AFTER-DRAG world-after-drag 200 250 "drag"))
     (define WORLD-NO-SPACE
       (new World%
            [x 50]
            [y 50]
            [mx-pos 0]
            [my-pos 0]
            [selected? false]
            [rectangles 
             (list
              (new Rectangle% 
                   [x 10] 
                   [y 50] 
                   [rmx 0] 
                   [rmy 0]
                   [selected? false] 
                   [direction "right"] 
                   [speed 10]))]
            [speed 10]))
     
     (define WORLD-WITH-SPACE
       (new World%
            [x 50]
            [y 50]
            [mx-pos 51]
            [my-pos 51]
            [selected? true]
            [rectangles 
             (list
              (new Rectangle% 
                   [x 50] 
                   [y 50] 
                   [rmx 0] 
                   [rmy 0]
                   [selected? false] 
                   [direction "right"] 
                   [speed 10]))]
            [speed 10])))
    (check-equal?
     (send DEFAULT-WORLD get-x)
     CIRCLE-INITIAL-X
     "Testing the initial X-coordinate")
    (check-equal?
     (send DEFAULT-WORLD get-y)
     CIRCLE-INITIAL-Y
     "Testing the initial Y-coordinate")
    (check-equal?
     (send DEFAULT-WORLD get-selected?)
     false
     "Testing the initial selection status")
    (check-equal?
     (send DEFAULT-WORLD get-shapes)
     empty
     "Testing the initial shapes")
    (check-equal? 
     (send (send DEFAULT-WORLD on-mouse 10 10 "button-down") get-selected?) 
     false
     "Testing button-down mouseevent for world")
    (check-equal? 
     (send (send DEFAULT-WORLD on-mouse 10 10 "drag") get-x) 
     200
     "Testing drag mouseevent for world")
    (check-equal? 
     (send (send DEFAULT-WORLD on-mouse 10 10 "button-up") get-selected?) 
     false
     "Testing button-up mouseevent for world")
    (check-equal? 
     (send (send DEFAULT-WORLD on-mouse 10 10 "enter") get-selected?) 
     false
     "Testing else case in mouseevent for world")
    (check-equal? 
     (send (send DEFAULT-WORLD world-after-button-down 10 10 "button-down")
           get-selected?) 
     false
     "Testing world-after-button-down in world")
    (check-equal?
     (send (send WORLD-1 on-tick) get-x)
     50
     "Testing to make sure on-tick returns a world that should follow tick.")
    (check-equal? 
     (image? (send WORLD-1 add-to-scene EMPTY-CANVAS))
     true
     "Testing to make sure scene is created when using add-to-scene")
    (check-equal?
     (length (send (send WORLD-1 on-key "r") get-shapes)) 2
     "Should create a second rectangle, so there should be 2.")
    (check-equal?
     (length (send (send WORLD-1 on-key "u") get-shapes)) 1
     "Should NOT create another rectangle, so there should be 1.")
    (check-equal?
     (length (send (send WORLD-2 on-key "r") get-shapes)) 1
     "Should NOT create another rectangle,
there is no room for a new rectangle due to target position.")
    (check-equal? 
     (send DEFAULT-WORLD my-pos-helper 50 50)
     50
     "Testing my-pos-helper, should return the given my")
    (check-equal? 
     (send DEFAULT-WORLD my-pos-helper 50 500)
     0
     "Testing my-pos-helper, should return 0, the minimum")
    (check-equal? 
     (send DEFAULT-WORLD my-pos-helper 50 -500)
     0
     "Testing my-pos-helper, should return 0, the minimum")
    (check-equal?
     (send DEFAULT-WORLD mx-pos-helper 50 50)
     50
     "Testing mx-pos-helper, should return the given my")
    (check-equal? 
     (send DEFAULT-WORLD mx-pos-helper 500 50)
     0
     "Testing mx-pos-helper, should return 0, the minimum")
    (check-equal? 
     (send DEFAULT-WORLD mx-pos-helper -500 50)
     0
     "Testing mx-pos-helper, should return 0, the minimum")
    (check-equal? 
     (send DEFAULT-WORLD y-helper 50 50)
     300
     "Testing y-helper, should return the given y")
    (check-equal? 
     (send DEFAULT-WORLD y-helper 50 500)
     495
     "Testing y-helper, should return 495, the maximum")
    (check-equal? 
     (send DEFAULT-WORLD y-helper 50 -500)
     5
     "Testing y-helper, should return 5, the minimum")
    (check-equal?
     (send DEFAULT-WORLD x-helper 50 50)
     250
     "Testing x-helper, should return the given y, 250")
    (check-equal? 
     (send DEFAULT-WORLD x-helper 500 50)
     395
     "Testing x-helper, should return 395, the maximum")
    (check-equal? 
     (send DEFAULT-WORLD x-helper -500 50)
     5
     "Testing x-helper, should return 5, the minimum")
    (check-equal?
     (send (send WORLD-NO-SPACE world-after-button-down 200 250 "button-down")
           get-selected?)
     false
     "Testing world-after-button-down, should return false.")
    (check-equal?
     (send (send WORLD-NO-SPACE world-after-button-down 51 51 "button-down")
           get-selected?)
     true
     "Testing world-after-button-down, should return true")
    (check-equal?
     (send (send WORLD-NO-SPACE world-after-button-up 51 51 "button-up")
           get-selected?)
     false
     "Testing world-after-button-up, should return true")
    (check-equal?
     (send (send WORLD-WITH-SPACE world-after-button-up 51 51 "button-up")
           get-selected?)
     false
     "Testing world-after-button-up, should return true")
    
    (check-equal?
     (send (send WORLD-NO-SPACE world-after-drag 51 51 "drag") get-x)
     50
     "Testing world-after-drag, should return 50")
    (check-equal?
     (send (send WORLD-WITH-SPACE world-after-drag 51 51 "drag") get-x)
     50
     "Testing world-after-drag, should return 50")))

(define-test-suite rectangle-tests
  (local 
    ((define DEFAULT-RECTANGLE (new Rectangle% [x 50] 
                                    [y 50] [rmx 0] [rmy 0] 
                                    [selected? false]
                                    [direction "right"] 
                                    [speed 10]))
     (define RECTANGLE-AFTER-BUTTON-DOWN 
       (send DEFAULT-RECTANGLE rectangle-after-button-down 50 50))
     (define RECTANGLE-AFTER-BUTTON-DOWN-ALT 
       (send DEFAULT-RECTANGLE rectangle-after-button-down 200 200))
     (define RECTANGLE-AFTER-DRAG
       (send RECTANGLE-AFTER-BUTTON-DOWN rectangle-after-drag 50 50))
     (define RECTANGLE-AFTER-DRAG-ALT 
       (send RECTANGLE-AFTER-BUTTON-DOWN-ALT rectangle-after-drag 200 200))
     (define RECTANGLE-AFTER-BUTTON-UP 
       (send RECTANGLE-AFTER-BUTTON-DOWN-ALT rectangle-after-button-up))
     (define RECTANGLE-RIGHT DEFAULT-RECTANGLE)
     (define RECTANGLE-RIGHT-CHANGING
       (new Rectangle% 
            [x 400]
            [y 50] 
            [rmx 0] 
            [rmy 0] 
            [selected? true] 
            [direction "right"] 
            [speed 10]))
     (define RECTANGLE-LEFT
       (new Rectangle% 
            [x 50]
            [y 50] 
            [rmx 0] 
            [rmy 0] 
            [selected? true] 
            [direction "left"] 
            [speed 10]))
     (define RECTANGLE-LEFT-CHANGING
       (new Rectangle% 
            [x 0]
            [y 50] 
            [rmx 0] 
            [rmy 0] 
            [selected? true] 
            [direction "left"] 
            [speed 10])))
    (check-equal? 
     (send (send DEFAULT-RECTANGLE on-tick) get-x) 60
     "Testing get-x for rectangle")
    (check-equal? 
     (send (send DEFAULT-RECTANGLE on-tick) get-y) 50
     "Testing get-y for rectangle")
    (check-equal? 
     (send (send DEFAULT-RECTANGLE on-tick) is-selected?) false
     "Testing is-selected? for rectangle")
    (check-equal? 
     (image? (send DEFAULT-RECTANGLE add-to-scene EMPTY-CANVAS)) true
     "Testing add-to-scene for rectangle, should create image")
    (check-equal? 
     (send DEFAULT-RECTANGLE in-rectangle? 51 51) true
     "Testing in-rectangle? for rectangle, should return true")
    (check-equal? 
     (send DEFAULT-RECTANGLE rmy-helper 51 51) 51
     "Testing rmy-helper for rectangle, should return passed value")
    (check-equal? 
     (send DEFAULT-RECTANGLE rmy-helper 51 501) 0
     "Testing rmy-helper for rectangle, should return 0")
    (check-equal? 
     (send DEFAULT-RECTANGLE rmx-helper 51 51) 51
     "Testing rmx-helper for rectangle, should return passed value")
    (check-equal? 
     (send DEFAULT-RECTANGLE rmx-helper 501 51) 0
     "Testing rmx-helper for rectangle, should return 0")
    (check-equal? 
     (send DEFAULT-RECTANGLE x-helper 51 51) 101
     "Testing x-helper for rectangle, should return 101")
    (check-equal? 
     (send DEFAULT-RECTANGLE x-helper 5001 51) 385
     "Testing x-helper for rectangle, should return 385")
    (check-equal? 
     (send DEFAULT-RECTANGLE x-helper -1000 51) 15
     "Testing x-helper for rectangle, should return 15")
    (check-equal? 
     (send DEFAULT-RECTANGLE y-helper 51 51) 101
     "Testing y-helper for rectangle, should return 101")
    (check-equal? 
     (send DEFAULT-RECTANGLE y-helper 51 51000) 490
     "Testing y-helper for rectangle, should return 490")
    (check-equal? 
     (send DEFAULT-RECTANGLE y-helper 51 -1000) 10
     "Testing y-helper for rectangle, should return 15")
    (check-equal?
     (send RECTANGLE-AFTER-BUTTON-DOWN get-x)
     50
     "Testing rectangle-after-button-down main")
    (check-equal?
     (send RECTANGLE-AFTER-BUTTON-DOWN-ALT get-x)
     50
     "Testing rectangle-after-button-down else clause")
    (check-equal?
     (send RECTANGLE-AFTER-DRAG get-x)
     50
     "Testing rectangle-after-drag main clause")
    (check-equal?
     (send RECTANGLE-AFTER-DRAG-ALT get-x)
     50
     "Testing rectangle-after-drag else clause")
    (check-equal?
     (send RECTANGLE-AFTER-BUTTON-UP is-selected?)
     false
     "Testing rectangle-after-button-down main clause")
    (check-equal?
     (send (send DEFAULT-RECTANGLE on-mouse 50 50 "button-down") get-x)
     50
     "Testing on-mouse button-down case")
    (check-equal?
     (send (send DEFAULT-RECTANGLE on-mouse 50 50 "drag") get-x)
     50
     "Testing on-mouse drag case")
    (check-equal?
     (send (send DEFAULT-RECTANGLE on-mouse 50 50 "button-up") get-x)
     50
     "Testing on-mouse button-up case")
    (check-equal?
     (send (send DEFAULT-RECTANGLE on-mouse 50 50 "enter") get-x)
     50
     "Testing on-mouse else case")
    (check-equal?
     (send (send DEFAULT-RECTANGLE on-key "r") get-x)
     50
     "Testing on-key case")
    (check-equal?
     (send (send RECTANGLE-AFTER-BUTTON-DOWN on-tick) is-selected?)
     true
     "Testing the on-tick event")
    (check-equal?
     (send (send RECTANGLE-RIGHT rectangle-on-tick 5) get-x)
     55
     "Testing to make sure rectangle moves right, 5 pixels")
    (check-equal?
     (send (send RECTANGLE-LEFT rectangle-on-tick 5) get-x)
     45
     "Testing to make sure rectangle moves left, 5 pixels")
    (check-equal?
     (send (send RECTANGLE-LEFT-CHANGING rectangle-on-tick-left-direction 5)
           get-x)
     15
     "Testing to make sure rectangle changes from left to right")
    (check-equal?
     (send (send RECTANGLE-RIGHT rectangle-on-tick-right-direction 5) get-x)
     55
     "Testing to make sure rectangle moves right, 5 pixels")
    (check-equal?
     (send (send RECTANGLE-RIGHT-CHANGING rectangle-on-tick-right-direction 5)
           get-x)
     385
     "Testing to make sure rectangle changes from right to left")))

(run-tests world-tests)
(run-tests rectangle-tests)

