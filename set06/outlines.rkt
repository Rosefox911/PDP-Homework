;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide
 nested-rep?
 nested-to-flat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;;An Sexp is one of the following
;;-- a String      (interp: any string)
;;-- a Nat         (interp: any natural number more than zero)
;;-- a ListOfSexp  (interp: a list of sexp)

;;template:
;;sexp-fn : Sexp ->??
;(define (sexp-fn sexp)
;  (cond
;    [(string? sexp) ...]
;    [(positive-integer? sexp) ...]
;    [(cons? sexp)
;     (... (los-fn sexp))]))

;;A ListOfSexp (LOS) is one of
;;-- empty     (interp : when the list if empty, has nothing)
;;-- (cons Sexp ListOfSexp) 
;;     (interp: when there is at least one sexp in the list)

;;template:
;;los-fn : los ->??
;(define (los-fn los)
;  (cond
;    [(empty? los) ...]
;    [else (...
;           (sexp-fn (first los))
;           (los-fn (rest los)))]))

;;A NSexp is a (cons String Nestedrep)
;;-- a String     (interp: a String representing the title's content)
;;-- a Nestedrep  (interp: a ListOf<NSexp> which represents the subsections)

;;template:
;;nsexp-fn : NSexp -> ??
;(define (nsexp-fn nsexp)
;  (cond
;    [(string? nsexp) ...]
;    [else (nestedrep-fn nsexp)]))

;;A Nestrep is ListOf<Nsexp> and is one of
;;  -- NSexp
;;      (interp : when there is one nsexp)
;;  -- (cons NSexp Nestrep)  
;;      (interp: a list of nsexp followed by a list of nestrep)

;;template:
;;nestrep-fn : Nestrep ->??
;;(define (nestrep-fn lons)
;;  (cond
;;   [(nsexp? lons) ...]
;;   [else (...
;;          (nsexp-fn (first lons))
;;              (nestrep-fn (rest lons)))]))


;;A FSexp is a 
;;(list ListOf<Number> String)
;;--ListOf<Number> (interp: a list of nunber representing the index of the list)
;;--String         (interp: a String representing the title's content)

;;template:
;fsexp-fn : FSexp -> ??
;(define (fsexp flat)
;  (...
;   (first flat)
;   (rest flat)))


;;A ListOf<Number> (LON) is
;;--Number                       (interp: a number)
;;--(cons Number ListOf<Number>) (interp: a number or a list of numbers)

;;template: 
;;lon-fn : LON -> ??
;(define (lon-fn lon)
;  (cond
;    [(empty? (rest lon)) (...(first lon))]
;    [else (...
;           (first lon)
;           (lon-fn (rest lon)))])) 

;;A FlatRep is a ListOf<FSexp> which is one of
;;-- FSexp                      (interp: there is nothing in the list)
;;-- (cons FSexp ListOf<FSexp>) 
;;(interp: a flat sexp folowed by a list of flat sexp)

;;template :
;;lofr-fn : FlatRep ->??
;(define (lofr-fn lofrp)
;  (cond
;    [(empty? lofrp) ...]
;    [else (...
;           (fsexp-fn (first lofrp))
;           (lofr-fn (rest lofrp)))]))
;                     

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nested-rep? : Sexp -> Boolean
;; GIVEN: an Sexp
;; RETURNS: true iff it is the nested representation of some outline
;; EXAMPLE : (nested-rep? '(("The first section"
;;       ("A subsection with no subsections")
;;       ("Another subsection"
;;        ("This is a subsection of 1.2")
;;        ("This is another subsection of 1.2"))
;;       ("The last subsection of 1"))
;;      ("Another section"
;;       ("More stuff")
;;       ("Still more stuff")))) -> #t
(define (nested-rep? sexp)
  (cond
    [(string? sexp) true]
    [(positive-integer? sexp) false]
    [else (los? sexp)]))


;;los? : LOS -> Boolean
;;GIVEN : a list of sexp
;;RETURN: true iff its an NestedRep. false, otherwise
;;EXAMPLE : see the tests
(define (los? los)
  (cond
    [(empty? los) false]
    [else (if (empty? (rest los))
              (nested-rep? (first los))
              (and (nested-rep? (first los))
                   (los? (rest los))))]))


;;positive-integer? : Sexp -> Boolean
;;GIVEN : a sexp
;;RETURN : true if it is a number and positive, otherwise false
;;EXAMPLE : see the tests
(define (positive-integer? sexp)
  (and (number? sexp) (> sexp 0)))


;;TESTS
(define-test-suite check-nested-rep?
  (check-equal? 
   (nested-rep? empty) 
   false  
   "When a list of empty is passed, 
    the result should be false")
  (check-equal?
   (nested-rep? 
    '(("The first section"
       ("A subsection with no subsections")
       ("Another subsection"
        ("This is a subsection of 1.2")
        ("This is another subsection of 1.2"))
       ("The last subsection of 1"))
      ("Another section"
       ("More stuff")
       ("Still more stuff"))))
   true)
  (check-equal?
  (nested-rep? 
   '(((1) "The first section")
 ((1 1) "A subsection with no subsections")
 ((1 2) "Another subsection")
 ((1 2 1) "This is a subsection of 1.2")
 ((1 2 2) "This is another subsection of 1.2")
 ((1 3) "The last subsection of 1")
 ((2) "Another section")
 ((2 1) "More stuff")
 ((2 2) "Still more stuff")))
  false
  "This is a flat rep not a nested rep!"))



;; nested-to-flat : NestedRep -> FlatRep
;; GIVEN: the representation of an outline as a nested list
;; RETURNS: the flat representation of the outline
;;EXAMPLE : (nested-to-flat
;    '(("The first section"
;       ("A subsection with no subsections")
;       ("Another subsection"
;        ("This is a subsection of 1.2")
;        ("This is another subsection of 1.2"))
;       ("The last subsection of 1"))
;      ("Another section"
;       ("More stuff")
;       ("Still more stuff")))) ->
;(((1) "The first section")
; ((1 1) "A subsection with no subsections")
; ((1 2) "Another subsection")
; ((1 2 1) "This is a subsection of 1.2")
; ((1 2 2) "This is another subsection of 1.2")
; ((1 3) "The last subsection of 1")
; ((2) "Another section")
; ((2 1) "More stuff")
; ((2 2) "Still more stuff"))
(define (nested-to-flat lons)
  (nested-to-flat-helper lons (cons 1 empty)))



;;nested-to-flat-helper : NestedRep ListOf<Number> -> FlatRep
;;GIVEN : a NestedRep and also a list of numbers
;;RETURN : a flatrep
;;EXAMPLE :(nested-to-flat-helper '(("The first section"
;;                  ("A subsection with no subsections"))) empty)
;; ->((() "The first section") ((1) "A subsection with no subsections"))
(define (nested-to-flat-helper lst lon)
  (cond
    [(empty? (rest lst)) (make-flat (first lst) lon)]
    [else (append
           (make-flat (first lst) lon)
           (nested-to-flat-helper (rest lst) (get-number lon)))]))


;;get-number : ListOf<Number> -> ListOf<Number>
;;GIVEN : a list of numbers
;;RETURNS : increase the index and return a list of numbers
(define (get-number lon)
  (cond
    [(empty? (rest lon)) (cons (add1 (first lon)) empty)]
    [else (cons (first lon) (get-number (rest lon)))]))


;;make-flat : NSexp ListOf<NSexp> -> FlatRep
;;GIVEN : a nsexp and also a list of nsexp
;;RETURN : add list of numbers to the list to make a flat rep
;;EXAMPLE : (make-flat '(("The first section")
;;       ("A subsection with no subsections")) '(1))
;;(((1) ("The first section")) ((1 1) "A subsection with no subsections"))
(define (make-flat nrep lon)
  (cons (list lon (first nrep))
        (make-flat-helper (rest nrep) lon)))



;;make-flat-helper : NSexp ListOf<NSexp> -> FSexp
;;GIVEN : a nsexp and also a list of nsexp
;;RETURNS : a fsexp, representing the index, title and subsection
;;EXAMPLE : See the tests
(define (make-flat-helper nrep lon)
  (cond
    [(empty? nrep) empty]
    [else (nested-to-flat-helper nrep (get-new-lon lon))]))



;;get-new-lon : ListOf<Number> -> ListOf<Number>
;;GIVEN : a list of number
;;RETURNS : update the list of numbers accordingly
;;EXAMPLE : (get-new-lon '(1 2 3)) ->(1 2 3 1)
(define (get-new-lon lon)
  (append lon (list 1)))

;;TESTS:
(define-test-suite check-nested-to-flat
  (check-equal? (nested-to-flat
                 '(("The first section"
                    ("A subsection with no subsections")
                    ("Another subsection"
                     ("This is a subsection of 1.2")
                     ("This is another subsection of 1.2"))
                    ("The last subsection of 1"))
                   ("Another section"
                    ("More stuff")
                    ("Still more stuff"))))
                '(((1) "The first section")
                  ((1 1) "A subsection with no subsections")
                  ((1 2) "Another subsection")
                  ((1 2 1) "This is a subsection of 1.2")
                  ((1 2 2) "This is another subsection of 1.2")
                  ((1 3) "The last subsection of 1")
                  ((2) "Another section")
                  ((2 1) "More stuff")
                  ((2 2) "Still more stuff"))
                "The result should be as follow"))

(run-tests check-nested-to-flat)
(run-tests check-nested-rep?)



