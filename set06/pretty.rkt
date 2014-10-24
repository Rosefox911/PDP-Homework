;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide
 SUM-EXP-4
 expr-to-strings
 make-sum-exp
 make-mult-exp
 sum-exp-exprs
 mult-exp-exprs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

(define-struct context (prefix suffix width))
;; A Context is a (make-context String String Nat)
;; prefix is a string that represents the prefix of the line
;; suffix is a string that represents the suffix of the line
;; width is a nat that represents the width allowed for the line

;; context-fn : Context -> ??
; (define (context-fn con)
;   (...
;    (context-prefix con)
;    (context-suffix con)
;    (context-width con)))

(define-struct sum-exp (exprs))
;; a Sum-Exp is a (make-sum-exp NELO<Expr>)
;; exprs is a NELO<Expr> which represent a list of operands for addition

;; TEMPLATE
;; Sum-Exp-fn : Sum-Exp -> ??
;(define (sum-exp-fn s)
;  (...
;   (sum-exp-exprs s)))

(define-struct mult-exp (exprs))
;; a Mult-Exp is a (make-mult-exp NELO<Expr>)
;; exprs is a NELO<Expr> which represent a list of operands for multiplication

;; TEMPLATE
;; Mult-Exp-fn : Mult-Exp -> ??
;(define (mult-exp-fn m)
;  (...
;   (mult-exp-exprs m)))

;; A Form is one of
;; -- (make-sum-exp NELO<Expr>) (interp: a sum-exp which represents
;; a summation form)
;; -- (make-mult-exp NELO<Expr>) (interp: a mult-exp which represents
;; a multiplication form)

;; Form-fn : Form -> ??
;(define (form-fn form)
;  (cond
;    [(sum-exp? form) ...]
;    [(mult-exp? form) ...]))

;; An Expr is one of
;; -- Integer (interp: an integer)
;; -- Form (interp: a form representing an operation on expressions)

;; Expr-fn : Expr -> ??
;(define (expr-fn expr)
;  (cond
;    [(integer? expr) ...]
;    [(sum-exp? expr)
;     (...
;      (loexpr-fn (sum-exp-exprs expr)))]
;    [(mult-exp? expr)
;     (...
;      (loexpr-fn (mult-exp-exprs expr)))]))

;; A LO<Expr> is one of
;; -- empty (interp: no exprs in the list)
;; -- (cons Expr LO<Expr>) (interp: there are exprs within it)

;; LOExpr-fn : LO<Expr> -> ??
;(define (loexpr-fn exprs)
;  (cond
;    [(empty? exprs) ...]
;    [(cons? exprs)
;     (...
;      (expr-fn (first exprs))
;      (loexpr-fn (rest exprs)))]))

;; A NELO<Expr> is a non-empty LO<Expr>.

;; A List of Numbers (LON) is one of:
;; -- empty (interp: no numbers in the list)
;; -- (cons Number LON) (interp: there are numbers within it)

;; Template:
;; LON-fn : LON -> ??
; (define (lon-fn lstn)
;   (cond
;     [(empty? lstn) ...]
;     [else (... (first lstn)
;                (lon-fn (rest lstn)))]))


 ;; A List of Strings (LOS) is one of:
;; -- empty (interp: no strings in the list)
;; -- (cons String LOS) (interp: there are strings within it)

;; Template:
;; LOS-fn : LON -> ??
; (define (los-fn lsts)
;   (cond
;     [(empty? lsts) ...]
;     [else (... (first lsts)
;                (los-fn (rest lsts)))]))

;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define SPACE " ")
;; just a space character

(define OPEN-PAR "(")
;; left parenthesis

(define CLOSE-PAR ")")
;; right parenthesis

(define SUM-OPERATOR "+")
;; the sum operator

(define SUM-HEADER (string-append OPEN-PAR SUM-OPERATOR))
;; header for sum forms

(define MULT-OPERATOR "*")
;; the multiplication operator

(define EMPTY "")
;; an empty string

(define MULT-HEADER (string-append OPEN-PAR MULT-OPERATOR))
;; header for multiplication forms

;;; Assumes that MULT-HEADER and SUM-HEADER are the same size
(define INDENTATION (string-length MULT-HEADER))

;;; Error message
(define ERROR-MESSAGE "not enough room")

;; CONSTANTS FOR TESTING

(define SUM-EXP-1 (make-sum-exp (list 22 333 44)))

(define SUM-EXP-2 (make-sum-exp (list 964 2145 49821)))

(define MULT-EXP-1 (make-mult-exp (list 2 56 3450)))

(define MULT-EXP-2 (make-mult-exp (list 911 8425 0)))

(define SUM-EXP-3 (make-sum-exp (list SUM-EXP-1 MULT-EXP-1)))

(define MULT-EXP-3 (make-mult-exp (list MULT-EXP-2 SUM-EXP-2)))

(define SUM-EXP-4 (make-sum-exp (list SUM-EXP-3 MULT-EXP-3)))

(define hw-example-2
  (make-sum-exp (list (make-mult-exp (list (make-mult-exp (list 1))
                                           (make-sum-exp (list 22 23))))
                      (make-mult-exp
                       (list (make-sum-exp (list 66 67 68))
                             (make-mult-exp (list 42 43 44 45 46 47 48))))
                      (make-mult-exp (list 77 88)))))

(define CONTEXT-1 (make-context "(+" ")" 5))

(define CONTEXT-2 (make-context "(+" ")" 100))

;; END OF CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; place-in : Context String -> String
;; GIVEN: a context and a string
;; RETURN: a string with the suffix and prefix within context appended
;; to the original string
;; EXAMPLES:
;; (place-in CONTEXT-1 "5") -> "(+5)"
;; STRATEGY: STRUCTURAL DECOMPOSITION on context : Context
(define (place-in context string)
  (string-append (context-prefix context)
                 string
                 (context-suffix context)))

;; TEST
(define-test-suite place-in-tests
  (check-equal?
   (place-in CONTEXT-1 "5")
   "(+5)"
   "Testing to make sure suffix and
prefix is extracted and appended to original string."))

;; pprint-int : Integer Context -> String
;; GIVEN: an integer, representing a number
;; RETURNS: a string with the context's suffix and suffix appended to a string
;; after said string is converted from a number to a string.
;; EXAMPLES:
;; (pprint-int 5 CONTEXT-1) -> "(+5)"
;; STRATEGY: FUNCTIONAL COMPOSITION

(define (pprint-int int context)
  (place-in context (number->string int)))


;; TESTS
(define-test-suite pprint-int-tests
  (check-equal?
   (pprint-int 5 CONTEXT-1)
   "(+5)"
   "Testing to make sure string provided is
converted to a string and appended to suffic and prefix of context."))

;; form-operands : Form -> NELO<Expr>
;; GIVEN: a form
;; RETURNS: a non-empty list of Expressions extracted from the form
;; EXAMPLES:
;; (form-operands SUM-EXP-1) -> (list 22 333 44)
;; STRATEGY: STRUCTURAL DECOMPOSITION on form : Form
(define (form-operands form)
  (cond
   [(sum-exp? form) (sum-exp-exprs form)]
   [(mult-exp? form) (mult-exp-exprs form)])) ;; Do we need an else here?

;; TESTS
(define-test-suite form-operands-tests
  (check-equal?
   (form-operands SUM-EXP-1)
   (list 22 333 44)
   "Testing to make sure sum expression list is extracted")
  (check-equal?
   (form-operands MULT-EXP-1)
   (list 2 56 3450)
   "Testing to make sure mult expression list is extracted"))

;; form-header : Form -> String
;; GIVEN: a form
;; RETURNS: A string, representing the header (left paren and operator)
;; EXAMPLES:
;; (form-header SUM-EXP-1) -> "(+"
;; (form-header MULT-EXP-1) -> "(*"
;; STRATEGY: STRUCTURAL DECOMPOSITION on form : Form
(define (form-header expr)
  (cond [(sum-exp? expr) SUM-HEADER]
        [(mult-exp? expr) MULT-HEADER]))

;; TESTS
(define-test-suite form-header-tests
   (check-equal?
    (form-header SUM-EXP-1)
    "(+"
    "Testing to make sure sum expression is extracted")
 (check-equal?
  (form-header MULT-EXP-1)
  "(*"
  "Testing to make sure mult expression is extracted"))

;; form? : Expr -> Boolean
;; GIVEN: an expr
;; RETURNS: true iff it is a Form
;; EXAMPLES:
;; (form? SUM-EXP-1) -> true
;; (form? MULT-EXP-1) -> true
;; (form? 1) -> false
;; STRATEGY: STRUCTURAL DECOMPOSITION on expr : Expr
(define (form? expr)
  (cond
   [(integer? expr) false]
   [(sum-exp? expr) true]
   [(mult-exp? expr) true]))

;; TESTS
(define-test-suite forms?-tests
 (check-equal?
  (form? SUM-EXP-1)
  true
  "Testing to make sure it detects its a sum expr.")
 (check-equal?
  (form? MULT-EXP-1)
  true
  "Testing to make sure it detects it is a mult expr.")
 (check-equal?
  (form? 1)
  false
  "Testing to make sure it detects that it is NOT a sum or mult expr"))

;; expr-width : Expr -> Number
;; GIVEN: an expr
;; RETURNS: the number of columns necessary to print the expr in one line
;; EXAMPLES:
;; (expr-width SUM-EXP-1) -> 13
;; STRATEGY: STRUCTURAL DECOMPOSITION on expr : Expr
(define (expr-width expr)
  (cond
    [(integer? expr) (string-length (number->string expr))]
    [(form? expr) (form-width expr)]))
 
;; TESTS
(define-test-suite expr-width-tests
  (check-equal?
   (expr-width SUM-EXP-1)
   13
   "Testing to make sure it extracts correct number of characters from list"))
 
;; form-width : Form -> Number
;; GIVEN: a form
;; RETURNS: the number of columns necessary to print the form in one line
;; EXAMPLES:
;; (expr-width-helper SUM-EXP-1) -> 13
;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
(define (form-width form)
  (foldr + 0
         (map add1 ; count spaces between words, and the close paren
              (cons (string-length (form-header form))
                    (map expr-width 
                         (form-operands form))))))
 
;; TESTS
(define-test-suite form-width-tests
  (check-equal?
   (form-width SUM-EXP-1)
   13
   "Testing to make sure it extracts correct number of characters from list"))

;; fits-on-line? : Expr Context -> Boolean
;; GIVEN: an expr and a context
;; RETURNS: a boolean determining if the expression will fit
;; on the line based on the width
;; EXAMPLES:
;; (fits-one-line? SUM-EXP-1 CONTEXT-1) -> false
;; STRATEGY: STRUCTURAL DECOMPOSITION on context : Context
(define (fits-one-line? expr context)
  (>= (context-width context)
      (+ (string-length (context-prefix context))
         (expr-width expr)
         (string-length (context-suffix context)))))

;; TESTS
(define-test-suite fits-one-line?-tests
  (check-equal?
   (fits-one-line? SUM-EXP-1 CONTEXT-1)
   false
   "Testing to make sure this returns false, it cannot fit based on width."))

;; pprint-expr : Expr Context -> LOS
;; GIVEN: an expr and a context
;; RETURNS: a LOS if expr will fit on one line (based on width)
;; otherwise, it will return an error
;; EXAMPLES:
;; (pprint-expr SUM-EXP-1 CONTEXT-1) -> (error "not enough room")
;; (pprint-expr SUM-EXP-1 CONTEXT-2) -> (list "(+(+ 22 333 44))")
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (pprint-expr expr context)
  (if (fits-one-line? expr context)
      (list (pprint-one-line expr context))
      (pprint-multiple expr context)))


;; TESTS
(define-test-suite pprint-expr-tests
  (check-equal?
   (pprint-expr SUM-EXP-1 CONTEXT-2)
   (list "(+(+ 22 333 44))")
   "Testing to make sure it produces a list of strings.")
  (check-error
   (pprint-expr SUM-EXP-1 CONTEXT-1)))

;; prefix-with : Context String -> Context
;; GIVEN: a context and a string which represents a prefix
;; RETURNS: a context like the original, except its prefix has been modified to
;; the string provided
;; EXAMPLES:
;; (prefix-with CONTEXT-1 "hi") -> (make-context "hi" ")" 5)
;; STRATEY: STRUCTURAL DECOMPOSITION on context : Context
(define (prefix-with context prefix)
  (make-context prefix (context-suffix context) (context-width context)))

;; TESTS
(define-test-suite prefix-with-tests
  (check-equal?
   (prefix-with CONTEXT-1 "hi")
   (make-context "hi" ")" 5)
   "Testing to make sure prefix within context is modified."))

;; suffix-with : Context String -> Context
;; GIVEN: a context and a string which represents a suffix
;; RETURNS: a context like the original, except its suffix has been modified to
;; the string provided
;; EXAMPLES:
;; (suffix-with CONTEXT-1 "hi2") -> (make-context "(+" "hi2" 5)
;; STRATEY: STRUCTURAL DECOMPOSITION on context : Context
(define (suffix-with context suffix)
  (make-context (context-prefix context) suffix (context-width context)))

;; TESTS
(define-test-suite suffix-with-tests
  (check-equal?
   (suffix-with CONTEXT-1 "hi2")
   (make-context "(+" "hi2" 5)
   "Testing to make sure suffix is modified."))

;; suffix-free : Context -> Context
;; GIVEN: a context
;; RETURNS: a context like the original, except its suffix has been modified
;; to "" (an empty string)
;; EXAMPLES:
;; (suffix-free CONTEXT-1) -> (make-context "(+" "" 5)
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (suffix-free context)
  (suffix-with context EMPTY))

;; TESTS
(define-test-suite suffix-free-tests
  (check-equal?
   (suffix-free CONTEXT-1)
   (make-context "(+" EMPTY 5)
   "Testing to make sure context suffix is empty string."))

;; prefix-append Context String -> Context
;; GIVEN: a context and a string
;; RETURNS: a context like the original, except its prefix is modified.
;; EXAMPLES:
;;
;; STRATEGY: STRUCTURAL DECOMPOSITION on context : Context
(define (prefix-append context addition)
  (prefix-with context (string-append (context-prefix context) addition)))

;; TESTS
(define-test-suite prefix-append-tests
  (check-equal?
   (prefix-append CONTEXT-1 "hi")
   (make-context "(+hi" ")" 5)
   "Testing to make sure prefix is appended."))


;; suffix-append : Context -> Context
;; GIVEN: a context and a string
;; RETURNS: adds another closing paren to the context's suffix
;; EXAMPLES:
;; (suffix-append CONTEXT-1) -> (make-context "(+" "))" 5)
;; STRATEGY: STRUCTURAL DECOMPOSITION on context : Context
(define (suffix-append context)
  (suffix-with context (string-append (context-suffix context) CLOSE-PAR)))

;; TEST
(define-test-suite suffix-append-tests
  (check-equal?
   (suffix-append CONTEXT-1)
   (make-context "(+" "))" 5)
   "Testing to make sure suffix is appended."))


;; indent : Context String -> Context
;; GIVEN: a context and a string representing the header of the expression
;; RETURNS: a string like the original, except with this prefix modified to be
;; the same as that of the header
;; EXAMPLES:
;; (indent CONTEXT-1 "header") -> (make-context "        " ")" 5)
;; STRATEGY: STRUCTURAL DECOMPOSITION on context : Context
(define (indent context header)
  (prefix-with context
               (make-string (+ (string-length header)
                               (string-length (context-prefix context)))
                            #\Space)))

;; TESTS
(define-test-suite context-tests
  (check-equal?
   (indent CONTEXT-1 "header")
   (make-context "        " ")" 5)
   "Testing to make sure prefix is modified"))

;; pprint-one-line-wrapper : Expr Context -> String
;; GIVEN: an expr and a context
;; RETURNS: a string representing the expr, shifted an additional space over
;; EXAMPLES:
;; (pprint-one-line-wrapper SUM-EXP-1 CONTEXT-1) -> "(+ (+ 22 333 44))"
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (pprint-one-line-wrapper expr context)
  (pprint-one-line expr (prefix-append context SPACE)))

;; TESTS
(define-test-suite pprint-one-line-wrapper-tests
  (check-equal?
   (pprint-one-line-wrapper SUM-EXP-1 CONTEXT-1)
   "(+ (+ 22 333 44))"
   "Testing to make sure list is converted to string."))

;; pprint-one-line : Expr Context -> String
;; GIVEN: an expr and a context
;; RETURNS: a string representing the expr, in the context
;; EXAMPLES:
;; (pprint-one-line 234 CONTEXT-1) -> "234"
;; (pprint-one-line SUM-EXP-1 CONTEXT-1) -> "(+(+ 22 333 44))"
(define (pprint-one-line expr context)
  (cond
   [(integer? expr) (pprint-int expr context)]
   [(form? expr) (pprint-form-one-line expr context)]))

;; TESTS
(define-test-suite pprint-one-line-tests
  (check-equal?
   (pprint-one-line SUM-EXP-1 CONTEXT-1)
   "(+(+ 22 333 44))"
   "Testing to make sure list is converted to string."))

;; pprint-form-one-line Expr Context -> String
;; GIVEN: an expr and a context
;; RETURNS: a string epresenting the list of the expr
;; EXAMPLES:
;; (pprint-form-one-line SUM-EXP-1 CONTEXT-1) -> "(+(+ 22 333 44)"
;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
(define (pprint-form-one-line form context)
  (place-in
   (suffix-append (foldl 
                       ;; Expr Context -> Context
                       ;; GIVEN: a expr and a context
                       ;; RETURNS: context for the next expression in line
                       ;; STRATEGY: FUNCTIONAL COMPOSITION
                   (lambda
                       (exp context)
                     (prefix-with context
                                  (pprint-one-line-wrapper 
                                   exp 
                                   (suffix-free context))))
                   (prefix-append context (form-header form))
                   (form-operands form)))
   EMPTY))

;; TESTS
(define-test-suite pprint-form-one-line-tests
  (check-equal?
   (pprint-form-one-line SUM-EXP-1 CONTEXT-1)
   "(+(+ 22 333 44))"
   "Testing to make sure list is converted to string."))

;; pprint-expr-wrapper : Expr Context -> LOS
;; GIVEN: an expr and a context
;; RETURNS: a list of strings
;; EXAMPLES:
;; (pprint-expr-wrapper SUM-EXP-1 CONTEXT-1) -> not enough room
;; (pprint-expr-wrapper SUM-EXP-1 CONTEXT-2) -> (list "(+ (+ 22 333 44)")
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (pprint-expr-wrapper expr context)
  (pprint-expr expr (prefix-append context SPACE)))

;; TESTS
(define-test-suite pprint-expr-wrapper-tests
  (check-equal?
   (pprint-expr-wrapper SUM-EXP-1 CONTEXT-2)
   (list "(+ (+ 22 333 44))")
   "Testing to make sure list of strings is created.")
  (check-error
   (pprint-expr-wrapper SUM-EXP-1 CONTEXT-1)))

;; pprint-multiple : Expr Context -> LOS
;; GIVEN: an expr and a context
;; RETURNS: a list of strings
;; EXAMPLES:
;; (pprint-multiple MULT-EXP-1 CONTEXT-2) ->
;; (list "(+(* 2" "     56" "     3450))")
;; (pprint-multiple MULT-EXP-1 CONTEXT-1) -> not enough room
;;STRATEGY: STRUCTURAL DECOMPOSITION on expr : Expr
(define (pprint-multiple expr context)
  (cond
   [(integer? expr) (error ERROR-MESSAGE)]
   [(form? expr) (pprint-form-multiple expr context)]))

;; TESTS
(define-test-suite pprint-multiple-tests
  (check-equal?
   (pprint-multiple MULT-EXP-1 CONTEXT-2)
   (list "(+(* 2" "     56" "     3450))")
   "Testing to make sure list of strings is created.")
  (check-error
   (pprint-multiple MULT-EXP-1 CONTEXT-1)))

;; pprint-form-multiple : Form Context -> LOS
;; GIVEN: a form and a context
;; RETURNS: a list of strings
;; EXAMPLES:
;; (pprint-form-multiple MULT-EXP-1 CONTEXT-2) ->
;; (list "(+(* 2" "     56" "     3450))")
;; STRATEGY: HIGHER ORDER FUNCTION COMPOSITION
;; NOTE: Assumes that the form has at least two operands
(define (pprint-form-multiple form context)
  (append ;; first element gets special treatment
   (pprint-expr-wrapper (first (form-operands form))
                        (prefix-append (suffix-free context)
                                       (form-header form)))
   (foldr
    ;; Expr LOS -> LOS
    ;; GIVEN: an expr and a list of strings
    ;; RETURNS: a list of strings
    ;; STRATEGY: FUNCTIONAL COMPOSITION
    (lambda (exp base)
      (if (empty? base)        ; last element gets special treatment
          (pprint-expr-wrapper exp
                               (suffix-append
                                (indent context (form-header form))))
          (append
           (pprint-expr-wrapper exp
                                (suffix-free
                                 (indent context (form-header form))))
           base)))
    empty
    (rest (form-operands form)))))

;; TESTS
(define-test-suite pprint-form-multiple-tests
  (check-equal?
   (pprint-form-multiple MULT-EXP-1 CONTEXT-2)
   (list "(+(* 2" "     56" "     3450))")
   "Testing to make sure list of strings is created."))

;; initial-context : Number -> Context
;; GIVEN: a number representing the max width
;; RETURNS: a context with the number within it
;; EXAMPLES:
;; (initial-context 5) -> (make-context "" "" 5)
;; STRATEGY: STRUCTURAL DECOMPOSITION on context : Context
(define (initial-context width)
  (make-context EMPTY EMPTY width))

;; TESTS
(define-test-suite initial-context-tests
  (check-equal?
   (initial-context 5)
   (make-context "" "" 5)
   "Testing to make sure an empty context is created with width 5"))

;; expr-to-strings : Expr Number -> LOS
;; GIVEN: An expression and a width
;; RETURNS: A representation of the expression as a sequence of lines, with
;; each line represented as a string of length not greater than the width.
;; EXAMPLES: (expr-to-strings MULT-EXP-1 100) -> (list "(* 2 56 3450)")
;; STRATEGY: FUNCTIONAL COMPOSITION
(define (expr-to-strings expr width)
  (pprint-expr expr (initial-context width)))

;; TESTS
(define-test-suite expr-to-strings-tests
  (check-equal?
   (expr-to-strings MULT-EXP-1 8)
   (list "(* 2"
         "   56"
         "   3450)")
   "Testing to make sure list of string is created."))


;; TESTS
(run-tests place-in-tests)
(run-tests pprint-int-tests)
(run-tests form-operands-tests)
(run-tests form-header-tests)
(run-tests forms?-tests)
(run-tests expr-width-tests)
(run-tests expr-width-tests)
(run-tests fits-one-line?-tests)
(run-tests pprint-expr-tests)
(run-tests pprint-expr-wrapper-tests)
(run-tests prefix-with-tests)
(run-tests suffix-with-tests)
(run-tests suffix-free-tests)
(run-tests prefix-append-tests)
(run-tests suffix-append-tests)
(run-tests context-tests)
(run-tests pprint-one-line-tests)
(run-tests pprint-one-line-wrapper-tests)
(run-tests pprint-form-one-line-tests)
(run-tests pprint-multiple-tests)
(run-tests pprint-form-multiple-tests)
(run-tests initial-context-tests)
(run-tests expr-to-strings-tests)
(run-tests form-width-tests)
