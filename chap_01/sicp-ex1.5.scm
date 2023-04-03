#lang sicp

; Exercise 1.5: Ben Bitdiddle has invented a test to determine
; whether the interpreter he is faced with is using applicative-
; order evaluation or normal-order evaluation. He defines the
; following two procedures:
; (define (p) (p))
; (define (test x y)
;   (if (= x 0) 0 y))
; Then he evaluates the expression
; (test 0 (p))
; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior will he observe
; with an interpreter that uses normal-order evaluation? Explain
; your answer. (Assume that the evaluation rule for the special
; form if is the same whether the interpreter is using normal or
; applicative order: The predicate expression is evaluated first,
; and the result determines whether to evaluate the consequent
; or the alternative expression.)

;; When the interpreter uses applicative-order evaluation, since
;; the procedure will evaluate the subexpressions first, Ben's
;; test will pass (p) to itself, infinitely: 
;;
;; (test 0 (p)) --> calculates (p), which returns (p)
;; (test 0 (p)) --> calculates (p), which returns... (p)
;; (test 0 (p)) --> ...
;;
;; In normal-order evaluation, since the first expression contains
;; a primitive, it will be evaluated first, and the expression
;; evaluates to zero:
;;
;; (test 0 (p))       --> passes 0 and (p) to test
;; (if (= 0 0) 0 (p)) --> calculates (= 0 0), due to the primitive "="
;; (if #t 0 (p))      --> since (= 0 0): #t, returns 0
