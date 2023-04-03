#lang sicp

; Exercise 1.20: The process that a procedure generates is
; of course dependent on the rules used by the interpreter.
; As an example, consider the iterative gcd procedure given
; above. Suppose we were to interpret this procedure using
; normal-order evaluation, as discussed in Section 1.1.5. (the
; normal-order-evaluation rule for if is described in Exercise
; 1.5.) Using the substitution method (for normal order),
; illustrate the process generated in evaluating (gcd 206 40)
; and indicate the remainder operations that are actually
; performed. How many remainder operations are actually
; performed in the normal-order evaluation of (gcd 206 40)?
; In the applicative-order evaluation?

;; In the normal-order evaluation:
;;
;; (gcd 206 40)  ; a = 206, b = 40
;; (if (= 40 0) ...)  ; #f
;; (gcd 40 (remainder 206 40))  ; not calculated here :)
;; (if (= (remainder 206 40) 0) ...)  ; 1x
;; (if (= 6 0) ...)  ; #f
;; (gcd (remainder 206 40)  ; a
;;      (remainder 40 (remainder 206 40)))  ; b
;; (if (= (remainder 40 (remainder 206 40)) 0) ...)  ; 2x
;; (if (= (remainder 40 6) 0) ...)  ; 3x
;; (if (= 4 0) ...) ; #f
;; (gcd (remainder 40 (remainder 206 40))  ; a
;;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))   ; b
;; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ...)  ; 4x
;; (if (= (remainder (remainder 206 40) (remainder 40 6)) 0) ...)  ; 5x
;; (if (= (remainder (remainder 206 40) 4) 0) ...)  ; 6x
;; (if (= (remainder 6 4) 0) ...)  ; 7x
;; (if (= 2 0) ...)  ; #f
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))  ; a
;;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))  ; b
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ...)  ; 8x
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 6))) 0) ...)  ; 9x
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) 4)) 0) ...)  ; 10x
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder 6 4)) 0) ...)  ; 11x
;; (if (= (remainder (remainder 40 (remainder 206 40)) 2) 0) ...)  ; 12x
;; (if (= (remainder (remainder 40 6) 2) 0) ...)  ; 13x
;; (if (= (remainder 4 2) 0) ...)  ; 14x
;; (if (= 0 0) ...)  ; #t; return a
;; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))  ; 15x
;; (remainder (remainder 206 40) (remainder 40 6))  ; 16x
;; (remainder (remainder 206 40) 4)  ; 17x
;; (remainder 6 4)  ; 18x
;; 2
;;
;; (remainder) is called eighteen times when we use normal-order evaluation.
;;
;; In the applicative-order evaluation, we have:
;;
;; (gcd 206 40)  ; a = 206, b = 40
;; (if (= 40 0) ...)  ; #f
;; (gcd 206 (remainder 206 40))  ; 1x
;; (gcd 40 6)  ; a = 40, b = 6
;; (if (= 6 0) ...)  ; #f
;; (gcd 6 (remainder 40 6))  ; 2x
;; (gcd 6 4)  ; a = 6, b = 4
;; (if (= 4 0) ...)  ; #f
;; (gcd 4 (remainder 6 4)) ;; 3x
;; (gcd 4 2)  ; a = 4, b = 2
;; (if (= 2 0) ...)  ; #f
;; (gcd 2 (remainder 4 2))  ;; 4x
;; (gcd 2 0)  ; a = 2, b = 0
;; (if (= 0 0) ...)  ; #t; returns a
;; 2
;;
;; (remainder) is called four times when we use applicative-order evaluation.