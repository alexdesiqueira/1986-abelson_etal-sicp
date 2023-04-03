#lang sicp

; Exercise 1.4: Observe that our model of evaluation allows
; for combinations whose operators are compound expressions.
; Use this observation to describe the behavior of the
; following procedure:
; (define (a-plus-abs-b a b)
;   ((if (> b 0) + -) a b))

;; In this case, the (if) special form returns the procedure (+)
;; when (> b 0), and the procedure (-) when (or (= b 0) (< b 0)).
;; Then, the resulting procedure will process a and b. 
;; Examples:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 4 -3)

;; Here (> -3 0) returns #f, (if (> -3 0) + -) returns (-), and
;; the result is (- 4 -3): 7.

(a-plus-abs-b 4 3)

;; Now, (> 3 0) returns #t, (if (> 3 0) + -) returns (+), and
;; the result is (+ 4 3): 7.
