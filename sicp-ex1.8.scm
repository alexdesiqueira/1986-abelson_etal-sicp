#lang sicp

; Exercise 1.8: Newton’s method for cube roots is based on
; the fact that if y is an approximation to the cube root of x,
; then a better approximation is given by the value
;                    ((x/y^2) + 2y) / 3
; Use this formula to implement a cube-root procedure analogous
; to the square-root procedure. (In Section 1.3.4 we will
; see how to implement Newton’s method in general as an
; abstraction of these square-root and cube-root procedures.)

(define (curt-iter guess x)
  (if (good-enough? guess x)
      guess
      (curt-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))

(define (square x)
  (* x x))

;; defining curt as curt-iter with a starting guess
;; equal to 1.0.
(define (curt x)
  (curt-iter 1.0 x))

;; Tests

(curt -8)  ;; exact result: -2

(curt 27)  ;; exact result: 3

(curt 64)  ;; exact result: 4

(curt -125)  ;; exact result: -5

;; Mistakes were made ;)
;;
;; At the first attempt, I tried to divide the first and 
;; second terms of the equation as:
;; ((x/y^2) / 3) + (2y / 3)
