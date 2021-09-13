#lang sicp

; Exercise 1.7: The good-enough? test used in computing
; square roots will not be very effective for finding the square
; roots of very small numbers. Also, in real computers,
; arithmetic operations are almost always performed with
; limited precision. This makes our test inadequate for very large
; numbers. Explain these statements, with examples showing
; how the test fails for small and large numbers. An alternative
; strategy for implementing good-enough? is to watch
; how guess changes from one iteration to the next and to
; stop when the change is a very small fraction of the guess.
; Design a square-root procedure that uses this kind of end
; test. Does this work better for small and large numbers?

;; Let's check how the current sqrt-iter works for small and large
;; numbers. First, defining the support procedures as in the book:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

;; defining sqrt as sqrt-iter with a starting guess
;; equal to 1.0.
(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Tests
(sqrt 4)

(sqrt 81)

(sqrt 144)

;; Now, let's try small numbers.

(sqrt 0.000005)  ;; should be close to 0.00224

;; result: 0.031303263143156616

;; The tolerance defined at good-enough? (0.001) is significantly large
;; when compared to the input value.

;; Now let's try large numbers.

(sqrt 500000000000000)

;; this runs fine. Result: 22360679.774997897

;; (sqrt 5000000000000000)

;; (sqrt 1000000000000000)

;; These will run forever :) At some point, (improve guess x) keeps returning
;; the same value, but good-enough? will keep on returning #f.

;; Now, let's try to write an alternative to good-enough?. The exercise says
;; "stop when the change is a very small fraction of the guess." We could use
;; (improve guess x) to check if the difference of the guess and the next
;; guess is within a certain tolerance:

(define (good-enough-2? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))

;; therefore, if abs(-...) > 0.001, the guess is good enough and the
;; procedure continues. Let's define sqrt and sqrt-iter again:

(define (sqrt-iter-2 guess x)
  (if (good-enough-2? guess x)
      guess
      (sqrt-iter-2 (improve guess x) x)))

(define (sqrt-2 x)
  (sqrt-iter-2 1.0 x))

;; Tests

(sqrt-2 0.000005)  ;; should be close to 0.00224

;; result: 0.0022365388240630493

;; The tolerance defined at good-enough? (0.001) is significantly large
;; to be a threshold, when compared to the input value.

;; Now let's try large numbers.

(sqrt-2 500000000000000)

;; this runs fine. Result: 22360679.774997897

(sqrt-2 5000000000000000)  ;; should be close to 70710678

;; result: 70710678.11866389

(sqrt-2 1000000000000000)  ;; should be close to 31622776

;; result: 31622776.601683907
