#lang sicp

; Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to
; be provided as a special form. “Why can’t I just define it as
; an ordinary procedure in terms of cond ?” she asks. Alyssa’s
; friend Eva Lu Ator claims this can indeed be done, and she
; defines a new version of if :
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))
; Eva demonstrates the program for Alyssa:
; (new-if (= 2 3) 0 5)
; 5
; (new-if (= 1 1) 0 5)
; 0
; Delighted, Alyssa uses new-if to rewrite the square-root
; program:
; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x) x)))
; What happens when Alyssa attempts to use this to compute
; square roots? Explain.


;; Alright, support functions first (all defined in the book):

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; Then, new-if:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Finally, sqrt-iter:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; When you run sqrt-iter, it runs endlessly, causing a memory
;; error! Since Scheme's if is a special form, it only evaluates
;; one of its parameters at a time, not both. new-if is not a
;; special form, and operate in applicative order. This way,
;; new-if will run sqrt-iter again and again.
