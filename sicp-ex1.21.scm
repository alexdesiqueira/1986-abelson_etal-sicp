#lang sicp

; Exercise 1.21: Use the smallest-divisor procedure to find
; the smallest divisor of each of the following numbers: 199,
; 1999, 19999.

;; The smallest-divisor procedure defined is:

(define (smallest-divisor n)
  (find-divisor n 2))

;; and the helper functions are:

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

;; Tests

(smallest-divisor 2)  ;; should be 2
(smallest-divisor 9)  ;; should be 3

;; Alright. Now, calculating the smallest divisor for the
;; numbers asked on the exercise:

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; There you go: the results are 199, 1999, and 7.