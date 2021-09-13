#lang sicp

; Exercise 1.12: The following pattern of numbers is called
; Pascal’s triangle.
;
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;    ...
; The numbers at the edge of the triangle are all 1, and each
; number inside the triangle is the sum of the two numbers
; above it. Write a procedure that computes elements of
; Pascal’s triangle by means of a recursive process.

(define (pascal i j)
  (cond ((= j 0) 1)
        ((= i j) 1)
        ((not (= i j))
         (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j)))))

;; Tests
(pascal 1 0)  ; should be 1
(pascal 3 3)  ; should be 1
(pascal 2 1)  ; should be 2
(pascal 4 2)  ; should be 6
(pascal 4 3)  ; should be 4
