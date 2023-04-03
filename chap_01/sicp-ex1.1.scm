#lang sicp

; Exercise 1.1: Below is a sequence of expressions. What is
; the result printed by the interpreter in response to each
; expression? Assume that the sequence is to be evaluated in
; the order in which it is presented.

;; Alex's thoughts: I thought on what the results would be
;; before typing them on the interpreter.

10  ;; returns 10

(+ 5 3 4)  ;; returns 12

(- 9 1)  ;; returns 8

(/ 6 2)  ;; returns 3

(+ (* 2 4) (- 4 6))  ;; returns 6

(define a 3)  ;; thought it'd return 'a; returns nothing on DrRacket!

(define b (+ a 1))   ;; returns nothing

(+ a b (* a b))  ;; returns 19 (+ 3 4 12)

(= a b)  ;; returns #f (false)

(if (and (> b a) (< b (* a b)))  ;; (> 4 3): #t  (< 4 (* 3 4)): #t
      b                          ;; (and (> 4 3) (< 4 (* 3 4)): #t
      a)                         ;; if returns 4

(cond ((= a 4) 6)          ;; (= a 4): #f
      ((= b 4) (+ 6 7 a))  ;; (= b 4): #t; cond returns (+ 6 7 a)
      (else 25))

(+ 2 (if (> b a) b a))  ;; returns 6. (> b a): #t; if returns b

(* (cond ((> a b) a)  ;; (> 3 4): #f
         ((< a b) b)  ;; (< 3 4): #t
         (else -1))   ;; cond returns 4
   (+ a 1))  ;; returns 16: (* 4 (+ 3 1))
