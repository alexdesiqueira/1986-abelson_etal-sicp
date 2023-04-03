#lang sicp

; Exercise 1.3: Define a procedure that takes three numbers
; as arguments and returns the sum of the squares of the two
; larger numbers.

;; Alex's thoughts: alright, I had to think a little bit on
;; this one. First I defined (square x). Then, I wrote the
;; possibilities:
;; * for a: (> a b), (> a c)
;; * for b: (> b a), (> b c)
;; * for c: (> c a), (> c b)
;; It took a while to figure the right conditional to group
;; them correctly :)
;; Update: defined (sum-of-two-squares x y), just for fun.

(define (square-of-two-larger a b c)
  (cond ((and (> a b) (> b c)) (sum-of-two-squares a b))
        ((and (> c b) (> b a)) (sum-of-two-squares b c))
        (else (sum-of-two-squares a c))))

(define (square x)
  (* x x))

(define (sum-of-two-squares x y)
  (+ (square x) (square y)))

;; tests:
(square-of-two-larger 3 5 5)  ;;  should be 34 (25 + 9)

(square-of-two-larger -1 0 1)  ;; should be 1

(square-of-two-larger -15 -4 3)  ;; should be 25 (16 + 9)

;; Mistakes were made ;)
;;
;; First version had ((> a b) and (> b c)) instead of
;; (and (> a b) (> b c)). It takes a while to get along
;; with the prefix notation :)