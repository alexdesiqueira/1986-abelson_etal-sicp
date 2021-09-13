#lang sicp

; Exercise 1.18: Using the results of Exercise 1.16 and
; Exercise 1.17, devise a procedure that generates an iterative
; process for multiplying two integers in terms of adding,
; doubling, and halving and uses a logarithmic number of steps.
; (This algorithm, which is sometimes known as the "Russian
; peasant method" of multiplication, is ancient. Examples of
; its use are found in the Rhind Papyrus, one of the two oldest
; mathematical documents in existence, written about 1700 B.C.
; (and copied from an even older document) by an Egyptian scribe
; named A’h-mose.)

(define (mult-peasant a b)
  (define (double x)
    (* x 2))
  (define (halve x)
    (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))

  (define (mult-iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (mult-iter acc (double a) (halve b)))
          (else (mult-iter (+ acc a) a (- b 1)))))
   (mult-iter 0 a b))

;; As an example, let's multiply 3 and 5. So, a = 3, b = 5, acc = 0. b starts as odd:
;;
;; (mult-iter 3 3 4)  ; now acc = 3 (+ acc a), a = 3, b = 4 (- b 1); b even
;; (mult-iter 3 6 2)  ; now acc = 3, a = 6 (double a), b = 2 (halve b); b even
;; (mult-iter 3 12 1)  ; now acc = 3, a = 12 (double a), b = 1 (halve b); b odd
;; (mult-iter 15 12 0)  ; now acc = 15 (+ acc a), a = 12, b = 0 (- b 1). STOP
;; 15
;;
;; Now, let's make 3 * 10. So, a = 3, b = 10, acc = 0. b starts as even:
;;
;; (mult-iter 0 6 5)  ; now acc = 0, a = 6 (double a), b = 5 (halve b); b odd
;; (mult-iter 6 6 4)  ; now acc = 6 (+ acc a), a = 6, b = 4 (- b 1); b even
;; (mult-iter 6 12 2)  ; now acc = 6, a = 12 (double a), b = 2 (halve b); b even
;; (mult-iter 6 24 1)  ; now acc = 6, a = 24 (double a), b = 1 (halve b); b odd
;; (mult-iter 30 24 0)  ; now acc = 30 (+ acc a), a = 24, b = 0 (- b 1). STOP
;; 30


;; Tests

(mult-peasant 3 5)