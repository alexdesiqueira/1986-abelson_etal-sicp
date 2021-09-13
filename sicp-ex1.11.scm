#lang sicp

; Exercise 1.11: A function f is defined by the rule that
; f(n) = n if n < 3, f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3.
; Write a procedure that computes f by means of a recursive
; process. Write a procedure that computes f by means of an
; iterative process.

;; recursive process is kinda ready:

(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

;; Testing

(f-recursive 2)
(f-recursive 5)
(f-recursive 8)

;; iterative process depends on three variables:
;;   a <- (a + 2b + 3c); b <- a; c <- b
;; starting condition is defined by (f 3):
;;   a = 2; b = 1; c = 0
(define (f-iterative n)
  (define (iter a b c count)
    (cond ((< n 3) n)
          ((= count 0) a)
          (else (iter (+ a (* 2 b) (* 3 c)) a b
                      (- count 1)))))
  (iter 2 1 0 (- n 2)))

;; Testing

(f-iterative 2)
(f-iterative 5)
(f-iterative 8)
(f-iterative 500)

;; Mistakes were made ;)
;;
;; Instead of writing (if (< n 3) ...), I did
;; (if (< 3 n) ...) first :)