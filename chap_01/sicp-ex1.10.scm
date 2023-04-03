#lang sicp

; Exercise 1.10: The following procedure computes a
; mathematical function called Ackermann’s function.
;
;    (define (A x y)
;      (cond ((= y 0) 0)
;            ((= x 0) (* 2 y))
;            ((= y 1) 2)
;            (else (A (- x 1) (A x (- y 1))))))
; What are the values of the following expressions?
;
; (A 1 10)
; (A 2 4)
; (A 3 3)
;
; Consider the following procedures, where A is the
; procedure defined above:
;
; (define (f n) (A 0 n))
; (define (g n) (A 1 n))
; (define (h n) (A 2 n))
; (define (k n) (* 5 n n))
;
; Give concise mathematical definitions for the functions
; computed by the procedures f, g, and h for positive integer
; values of n. For example, (k n) computes $5 \times n^2$.

;; First part:
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)  ;; result: 1024
(A 2 4)  ;; result: 65536
(A 3 3)  ;; result: 65536

;; On the second part,
;;
;; 1. (define (f n) (A 0 n))  ; x = 0; y = n
;; (f n) calculates $2 \times n$.
;;
;; 2. (define (g n) (A 1 n))  ; x = 1; y = n
;; * y = 1: (A 1 1) -> 2
;; * y = 2: (A 1 2) -> 4
;; * y = 3: (A 1 3) -> 8
;; * y = 4: (A 1 4) -> 16
;; ...
;; (g n) calculates $2^n$, for $n > 0$.
;;
;; 3. (define (h n) (A 2 n))  ; x = 2; y = n
;; * y = 1: (A 2 1) -> 2
;; * y = 2: (A 2 2) -> 4
;; * y = 3: (A 2 3) -> 16
;; * y = 4: (A 2 4) -> 655536
;; * y = 4: (A 2 5) -> 200352993040684646497907235156025575044...  ; more than 19000 digits!
;; It blows up really quickly!
;; We can write the terms this way:
;; (h 1) = 2;
;; (h 2) = 4 = 2^(2);
;; (h 3) = 16 = 2^(2^(2));
;; (h 4) = 655536 = 2^(2^(2^(2)));
;; ...
;; This way, (h n) calculates $2^2^2^2...^n, for $n > 0$.

