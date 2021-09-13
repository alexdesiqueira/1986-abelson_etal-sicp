#lang sicp

; Exercise 1.17: The exponentiation algorithms in this
; section are based on performing exponentiation by means of
; repeated multiplication. In a similar way, one can perform
; integer multiplication by means of repeated addition. The
; following multiplication procedure (in which it is assumed
; that our language can only add, not multiply) is analogous
; to the expt procedure:
; (define (* a b)
;    (if (= b 0)
;        0
;        (+ a (* a (- b 1)))))
; This algorithm takes a number of steps that is linear in b .
; Now suppose we include, together with addition,
; operations double, which doubles an integer, and halve,
; which divides an (even) integer by 2. Using these, design
; a multiplication procedure analogous to fast-expt that uses a
; logarithmic number of steps.

;; The linear one:

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(mult 3 5)

;; The basis of the fast-expt procedure is:
;; b^n = (b^(n/2))^2 for n even, b^n = b * b^(n-1) for n odd.
;; Adapting the process, we'd have:

(define (mult-iter a b)
  (define (double x)
    (* x 2))
  (define (halve x)
    (/ x 2))
  (define (even? x)
    (= (remainder x 2) 0))
  
  (cond ((= b 0) 0)
        ((even? b) (double (mult-iter a (halve b))))
        (else (+ a (mult-iter a (- b 1))))))

;; Tests

(mult-iter 3 5)  ;; should be 15

;; Mistakes were made ;)
;;
;; In the first version, I didn't check if b was even!
