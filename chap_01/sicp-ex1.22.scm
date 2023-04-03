#lang sicp

; Exercise 1.22: Most Lisp implementations include a
; primitive called runtime that returns an integer that specifies
; the amount of time the system has been running (measured,
; for example, in microseconds). The following timed-prime-test
; procedure, when called with an integer n, prints n and checks
; to see if n is prime. If n is prime, the procedure
; prints three asterisks followed by the amount of time used
; in performing the test.
;
; (define (timed-prime-test n)
;   (newline)
;   (display n)
;   (start-prime-test n (runtime)))
; (define (start-prime-test n start-time)
;   (if (prime? n)
;       (report-prime (- (runtime) start-time))))
; (define (report-prime elapsed-time)
;   (display " *** ")
;   (display elapsed-time))
;
; Using this procedure, write a procedure search-for-primes
; that checks the primality of consecutive odd integers in a
; specified range. Use your procedure to find the three smallest
; primes larger than 1000; larger than 10,000; larger than
; 100,000; larger than 1,000,000. Note the time needed to test
; each prime. Since the testing algorithm has order of growth
; of Θ(√n), you should expect that testing for primes around
; 10,000 should take about √10 times as long as testing for
; primes around 1000. Do your timing data bear this out?
; How well do the data for 100,000 and 1,000,000 support the
; Θ(√n) prediction? Is your result compatible with the notion
; that programs on your machine run in time proportional to
; the number of steps required for the computation?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; the other procedures are defined in the book:

(define (prime? n)
  (= n (smallest-divisor n)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (square n) (* n n))

;; now, defining search-for-primes:

(define (search-for-primes begin end)
  (timed-prime-test begin)
  (if (< begin end)
      (search-for-primes (+ begin 2) end)
      (newline)))

;; I'm taking lots of freedoms here; the main one is that
;; I assume that begin and end are odd numbers. I could
;; write a procedure to check if begin is odd, before starting
;; to work.

;; Tests:

(search-for-primes 1 11) ;; should return all of them :)

;; now, the ones asked on the exercise:

(search-for-primes 1001 1021)  ;; three smaller: 1009, 1013, 1019
                               ;; time:             4,    4,    3
(search-for-primes 10001 10039)  ;; three smaller: 10007, 10009, 10037
                                 ;; time:             11,    12,    17
(search-for-primes 100001 100045)  ;; three smaller: 100003, 100019, 100043
                                   ;; time:              41,     41,     41
(search-for-primes 1000001 1000039)  ;; three smaller: 1000003, 1000033, 1000037
                                     ;; time:              122,     134,     117

;; now, answering the other questions:

; Since the testing algorithm has order of growth
; of Θ(√n), you should expect that testing for primes around
; 10,000 should take about √10 times as long as testing for
; primes around 1000. Do your timing data bear this out?

;; From 1000 to 10000:
;; * mean time to calculate for 1000's:
;; (4 + 4 + 3) / 3 * sqrt(10) ~= 12
;; * mean time to calculate for 10000's: 13
;; I think so :) approximation looks good enough.

; How well do the data for 100,000 and 1,000,000 support the
; Θ(√n) prediction? Is your result compatible with the notion
; that programs on your machine run in time proportional to
; the number of steps required for the computation?

;; From 100000 to 1000000:
;; * mean time to calculate for 100000's:
;; (41 + 41 + 41) / 3 * sqrt(10) ~= 130
;; * mean time to calculate for 1000000's: (122 + 134 + 117) / 3 ~= 124
;; Yes, I'd say the same: approximation looks good enough.
;; The result looks compatible.