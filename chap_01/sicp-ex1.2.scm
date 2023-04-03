#lang sicp

; Exercise 1.2: Translate the following expression into prefix
; form:
;
;            5 + 4 + (2 - (3 - (6 + (4/5)))
;            ------------------------------
;                      3(6-2)(2-7)

;; Alex's thoughts: I started from the smaller expressions
;; according to the parentheses, before getting the larger
;; one together:

(/ (+ 4 5 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; The interpreter returns a fraction. To return a float, use:

(/ (+ 4 5 (- 2 (- 3 (+ 6 (/ 4. 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Mistakes were made ;)
;;
;; In my first version, I put the procedure (-) together with
;; the number, as in:
;; (-2 (- 3 (+ 6 (/ 4 5))))
;; Try it! :)