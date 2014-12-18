#lang racket
(require "parenthec.rkt")

;;Factorial trial based on the parentheC/PC tutorial
;File 1

;Standard recursive definition of factorial

(define-union kt
  (empty_k)
  (extend n k))

(define apply_k
  (lambda (k^ v)
    (union-case k^ kt
                [(empty_k) v]
                [(extend n k) (apply_k k (* n v))])))

(define fact_cps
  (lambda (n k)
    (cond
      [(zero? n) (apply_k k 1)]
      [else (fact_cps (sub1 n) (kt_extend n k))])))

(define fact
  (lambda (n)
    (fact_cps n (kt_empty_k))))

(define main
  (lambda ()
    (fact 5)))

(main)

