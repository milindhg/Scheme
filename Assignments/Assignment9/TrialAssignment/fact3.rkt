#lang racket

;;Factorial trial based on the parentheC/PC tutorial
;File 1

;Standard recursive definition of factorial

(define apply_k
  (lambda (k^ v)
    (k^ v)))


(define kt_empty_k
  (lambda ()
    (lambda (v) v)))

(define kt_extend
  (lambda (n k)
    (lambda (v)
    (apply_k k (* n v)))))

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

