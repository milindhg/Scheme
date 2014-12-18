#lang racket

;;Factorial trial based on the parentheC/PC tutorial
;File 1

;Standard recursive definition of factorial

(define fact_cps
  (lambda (n k)
    (cond
      [(zero? n) (k 1)]
      [else (fact_cps (sub1 n) (lambda (v) (k (* n v))))])))


(define fact
  (lambda (n)
    (fact_cps n (lambda (v) v))))

(define main
  (lambda ()
    (fact 5)))

(main)

