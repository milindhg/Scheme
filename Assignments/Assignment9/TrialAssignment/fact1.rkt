#lang racket

;;Factorial trial based on the parentheC/PC tutorial
;File 1

;Standard recursive definition of factorial

(define fact
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (fact (sub1 n)))))))

(define main
  (lambda ()
    (fact 5)))

(main)

