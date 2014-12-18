#lang racket
(require "parenthec.rkt")

;;Factorial trial based on the parentheC/PC tutorial
;File 1

;Standard recursive definition of factorial

(define-registers n k k^ v)
(define-program-counter pc)


(define-union kt
  (empty_k)
  (extend n k))

(define apply_k
  (lambda () ;(k^ v)
    (union-case k^ kt
                [(empty_k) v]
                [(extend n k) 
                 ;(apply_k k (* n v))])))
                 (begin
                   (set! v (* n v))
                   (set! k^ k)
                   (apply_k))])))

(define fact_cps
  (lambda () ;(n k)
    (cond
      [(zero? n) 
       ;(apply_k k 1)]
       (begin
         (set! k^ k)
         (set! v 1)
         (apply_k))]
      [else 
       ;(fact_cps (sub1 n) (kt_extend n k))])))
       (begin
         (set! k (kt_extend n k))
         (set! n (sub1 n))
         (fact_cps))])))

(define fact
  (lambda () ;(n)
    ;(fact_cps n (kt_empty_k))))
    (begin
      (set! n n)
      (set! k (kt_empty_k))
      (fact_cps))))

(define main
  (lambda ()
    ;(fact 5)))
    (begin
      (set! n 5)
      (fact))))

(main)

