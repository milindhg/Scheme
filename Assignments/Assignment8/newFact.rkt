#lang racket
(require C311/trace)
(require C311/pmatch)

(define v 'kahi)
(define k 'nn)
(define n 'nn)

(define empty-k-reg
 (lambda ()
  (list 'empty-k-reg)))

(define fact-reg
 (lambda ()
   ((lambda (mn)
     (mn mn))
    (lambda (aa)
     (cond
      [(zero? n) 
       (begin 
         (set! v 1)
         (apply-k-reg))]
      [else 
       (begin 
         (set! k (inner-k-reg-fact n k))
         (set! n (sub1 n)) 
         (aa aa))]))
    )))

(define inner-k-reg-fact
 (lambda (n^ k^)
  (list 'inner-k-reg-fact n^ k^)))


(define apply-k-reg
 (lambda ()
  (pmatch k
   [`(empty-k-reg) v]
   [`(inner-k-reg-fact ,n^ ,k^) 
    (begin 
      (set! k k^)
      (set! v (* n^ v)) 
      (apply-k-reg))])))


(define fact-reg-driver
  (lambda (n^)
    (begin
      (set! n n^)
      (set! k (empty-k-reg)) 
    (fact-reg))))

(fact-reg-driver 5)