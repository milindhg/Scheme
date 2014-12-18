#lang racket
(require C311/pmatch)
(require C311/trace)

(define empty-k-reg
 (lambda ()
  (list 'empty-k-reg)))

(define v* 'anything)
(define m* 'anything)
(define a* 'anything)
(define k* 'anything)
(define n* 'anything)


(define apply-k-reg
 (lambda () ;(k v)
  (pmatch k*
   [`(empty-k-reg) v*]
   [`(inner-k-reg-pascal ,a^ ,k^)
    (begin
      (set! k* k^)
      (set! v* (cons a^ v*))
      (apply-k-reg))]
   [`(outer-k-reg-pascal ,m^ ,a^ ,k^) 
    (begin 
      (set! v* v*)
      (set! m* (add1 m^))
      (set! a* a^)
      (set! k* (inner-k-reg-pascal a^ k^))
      (v*))]
   [`(inner-k-reg-single-pascal ,k^)
    (begin 
      (set! v* v*)
      (set! m* 1)
      (set! a* 0)
      (set! k* k^) 
      (v*))])))

(define inner-k-reg-pascal
 (lambda (a^ k^)
  (list 'inner-k-reg-pascal a^ k^)))

(define outer-k-reg-pascal
 (lambda (m^ a^ k^)
  (list 'outer-k-reg-pascal m^ a^ k^)))

(define inner-k-reg-single-pascal
 (lambda (k^)
  (list 'inner-k-reg-single-pascal k^)))


(define pascal-reg
  (lambda () ;(n k)
    (let ([pascal-reg
           (lambda (pascal-reg)
             (begin
               (set! k* k*)
               (set! v* (lambda () ;(m a k)
                          (cond
                            [(> m* n*)
                             (begin 
                               (set! v* '()) 
                               (apply-k-reg))]
                            [else  
                             (begin 
                               (set! a* (+ a* m*))
                               (set! k* (outer-k-reg-pascal m* a* k*))
                               (pascal-reg pascal-reg))])))
               (apply-k-reg)))])
      (begin 
        (set! k* k*)
        (set! k* (inner-k-reg-single-pascal k*))
        (pascal-reg pascal-reg)))))

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! n* n)
      (set! k* (empty-k-reg))
      (pascal-reg))))

(pascal-reg-driver 10)