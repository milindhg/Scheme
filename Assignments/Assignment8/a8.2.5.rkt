#lang racket
(require C311/pmatch)
(require C311/trace)

(define n* 'kahii)
(define v* 'kahii)
(define k* 'kahii)
(define n1* 'kahitari)
(define n2* 'kahitari)
(define m* 'kahipan)
(define a* 'anotherone)
(define ls* 'kahii)

(define empty-k-reg
 (lambda ()
  (list 'empty-k-reg)))

	  
;-----------------------------------------------------------------------------------------------------------------------------

(define inner-k-ack-reg
 (lambda (m^ k^)
  (list 'inner-k-ack-reg m^ k^)))

(define apply-k-ack-reg
 (lambda ()
  (pmatch k*
   [`(empty-k-reg) v*]
   [`(inner-k-ack-reg ,m^ ,k^) 
    (begin
     (set! n1* (sub1 m^))
     (set! k* k^)
     (set! n2* v*)
     (ack-reg))])))

(define ack-reg
 (lambda ()
  (cond
   [(zero? n1*) 
    (begin
     (set! v* (add1 n2*))
    (apply-k-ack-reg))]
   [(zero? n2*) 
    (begin
     (set! n1* (sub1 n1*))
     (set! n2* 1)
    (ack-reg))]
   [else 
    (begin 
     (set! k* (inner-k-ack-reg n1* k*))
     (set! n2* (sub1 n2*))
    (ack-reg))])))
       
(define ack-reg-driver
 (lambda (m n)
  (begin
   (set! n1* m)
   (set! n2* n)
   (set! k* (empty-k-reg))
  (ack-reg))))

;--------------------------------------------------------------------

(define inner-k-depth-reg
 (lambda (l^ k^)
  (list 'inner-k-depth-reg l^ k^)))

(define outer-k-depth-reg
 (lambda (ls^ k^)
  (list 'outer-k-depth-reg ls^ k^)))

(define apply-k-depth-reg
 (lambda ()
  (pmatch k*
   [`(empty-k-reg) v*]
   [`(inner-k-depth-reg ,l^ ,k^) 
    (begin
     (set! n* (add1 l^))
     (set! k* k^)
     (if (< n* v*)
      (set! v* v*)
      (set! v* n*))
    (apply-k-depth-reg))]      
   [`(outer-k-depth-reg ,ls^ ,k^) 
    (begin
     (set! ls* (cdr ls^))
     (set! k* (inner-k-depth-reg v* k^))
    (depth-reg))])))

(define depth-reg
 (lambda ()
  (cond
   [(null? ls*) 
    (begin
     (set! v* 1)
    (apply-k-depth-reg))]
   [(pair? (car ls*))
    (begin
     (set! k* (outer-k-depth-reg ls* k*))
     (set! ls* (car ls*))
    (depth-reg))]
   [else 
    (begin
     (set! ls* (cdr ls*))
    (depth-reg))])))


(define depth-reg-driver
 (lambda (ls)
  (begin 
   (set! ls* ls)
   (set! k* (empty-k-reg))
  (depth-reg))))

;--------------------------------------------------------------------

(define inner-k-fact-reg
 (lambda (n^ k^)
  (list 'inner-k-fact-reg n^ k^)))

(define apply-k-fact-reg
 (lambda ()
  (pmatch k*
   [`(empty-k-reg) v*]
   [`(inner-k-fact-reg ,n^ ,k^)
    (begin
     (set! k* k^)
     (set! v* (* n^ v*))
    (apply-k-fact-reg))])))
	  

(define fact-reg
 (lambda ()
  ((lambda (fact-reg)
    (fact-reg fact-reg))
   (lambda (fact-reg)
    (cond
     [(zero? n*) 
      (begin 
       (set! v* 1)
      (apply-k-fact-reg))]
     [else 
      (begin 
       (set! k* (inner-k-fact-reg n* k*))
       (set! n* (sub1 n*))
      (fact-reg fact-reg))])))))

(define fact-reg-driver 
 (lambda (n)
  (begin
   (set! n* n)
   (set! k* (empty-k-reg))
  (fact-reg))))

;--------------------------------------------------------------------

(define inner-k-pascal-reg
 (lambda (a^ k^)
  (list 'inner-k-pascal-reg a^ k^)))

(define outer-k-pascal-reg
 (lambda (m^ a^ k^)
  (list 'outer-k-pascal-reg m^ a^ k^)))

(define inner-k-single-pascal-reg
 (lambda (k^)
  (list 'inner-k-single-pascal-reg k^)))

(define apply-k-pascal-reg
 (lambda ()
  (pmatch k*
   [`(empty-k-reg) v*]
   [`(inner-k-pascal-reg ,a^ ,k^) 
    (begin
     (set! k* k^)
     (set! a* a^)
     (set! v* (cons a* v*))
    (apply-k-pascal-reg))]
   [`(outer-k-pascal-reg ,m^ ,a^ ,k^) 
    (begin
     (set! m* (add1 m^))
     (set! a* a^)
     (set! k* (inner-k-pascal-reg a* k^))
    (v*))]
   [`(inner-k-single-pascal-reg ,k^) 
    (begin
     (set! m* 1)
     (set! a* 0)
     (set! k* k^)
    (v*))])))


(define pascal-reg
 (lambda ()
  (let ([pascal-reg
   (lambda (pascal-reg)
    (begin
     (set! v* (lambda ()
      (cond
       [(> m* n*) 
        (begin
         (set! v* '())
        (apply-k-pascal-reg))]
       [else
        (begin
         (set! a* (+ a* m*))
         (set! k* (outer-k-pascal-reg m* a* k*))
        (pascal-reg pascal-reg))])))
    (apply-k-pascal-reg)))])
     (begin
      (set! k* (inner-k-single-pascal-reg k*))
     (pascal-reg pascal-reg)))))

(define pascal-reg-driver
 (lambda (n)
  (begin 
   (set! n* n)
   (set! k* (empty-k-reg))
  (pascal-reg))))

;-----------------------------------------------------------------------------------------------------------------------------
;;testcase to execute

(ack-reg-driver 2 2)
;7
(ack-reg-driver 2 3)
;9
(depth-reg-driver '((((a) b (c (d))) e)))
;5
(fact-reg-driver 3)
;6
(fact-reg-driver 5)
;120
(pascal-reg-driver 10)
;(1 3 6 10 15 21 28 36 45 55)

