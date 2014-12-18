#lang racket
(require C311/pmatch)
(require C311/trace)

(define empty-k
 (lambda ()
  (list 'empty-k)))

(define v* 'kahii)

(define apply-k
 (lambda () ;(k v)
  (pmatch k*
   [`(empty-k) v*]
   [`(inner-k ,m^ ,k^) 
    ;(ack (sub1 m^) v* k^)]
    (begin
      (set! n1* (sub1 m^))
      (set! k* k^)
      (set! n2* v*)
      (ack))]
   [`(inner-k-depth ,l^ ,k^) 
    ;(let ((l^ (add1 l^))) (apply-k k^ (if (< l^ v) v l^)))]
    (begin
      (set! n* (add1 l^))
      (set! k* k^)
      (if (< n* v*)
          (set! v* v*)
          (set! v* n*))
      (apply-k))]      
   [`(outer-k-depth ,ls^ ,k^) 
    ;(depth (cdr ls^) (inner-k-depth v k^))]
    (begin
      (set! ls* (cdr ls^))
      (set! k* (inner-k-depth v* k^))
      (depth))]
   [`(inner-k-fact ,n^ ,k^) 
    ;(apply-k k^ (* n^ v*))]
    (begin
      (set! k* k^)
      (set! v* (* n^ v*))
      (apply-k))]
   [`(inner-k-pascal ,a^ ,k^) 
    ;(apply-k k^ (cons a^ v*))]
    (begin
      (set! k* k^)
      (set! a* a^)
      (set! v* (cons a* v*))
      (apply-k))]
   [`(outer-k-pascal ,m^ ,a^ ,k^) 
    ;(v* (add1 m^) a^ (inner-k-pascal a^ k^))]
    (begin
      (set! m* (add1 m^))
      (set! a* a^)
      (set! k* (inner-k-pascal a* k^))
      (v*))]
   [`(inner-k-single-pascal ,k^) 
    ;(v* 1 0 k^)])))
    (begin
      (set! m* 1)
      (set! a* 0)
      (set! k* k^)
      (v*))])))

(define inner-k
 (lambda (m^ k^)
  (list 'inner-k m^ k^)))


(define n1* 'kahitari)
(define n2* 'kahitari)

(define ack
 (lambda () ;(m n k)
   (cond
    [(zero? n1*) 
     ;(apply-k k* (add1 n2*))]
     (begin
       (set! v* (add1 n2*))
       (apply-k))]
    [(zero? n2*) 
     ;(ack (sub1 n1*) 1 k*)]
     (begin
       (set! n1* (sub1 n1*))
       (set! n2* 1)
       (ack))]
    [else 
     ;(ack m (sub1 n) (inner-k m k))])))
     (begin 
       (set! k* (inner-k n1* k*))
       (set! n2* (sub1 n2*))
       (ack))])))
       
(define ack-reg-driver
  (lambda (m n)
    (begin
      (set! n1* m)
      (set! n2* n)
      (set! k* (empty-k))
      (ack))))

;--------------------------------------------------------------------
(define inner-k-depth
 (lambda (l^ k^)
  (list 'inner-k-depth l^ k^)))

(define outer-k-depth
 (lambda (ls^ k^)
  (list 'outer-k-depth ls^ k^)))

(define n* 'kahii)
(define k* 'kahii)
(define ls* 'kahii)

(define depth
 (lambda () ;(ls k)
   (cond
    [(null? ls*) 
     ;(apply-k k* 1)]
     (begin
       (set! v* 1)
       (apply-k))]
    [(pair? (car ls*))
     ;(depth (car ls) (outer-k-depth ls* k*))]
     (begin
       (set! k* (outer-k-depth ls* k*))
       (set! ls* (car ls*))
       (depth))]
    [else 
     ;(depth (cdr ls) k)])))
     (begin
       (set! k* k*)
       (set! ls* (cdr ls*))
       (depth))])))


(define depth-reg-driver
  (lambda (ls)
    (begin 
      (set! ls* ls)
      (set! k* (empty-k))
      (depth))))

;--------------------------------------------------------------------
(define inner-k-fact
 (lambda (n^ k^)
  (list 'inner-k-fact n^ k^)))

(define fr* 'kahipan)

(define fact-reg
 (lambda () ;(n k)
   ((lambda (fact-reg)
     (fact-reg fact-reg))
    (lambda (fact-reg)
     (cond
      [(zero? n*) 
       (begin 
         (set! v* 1)
         (apply-k))]
      [else 
       ;(fact-reg fact-reg (sub1 n) (inner-k-reg-fact n k))]))
       (begin 
         (set! k* (inner-k-fact n* k*))
         (set! n* (sub1 n*))
         (fact-reg fact-reg))]
    )))))

(define fact-driver 
  (lambda (n)
    (begin
      (set! n* n)
      (set! k* (empty-k))
      (fact-reg))))

;--------------------------------------------------------------------
(define inner-k-pascal
 (lambda (a^ k^)
  (list 'inner-k-pascal a^ k^)))

(define outer-k-pascal
 (lambda (m^ a^ k^)
  (list 'outer-k-pascal m^ a^ k^)))

(define inner-k-single-pascal
 (lambda (k^)
  (list 'inner-k-single-pascal k^)))

(define m* 'kahipan)
(define a* 'anotherone)

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
                               (apply-k))]
                            [else
                             (begin
                               (set! a* (+ a* m*))
                               (set! k* (outer-k-pascal m* a* k*))
                               (pascal-reg pascal-reg))])))
               (apply-k)))])
      (begin
        (set! k* k*)
        (set! k* (inner-k-single-pascal k*))
        (pascal-reg pascal-reg)))))

(define pascal-driver
  (lambda (n)
    (begin 
      (set! n* n)
      (set! k* (empty-k))
      (pascal-reg))))

;-----------------------------------------------------------------------------------------------------------------------------
;;testcase to execute

(ack-reg-driver 2 2)
;7
(ack-reg-driver 2 3)
;9
(depth-reg-driver '((((a) b (c (d))) e)))
;5
(fact-driver 3)
;6
(fact-driver 5)
;120
(pascal-driver 10)
;(1 3 6 10 15 21 28 36 45 55)

