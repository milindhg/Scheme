#lang racket
(require C311/pmatch)
;;(require "a8-student-tests.rkt")
;;(test-file #:file-name "a8.rkt")


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Part I: Trampolinized version of the original programs and the corresponding drivers


(define trampoline
 (lambda (th)
  (trampoline (th))))

(define empty-k-trp
 (lambda (jumpout^)
  (list 'empty-k-trp jumpout^)))

;--------------------------------------------------------------------
(define inner-k-ack
 (lambda (m k^)
  (list 'inner-k-ack m k^)))

(define apply-k-ack
 (lambda (k v)
  (pmatch k
   [`(empty-k-trp ,jumpout^) (jumpout^ v)]
   [`(inner-k-ack ,m^ ,k^) (lambda () (ack (sub1 m^) v k^))])))

(define ack
 (lambda (m n k)
  (lambda ()
   (cond
    [(zero? m) (apply-k-ack k (add1 n))]
    [(zero? n) (lambda () (ack (sub1 m) 1 k))]
    [else (lambda () (ack m (sub1 n) (inner-k-ack m k)))]))))

(define ack-tramp-driver 
 (lambda (n1 n2)
  (call/cc 
   (lambda (jumpout^) 
    (trampoline (ack n1 n2 (empty-k-trp jumpout^)))))))

;--------------------------------------------------------------------
(define inner-k-depth
 (lambda (l^ k^)
  (list 'inner-k-depth l^ k^)))

(define outer-k-depth
 (lambda (ls^ k^)
  (list 'outer-k-depth ls^ k^)))
  
(define apply-k-depth
 (lambda (k v)
  (pmatch k
   [`(empty-k-trp ,jumpout^) (jumpout^ v)]
   [`(inner-k-depth ,l^ ,k^) (let ((l^ (add1 l^))) (apply-k-depth k^ (if (< l^ v) v l^)))]
   [`(outer-k-depth ,ls^ ,k^) (lambda () (depth (cdr ls^) (inner-k-depth v k^)))])))

(define depth
 (lambda (ls k)
  (lambda ()
   (cond
    [(null? ls) (apply-k-depth k 1)]
    [(pair? (car ls))
     (lambda () (depth (car ls) (outer-k-depth ls k)))]
    [else (lambda () (depth (cdr ls) k))]))))


(define depth-tramp-driver
 (lambda (ls) 
  (call/cc (lambda (jumpout^)(trampoline (depth ls (empty-k-trp jumpout^)))))))

;--------------------------------------------------------------------
(define inner-k-fact
 (lambda (n^ k^)
  (list 'inner-k-fact n^ k^)))

(define apply-k-fact
 (lambda (k v)
  (pmatch k
   [`(empty-k-trp ,jumpout^) (jumpout^ v)]
   [`(inner-k-fact ,n^ ,k^) (apply-k-fact k^ (* n^ v))])))

(define fact
 (lambda (n k)
  (lambda ()
   ((lambda (fact k)
     (fact fact n k))
    (lambda (fact n k)
     (cond
      [(zero? n) (apply-k-fact k 1)]
      [else (lambda () (fact fact (sub1 n) (inner-k-fact n k)))]))
    k))))

(define fact-tramp-driver 
 (lambda (num) 
  (call/cc 
   (lambda (jumpout^) 
    (trampoline (fact num (empty-k-trp jumpout^)))))))


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

(define apply-k-pascal
 (lambda (k v)
  (pmatch k
   [`(empty-k-trp ,jumpout^) (jumpout^ v)]
   [`(inner-k-pascal ,a^ ,k^) (apply-k-pascal k^ (cons a^ v))]
   [`(outer-k-pascal ,m^ ,a^ ,k^) (v (add1 m^) a^ (inner-k-pascal a^ k^))]
   [`(inner-k-single-pascal ,k^) (v 1 0 k^)])))

(define pascal
 (lambda (n k)
  (lambda ()
   (let 
    ((pascal
      (lambda (pascal k)
       (apply-k-pascal k 
       (lambda (m a k)
        (cond
         [(> m n) (apply-k-pascal k '())]
         [else 
          (lambda () 
           (let ((a (+ a m)))
            (lambda () (pascal pascal (outer-k-pascal m a k)))))]))))))
   (lambda () (pascal pascal (inner-k-single-pascal k)))))))

(define pascal-tramp-driver
 (lambda (n)
  (call/cc
   (lambda (jumpout^)
    (trampoline
     (pascal n (empty-k-trp jumpout^)))))))
;-----------------------------------------------------------------------------------------------------------------------------

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Part II: registerized version of the original without trampolining

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

;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Part III: Brainteaser
;Discussed approach for brainteaser with Renuka Deshmukh

(define empty-k
 (lambda (jumpout)
  (list 'empty-k jumpout)))

(define fib
 (lambda (n k)
  (lambda ()
   (cond
    [(= n 0) (apply-k k 1)]
    [(= n 1) (apply-k k 1)]
    [else (fib (sub1 n) (outer-k n k))]))))

(define inner-k
 (lambda (x^ k^)
  (list 'inner-k x^ k^)))

(define outer-k
 (lambda (n^ k^)
  (list 'outer-k n^ k^)))

(define apply-k
 (lambda (k v)
  (lambda ()
   (pmatch k
    [`(empty-k ,jumpout) (jumpout v)]
    [`(inner-k ,x^ ,k^) (apply-k k^ (+ x^ v))]
    [`(outer-k ,n^ ,k^) (fib (sub1 (sub1 n^)) (inner-k v k^))]))))

(define rampoline
  (lambda (th1 th2 th3)
        (rampoline (th1) (th2) (th3))))

(define fib-dr
 (lambda (n1 n2 n3)
  (call/cc
   (lambda (jumpout)
    (rampoline
     (lambda ()
      (fib n1 (empty-k jumpout)))
     (lambda ()
      (fib n2 (empty-k jumpout)))
     (lambda ()
      (fib n3 (empty-k jumpout))))))))  


;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------------------------------------------------------------------------------
