#lang racket
(require C311/pmatch)
(require C311/trace)

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
;(lambda (y) (k (+ x y)))

(define outer-k
  (lambda (n^ k^)
    (list 'outer-k n^ k^)))
;(lambda (x) (fib (sub1 (sub1 n)) (inner-k x k)))

(define apply-k
  (lambda (k v)
    (lambda ()
    (pmatch k
            [`(empty-k ,jumpout) (jumpout v)]
            [`(inner-k ,x^ ,k^) (apply-k k^ (+ x^ v))]
            [`(outer-k ,n^ ,k^) (fib (sub1 (sub1 n^)) (inner-k v k^))]))))
            ;[else (k v)])))

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

;(fib 5 (empty-k))

(fib-dr 5 6 4)
(fib-dr 5 6 4)
(fib-dr 5 6 4)
(fib-dr 5 6 4)
(fib-dr 5 6 4)
(fib-dr 5 4 6)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)
(fib-dr 5 3 2)

;(rampoline 1 2 3)