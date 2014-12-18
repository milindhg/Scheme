#lang racket
(require C311/trace)
(require C311/pmatch)


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(fib 16)

(define fib-cps
  (lambda (n k)
    ((lambda (fib-cps k)
       (fib-cps fib-cps n k))
     (lambda (fib-cps n k)
       (cond
	 [(zero? n) (k 0)]
	 [(= 1 n) (k 1)]
	 [else (fib-cps fib-cps (sub1 (sub1 n)) (lambda (x) (fib-cps fib-cps (sub1 n) (lambda (y) (k (+ x y))))))])) k)))   


(fib-cps 16 (empty-k))

