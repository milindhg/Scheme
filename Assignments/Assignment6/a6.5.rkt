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

(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

((plus 2) 3)
((plus ((plus 2) 3)) 5)


(define plus-cps
  (lambda (m k)
    (k (lambda (n k)
      (k (+ m n))))))

;((plus-cps 2 (empty-k)) 3)

(plus-cps 2 (lambda (v) (v 3 (lambda (x) x))))

;((plus-cps ((plus-cps 2 (empty-k)) 3) (empty-k)) 5)

(plus-cps 2 (lambda (v) (v 3 (lambda (y) (plus-cps y (lambda (pp) (pp 5 (lambda (x) x))))))))
