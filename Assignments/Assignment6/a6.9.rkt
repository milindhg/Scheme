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

;; ack: computes the Ackermann function
;; (http://en.wikipedia.org/wiki/Ackermann_function).  Warning: if you
;; run this program with m >= 4 and n >= 2, you'll be in for a long
;; wait.
(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(ack 3 1)

(ack 1 1)

(ack 2 1)

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (x) (ack-cps (sub1 m) x k)))])))

(ack-cps 3 1 (empty-k))

(ack-cps 1 1 (empty-k))

(ack-cps 2 1 (empty-k))
