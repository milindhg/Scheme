#lang racket
(require C311/trace)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))


(define almost-length
    (lambda (f)
      (lambda (ls)
        (if (null? ls)
            0
            (add1 (f (cdr ls)))))))

(trace-define why-cps
  (trace-lambda first (f k)
    ((lambda (g k)
       (f (lambda (x k) (g g (lambda (v) (v x k)))) k))
     (lambda (g k)
       (f (lambda (x k) (g g (lambda (v) (v x k)))) k)) k)))




(trace-define almost-length-cps
    (trace-lambda seventh (f k)
      (k (trace-lambda eight (ls k)
        (if (null? ls)
            (k 0)
            (f (cdr ls) (lambda (x) (k (add1 x)))))))))
            ;(add1 (f (cdr ls)))))))


;((why almost-length) '(a b c d e))
;5

(why-cps almost-length-cps (lambda (x) (x '(a b c d e) (empty-k))))


(trace-define why-cps-cps
  (trace-lambda first (f k k1)
    ((lambda (g k k1)
       (f (lambda (x k k1) (g g (lambda (v k1) (v x k k1)) k1)) k k1))
     (lambda (g k k1)
       (f (lambda (x k k1) (g g (lambda (v k1) (v x k k1)) k1)) k k1)) k k1)))



(trace-define almost-length-cps-cps
    (trace-lambda seventh (f k k1)
      (k (trace-lambda eight (ls k k1)
        (if (null? ls)
            (k 0 k1)
            (f (cdr ls) (lambda (x k1) (k (add1 x) k1)) k1))) k1)))


;(why-cps-cps almost-length-cps-cps (lambda (x k) (x '(a b c d e) (lambda (v) k v) (lambda (v) v))))

((why-cps-cps almost-length-cps-cps (lambda(v k)(k v)) (lambda(v)v)) '(a b c d e) (lambda(v k)(k v)) (lambda(v)v))



;Dont forget to remove the let line from unify. No need of let. Directly write lambda 