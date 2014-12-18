#lang racket
(require C311/trace)
(require C311/pmatch)

(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

(times '(1 2 3 4 5))
;120
(times '(1 2 3 0 3))
;0

(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (x) (k (* (car ls) x))))])))

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(times-cps '(1 2 3 4 5) (empty-k))
;120
(times-cps '(1 2 3 0 3) (empty-k))
;0

