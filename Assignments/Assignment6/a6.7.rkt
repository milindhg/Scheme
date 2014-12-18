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

(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls) 
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(cons-cell-count '(1 2 (3 4 (4 5)) 3))

(cons-cell-count '(3))

(cons-cell-count '(2 . 5))

(define cons-cell-count-cps
  (lambda (ls k)
    (cond
      [(pair? ls) (cons-cell-count-cps (car ls) (lambda (x) (cons-cell-count-cps (cdr ls) (lambda (y) (k (add1 (+ x y)))))))]
      [else (k 0)])))

(cons-cell-count-cps '(1 2 (3 4 (4 5)) 3) (empty-k))

(cons-cell-count-cps '(3) (empty-k))

(cons-cell-count-cps '(2 . 5) (empty-k))

