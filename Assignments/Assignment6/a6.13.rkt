#lang racket


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))


(define M-cps
  (lambda (f k)
   (k (lambda (ls k)
      (cond
        ((null? ls) (k '()))
        (else (M-cps f (lambda (x) (x (cdr ls) (lambda (y) (f (car ls) (lambda (z) (k (cons z y))))))))))))))


(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

use-of-M

(define use-of-M-cps
  (M-cps (lambda (n k) (k (add1 n))) (lambda (x) (x  '(1 2 3 4 5) (empty-k)))))

use-of-M-cps 