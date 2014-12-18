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


(define rember*1
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (rember*1 (car ls)))
          (cons (car ls) (rember*1 (cdr ls)))]
         [else (cons (rember*1 (car ls)) (cdr ls))])]
      [(eqv? (car ls) '?) (cdr ls)]
      [else (cons (car ls) (rember*1 (cdr ls)))])))

(rember*1 '(1 2 3 ? 4 5))
(rember*1 '(1 2 3 ((?) ?) 4 5))

(define rember*1-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (cond
         [(rember*1-cps (car ls) (lambda (x) (equal? (car ls) x)))
          (rember*1-cps (cdr ls) (lambda (y) (k (cons (car ls) y))))]
         [else (rember*1-cps (car ls) (lambda (p) (k (cons p (cdr ls)))))])]
      [(eqv? (car ls) '?) (k (cdr ls))]
      [else (rember*1-cps (cdr ls) (lambda (z) (k (cons (car ls) z))))])))


(rember*1-cps '(1 2 3 ? 4 5) (empty-k))
(rember*1-cps '(1 2 3 (7 ?) 4 5) (empty-k))
(rember*1-cps '(1 2 3 ((?) ?) 4 5) (empty-k))


