#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define-syntax cons$
 (syntax-rules ()
  ((cons$ x y) (cons x (delay y)))))

(define car$ car)

(define cdr$
 (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
 (lambda (n $)
  (cond
   ((zero? n) '())
   (else (cons (car$ $) (take$ (sub1 n) (cdr$ $)))))))

(take$ 5 inf-1s)
;(1 1 1 1 1)
(take$ 10 inf-1s)
;(1 1 1 1 1 1 1 1 1 1)

(define worst-random
 (delay (random 4)))

(force worst-random)
;2
(force worst-random)
;2
(force worst-random)
;2

(define trib
  (lambda (x1 x2 x3)
    
     (begin
       (let ([sum (+ x1 x2 x3)])
      (cons$ sum (trib x2 x3 sum))
      ))))


(define trib$ (cons$ 0 (cons$ 1 (cons$ 1 (trib 0 1 1)))))



(car$ trib$)
;0
(car$ (cdr$ trib$))
;1
(take$ 7 trib$)
;(0 1 1 2 4 7 13)