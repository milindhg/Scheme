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

(define binary-to-decimal
 (lambda (n)
  (cond
   [(null? n) 0]
   [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))


(binary-to-decimal '())
;0
(binary-to-decimal '(1))
;1
(binary-to-decimal '(0 1))
;2
(binary-to-decimal '(1 1 0 1))
;11


(define binary-to-decimal-cps
 (lambda (n k)
  (cond
   [(null? n) (k 0)]
   [else (binary-to-decimal-cps (cdr n) (lambda (x) (k (+ (car n) (* 2 x)))))])))

(binary-to-decimal-cps '() (empty-k))
;0
(binary-to-decimal-cps '(1) (empty-k))
;1
(binary-to-decimal-cps '(0 1) (empty-k))
;2
(binary-to-decimal-cps '(1 1 0 1) (empty-k))
;11

