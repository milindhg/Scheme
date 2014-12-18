#lang racket


(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only 
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))


(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (g k) (k (lambda (x k) (g g k)))) k)))

(strange (lambda (x) x))

(strange-cps (lambda (x) x) (empty-k))


(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(use-of-strange (empty-k))


(define use-of-strange-cps
  (strange-cps 5 (lambda (a) (a 6 (lambda (b) (b 7 (lambda (x) (let ((strange^ x))
  ;(let ([strange^ (((strange 5) 6) 7)])
    (strange^ 8 (lambda (v1) (v1 9 (lambda (v2) (v2 10 (empty-k))))))))))))))

(use-of-strange-cps 7 (empty-k))


;(use-of-strange-cps 6 (use-of-strange-cps 7 (empty-k)))
