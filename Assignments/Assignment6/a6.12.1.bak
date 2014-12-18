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


(define walk
  (lambda (v ls)
    (cond
      [(symbol? v)
       (let ((p (assq v ls)))
         (cond
           [p (walk (cdr p) ls)]
           [else v]))]
      [else v])))


(define empty-s
  (lambda ()
    '()))
 
(define extend-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))
 
(define unify
  (lambda (v w s)
    (let ([v (walk v s)])
      (let ([w (walk w s)])
        (cond
          [(eqv? v w) s]
          [(symbol? v) (extend-s v w s)]
          [(symbol? w) (extend-s w v s)]
          [(and (pair? v) (pair? w))
           (let ((s (unify (car v) (car w) s)))
             (cond
               [s (unify (cdr v) (cdr w) s)]
               [else #f]))]
          [(equal? v w) s]
          [else #f])))))


(unify 'x 5 (empty-s))
;((x . 5))
(unify 'x 5 (unify 'y 6 (empty-s)))
;((x . 5) (y . 6))
(unify '(x y) '(5 6) (empty-s))
;((y . 6) (x . 5))
(unify 'x 5 (unify 'x 6 (empty-s)))
;#f
(unify '(x x) '(5 6) (empty-s))
;#f
(unify '(x y z) '(5 x y) (empty-s))
;((z . 5) (y . 5) (x . 5))


(define unify-cps
  (lambda (v w s k)
    (let ([v (walk-cps v s k)])
      (let ([w (walk-cps w s k)])
        (cond
          [(eqv? v w) (k s)]
          [(symbol? v) (k (extend-s v w s))]
          [(symbol? w) (k (extend-s w v s))]
          [(and (pair? v) (pair? w))
           (let ((s (unify-cps (car v) (car w) s)))
             (cond
               [s (unify-cps (cdr v) (cdr w) s)]
               [else #f]))]
          [(equal? v w) (k s)]
          [else #f])))))


(define walk-cps
  (lambda (v ls k)
    (cond
      [(symbol? v) (let ((p (assq v ls)))
         (cond
           [p (walk-cps (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))


(unify-cps 'x 5 (empty-s) (empty-k))
;((x . 5))
(unify-cps 'x 5 (unify-cps 'y 6 (empty-s)))
;((x . 5) (y . 6))
(unify-cps '(x y) '(5 6) (empty-s))
;((y . 6) (x . 5))
(unify-cps 'x 5 (unify-cps 'x 6 (empty-s)))
;#f
(unify-cps '(x x) '(5 6) (empty-s))
;#f
(unify-cps '(x y z) '(5 x y) (empty-s))
;((z . 5) (y . 5) (x . 5))
