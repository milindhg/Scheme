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

(define count-syms*
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      [else (count-syms* (cdr ls))])))

(count-syms* '(a 1 b 2 c 3))
;3
(count-syms* '((a 1) (b 2) (c 3)))
;3
(count-syms* '(1 (b (3 (d (5 e) 7) (g)) 9) ((h))))
;5


(define count-syms*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls)) (count-syms*-cps (car ls) (lambda (x) (count-syms*-cps (cdr ls) (lambda (y) (k (+ x y))))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (x) (k (add1 x))))]
      [else (count-syms*-cps (cdr ls) k)])))

(count-syms*-cps '(a 1 b 2 c 3) (empty-k))
;3
(count-syms*-cps '((a 1) (b 2) (c 3)) (empty-k))
;3
(count-syms*-cps '(1 (b (3 (d (5 e) 7) (g)) 9) ((h))) (empty-k))
;5

