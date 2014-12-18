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


(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

(unfold null? car cdr '(a b c d e))
;(e d c b a)

(define unfold-cps
 (lambda (p f g seed k)
  (lambda (h k)
   ((h h) (lambda (x) (x seed '() (lambda (rat)
    (lambda (h k)
     (lambda (seed ans k)
      (p seed (lambda (b)
       (if b (k ans) (h h (lambda (v1) (g seed (lambda (v2) (f seed (lambda (v3) (v1 v2 (cons v3 ans) (lambda (rand) (rat rand k)))))))))))))))))))))
	     ;(cons (f seed) ans (lambda (x) (g seed (lambda (y) (k (x y))))))))))))  ;((h h) (g seed) (cons (f seed) ans))))))))


(unfold-cps null? car cdr '(a b c d e) (empty-k))

