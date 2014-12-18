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
    ((lambda (h k)
       (h h (lambda (x) (x seed '() k))))
     (lambda (h k)
       (k (lambda (seed ans k)
            (p seed (lambda (pv)
                      (if pv (k ans) (h h (lambda (hh) (g seed (lambda (gs) (f seed (lambda (fs) (hh gs (cons fs ans) k))))))))))))) 
     k)))
;(cons (f seed) ans (lambda (x) (g seed (lambda (y) (k (x y))))))))))))  ;((h h) (g seed) (cons (f seed) ans))))))))


;(unfold-cps null? car cdr '(a b c d e) (empty-k))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))
(unfold-cps null?-cps car-cps cdr-cps '(a b c d e) (empty-k))
;(e d c b a)

