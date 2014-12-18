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

(walk 'a '((a . 5) (b . 6) (c . 7)))
;5
(walk 'a '((a . b) (b . c) (c . 7)))
;7
(walk 'a '((a . q) (r . s) (q . r)))
;s
(walk 'a '((a . q) (r . s) (q . r) (s . 10)))
;10


(define walk-cps
  (lambda (v ls k)
    (cond
      [(symbol? v) (let ((p (assq v ls)))
         (cond
           [p (walk-cps (cdr p) ls k)]
           [else (k v)]))]
      [else (k v)])))

(walk-cps 'a '((a . 5) (b . 6) (c . 7)) (empty-k))
;5
(walk-cps 'a '((a . b) (b . c) (c . 7)) (empty-k))
;7
(walk-cps 'a '((a . q) (r . s) (q . r)) (empty-k))
;s
(walk-cps 'a '((a . q) (r . s) (q . r) (s . 10)) (empty-k))
;10

