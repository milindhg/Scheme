#lang racket
(require C311/pmatch)
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)

(define fib-sps
 (lambda (n s)
  (cond
   ((assv n s) => (lambda (pr) `(,(cdr pr) . ,s)))
   ((< n 2) `(,n . ((,n . ,n) . ,s)))
   (else
    (let-pair ((v . s^) (fib-sps (sub1 (sub1 n)) s))
     (let-pair ((u . s^^) (fib-sps (sub1 n) s^))
      (let ((v+u (+ v u)))
       `(,v+u . ((,n . ,v+u) . ,s^^)))))))))


(fib-sps 0 '())
;(0 (0 . 0))
 
(fib-sps 1 '())
;(1 . ((1 . 1)))
 
(fib-sps 3 '())
;(2 (3 . 2) (2 . 1) (0 . 0) (1 . 1))
 
(fib-sps 10 '())
;(55
; (10 . 55)
; (9 . 34)
; (8 . 21)
; (7 . 13)
; (6 . 8)
; (5 . 5)
; (4 . 3)
; (3 . 2)
; (2 . 1)
; (1 . 1)
; (0 . 0))

(fib-sps 5 '())
;(5
; (5 . 5)
; (4 . 3)
; (3 . 2)
; (2 . 1)
; (1 . 1)
; (0 . 0))

