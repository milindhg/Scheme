#lang racket
(require C311/numbers)
(require C311/mk)
(require C311/let-pair)
(provide (all-defined-out))

(define facto
  (lambda (n o)
    (conde
     [(== n '()) (== o (build-num 1))]
     ;[(== n (build-num 0)) (== o (build-num 1))]
     [(fresh (sub1num res)
            (minuso n (build-num 1) sub1num)
            (facto sub1num res)
            (*o res n o))]
     )))

(run 1 (q) (facto '(1) q))

(run 1 (q) (facto  q '(0 0 0 1 1 1 1)))
;((1 0 1))
 
(run 1 (q) (facto (build-num 5) q))
;((0 0 0 1 1 1 1))
 
(run 6 (q) (fresh (n1 n2) (facto n1 n2) (== `(,n1 ,n2) q)))
;((() (1)) 
; ((1) (1)) 
; ((0 1) (0 1)) 
; ((1 1) (0 1 1))
; ((0 0 1) (0 0 0 1 1)) 
; ((1 0 1) (0 0 0 1 1 1 1)))

