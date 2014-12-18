#lang racket
(require C311/numbers)
(require C311/mk)
(require C311/let-pair)
(provide (all-defined-out))

(define fibs
    (lambda (n)
      (cond
        ((eqv? n 0) (values 1 1))
        (else
         (let ((n- (- n 1)))
           (let-values (((u v) (fibs n-)))
             (let ((u+v (+ u v)))
               (values v u+v))))))))
(fibs 0)
;1
;1
(fibs 1)
;1
;2
(fibs 2)
;2
;3
(fibs 3)
;3
;5

;(define fibso
;  (lambda (num o1 o2)
;    (conde
;     [(== num '()) (== o1 (build-num 1)) (== o2 (build-num 1))]
;     [(fresh (ad sub1num res1 res2)
;             (== res2 o1)
;             (== ad o2)
;             (minuso num (build-num 1) sub1num)
;             (fibso sub1num res1 res2)
;             (pluso res1 res2 ad))])))

(define fibso
  (lambda (num o1 o2)
    (conde
     [(== num '()) (== o1 (build-num 1)) (== o2 (build-num 1))]
     [(fresh (ad sub1num res1 res2)
             (minuso num (build-num 1) sub1num)
             (fibso sub1num res1 res2)
             (pluso res1 res2 ad)
             (== res2 o1)
             (== ad o2)
             )])))
(run 4 (q) 
    (fresh (n o1 o2) 
      (== q `(,n ,o1 ,o2)) 
      (fibso n o1 o2)))
;((() (1) (1))
; ((1) (1) (0 1))
; ((0 1) (0 1) (1 1))
; ((1 1) (1 1) (1 0 1)))
(run 1 (q) 
    (fresh (n o1) 
      (== q `(,n ,o1))
      (fibso n o1 (build-num 5))))
;(((1 1) (1 1)))
(run 1 (q) 
    (fresh (n o2) 
      (== q `(,n ,o2))
      (fibso n (build-num 5) o2)))
;(((0 0 1) (0 0 0 1)))