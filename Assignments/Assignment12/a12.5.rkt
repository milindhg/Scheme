#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/monads)

(define reciprocal
  (lambda (n)
    (cond
      [(zero? n) (fail)]
      [else (return-maybe (/ 1 n))])))

(reciprocal 0)
;(Nothing)
 
(reciprocal 2)
;(Just 1/2)

(reciprocal 3)

(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))


(define traverse-reciprocal
    (traverse return-maybe bind-maybe reciprocal))
 
(traverse-reciprocal '((1 . 2) . (3 . (4 . 5))))
;(Just ((1 . 1/2) . (1/3 . (1/4 . 1/5))))
 
(traverse-reciprocal '((1 . 2) . (0 . (4 . 5))))
;(Nothing)