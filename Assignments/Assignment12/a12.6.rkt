#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/monads)

;;With remainder function used.
(define halve2
  (lambda (n)
    (cond 
      [(zero? (remainder n 2)) (return-writer (/ n 2))]
      [else (bind-writer (tell-writer n) (lambda (_) (return-writer n)))])))

;;without remainder function used
(define halve
  (lambda (n)
    (cond 
      [(even? n) (return-writer (/ n 2))]
      [else (bind-writer (tell-writer n) (lambda (_) (return-writer n)))])))

(halve 0)
(halve 2)
(halve 3)
 
(halve 6)
;(3 . ())
 
(halve 5)
;(5 . (5))

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

(define traverse-halve
    (traverse return-writer bind-writer halve))
 
(traverse-halve '((1 . 2) . (3 . (4 . 5))))
;(((1 . 1) . (3 . (2 . 5))) . (1 3 5))

