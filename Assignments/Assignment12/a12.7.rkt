#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/monads)

(define state/sum
  (lambda (n)
    (do bind-state 
      (s <- get-state)
      (put-state (+ n s))
      (return-state s))))

((state/sum 5) 0)
;(0 . 5)
 
((state/sum 2) 0)
;(0 . 2)
 
((state/sum 2) 3)
;(3 . 5)

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

(define traverse-state/sum
    (traverse return-state bind-state state/sum))
 
((traverse-state/sum '((1 . 2) . (3 . (4 . 5)))) 0)
;(((0 . 1) 3 6 . 10) . 15)