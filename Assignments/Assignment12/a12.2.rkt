#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/trace)
(require C311/monads)

;WIthout the do syntax. This is required.
(define partition-writer
  (lambda (pred ls)
    (cond
      [(eqv? ls '()) (return-writer '())]
      [(pred (car ls)) (bind-writer (tell-writer (car ls)) (lambda (x) (partition-writer pred (cdr ls))))]
      [else (bind-writer (partition-writer pred (cdr ls)) (lambda (d) (return-writer (cons (car ls) d))))])))

;;With do syntax. This is tried to understand the do syntax for using bind-writer.
(define partition-writer2
  (lambda (pred ls)
    (cond
      [(eqv? ls '()) (return-writer '())]
      [(pred (car ls)) (do bind-writer (tell-writer (car ls)) (partition-writer pred (cdr ls)))]
      [else (do bind-writer (d <- (partition-writer pred (cdr ls))) (return-writer (cons (car ls) d)))])))


(partition-writer even? '())
;(())
(partition-writer even? '(1 2 3 4))
;((1 3) . (2 4))
(partition-writer even? '(1 2 3 4 5 6 7 8 9 10))
;((1 3 5 7 9) . (2 4 6 8 10))
(partition-writer odd? '(1 2 3 4 5 6 7 8 9 10))
;((2 4 6 8 10) . (1 3 5 7 9))

