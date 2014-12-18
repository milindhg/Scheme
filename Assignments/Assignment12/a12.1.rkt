#lang racket
(require C311/let-pair)
(provide (all-defined-out))
(require C311/monads)

(define assv-maybe
  (lambda (x ls)
    (cond
      ((eqv? ls '()) (fail))
      ((eqv? (car (car ls)) x) (return-maybe (cdr (car ls))))
      (else (assv-maybe x (cdr ls))))))

(assv-maybe 'c '((a . 1) (b . 2) (c . 3)))
;(Just 3)
(assv-maybe 'd '((a . 1) (b . 2) (c . 3)))
;(Nothing)

