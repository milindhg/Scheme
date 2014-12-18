#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (cond
             ((zero? n) ls)
             (else (cdr (nth-cdr (sub1 n))) )
              ))))
      (car (nth-cdr n)))))

(list-ref '(a b c) 0)
(list-ref '(a b c) 1)
(list-ref '(a b c) 2)
(list-ref '(a b c d f g r e s c g h) 8)


