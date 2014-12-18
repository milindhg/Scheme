#lang racket
(require C311/mk)
(require C311/numbers)
(provide (all-defined-out) (all-from-out C311/mk) (all-from-out C311/numbers))
(require C311/let-pair)
(require C311/trace)


(run 1 (q)
  (== 5 q)
  (conde
    [(conde [(== 5 q)
	     (== 6 q)])
     (== 5 q)]
    [(== q 5)]
    ))


(define my-length
  (lambda (ls)
    (cond
      ((null? ls) 0)
      (else (add1 (length (cdr ls)))))))

(my-length '(a b c d e))

(trace-define my-length-bin
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? ls) 
       (cons (my-length-bin (car ls)) (my-length-bin (cdr ls))))
      (else (cons '1 '())))))

(my-length-bin '(a b c d e))

