#lang racket
(require C311/trace)
(require C311/pmatch)

(define last-non-zero
  (lambda (ls) 
    (call/cc
     (lambda (k)
       (letrec
         ((lnz
           (lambda (ls)
             (cond
               ((null? ls) '())
               ((zero? (car ls)) (k (lnz (cdr ls))))
               (else (cons (car ls) (lnz (cdr ls))))
               ;;Try building a list and whenever a zero is occuring , try to break the loop and print the numbers after zero.
               )
	     )))
         (lnz ls))))))

(last-non-zero '(1 2 3 4 5 6 7 8 9 0 1 2 3 1))

(last-non-zero '(0))
;()
(last-non-zero '(1 2 3 0 4 5))
;(4 5)
(last-non-zero '(1 0 2 3 0 4 5))
;(4 5)
(last-non-zero '(1 2 3 4 5))
;(1 2 3 4 5)
