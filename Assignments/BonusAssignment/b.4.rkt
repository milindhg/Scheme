#lang racket

(define-syntax and*
 (syntax-rules ()
  ((_) #t) ;; 0
  ((_ b) b) ;; 1
  ((_ b0 b1 b ...) ;; 2*
   (let ((v b0))
    (if (and* b1 b ...)
     (and* b1 b ...)
     #f )))))

(and* #f)

(and*)

(and* 'a)

(and* 1 2 3)