#lang racket

(define list (lambda a a))
(list 1 2 3 4)

(define-syntax macro-list
 (syntax-rules()
  ((_) '())
  ((_ b) `(,b))
  ((_ b0 b1 b2 ...)
   (let ((v b0))
    `(,b0 ,b1 ,b2 ...)))))

(macro-list)
;()
(macro-list 1 'b 2 'd)
;(1 b 2 d)

(macro-list 1 'b)

(macro-list '1)
