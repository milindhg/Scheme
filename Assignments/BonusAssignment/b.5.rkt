#lang racket

(define-syntax cons*
 (syntax-rules()
  ((_) (raise-syntax-error 'cons* "Incorrect argument-count to cons*"))
  ((_ b) b)
  ((_ b0 b1 b2 ...)
   (let ((v b0))
    (cons v (cons* b1 b2 ...))))))

(cons* 'a 'b 'c 'd)
;(a b c . d)
(cons* 'a)
;a
(cons*)
;cons*: Incorrect argument-count to cons*