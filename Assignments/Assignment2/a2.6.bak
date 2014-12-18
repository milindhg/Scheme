#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define lambda->lumbda
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) x)
            (`(lambda (,x) ,body) `(lumbda (,x) ,(lambda->lumbda body)))
            (`(,rator ,rand) `(,(lambda->lumbda rator) ,(lambda->lumbda rand))) 
            )))

(lambda->lumbda 'x)
      ;;x
(lambda->lumbda '(lambda (x) x))
      ;;(lumbda (x) x)
(lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
      ;;(lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a)))))
(lambda->lumbda '(lambda (lambda) lambda)) 
      ;;(lumbda (lambda) lambda)
(lambda->lumbda '((lambda (lambda) lambda) (lambda (y) y)))
      ;;((lumbda (lambda) lambda) (lumbda (y) y))

