#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define vars
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) x )
            (`(lambda (,x) ,x) `(,(vars x)))
            ;;(`(lambda (,x) ,body) (append `(,body ,x) `(,x)))            
            (`(,rator ,rand) `(,(vars rator) ,(vars rand)))
            )))

(vars 'x)
;;(x)
(vars '(lambda (x) x))
;;(x)
(vars '((lambda (y) (x x)) (x y)))
;;(x x x y)
(vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;;(a z h h a)

