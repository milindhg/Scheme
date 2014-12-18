#lang racket
(require C311/pmatch) ;; <-- important
(require C311/trace)

(define vars
  (λ (e)
    (pmatch e
            (`,x (guard (symbol? x)) (list x) )            
            (`(lambda (,x) ,body) (cond ((eqv? x body) (vars x)) (else (vars body) )))            
            (`(,rator ,rand) (append (vars rator) (vars rand)))
            )))

(vars 'x)
;;(x)
(vars '(lambda (x) x))
;;(x)
(vars '((lambda (y) (x x)) (x y)))
;;(x x x y)
(vars '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;;(a z h h a)

